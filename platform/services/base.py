"""Service Base - 統一サービス基盤.

このモジュールは、全サービスの基底クラスとイベント定義を提供します。

設計原則:
- フロントエンド非依存: API/CLI/Studio 全て同じインターフェース
- 3種類の実行モード:
  1. execute() - 結果のみ返す（API向け）
  2. execute_stream() - AsyncIterator（WebSocket/SSE向け）
  3. execute_with_callback() - コールバック（CLI進捗表示向け）
"""

from __future__ import annotations

import asyncio
import logging
import time
import uuid
from abc import ABC, abstractmethod
from collections.abc import AsyncIterator, Callable
from datetime import datetime
from enum import Enum
from typing import Any, TypeVar

from pydantic import BaseModel, Field


# =============================================================================
# イベントタイプ（全モード共通）
# =============================================================================


class ServiceEventType(str, Enum):
    """サービスイベントタイプ.

    API/CLI/Studio 全てで共通のイベント種別。
    """

    # ライフサイクル
    START = "start"
    COMPLETE = "complete"
    ERROR = "error"
    CANCEL = "cancel"

    # 進捗
    PROGRESS = "progress"
    PHASE = "phase"  # フェーズ変更

    # ログ
    LOG = "log"
    DEBUG = "debug"

    # Agent/Workflow 固有
    AGENT_START = "agent.start"
    AGENT_COMPLETE = "agent.complete"
    TOOL_CALL = "tool.call"
    TOOL_RESULT = "tool.result"

    # HITL
    APPROVAL_REQUIRED = "approval.required"
    APPROVAL_RESPONSE = "approval.response"
    INPUT_REQUIRED = "input.required"


class LogLevel(str, Enum):
    """ログレベル."""

    DEBUG = "debug"
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"


# =============================================================================
# イベントモデル
# =============================================================================


class ServiceEvent(BaseModel):
    """サービスイベント基底クラス.

    全てのイベントはこのクラスを継承。
    API/CLI/Studio で同じフォーマット。
    """

    id: str = Field(default_factory=lambda: f"evt_{uuid.uuid4().hex[:12]}")
    type: ServiceEventType = Field(...)
    timestamp: float = Field(default_factory=time.time)
    execution_id: str = Field(default="", description="実行ID")

    # 共通データ
    message: str = Field(default="")
    data: dict[str, Any] = Field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return self.model_dump()

    def to_json(self) -> str:
        """JSON文字列に変換."""
        import json

        return json.dumps(self.to_dict(), ensure_ascii=False, default=str)

    def to_sse(self) -> str:
        """SSE形式に変換."""
        return f"event: {self.type.value}\ndata: {self.to_json()}\n\n"


class ProgressEvent(ServiceEvent):
    """進捗イベント.

    CLI の進捗バー、Studio の進捗表示、API のポーリングで使用。
    """

    type: ServiceEventType = Field(default=ServiceEventType.PROGRESS)
    progress: float = Field(default=0.0, ge=0.0, le=100.0, description="進捗率 0-100%")
    current_step: int = Field(default=0, description="現在のステップ")
    total_steps: int = Field(default=0, description="総ステップ数")
    phase: str = Field(default="", description="現在のフェーズ名")


class LogEvent(ServiceEvent):
    """ログイベント.

    CLI の出力、Studio のログパネル、API のログエンドポイントで使用。
    """

    type: ServiceEventType = Field(default=ServiceEventType.LOG)
    level: LogLevel = Field(default=LogLevel.INFO)
    source: str = Field(default="", description="ログ発生元")


class ResultEvent(ServiceEvent):
    """結果イベント.

    実行完了時の結果。
    """

    type: ServiceEventType = Field(default=ServiceEventType.COMPLETE)
    result: dict[str, Any] = Field(default_factory=dict)
    duration_ms: float = Field(default=0.0, description="実行時間（ミリ秒）")


class ErrorEvent(ServiceEvent):
    """エラーイベント."""

    type: ServiceEventType = Field(default=ServiceEventType.ERROR)
    error_code: str = Field(default="unknown_error")
    error_message: str = Field(default="")
    recoverable: bool = Field(default=False, description="リカバリ可能か")
    stack_trace: str | None = Field(default=None)


class ApprovalRequiredEvent(ServiceEvent):
    """承認要求イベント（HITL）."""

    type: ServiceEventType = Field(default=ServiceEventType.APPROVAL_REQUIRED)
    action: str = Field(..., description="承認対象アクション")
    reason: str = Field(default="", description="承認理由")
    options: list[str] = Field(default_factory=lambda: ["approve", "reject"])
    timeout_seconds: int | None = Field(default=None)


# =============================================================================
# 結果・エラーモデル
# =============================================================================


class ServiceResult(BaseModel):
    """サービス実行結果.

    全サービスの統一結果フォーマット。
    """

    success: bool = Field(default=True)
    execution_id: str = Field(default_factory=lambda: f"exec_{uuid.uuid4().hex[:12]}")
    data: dict[str, Any] = Field(default_factory=dict)
    duration_ms: float = Field(default=0.0)
    timestamp: datetime = Field(default_factory=datetime.now)
    metadata: dict[str, Any] = Field(default_factory=dict)

    # エラー情報（失敗時のみ）
    error_code: str | None = Field(default=None)
    error_message: str | None = Field(default=None)


class ServiceError(Exception):
    """サービスエラー.

    全サービスの統一例外。
    """

    def __init__(
        self,
        code: str,
        message: str,
        *,
        recoverable: bool = False,
        data: dict[str, Any] | None = None,
        cause: Exception | None = None,
    ) -> None:
        """初期化."""
        self.code = code
        self.message = message
        self.recoverable = recoverable
        self.data = data or {}
        self.cause = cause
        super().__init__(message)

    def to_event(self, execution_id: str = "") -> ErrorEvent:
        """ErrorEvent に変換."""
        import traceback

        return ErrorEvent(
            execution_id=execution_id,
            error_code=self.code,
            error_message=self.message,
            message=self.message,
            recoverable=self.recoverable,
            data=self.data,
            stack_trace=traceback.format_exc() if self.cause else None,
        )


# =============================================================================
# コールバック型定義
# =============================================================================

# イベントコールバック（全イベント受信）
EventCallback = Callable[[ServiceEvent], None]

# 進捗コールバック（進捗のみ受信、CLI向け）
ProgressCallback = Callable[[float, str], None]  # (progress%, message)


# =============================================================================
# サービス基底クラス
# =============================================================================

T = TypeVar("T")


class ServiceBase[T](ABC):
    """サービス基底クラス.

    全サービスはこのクラスを継承。
    3つの実行モードを提供:
    1. execute() - 結果のみ返す
    2. execute_stream() - イベントストリーム
    3. execute_with_callback() - コールバック

    サブクラスは _execute_internal() を実装する。
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)

    # =========================================================================
    # 公開API（フロントエンドが呼び出す）
    # =========================================================================

    async def execute(self, **kwargs: Any) -> ServiceResult:
        """実行して結果のみ返す（API向け）.

        Args:
            **kwargs: 実行パラメータ

        Returns:
            実行結果
        """
        execution_id = f"exec_{uuid.uuid4().hex[:12]}"
        start_time = time.time()

        try:
            # 内部実行（イベントは無視）
            result_data = {}
            async for event in self._execute_internal(execution_id, **kwargs):
                if event.type == ServiceEventType.COMPLETE:
                    if isinstance(event, ResultEvent):
                        result_data = event.result
                elif event.type == ServiceEventType.ERROR:
                    if isinstance(event, ErrorEvent):
                        return ServiceResult(
                            success=False,
                            execution_id=execution_id,
                            duration_ms=(time.time() - start_time) * 1000,
                            error_code=event.error_code,
                            error_message=event.error_message,
                        )

            return ServiceResult(
                success=True,
                execution_id=execution_id,
                data=result_data,
                duration_ms=(time.time() - start_time) * 1000,
            )

        except ServiceError as e:
            return ServiceResult(
                success=False,
                execution_id=execution_id,
                duration_ms=(time.time() - start_time) * 1000,
                error_code=e.code,
                error_message=e.message,
            )
        except Exception as e:
            self._logger.exception("Unexpected error")
            return ServiceResult(
                success=False,
                execution_id=execution_id,
                duration_ms=(time.time() - start_time) * 1000,
                error_code="internal_error",
                error_message=str(e),
            )

    async def execute_stream(self, **kwargs: Any) -> AsyncIterator[ServiceEvent]:
        """イベントストリームを返す（WebSocket/SSE向け）.

        Args:
            **kwargs: 実行パラメータ

        Yields:
            イベント
        """
        execution_id = f"exec_{uuid.uuid4().hex[:12]}"

        # 開始イベント
        yield ServiceEvent(
            type=ServiceEventType.START,
            execution_id=execution_id,
            message="Execution started",
        )

        try:
            async for event in self._execute_internal(execution_id, **kwargs):
                yield event

        except ServiceError as e:
            yield e.to_event(execution_id)
        except Exception as e:
            self._logger.exception("Unexpected error")
            yield ErrorEvent(
                execution_id=execution_id,
                error_code="internal_error",
                error_message=str(e),
                message=str(e),
            )

    async def execute_with_callback(
        self,
        on_event: EventCallback | None = None,
        on_progress: ProgressCallback | None = None,
        **kwargs: Any,
    ) -> ServiceResult:
        """コールバック付き実行（CLI向け）.

        Args:
            on_event: 全イベント受信コールバック
            on_progress: 進捗のみ受信コールバック
            **kwargs: 実行パラメータ

        Returns:
            実行結果
        """
        execution_id = f"exec_{uuid.uuid4().hex[:12]}"
        start_time = time.time()
        result_data = {}

        try:
            async for event in self._execute_internal(execution_id, **kwargs):
                # コールバック呼び出し
                if on_event:
                    on_event(event)
                if on_progress and isinstance(event, ProgressEvent):
                    on_progress(event.progress, event.message)

                # 結果を保存
                if event.type == ServiceEventType.COMPLETE:
                    if isinstance(event, ResultEvent):
                        result_data = event.result
                elif event.type == ServiceEventType.ERROR:
                    if isinstance(event, ErrorEvent):
                        return ServiceResult(
                            success=False,
                            execution_id=execution_id,
                            duration_ms=(time.time() - start_time) * 1000,
                            error_code=event.error_code,
                            error_message=event.error_message,
                        )

            return ServiceResult(
                success=True,
                execution_id=execution_id,
                data=result_data,
                duration_ms=(time.time() - start_time) * 1000,
            )

        except ServiceError as e:
            if on_event:
                on_event(e.to_event(execution_id))
            return ServiceResult(
                success=False,
                execution_id=execution_id,
                duration_ms=(time.time() - start_time) * 1000,
                error_code=e.code,
                error_message=e.message,
            )
        except Exception as e:
            self._logger.exception("Unexpected error")
            return ServiceResult(
                success=False,
                execution_id=execution_id,
                duration_ms=(time.time() - start_time) * 1000,
                error_code="internal_error",
                error_message=str(e),
            )

    def execute_sync(self, **kwargs: Any) -> ServiceResult:
        """同期実行（CLI簡易モード向け）.

        Args:
            **kwargs: 実行パラメータ

        Returns:
            実行結果
        """
        return asyncio.run(self.execute(**kwargs))

    # =========================================================================
    # サブクラス実装必須
    # =========================================================================

    @abstractmethod
    def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """内部実行ロジック.

        サブクラスで実装必須。
        進捗イベントを yield しながら処理を進める。

        Args:
            execution_id: 実行ID
            **kwargs: 実行パラメータ

        Yields:
            イベント
        """

    # =========================================================================
    # ヘルパーメソッド（サブクラス用）
    # =========================================================================

    def _emit_progress(
        self,
        execution_id: str,
        progress: float,
        message: str = "",
        phase: str = "",
        current_step: int = 0,
        total_steps: int = 0,
    ) -> ProgressEvent:
        """進捗イベントを作成."""
        return ProgressEvent(
            execution_id=execution_id,
            progress=progress,
            message=message,
            phase=phase,
            current_step=current_step,
            total_steps=total_steps,
        )

    def _emit_log(
        self,
        execution_id: str,
        message: str,
        level: LogLevel = LogLevel.INFO,
        source: str = "",
    ) -> LogEvent:
        """ログイベントを作成."""
        return LogEvent(
            execution_id=execution_id,
            message=message,
            level=level,
            source=source or self.__class__.__name__,
        )

    def _emit_result(
        self,
        execution_id: str,
        result: dict[str, Any],
        duration_ms: float = 0.0,
    ) -> ResultEvent:
        """結果イベントを作成."""
        return ResultEvent(
            execution_id=execution_id,
            result=result,
            duration_ms=duration_ms,
            message="Execution completed",
        )

    def _emit_error(
        self,
        execution_id: str,
        code: str,
        message: str,
        recoverable: bool = False,
    ) -> ErrorEvent:
        """エラーイベントを作成."""
        return ErrorEvent(
            execution_id=execution_id,
            error_code=code,
            error_message=message,
            message=message,
            recoverable=recoverable,
        )


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    "ApprovalRequiredEvent",
    "ErrorEvent",
    # コールバック
    "EventCallback",
    "LogEvent",
    "LogLevel",
    "ProgressCallback",
    "ProgressEvent",
    "ResultEvent",
    # 基底クラス
    "ServiceBase",
    "ServiceError",
    # イベントモデル
    "ServiceEvent",
    # イベントタイプ
    "ServiceEventType",
    # 結果・エラー
    "ServiceResult",
]
