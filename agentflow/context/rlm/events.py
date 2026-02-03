"""RLM Events - AG-UI イベント定義.

RLM (Recursive Language Model) の進行状況を報告するための
AG-UI プロトコルイベント定義。

イベントタイプ:
- rlm.start: RLM処理開始
- rlm.iteration: イテレーション進行
- rlm.subcall: サブコール実行
- rlm.action: アクション実行
- rlm.complete: RLM処理完了
- rlm.error: エラー発生

使用例:
    >>> event = RLMStartEvent(
    ...     flow_id="flow-123",
    ...     total_contexts=3,
    ...     total_tokens=45000,
    ... )
    >>> sse_string = event.to_sse()
"""

from __future__ import annotations

import json
import time
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class RLMEventType(str, Enum):
    """RLM イベントタイプ."""

    # ライフサイクルイベント
    RLM_START = "rlm.start"
    RLM_COMPLETE = "rlm.complete"
    RLM_ERROR = "rlm.error"

    # 進行イベント
    RLM_ITERATION = "rlm.iteration"
    RLM_SUBCALL = "rlm.subcall"
    RLM_ACTION = "rlm.action"

    # ワークスペースイベント
    RLM_WORKSPACE_UPDATE = "rlm.workspace_update"


class RLMEvent(BaseModel):
    """RLM イベントベースクラス.

    全ての RLM イベントの基底クラス。
    AG-UI プロトコルと互換性を持つ。
    """

    event_type: RLMEventType = Field(..., description="イベントタイプ")
    timestamp: float = Field(default_factory=time.time, description="イベント発生時刻")
    flow_id: str = Field(..., description="フロー実行 ID")
    data: dict[str, Any] = Field(default_factory=dict, description="イベントデータ")

    def to_dict(self) -> dict[str, Any]:
        """イベントを辞書形式に変換.

        Returns:
            SSE 送信用の辞書
        """
        result = self.model_dump(exclude_none=True)
        if "event_type" in result:
            result["event_type"] = self.event_type.value
        return result

    def to_sse(self, ensure_ascii: bool = False) -> str:
        """SSE 形式の文字列に変換.

        Args:
            ensure_ascii: ASCII エスケープするか

        Returns:
            "data: {JSON}\\n\\n" 形式の文字列
        """
        return f"data: {json.dumps(self.to_dict(), ensure_ascii=ensure_ascii)}\n\n"


class RLMStartEvent(RLMEvent):
    """RLM処理開始イベント.

    RLMモードが有効化され、処理が開始されたときに発行。
    """

    event_type: RLMEventType = Field(default=RLMEventType.RLM_START)
    total_contexts: int = Field(..., description="処理対象コンテキスト数")
    total_tokens: int = Field(..., description="総入力Token数")
    activation_reason: str = Field(default="", description="発動理由")
    config_summary: dict[str, Any] = Field(
        default_factory=dict,
        description="設定サマリ",
    )


class RLMIterationEvent(RLMEvent):
    """RLMイテレーションイベント.

    メインループの各イテレーションで発行。
    """

    event_type: RLMEventType = Field(default=RLMEventType.RLM_ITERATION)
    iteration: int = Field(..., description="現在のイテレーション番号")
    max_iterations: int = Field(..., description="最大イテレーション数")
    confidence: float = Field(..., description="現在の信頼度 (0-1)")
    action_planned: str = Field(default="", description="計画されたアクション")
    budget_remaining: dict[str, int] = Field(
        default_factory=dict,
        description="残り予算（tokens, calls）",
    )


class RLMSubcallEvent(RLMEvent):
    """RLMサブコールイベント.

    LLMサブコール（要約、抽出等）を実行したときに発行。
    """

    event_type: RLMEventType = Field(default=RLMEventType.RLM_SUBCALL)
    subcall_type: str = Field(..., description="サブコールタイプ")
    tokens_used: int = Field(..., description="消費Token数")
    total_tokens_used: int = Field(..., description="累計消費Token数")
    result_summary: str = Field(default="", description="結果要約")


class RLMActionEvent(RLMEvent):
    """RLMアクションイベント.

    コンテキスト操作（peek, search等）を実行したときに発行。
    """

    event_type: RLMEventType = Field(default=RLMEventType.RLM_ACTION)
    action_type: str = Field(..., description="アクションタイプ")
    target_handle: str = Field(default="", description="対象コンテキストハンドル")
    parameters: dict[str, Any] = Field(default_factory=dict, description="パラメータ")
    result_count: int = Field(default=0, description="結果件数")
    execution_time_ms: float = Field(default=0.0, description="実行時間（ミリ秒）")


class RLMWorkspaceUpdateEvent(RLMEvent):
    """RLMワークスペース更新イベント.

    ワークスペースに変数が追加/更新されたときに発行。
    """

    event_type: RLMEventType = Field(default=RLMEventType.RLM_WORKSPACE_UPDATE)
    variable_name: str = Field(..., description="変数名")
    variable_type: str = Field(..., description="変数タイプ")
    operation: str = Field(default="set", description="操作（set/delete）")
    workspace_size: int = Field(default=0, description="ワークスペースサイズ")
    workspace_tokens: int = Field(default=0, description="ワークスペースToken数")


class RLMCompleteEvent(RLMEvent):
    """RLM処理完了イベント.

    RLM処理が正常に完了したときに発行。
    """

    event_type: RLMEventType = Field(default=RLMEventType.RLM_COMPLETE)
    stop_reason: str = Field(..., description="停止理由")
    total_iterations: int = Field(..., description="実行イテレーション数")
    total_subcalls: int = Field(..., description="実行サブコール数")
    total_tokens_used: int = Field(..., description="消費Token数")
    final_confidence: float = Field(..., description="最終信頼度")
    has_answer: bool = Field(..., description="回答が見つかったか")
    answer_preview: str = Field(default="", description="回答プレビュー（先頭100文字）")


class RLMErrorEvent(RLMEvent):
    """RLMエラーイベント.

    RLM処理中にエラーが発生したときに発行。
    """

    event_type: RLMEventType = Field(default=RLMEventType.RLM_ERROR)
    error_message: str = Field(..., description="エラーメッセージ")
    error_type: str = Field(..., description="エラータイプ")
    iteration: int = Field(default=0, description="エラー発生イテレーション")
    recoverable: bool = Field(default=False, description="復旧可能か")


class RLMEventEmitter:
    """RLMイベントエミッター.

    RLMコントローラーからイベントを発行するためのヘルパー。

    使用例:
        >>> emitter = RLMEventEmitter(flow_id="flow-123")
        >>> emitter.set_callback(lambda e: print(e.to_sse()))
        >>> emitter.emit_start(total_contexts=3, total_tokens=45000)
    """

    def __init__(
        self,
        flow_id: str,
        callback: Any = None,
        enabled: bool = True,
    ) -> None:
        """初期化.

        Args:
            flow_id: フローID
            callback: イベントコールバック関数
            enabled: イベント発行を有効にするか
        """
        self._flow_id = flow_id
        self._callback = callback
        self._enabled = enabled

    def set_callback(self, callback: Any) -> None:
        """コールバックを設定.

        Args:
            callback: イベントコールバック関数
        """
        self._callback = callback

    def _emit(self, event: RLMEvent) -> None:
        """イベントを発行.

        Args:
            event: 発行するイベント
        """
        if self._enabled and self._callback:
            self._callback(event.to_dict())

    def emit_start(
        self,
        total_contexts: int,
        total_tokens: int,
        activation_reason: str = "",
        config_summary: dict[str, Any] | None = None,
    ) -> None:
        """開始イベントを発行.

        Args:
            total_contexts: コンテキスト数
            total_tokens: 総Token数
            activation_reason: 発動理由
            config_summary: 設定サマリ
        """
        event = RLMStartEvent(
            flow_id=self._flow_id,
            total_contexts=total_contexts,
            total_tokens=total_tokens,
            activation_reason=activation_reason,
            config_summary=config_summary or {},
        )
        self._emit(event)

    def emit_iteration(
        self,
        iteration: int,
        max_iterations: int,
        confidence: float,
        action_planned: str = "",
        budget_remaining: dict[str, int] | None = None,
    ) -> None:
        """イテレーションイベントを発行.

        Args:
            iteration: イテレーション番号
            max_iterations: 最大イテレーション数
            confidence: 信頼度
            action_planned: 計画アクション
            budget_remaining: 残り予算
        """
        event = RLMIterationEvent(
            flow_id=self._flow_id,
            iteration=iteration,
            max_iterations=max_iterations,
            confidence=confidence,
            action_planned=action_planned,
            budget_remaining=budget_remaining or {},
        )
        self._emit(event)

    def emit_subcall(
        self,
        subcall_type: str,
        tokens_used: int,
        total_tokens_used: int,
        result_summary: str = "",
    ) -> None:
        """サブコールイベントを発行.

        Args:
            subcall_type: サブコールタイプ
            tokens_used: 消費Token数
            total_tokens_used: 累計Token数
            result_summary: 結果要約
        """
        event = RLMSubcallEvent(
            flow_id=self._flow_id,
            subcall_type=subcall_type,
            tokens_used=tokens_used,
            total_tokens_used=total_tokens_used,
            result_summary=result_summary,
        )
        self._emit(event)

    def emit_action(
        self,
        action_type: str,
        target_handle: str = "",
        parameters: dict[str, Any] | None = None,
        result_count: int = 0,
        execution_time_ms: float = 0.0,
    ) -> None:
        """アクションイベントを発行.

        Args:
            action_type: アクションタイプ
            target_handle: 対象ハンドル
            parameters: パラメータ
            result_count: 結果件数
            execution_time_ms: 実行時間
        """
        event = RLMActionEvent(
            flow_id=self._flow_id,
            action_type=action_type,
            target_handle=target_handle,
            parameters=parameters or {},
            result_count=result_count,
            execution_time_ms=execution_time_ms,
        )
        self._emit(event)

    def emit_workspace_update(
        self,
        variable_name: str,
        variable_type: str,
        operation: str = "set",
        workspace_size: int = 0,
        workspace_tokens: int = 0,
    ) -> None:
        """ワークスペース更新イベントを発行.

        Args:
            variable_name: 変数名
            variable_type: 変数タイプ
            operation: 操作
            workspace_size: ワークスペースサイズ
            workspace_tokens: ワークスペースToken数
        """
        event = RLMWorkspaceUpdateEvent(
            flow_id=self._flow_id,
            variable_name=variable_name,
            variable_type=variable_type,
            operation=operation,
            workspace_size=workspace_size,
            workspace_tokens=workspace_tokens,
        )
        self._emit(event)

    def emit_complete(
        self,
        stop_reason: str,
        total_iterations: int,
        total_subcalls: int,
        total_tokens_used: int,
        final_confidence: float,
        has_answer: bool,
        answer_preview: str = "",
    ) -> None:
        """完了イベントを発行.

        Args:
            stop_reason: 停止理由
            total_iterations: イテレーション数
            total_subcalls: サブコール数
            total_tokens_used: Token消費数
            final_confidence: 最終信頼度
            has_answer: 回答有無
            answer_preview: 回答プレビュー
        """
        event = RLMCompleteEvent(
            flow_id=self._flow_id,
            stop_reason=stop_reason,
            total_iterations=total_iterations,
            total_subcalls=total_subcalls,
            total_tokens_used=total_tokens_used,
            final_confidence=final_confidence,
            has_answer=has_answer,
            answer_preview=answer_preview[:100] if answer_preview else "",
        )
        self._emit(event)

    def emit_error(
        self,
        error_message: str,
        error_type: str,
        iteration: int = 0,
        recoverable: bool = False,
    ) -> None:
        """エラーイベントを発行.

        Args:
            error_message: エラーメッセージ
            error_type: エラータイプ
            iteration: イテレーション番号
            recoverable: 復旧可能か
        """
        event = RLMErrorEvent(
            flow_id=self._flow_id,
            error_message=error_message,
            error_type=error_type,
            iteration=iteration,
            recoverable=recoverable,
        )
        self._emit(event)
