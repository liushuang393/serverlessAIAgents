# -*- coding: utf-8 -*-
"""Context Bridge - 統合コンテキスト管理モジュール.

既存システムからAgentFlowを呼び出す際の統一コンテキスト受け渡し規約を提供。
セッション、ユーザー、テナント、業務コンテキストを標準化された形式で受け渡す。

設計原則:
- 統一規約: 全ての呼び出し元で同じ形式
- 追跡可能: 分散トレーシングとの連携
- 拡張可能: 業務固有情報を柔軟に追加

使用例:
    >>> from agentflow.integrations.context_bridge import FlowContext, ContextBridge
    >>>
    >>> # コンテキスト作成
    >>> ctx = FlowContext(
    ...     session_id="sess-123",
    ...     user_id="user-456",
    ...     tenant_id="tenant-789",
    ...     source_system="crm",
    ... )
    >>>
    >>> # ブリッジ経由でフロー実行
    >>> bridge = ContextBridge()
    >>> result = await bridge.invoke_flow(flow, ctx, inputs)
"""

from __future__ import annotations

import logging
import uuid
from contextvars import ContextVar
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

if TYPE_CHECKING:
    from agentflow.flow.flow import Flow


# コンテキスト変数（スレッドローカル相当）
_current_context: ContextVar["FlowContext | None"] = ContextVar(
    "current_flow_context", default=None
)

logger = logging.getLogger(__name__)


class SourceSystemType(str, Enum):
    """呼び出し元システムタイプ."""

    WEB_APP = "web_app"          # Webアプリケーション
    MOBILE_APP = "mobile_app"    # モバイルアプリ
    API = "api"                  # 外部API
    INTERNAL = "internal"        # 内部システム
    BATCH = "batch"              # バッチ処理
    WORKFLOW = "workflow"        # ワークフローエンジン
    CHAT = "chat"                # チャットシステム
    CLI = "cli"                  # コマンドライン


class FlowContext(BaseModel):
    """統合フローコンテキスト.

    既存システムからAgentFlowを呼び出す際の標準コンテキスト。
    全ての呼び出しでこのコンテキストを通じて情報を受け渡す。

    Attributes:
        session_id: セッション識別子
        user_id: ユーザー識別子
        tenant_id: テナント識別子
        source_system: 呼び出し元システム識別子
        source_system_type: 呼び出し元システムタイプ
        trace_id: 分散トレーシングID
        span_id: 現在のスパンID
        business_context: 業務固有情報
        user_context: ユーザー関連情報（ロール、部署等）
        request_metadata: リクエストメタデータ
    """

    # 必須識別子
    session_id: str = Field(..., description="セッション識別子")
    user_id: str = Field(..., description="ユーザー識別子")
    tenant_id: str = Field(..., description="テナント識別子")

    # 呼び出し元情報
    source_system: str = Field(..., description="呼び出し元システム名")
    source_system_type: SourceSystemType = Field(
        default=SourceSystemType.WEB_APP,
        description="呼び出し元システムタイプ",
    )

    # トレーシング
    trace_id: str = Field(
        default_factory=lambda: uuid.uuid4().hex,
        description="分散トレーシングID",
    )
    span_id: str | None = Field(default=None, description="現在のスパンID")
    parent_span_id: str | None = Field(default=None, description="親スパンID")

    # 業務コンテキスト
    business_context: dict[str, Any] = Field(
        default_factory=dict,
        description="業務固有情報（案件ID、商品ID等）",
    )

    # ユーザーコンテキスト
    user_context: dict[str, Any] = Field(
        default_factory=dict,
        description="ユーザー関連情報（ロール、部署、権限等）",
    )

    # リクエストメタデータ
    request_metadata: dict[str, Any] = Field(
        default_factory=dict,
        description="リクエストメタデータ（IP、UA、言語等）",
    )

    # タイムスタンプ
    created_at: datetime = Field(default_factory=datetime.utcnow)

    # カスタム拡張
    extensions: dict[str, Any] = Field(
        default_factory=dict,
        description="カスタム拡張フィールド",
    )

    def with_business_data(self, **kwargs: Any) -> "FlowContext":
        """業務データを追加した新しいコンテキストを返す."""
        new_ctx = self.model_copy(deep=True)
        new_ctx.business_context.update(kwargs)
        return new_ctx

    def with_user_data(self, **kwargs: Any) -> "FlowContext":
        """ユーザーデータを追加した新しいコンテキストを返す."""
        new_ctx = self.model_copy(deep=True)
        new_ctx.user_context.update(kwargs)
        return new_ctx

    def to_headers(self) -> dict[str, str]:
        """HTTPヘッダー形式に変換（外部API連携用）."""
        return {
            "X-Session-Id": self.session_id,
            "X-User-Id": self.user_id,
            "X-Tenant-Id": self.tenant_id,
            "X-Trace-Id": self.trace_id,
            "X-Source-System": self.source_system,
        }

    @classmethod
    def from_headers(cls, headers: dict[str, str], **kwargs: Any) -> "FlowContext":
        """HTTPヘッダーからコンテキストを復元."""
        return cls(
            session_id=headers.get("X-Session-Id", str(uuid.uuid4())),
            user_id=headers.get("X-User-Id", "anonymous"),
            tenant_id=headers.get("X-Tenant-Id", "default"),
            trace_id=headers.get("X-Trace-Id", uuid.uuid4().hex),
            source_system=headers.get("X-Source-System", "unknown"),
            **kwargs,
        )


@dataclass
class InvocationResult:
    """フロー呼び出し結果.

    Attributes:
        success: 成功フラグ
        data: 結果データ
        error: エラー情報
        context: 使用したコンテキスト
        duration_ms: 実行時間（ミリ秒）
        trace_id: トレースID
    """

    success: bool
    data: dict[str, Any] = field(default_factory=dict)
    error: str | None = None
    error_type: str | None = None
    context: FlowContext | None = None
    duration_ms: float = 0.0
    trace_id: str = ""


class ContextBridge:
    """コンテキストブリッジ.

    既存システムからAgentFlowを呼び出す際の統一インターフェース。
    コンテキストの伝播、トレーシング連携、エラーハンドリングを提供。

    Example:
        >>> bridge = ContextBridge()
        >>> ctx = FlowContext(
        ...     session_id="sess-123",
        ...     user_id="user-456",
        ...     tenant_id="tenant-789",
        ...     source_system="crm",
        ... )
        >>> result = await bridge.invoke_flow(my_flow, ctx, {"query": "..."})
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(__name__)

    async def invoke_flow(
        self,
        flow: "Flow",
        context: FlowContext,
        inputs: dict[str, Any],
        **options: Any,
    ) -> InvocationResult:
        """フローを呼び出し.

        Args:
            flow: 実行するフロー
            context: フローコンテキスト
            inputs: 入力データ
            **options: 追加オプション

        Returns:
            InvocationResult
        """
        import time

        start_time = time.time()

        # コンテキストを設定
        token = _current_context.set(context)

        try:
            self._logger.info(
                "Invoking flow: source=%s, user=%s, tenant=%s, trace=%s",
                context.source_system,
                context.user_id,
                context.tenant_id,
                context.trace_id,
            )

            # 入力にコンテキスト情報を注入
            enriched_inputs = {
                **inputs,
                "_context": {
                    "session_id": context.session_id,
                    "user_id": context.user_id,
                    "tenant_id": context.tenant_id,
                    "trace_id": context.trace_id,
                    "source_system": context.source_system,
                    "user_context": context.user_context,
                    "business_context": context.business_context,
                },
            }

            # フロー実行
            result = await flow.run(enriched_inputs)

            duration_ms = (time.time() - start_time) * 1000

            return InvocationResult(
                success=True,
                data=result if isinstance(result, dict) else {"result": result},
                context=context,
                duration_ms=duration_ms,
                trace_id=context.trace_id,
            )

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            self._logger.exception(
                "Flow invocation failed: trace=%s, error=%s",
                context.trace_id,
                str(e),
            )
            return InvocationResult(
                success=False,
                error=str(e),
                error_type=type(e).__name__,
                context=context,
                duration_ms=duration_ms,
                trace_id=context.trace_id,
            )

        finally:
            _current_context.reset(token)

    async def invoke_agent(
        self,
        agent: Any,
        context: FlowContext,
        inputs: dict[str, Any],
        **options: Any,
    ) -> InvocationResult:
        """Agentを直接呼び出し.

        Args:
            agent: 実行するAgent
            context: フローコンテキスト
            inputs: 入力データ
            **options: 追加オプション

        Returns:
            InvocationResult
        """
        import time

        start_time = time.time()
        token = _current_context.set(context)

        try:
            self._logger.info(
                "Invoking agent: %s, source=%s, trace=%s",
                type(agent).__name__,
                context.source_system,
                context.trace_id,
            )

            # Agent実行
            result = await agent.run(inputs)

            duration_ms = (time.time() - start_time) * 1000

            return InvocationResult(
                success=True,
                data=result if isinstance(result, dict) else {"result": result},
                context=context,
                duration_ms=duration_ms,
                trace_id=context.trace_id,
            )

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            self._logger.exception(
                "Agent invocation failed: trace=%s, error=%s",
                context.trace_id,
                str(e),
            )
            return InvocationResult(
                success=False,
                error=str(e),
                error_type=type(e).__name__,
                context=context,
                duration_ms=duration_ms,
                trace_id=context.trace_id,
            )

        finally:
            _current_context.reset(token)


# ヘルパー関数
def get_current_context() -> FlowContext | None:
    """現在のフローコンテキストを取得."""
    return _current_context.get()


def set_current_context(context: FlowContext) -> Any:
    """現在のフローコンテキストを設定.

    Returns:
        リセット用トークン
    """
    return _current_context.set(context)


def reset_context(token: Any) -> None:
    """コンテキストをリセット."""
    _current_context.reset(token)


__all__ = [
    "FlowContext",
    "ContextBridge",
    "InvocationResult",
    "SourceSystemType",
    "get_current_context",
    "set_current_context",
    "reset_context",
]

