"""ガバナンス判定エンジン.

ツール実行の認可・承認・監査の統合判定を行う。
"""

from __future__ import annotations

import logging
from enum import Enum

from pydantic import BaseModel, Field

from agentflow.governance.audit import AuditEvent, AuditLogger, LoggingAuditLogger
from agentflow.providers.tool_provider import RegisteredTool
from agentflow.security.policy_engine import (
    AuthContext,
    AuthDecision,
    AuthMode,
    AuthResult,
    PolicyEngine,
)


class GovernanceDecision(str, Enum):
    """ガバナンス判定."""

    ALLOW = "allow"
    DENY = "deny"
    APPROVAL_REQUIRED = "approval_required"


class ToolExecutionContext(BaseModel):
    """ツール実行コンテキスト."""

    auth_context: AuthContext | None = Field(default=None, description="認証コンテキスト")
    run_id: str | None = Field(default=None, description="実行ID")
    trace_id: str | None = Field(default=None, description="トレースID")
    thread_id: str | None = Field(default=None, description="スレッドID")
    flow_id: str | None = Field(default=None, description="フローID")
    metadata: dict[str, object] = Field(default_factory=dict, description="追加メタデータ")


class GovernanceResult(BaseModel):
    """ガバナンス判定結果."""

    decision: GovernanceDecision = Field(default=GovernanceDecision.DENY)
    reason: str = Field(default="")
    auth_result: AuthResult | None = Field(default=None)
    missing_permissions: list[str] = Field(default_factory=list)
    requires_approval: bool = Field(default=False)

    @property
    def allowed(self) -> bool:
        """実行許可かどうか."""

        return self.decision == GovernanceDecision.ALLOW


class GovernanceEngine:
    """ガバナンス判定エンジン."""

    def __init__(
        self,
        policy_engine: PolicyEngine | None = None,
        audit_logger: AuditLogger | None = None,
        auth_mode: AuthMode = AuthMode.HYBRID,
    ) -> None:
        """初期化.

        Args:
            policy_engine: ポリシーエンジン
            audit_logger: 監査ロガー
            auth_mode: 認可モード
        """

        self._policy_engine: PolicyEngine = policy_engine or PolicyEngine()
        self._audit_logger: AuditLogger = audit_logger or LoggingAuditLogger(
            logging.getLogger(__name__)
        )
        self._auth_mode: AuthMode = auth_mode

    async def evaluate_tool(
        self,
        tool: RegisteredTool,
        tool_call_id: str | None,
        arguments: dict[str, object],
        context: ToolExecutionContext | None = None,
    ) -> GovernanceResult:
        """ツール実行のガバナンス判定を実行.

        Args:
            tool: ツール定義
            tool_call_id: ツール呼び出しID
            arguments: 引数
            context: 実行コンテキスト

        Returns:
            ガバナンス判定結果
        """

        effective_context = context or ToolExecutionContext()
        auth_context = effective_context.auth_context

        if auth_context is None:
            result = GovernanceResult(
                decision=GovernanceDecision.DENY,
                reason="認証コンテキストが存在しないため拒否",
                requires_approval=tool.requires_approval,
            )
            self._emit_audit(tool, tool_call_id, result, None, effective_context, arguments)
            return result

        normalized_context = self._normalize_auth_context(auth_context, tool)
        auth_result = await self._policy_engine.authorize(normalized_context, mode=self._auth_mode)
        missing_permissions = self._missing_permissions(tool, normalized_context)

        if missing_permissions:
            result = GovernanceResult(
                decision=GovernanceDecision.DENY,
                reason=f"必要な権限が不足しています: {missing_permissions}",
                auth_result=auth_result,
                missing_permissions=missing_permissions,
                requires_approval=tool.requires_approval,
            )
            self._emit_audit(tool, tool_call_id, result, auth_result, effective_context, arguments)
            return result

        if auth_result.decision != AuthDecision.ALLOW:
            result = GovernanceResult(
                decision=GovernanceDecision.DENY,
                reason=auth_result.reason or "認可されませんでした",
                auth_result=auth_result,
                requires_approval=tool.requires_approval,
            )
            self._emit_audit(tool, tool_call_id, result, auth_result, effective_context, arguments)
            return result

        if tool.requires_approval:
            result = GovernanceResult(
                decision=GovernanceDecision.APPROVAL_REQUIRED,
                reason="承認が必要なツールです",
                auth_result=auth_result,
                requires_approval=True,
            )
            self._emit_audit(tool, tool_call_id, result, auth_result, effective_context, arguments)
            return result

        result = GovernanceResult(
            decision=GovernanceDecision.ALLOW,
            reason="ポリシーにより許可されました",
            auth_result=auth_result,
            requires_approval=False,
        )
        self._emit_audit(tool, tool_call_id, result, auth_result, effective_context, arguments)
        return result

    def _normalize_auth_context(
        self, auth_context: AuthContext, tool: RegisteredTool
    ) -> AuthContext:
        """認可コンテキストを補完."""

        action = auth_context.action or tool.operation_type.value
        resource = dict(auth_context.resource)
        if "type" not in resource:
            resource["type"] = tool.name
        return auth_context.model_copy(update={"action": action, "resource": resource})

    def _missing_permissions(
        self,
        tool: RegisteredTool,
        auth_context: AuthContext,
    ) -> list[str]:
        """不足権限を計算."""

        required = tool.required_permissions
        if not required:
            return []
        permissions_value: object = auth_context.subject.get("permissions", [])
        if not isinstance(permissions_value, list):
            return required
        permissions = [perm for perm in permissions_value if isinstance(perm, str)]
        return [perm for perm in required if perm not in permissions]

    def _emit_audit(
        self,
        tool: RegisteredTool,
        tool_call_id: str | None,
        result: GovernanceResult,
        auth_result: AuthResult | None,
        context: ToolExecutionContext,
        arguments: dict[str, object],
    ) -> None:
        """監査イベントを出力."""

        user_id = None
        if context.auth_context is not None:
            user_id = context.auth_context.subject.get("user_id")

        event = AuditEvent(
            tool_name=tool.name,
            tool_call_id=tool_call_id,
            decision=result.decision.value,
            reason=result.reason,
            auth_decision=auth_result.decision if auth_result else None,
            auth_mode=auth_result.mode if auth_result else None,
            auth_reason=auth_result.reason if auth_result else None,
            requires_approval=tool.requires_approval,
            audit_required=tool.needs_audit(),
            operation_type=tool.operation_type.value,
            risk_level=tool.risk_level.value,
            run_id=context.run_id,
            trace_id=context.trace_id,
            thread_id=context.thread_id,
            flow_id=context.flow_id,
            user_id=user_id,
            metadata={"arguments": arguments, **context.metadata},
        )
        self._audit_logger.log_event(event)


__all__ = [
    "GovernanceDecision",
    "GovernanceEngine",
    "GovernanceResult",
    "ToolExecutionContext",
]
