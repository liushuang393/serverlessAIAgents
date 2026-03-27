"""ガバナンス判定エンジン - Layer 4 Harness ガードレール.

ツール実行の認可・承認・監査の統合判定を行う。

移行元: legacy governance layer/engine.py → harness/guardrails/service.py
"""

from __future__ import annotations

import importlib
import logging
from enum import StrEnum
from typing import TYPE_CHECKING, Any, Protocol

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from harness.governance.audit import AuditLogger
    from harness.governance.plugin_registry import PluginRegistry, PluginRuntimeAssessment
    from harness.security.policy_engine import (
        AuthContext,
        AuthMode,
        AuthResult,
        PolicyEngine,
    )


class RegisteredToolLike(Protocol):
    """ガバナンス判定に必要なツール契約."""

    name: str
    plugin_id: str | None
    plugin_version: str | None
    requires_approval: bool
    required_permissions: list[str]
    operation_type: Any
    risk_level: Any

    def needs_audit(self) -> bool:
        """監査要否を返す."""


def _lazy(module_path: str, name: str) -> Any:
    """遅延インポートで循環依存を回避."""
    mod = importlib.import_module(module_path)
    return getattr(mod, name)


def _get_auth_context_type() -> type:
    """AuthContext 型を遅延取得."""
    from harness.security.policy_engine import AuthContext

    return AuthContext


class GovernanceDecision(StrEnum):
    """ガバナンス判定."""

    ALLOW = "allow"
    DENY = "deny"
    APPROVAL_REQUIRED = "approval_required"


class ToolExecutionContext(BaseModel):
    """ツール実行コンテキスト."""

    model_config = {"arbitrary_types_allowed": True}

    auth_context: Any | None = Field(default=None, description="認証コンテキスト")
    run_id: str | None = Field(default=None, description="実行ID")
    trace_id: str | None = Field(default=None, description="トレースID")
    thread_id: str | None = Field(default=None, description="スレッドID")
    flow_id: str | None = Field(default=None, description="フローID")
    app_name: str | None = Field(default=None, description="実行対象 App 名")
    product_line: str | None = Field(default=None, description="製品主線")
    metadata: dict[str, object] = Field(default_factory=dict, description="追加メタデータ")


class GovernanceResult(BaseModel):
    """ガバナンス判定結果."""

    decision: GovernanceDecision = Field(default=GovernanceDecision.DENY)
    reason: str = Field(default="")
    auth_result: Any | None = Field(default=None)
    missing_permissions: list[str] = Field(default_factory=list)
    requires_approval: bool = Field(default=False)
    warnings: list[str] = Field(default_factory=list)
    plugin_id: str | None = Field(default=None)
    plugin_version: str | None = Field(default=None)
    plugin_risk_tier: str | None = Field(default=None)

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
        plugin_registry: PluginRegistry | None = None,
        auth_mode: AuthMode | None = None,
    ) -> None:
        """初期化.

        Args:
            policy_engine: ポリシーエンジン
            audit_logger: 監査ロガー
            plugin_registry: プラグインレジストリ
            auth_mode: 認可モード
        """
        policy_engine_cls = _lazy("harness.security.policy_engine", "PolicyEngine")
        audit_logger_cls = _lazy("harness.governance.audit", "LoggingAuditLogger")
        plugin_registry_cls = _lazy("harness.governance.plugin_registry", "PluginRegistry")
        auth_mode_enum = _lazy("harness.security.policy_engine", "AuthMode")

        self._policy_engine = policy_engine or policy_engine_cls()
        self._audit_logger = audit_logger or audit_logger_cls(logging.getLogger(__name__))
        self._plugin_registry = plugin_registry or plugin_registry_cls()
        self._auth_mode = auth_mode or auth_mode_enum.HYBRID

    async def evaluate_tool(
        self,
        tool: RegisteredToolLike,
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
        plugin_assessment: PluginRuntimeAssessment | None = None

        if auth_context is None:
            result = GovernanceResult(
                decision=GovernanceDecision.DENY,
                reason="認証コンテキストが存在しないため拒否",
                requires_approval=tool.requires_approval,
                plugin_id=tool.plugin_id,
                plugin_version=tool.plugin_version,
            )
            self._emit_audit(
                tool,
                tool_call_id,
                result,
                None,
                effective_context,
                arguments,
                plugin_assessment=plugin_assessment,
            )
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
                plugin_id=tool.plugin_id,
                plugin_version=tool.plugin_version,
            )
            self._emit_audit(
                tool,
                tool_call_id,
                result,
                auth_result,
                effective_context,
                arguments,
                plugin_assessment=plugin_assessment,
            )
            return result

        auth_decision_cls = _lazy("harness.security.policy_engine", "AuthDecision")
        if auth_result.decision != auth_decision_cls.ALLOW:
            result = GovernanceResult(
                decision=GovernanceDecision.DENY,
                reason=auth_result.reason or "認可されませんでした",
                auth_result=auth_result,
                requires_approval=tool.requires_approval,
                plugin_id=tool.plugin_id,
                plugin_version=tool.plugin_version,
            )
            self._emit_audit(
                tool,
                tool_call_id,
                result,
                auth_result,
                effective_context,
                arguments,
                plugin_assessment=plugin_assessment,
            )
            return result

        plugin_assessment = self._plugin_registry.evaluate_tool(tool, effective_context)
        if plugin_assessment.errors:
            result = GovernanceResult(
                decision=GovernanceDecision.DENY,
                reason=plugin_assessment.errors[0],
                auth_result=auth_result,
                requires_approval=tool.requires_approval,
                warnings=list(plugin_assessment.warnings),
                plugin_id=tool.plugin_id,
                plugin_version=tool.plugin_version,
                plugin_risk_tier=plugin_assessment.plugin_risk_tier,
            )
            self._emit_audit(
                tool,
                tool_call_id,
                result,
                auth_result,
                effective_context,
                arguments,
                plugin_assessment=plugin_assessment,
            )
            return result

        if tool.requires_approval:
            result = GovernanceResult(
                decision=GovernanceDecision.APPROVAL_REQUIRED,
                reason="承認が必要なツールです",
                auth_result=auth_result,
                requires_approval=True,
                warnings=list(plugin_assessment.warnings),
                plugin_id=tool.plugin_id,
                plugin_version=tool.plugin_version,
                plugin_risk_tier=plugin_assessment.plugin_risk_tier,
            )
            self._emit_audit(
                tool,
                tool_call_id,
                result,
                auth_result,
                effective_context,
                arguments,
                plugin_assessment=plugin_assessment,
            )
            return result

        result = GovernanceResult(
            decision=GovernanceDecision.ALLOW,
            reason="ポリシーにより許可されました",
            auth_result=auth_result,
            requires_approval=False,
            warnings=list(plugin_assessment.warnings),
            plugin_id=tool.plugin_id,
            plugin_version=tool.plugin_version,
            plugin_risk_tier=plugin_assessment.plugin_risk_tier,
        )
        self._emit_audit(
            tool,
            tool_call_id,
            result,
            auth_result,
            effective_context,
            arguments,
            plugin_assessment=plugin_assessment,
        )
        return result

    def _normalize_auth_context(self, auth_context: AuthContext, tool: RegisteredToolLike) -> AuthContext:
        """認可コンテキストを補完."""

        action = auth_context.action or tool.operation_type.value
        resource = dict(auth_context.resource)
        if "type" not in resource:
            resource["type"] = tool.name
        return auth_context.model_copy(update={"action": action, "resource": resource})

    def _missing_permissions(
        self,
        tool: RegisteredToolLike,
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
        tool: RegisteredToolLike,
        tool_call_id: str | None,
        result: GovernanceResult,
        auth_result: AuthResult | None,
        context: ToolExecutionContext,
        arguments: dict[str, object],
        *,
        plugin_assessment: PluginRuntimeAssessment | None,
    ) -> None:
        """監査イベントを出力."""

        user_id = None
        if context.auth_context is not None:
            user_id = context.auth_context.subject.get("user_id")

        metadata: dict[str, object] = {"arguments": arguments, **context.metadata}
        metadata["governance_warnings"] = list(result.warnings)
        metadata["plugin_id"] = result.plugin_id
        metadata["plugin_version"] = result.plugin_version
        metadata["plugin_risk_tier"] = result.plugin_risk_tier
        metadata["plugin_signature_status"] = (
            plugin_assessment.plugin_signature_status
            if plugin_assessment and plugin_assessment.plugin_signature_status
            else "not_evaluated"
        )
        metadata["plugin_signature_reason"] = (
            plugin_assessment.plugin_signature_reason
            if plugin_assessment and plugin_assessment.plugin_signature_reason
            else ""
        )
        metadata["app_name"] = context.app_name or "unknown_app"
        metadata["product_line"] = context.product_line or "unknown_product_line"
        if plugin_assessment is not None and plugin_assessment.binding is not None:
            metadata["plugin_binding_version"] = plugin_assessment.binding.version

        audit_event_cls = _lazy("harness.governance.audit", "AuditEvent")
        event = audit_event_cls(
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
            metadata=metadata,
        )
        self._audit_logger.log_event(event)


__all__ = [
    "GovernanceDecision",
    "GovernanceEngine",
    "GovernanceResult",
    "ToolExecutionContext",
]
