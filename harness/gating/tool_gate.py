"""Kernel 前段に差し込む Tool Gating.

NOTE: harness 層は agentflow.governance を静的インポートしない。
GovernanceEngineProtocol (contracts/policy) を介して DI する。
"""

from __future__ import annotations

from enum import Enum
from typing import TYPE_CHECKING

from contracts.policy import GovernanceEngineProtocol, PolicyDecision
from shared.registry import ComponentToggle


if TYPE_CHECKING:
    from harness.governance import ToolExecutionContext
    from infrastructure.sandbox.tool_provider import RegisteredTool


class ToolGate:
    """GovernanceEngine を harness 側へ隔離する."""

    def __init__(
        self,
        toggle: ComponentToggle | None = None,
        engine: GovernanceEngineProtocol | None = None,
    ) -> None:
        self._toggle = toggle or ComponentToggle()
        if engine is None:
            # 遅延インポート: harness → agentflow.governance の静的依存を回避
            from harness.governance import GovernanceEngine

            engine = GovernanceEngine()
        self._engine: GovernanceEngineProtocol = engine

    async def evaluate(
        self,
        tool: RegisteredTool,
        *,
        tool_call_id: str | None,
        arguments: dict[str, object],
        context: ToolExecutionContext | None = None,
    ) -> PolicyDecision:
        """ツール実行可否を PolicyDecision 契約へ変換する."""
        if not self._toggle.enabled:
            return PolicyDecision(
                policy_name="tool_gate",
                decision="allow",
                reason="tool gate disabled",
                metadata={"implementation": "noop"},
            )

        result = await self._engine.evaluate_tool(
            tool,
            tool_call_id=tool_call_id,
            arguments=arguments,
            context=context,
        )
        # decision が Enum の場合は .value で文字列化
        decision_val = result.decision
        if isinstance(decision_val, Enum):
            decision_val = decision_val.value
        return PolicyDecision(
            policy_name="tool_gate",
            decision=str(decision_val),
            reason=result.reason,
            trace_id=context.trace_id if context is not None else None,
            flow_id=context.flow_id if context is not None else None,
            metadata={
                "requires_approval": result.requires_approval,
                "warnings": list(result.warnings),
                "plugin_id": result.plugin_id,
                "plugin_version": result.plugin_version,
            },
        )
