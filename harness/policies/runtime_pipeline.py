"""Harness と Kernel を接続する runtime パイプライン。

旧 ``harness.hooks.runtime`` から移動。
gating / approval / evaluation を KernelToolExecutor に外付けする。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from contracts.policy import ApprovalRequest
from harness.approval.service import ApprovalService
from harness.evaluation.service import EvaluationService
from harness.gating.tool_gate import ToolGate


if TYPE_CHECKING:
    from contracts.policy import EvalResult
    from contracts.tool import ToolRequest, ToolResult
    from harness.governance import ToolExecutionContext
    from infrastructure.sandbox.tool_provider import RegisteredTool, ToolProvider


class HarnessedToolRuntime:
    """KernelToolExecutor に gating/eval/approval を外付けする."""

    def __init__(
        self,
        *,
        tool_provider: Any,
        tool_gate: ToolGate | None = None,
        approval_service: ApprovalService | None = None,
        evaluation_service: EvaluationService | None = None,
    ) -> None:
        # 遅延 import: kernel 層への直接依存を回避
        from kernel.tools import KernelToolExecutor

        self._executor = KernelToolExecutor(tool_provider)
        self._tool_gate = tool_gate or ToolGate()
        self._approval_service = approval_service or ApprovalService()
        self._evaluation_service = evaluation_service or EvaluationService()

    async def execute(
        self,
        request: ToolRequest,
        *,
        tool: RegisteredTool,
        context: ToolExecutionContext | None = None,
    ) -> ToolResult | ApprovalRequest | EvalResult:
        """gating 結果に応じて実行または承認要求を返す."""
        decision = await self._tool_gate.evaluate(
            tool,
            tool_call_id=request.tool_call_id,
            arguments=request.arguments,
            context=context,
        )
        if decision.decision == "approval_required":
            approval = ApprovalRequest(
                action=request.name,
                reason=decision.reason,
                trace_id=request.trace_id,
                flow_id=request.flow_id,
            )
            return self._approval_service.submit(approval)
        if decision.decision != "allow":
            return self._evaluation_service.evaluate(
                evaluator="tool_gate",
                passed=False,
                reason=decision.reason,
                metadata=decision.metadata,
            )
        return await self._executor.execute(request)
