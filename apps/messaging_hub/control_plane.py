"""Messaging Hub coordinator 向け control plane."""

from __future__ import annotations

from enum import StrEnum

from pydantic import BaseModel, Field

from apps.messaging_hub.focused_subagents import FocusedTaskPlan, SemanticOperationType


class ControlPlaneDecision(StrEnum):
    """control plane の判定."""

    ALLOW = "allow"
    APPROVAL_REQUIRED = "approval_required"
    DENY = "deny"


class ControlPlaneEvaluation(BaseModel):
    """task plan に対する deterministic control plane 判定."""

    decision: ControlPlaneDecision = Field(default=ControlPlaneDecision.ALLOW)
    reason: str = Field(default="")
    blocked_operations: list[str] = Field(default_factory=list)
    required_actions: list[str] = Field(default_factory=list)
    notes: list[str] = Field(default_factory=list)


class CoordinatorControlPlane:
    """focused task plan へ deterministic gate を適用する."""

    def evaluate(
        self,
        *,
        plan: FocusedTaskPlan,
        security_mode: str,
    ) -> ControlPlaneEvaluation:
        """security mode と semantic contracts から判定を返す."""
        write_contracts = [
            contract for contract in plan.semantic_contracts if contract.operation_type == SemanticOperationType.WRITE
        ]
        human_gate_contracts = [contract for contract in plan.semantic_contracts if contract.requires_human_gate]

        if security_mode == "read_only" and write_contracts:
            return ControlPlaneEvaluation(
                decision=ControlPlaneDecision.DENY,
                reason="read_only モードでは write 系 semantic action を許可しません",
                blocked_operations=[contract.action_name for contract in write_contracts],
                required_actions=["switch_security_mode", "remove_write_operation"],
                notes=["structural_stop_for_mutation"],
            )

        if security_mode == "approval_required" and human_gate_contracts:
            return ControlPlaneEvaluation(
                decision=ControlPlaneDecision.APPROVAL_REQUIRED,
                reason="human gate が必要な semantic action を含みます",
                blocked_operations=[],
                required_actions=[contract.action_name for contract in human_gate_contracts],
                notes=["pre_action_human_gate", "gateway_confirmation_required"],
            )

        return ControlPlaneEvaluation(
            decision=ControlPlaneDecision.ALLOW,
            reason="semantic contracts と security mode の組み合わせで許可されました",
            blocked_operations=[],
            required_actions=[],
            notes=["deterministic_allow"],
        )


__all__ = [
    "ControlPlaneDecision",
    "ControlPlaneEvaluation",
    "CoordinatorControlPlane",
]
