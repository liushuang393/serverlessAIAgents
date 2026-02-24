# -*- coding: utf-8 -*-
"""Execution control plane helpers."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from agentflow.providers.tool_provider import RiskLevel
from agentflow.security.policy_engine import AuthContext


@dataclass(slots=True)
class ExecutionOptions:
    """Pipeline execution options."""

    autonomy_level: str = "balanced"
    risk_profile: str = "normal"
    human_policy: str = "risk_based"
    verification_mode: str = "strict"
    skill_mode: str = "skill_first"
    acceptance_threshold: float = 85.0
    max_auto_iterations: int = 3
    business_context: dict[str, Any] | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "autonomy_level": self.autonomy_level,
            "risk_profile": self.risk_profile,
            "human_policy": self.human_policy,
            "verification_mode": self.verification_mode,
            "skill_mode": self.skill_mode,
            "acceptance_threshold": self.acceptance_threshold,
            "max_auto_iterations": self.max_auto_iterations,
            "business_context": self.business_context or {},
        }


def resolve_execution_options(inputs: dict[str, Any]) -> ExecutionOptions:
    """Merge top-level and nested options into normalized execution options."""
    raw = inputs.get("options", {})
    options = raw.copy() if isinstance(raw, dict) else {}

    for key in (
        "autonomy_level",
        "risk_profile",
        "human_policy",
        "verification_mode",
        "skill_mode",
        "acceptance_threshold",
        "max_auto_iterations",
        "business_context",
    ):
        if key in inputs and key not in options:
            options[key] = inputs[key]

    threshold_raw = options.get("acceptance_threshold", 85.0)
    try:
        threshold = float(threshold_raw)
    except (TypeError, ValueError):
        threshold = 85.0

    max_iter_raw = options.get("max_auto_iterations", 3)
    try:
        max_iterations = max(1, int(max_iter_raw))
    except (TypeError, ValueError):
        max_iterations = 3

    business_context = options.get("business_context", {})
    if not isinstance(business_context, dict):
        business_context = {}

    verification_mode = str(options.get("verification_mode", "strict")).lower()
    if verification_mode not in {"strict", "fast"}:
        verification_mode = "strict"

    skill_mode = str(options.get("skill_mode", "skill_first")).lower()
    if skill_mode not in {"skill_first", "native_only"}:
        skill_mode = "skill_first"

    return ExecutionOptions(
        autonomy_level=str(options.get("autonomy_level", "balanced")),
        risk_profile=str(options.get("risk_profile", "normal")).lower(),
        human_policy=str(options.get("human_policy", "risk_based")).lower(),
        verification_mode=verification_mode,
        skill_mode=skill_mode,
        acceptance_threshold=threshold,
        max_auto_iterations=max_iterations,
        business_context=business_context,
    )


def map_tool_risk_to_hitl(risk_level: RiskLevel | str) -> str:
    """Map tool-provider risk to HITL risk levels."""
    value = str(risk_level.value if isinstance(risk_level, RiskLevel) else risk_level).lower()
    if value == "medium":
        return "normal"
    return value


def should_require_human_approval(
    *,
    tool_risk: RiskLevel | str,
    execution_options: ExecutionOptions,
) -> bool:
    """Decide whether an approval gate should be triggered."""
    policy = execution_options.human_policy
    if policy == "manual_all":
        return True
    if policy == "auto_with_sampling":
        return False

    # Default: risk_based
    effective_risk = map_tool_risk_to_hitl(tool_risk)
    if execution_options.risk_profile in {"high", "critical"}:
        return True
    return effective_risk in {"high", "critical"}


def build_auth_context(flow_context: Any, tool_name: str, action: str) -> AuthContext | None:
    """Build AuthContext from FlowContext for governance evaluation."""
    if flow_context is None:
        return None

    default_permissions = [
        "read",
        "write",
        "execute",
        "manage",
        "repo.read",
        "repo.write",
        "os.exec",
    ]
    subject: dict[str, Any] = {
        "user_id": getattr(flow_context, "user_id", "unknown-user"),
        "role": "manager",
        "permissions": default_permissions,
    }

    user_context = getattr(flow_context, "user_context", {})
    if isinstance(user_context, dict):
        for key, value in user_context.items():
            subject[key] = value
        if "permissions" in user_context and isinstance(user_context["permissions"], list):
            custom_permissions = [
                perm
                for perm in user_context["permissions"]
                if isinstance(perm, str) and perm
            ]
            subject["permissions"] = list(dict.fromkeys(default_permissions + custom_permissions))

    return AuthContext(
        subject=subject,
        resource={"type": tool_name},
        action=action,
        tenant_id=getattr(flow_context, "tenant_id", None),
    )
