"""互換 shim — agentflow.core.capability_spec.

本体は kernel.core.capability_spec に移行済み。後方互換のため re-export する。
"""

from kernel.core.capability_spec import (  # noqa: F401
    AgentCapabilitySpec,
    CapabilityRequirement,
    LLMRequirements,
)

__all__ = [
    "AgentCapabilitySpec",
    "CapabilityRequirement",
    "LLMRequirements",
]
