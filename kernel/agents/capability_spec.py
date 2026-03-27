"""kernel.agents.capability_spec — kernel.core.capability_spec への再エクスポート."""

from kernel.core.capability_spec import (
    AgentCapabilitySpec,
    CapabilityRequirement,
    LLMRequirements,
)


__all__ = [
    "AgentCapabilitySpec",
    "CapabilityRequirement",
    "LLMRequirements",
]
