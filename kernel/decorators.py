"""agentflow.decorators 後方互換shim. 実体は kernel.agents.decorators."""

from kernel.agents.decorators import auto_adapt  # noqa: F401

__all__ = ["auto_adapt"]
