"""agentflow.sandbox.manager 後方互換スタブ. 実体は infrastructure.sandbox.manager."""

from infrastructure.sandbox.manager import (  # noqa: F401
    SandboxManager,
    get_sandbox_manager,
)

__all__ = ["SandboxManager", "get_sandbox_manager"]
