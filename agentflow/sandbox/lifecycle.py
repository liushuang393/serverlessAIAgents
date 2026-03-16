"""agentflow.sandbox.lifecycle 後方互換スタブ. 実体は infrastructure.sandbox.lifecycle."""

from infrastructure.sandbox.lifecycle import (  # noqa: F401
    EventType,
    ManagedSandbox,
    SandboxEvent,
)

__all__ = ["EventType", "ManagedSandbox", "SandboxEvent"]
