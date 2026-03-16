"""agentflow.sandbox 後方互換スタブ.

実体は infrastructure.sandbox に移動済み。
既存コードの import 互換性を維持するための re-export モジュール。
"""

from infrastructure.sandbox import (  # noqa: F401
    ActionResult,
    ActionTemplate,
    ActionType,
    CodeActExecutor,
    EventType,
    ExecutionResult,
    ExecutionStatus,
    FileInfo,
    ManagedSandbox,
    MockSandbox,
    ResourceLimits,
    ResourceUsage,
    SandboxConfig,
    SandboxEvent,
    SandboxManager,
    SandboxProvider,
    SandboxState,
    Workspace,
    WorkspaceManager,
    WorkspaceState,
    get_sandbox,
    get_sandbox_manager,
    get_workspace_manager,
    reset_sandbox,
)

__all__ = [
    "ActionResult",
    "ActionTemplate",
    "ActionType",
    "CodeActExecutor",
    "EventType",
    "ExecutionResult",
    "ExecutionStatus",
    "FileInfo",
    "ManagedSandbox",
    "MockSandbox",
    "ResourceLimits",
    "ResourceUsage",
    "SandboxConfig",
    "SandboxEvent",
    "SandboxManager",
    "SandboxProvider",
    "SandboxState",
    "Workspace",
    "WorkspaceManager",
    "WorkspaceState",
    "get_sandbox",
    "get_sandbox_manager",
    "get_workspace_manager",
    "reset_sandbox",
]
