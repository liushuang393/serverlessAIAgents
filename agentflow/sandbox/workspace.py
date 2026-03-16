"""agentflow.sandbox.workspace 後方互換スタブ. 実体は infrastructure.sandbox.workspace."""

from infrastructure.sandbox.workspace import (  # noqa: F401
    FileInfo,
    Workspace,
    WorkspaceManager,
    WorkspaceState,
    get_workspace_manager,
)

__all__ = ["FileInfo", "Workspace", "WorkspaceManager", "WorkspaceState", "get_workspace_manager"]
