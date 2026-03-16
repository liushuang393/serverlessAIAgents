"""Layer 1 の Sandbox infrastructure 公開 API.

セキュアなコード実行サンドボックスを提供。
サポート環境: microsandbox, docker, e2b, mock
"""

from __future__ import annotations

import logging
from typing import Any

from infrastructure.sandbox.base import (
    ExecutionResult,
    ResourceLimits,
    ResourceUsage,
    SandboxConfig,
    SandboxProvider,
    SandboxState,
)
from infrastructure.sandbox.codeact_executor import (
    ActionResult,
    ActionTemplate,
    ActionType,
    CodeActExecutor,
    ExecutionStatus,
)
from infrastructure.sandbox.lifecycle import (
    EventType,
    ManagedSandbox,
    SandboxEvent,
)
from infrastructure.sandbox.manager import (
    SandboxManager,
    get_sandbox_manager,
)
from infrastructure.sandbox.mock_provider import MockSandbox
from infrastructure.sandbox.workspace import (
    FileInfo,
    Workspace,
    WorkspaceManager,
    WorkspaceState,
    get_workspace_manager,
)


logger = logging.getLogger(__name__)

# get_sandbox() のキャッシュ（同一プロバイダは同一インスタンスを返す）
_sandbox_cache: dict[str, SandboxProvider] = {}


def reset_sandbox() -> None:
    """サンドボックスキャッシュをリセットする."""
    _sandbox_cache.clear()


def get_sandbox(
    provider: str = "mock",
    config: SandboxConfig | None = None,
    **kwargs: Any,
) -> SandboxProvider:
    """サンドボックスプロバイダを取得.

    Args:
        provider: プロバイダ名（microsandbox/docker/e2b/mock）
        config: サンドボックス設定
        **kwargs: 追加キーワード引数

    Returns:
        SandboxProvider インスタンス

    Raises:
        ValueError: 不明なプロバイダ
    """
    # キャッシュヒット
    if provider in _sandbox_cache:
        return _sandbox_cache[provider]

    config = config or SandboxConfig()
    logger.info("Sandbox provider: %s", provider)

    if provider == "mock":
        instance: SandboxProvider = MockSandbox(config)
    elif provider == "microsandbox":
        from infrastructure.sandbox.microsandbox_provider import MicrosandboxProvider

        instance = MicrosandboxProvider(config)
    elif provider == "docker":
        from infrastructure.sandbox.docker_provider import DockerProvider

        instance = DockerProvider(config)
    elif provider == "e2b":
        from infrastructure.sandbox.e2b_provider import E2BProvider

        instance = E2BProvider(config)
    else:
        msg = f"Unknown sandbox provider: {provider}. Supported: mock, microsandbox, docker, e2b"
        raise ValueError(msg)

    _sandbox_cache[provider] = instance
    return instance


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
