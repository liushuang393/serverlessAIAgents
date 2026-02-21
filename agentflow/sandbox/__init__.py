"""AgentFlow Sandbox モジュール.

セキュアなコード実行サンドボックスを提供。
Daytonaの設計思想を参考に、ライフサイクル管理とワークスペース機能を追加。

サポート環境:
- microsandbox: microVM ベース（デフォルト・推奨）
- docker: コンテナベース
- e2b: クラウド SaaS

基本使用例:
    >>> from agentflow.sandbox import get_sandbox
    >>> sandbox = get_sandbox(provider="docker")
    >>> result = await sandbox.execute("print('Hello')")

ライフサイクル管理（Daytonaスタイル）:
    >>> from agentflow.sandbox import ManagedSandbox
    >>> async with ManagedSandbox.create(provider="docker") as sandbox:
    ...     result = await sandbox.execute("print('Hello')")
    ...     print(sandbox.state)  # SandboxState.STARTED

ワークスペース使用:
    >>> from agentflow.sandbox import Workspace
    >>> async with Workspace.create(name="my-project") as ws:
    ...     await ws.write_file("main.py", b"print('Hello')")
    ...     result = await ws.run_file("main.py")

マネージャー使用:
    >>> from agentflow.sandbox import get_sandbox_manager
    >>> manager = get_sandbox_manager()
    >>> sandbox = await manager.create(provider="docker")
    >>> await sandbox.start()

環境変数:
    MICROSANDBOX_SERVER: microsandbox サーバー URL
    E2B_API_KEY: e2b API キー
"""

import logging
from typing import Any

from agentflow.sandbox.base import (
    ExecutionResult,
    ResourceLimits,
    ResourceUsage,
    SandboxConfig,
    SandboxProvider,
    SandboxState,
)
from agentflow.sandbox.codeact_executor import (
    ActionResult,
    ActionTemplate,
    ActionType,
    CodeActExecutor,
    ExecutionStatus,
)
from agentflow.sandbox.lifecycle import (
    EventType,
    ManagedSandbox,
    SandboxEvent,
)
from agentflow.sandbox.manager import (
    SandboxManager,
    get_sandbox_manager,
)
from agentflow.sandbox.mock_provider import MockSandbox
from agentflow.sandbox.workspace import (
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
    """サンドボックスキャッシュをリセットする.

    テスト等でサンドボックスの状態をリセットしたい場合に使用。
    """
    _sandbox_cache.clear()


def get_sandbox(
    provider: str = "mock",
    config: SandboxConfig | None = None,
    **kwargs: Any,
) -> SandboxProvider:
    """サンドボックスプロバイダを取得.

    Args:
        provider: プロバイダ名（microsandbox/docker/e2b）
        config: サンドボックス設定

    Returns:
        SandboxProvider インスタンス

    Raises:
        ValueError: 不明なプロバイダ
    """
    # キャッシュヒット
    if provider in _sandbox_cache:
        return _sandbox_cache[provider]

    config = config or SandboxConfig()
    logger.info(f"Sandbox provider: {provider}")

    if provider == "mock":
        instance: SandboxProvider = MockSandbox(config)
    elif provider == "microsandbox":
        from agentflow.sandbox.microsandbox_provider import MicrosandboxProvider

        instance = MicrosandboxProvider(config)
    elif provider == "docker":
        from agentflow.sandbox.docker_provider import DockerProvider

        instance = DockerProvider(config)
    elif provider == "e2b":
        from agentflow.sandbox.e2b_provider import E2BProvider

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
    # CodeAct執行器
    "CodeActExecutor",
    "EventType",
    "ExecutionResult",
    "ExecutionStatus",
    "FileInfo",
    # ライフサイクル管理（Daytonaスタイル）
    "ManagedSandbox",
    "MockSandbox",
    # テスト・開発用モックプロバイダ
    "MockSandbox",
    "ResourceLimits",
    "ResourceUsage",
    "SandboxConfig",
    "SandboxEvent",
    # マネージャー
    "SandboxManager",
    # 基底クラス・型
    "SandboxProvider",
    "SandboxState",
    # ワークスペース
    "Workspace",
    "WorkspaceManager",
    "WorkspaceState",
    # プロバイダ取得・制御
    "get_sandbox",
    "get_sandbox_manager",
    "get_workspace_manager",
    "reset_sandbox",
    "reset_sandbox",
]
