# -*- coding: utf-8 -*-
"""AgentFlow Sandbox モジュール.

セキュアなコード実行サンドボックスを提供。
Agent で sandbox_provider を設定すると、execute_python Tool が利用可能。

サポート環境:
- microsandbox: microVM ベース（デフォルト・推奨）
- docker: コンテナベース
- e2b: クラウド SaaS

使用例（Agent 設定）:
    >>> @agent
    ... class AnalysisAgent:
    ...     sandbox_provider = "microsandbox"  # サンドボックス有効化
    ...     # execute_python Tool が自動的に利用可能に

環境変数:
    MICROSANDBOX_SERVER: microsandbox サーバー URL
    E2B_API_KEY: e2b API キー
"""

import logging

from agentflow.sandbox.base import (
    ExecutionResult,
    SandboxConfig,
    SandboxProvider,
)

logger = logging.getLogger(__name__)


def get_sandbox(
    provider: str = "microsandbox",
    config: SandboxConfig | None = None,
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
    config = config or SandboxConfig()

    logger.info(f"Sandbox provider: {provider}")

    if provider == "microsandbox":
        from agentflow.sandbox.microsandbox_provider import MicrosandboxProvider
        return MicrosandboxProvider(config)
    elif provider == "docker":
        from agentflow.sandbox.docker_provider import DockerProvider
        return DockerProvider(config)
    elif provider == "e2b":
        from agentflow.sandbox.e2b_provider import E2BProvider
        return E2BProvider(config)
    else:
        raise ValueError(
            f"Unknown sandbox provider: {provider}. "
            "Supported: microsandbox, docker, e2b"
        )


__all__ = [
    # プロバイダ取得
    "get_sandbox",
    # 基底クラス
    "SandboxProvider",
    "SandboxConfig",
    "ExecutionResult",
]

