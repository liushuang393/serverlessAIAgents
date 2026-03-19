"""IDeployExecutor - デプロイ執行器インターフェース.

contracts 層の正規配置。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from pathlib import Path

    from contracts.core import (
        ConfigField,
        DeployConfig,
        DeployEvent,
        DeployResult,
        DeployTarget,
        GeneratedCode,
        ValidationResult,
    )


@runtime_checkable
class IDeployExecutor(Protocol):
    """デプロイ執行器インターフェース."""

    def deploy(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]: ...

    async def deploy_sync(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: DeployConfig,
    ) -> DeployResult: ...

    async def get_required_config(
        self,
        target: DeployTarget,
    ) -> list[ConfigField]: ...

    async def validate_config(
        self,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> ValidationResult: ...

    def get_supported_targets(self) -> list[DeployTarget]: ...


__all__ = ["IDeployExecutor"]
