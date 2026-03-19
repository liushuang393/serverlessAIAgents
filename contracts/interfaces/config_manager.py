"""IConfigManager - 設定管理器インターフェース.

contracts 層の正規配置。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


if TYPE_CHECKING:
    from contracts.core import (
        ConfigField,
        ConfigTemplate,
        DeployTarget,
        ValidationResult,
    )


@runtime_checkable
class IConfigManager(Protocol):
    """設定管理器インターフェース."""

    async def get_template(
        self,
        target: DeployTarget,
    ) -> ConfigTemplate: ...

    async def get_required_fields(
        self,
        target: DeployTarget,
        current_config: dict[str, Any] | None = None,
    ) -> list[ConfigField]: ...

    async def merge_config(
        self,
        template: ConfigTemplate,
        user_config: dict[str, Any],
    ) -> dict[str, Any]: ...

    async def validate(
        self,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> ValidationResult: ...

    async def save_config(
        self,
        name: str,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> None: ...

    async def load_config(
        self,
        name: str,
    ) -> dict[str, Any] | None: ...

    async def list_configs(
        self,
        target: DeployTarget | None = None,
    ) -> list[str]: ...


__all__ = ["IConfigManager"]
