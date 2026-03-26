"""Control-plane lifecycle facade."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any

from control_plane.schemas.app_config_schemas import AppConfig
from control_plane.services.app_lifecycle import AppLifecycleManager
from control_plane.services.runtime_command_resolver import (
    CommandAction,
    RuntimeCommandResolver,
)


if TYPE_CHECKING:
    from contracts.app import AppManifest


class LifecycleService:
    """Public lifecycle facade over canonical control-plane services."""

    def __init__(self) -> None:
        self._manager = AppLifecycleManager()
        self._command_resolver = RuntimeCommandResolver()

    async def check_health(self, manifest: AppManifest) -> dict[str, Any]:
        """Delegate health checks to the canonical lifecycle manager."""
        config = AppConfig.model_validate(manifest.model_dump(mode="python"))
        config_path = _resolve_config_path(manifest)
        return (await self._manager.check_health(config, config_path=config_path)).to_dict()

    def build_command_plan(self, manifest: AppManifest, action: str) -> list[str]:
        """Resolve runtime commands through the canonical command resolver."""
        config = AppConfig.model_validate(manifest.model_dump(mode="python"))
        config_path = _resolve_config_path(manifest)
        if config_path is not None:
            commands = self._command_resolver.resolve(
                app_name=config.name,
                app_dir=config_path.parent,
                runtime_commands=config.runtime.commands,
            )
            action_key = _normalize_command_action(action)
            resolved = commands.get(action_key) if action_key is not None else None
        else:
            resolved = getattr(config.runtime.commands, action, None)
        if isinstance(resolved, str) and resolved.strip():
            return [resolved.strip()]
        return []


def _resolve_config_path(manifest: AppManifest) -> Path | None:
    metadata = manifest.metadata
    raw_path = metadata.get("config_path") if isinstance(metadata, dict) else None
    if not isinstance(raw_path, str) or not raw_path.strip():
        return None
    return Path(raw_path)


def _normalize_command_action(action: str) -> CommandAction | None:
    if action == "backend_dev":
        return "backend_dev"
    if action == "frontend_dev":
        return "frontend_dev"
    if action == "publish":
        return "publish"
    if action == "start":
        return "start"
    if action == "stop":
        return "stop"
    return None
