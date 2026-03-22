"""Control-plane discovery facade."""

from __future__ import annotations

import asyncio
from typing import TYPE_CHECKING

from contracts.app import AppManifest
from control_plane.services.app_discovery import AppDiscoveryService


if TYPE_CHECKING:
    from pathlib import Path


class DiscoveryService:
    """Public discovery facade over the canonical AppDiscoveryService."""

    def __init__(self, apps_dir: Path | None = None) -> None:
        self._service = AppDiscoveryService(apps_dir=apps_dir)

    def scan(self) -> list[AppManifest]:
        """Return normalized manifests for the current apps directory."""
        asyncio.run(self._service.scan())
        return [AppManifest.model_validate(config.model_dump(mode="python")) for config in self._service.list_apps()]
