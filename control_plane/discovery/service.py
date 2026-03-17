"""Control-plane の discovery サービス."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

from shared import load_app_manifest


if TYPE_CHECKING:
    from contracts.app import AppManifest


class DiscoveryService:
    """apps/*/app_config.json を走査して AppManifest を返す."""

    def __init__(self, apps_dir: Path | None = None) -> None:
        self._apps_dir = apps_dir or (Path.cwd() / "apps")

    def scan(self) -> list[AppManifest]:
        """現在の apps 一覧を返す."""
        manifests: list[AppManifest] = []
        for path in sorted(self._apps_dir.glob("*/app_config.json")):
            manifests.append(load_app_manifest(path))
        return manifests
