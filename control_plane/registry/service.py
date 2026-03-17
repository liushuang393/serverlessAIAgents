"""Control-plane の registry サービス."""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from contracts.app import AppManifest


class AppRegistryService:
    """AppManifest の registry."""

    def __init__(self) -> None:
        self._manifests: dict[str, AppManifest] = {}

    def sync(self, manifests: list[AppManifest]) -> None:
        """manifest 一覧で registry を更新する."""
        self._manifests = {manifest.name: manifest for manifest in manifests}

    def list(self) -> list[AppManifest]:
        """登録済み App 一覧を返す."""
        return sorted(self._manifests.values(), key=lambda item: item.name)

    def get(self, name: str) -> AppManifest | None:
        """単一 App を返す."""
        return self._manifests.get(name)
