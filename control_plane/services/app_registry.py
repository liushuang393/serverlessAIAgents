"""Canonical app registry service."""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from contracts.app import AppManifest


class AppRegistryService:
    """Canonical registry for normalized app manifests."""

    def __init__(self) -> None:
        self._manifests: dict[str, AppManifest] = {}

    def sync(self, manifests: list[AppManifest]) -> None:
        """Replace the registry contents with the provided manifests."""
        self._manifests = {manifest.name: manifest for manifest in manifests}

    def list(self) -> list[AppManifest]:
        """Return registered apps sorted by name."""
        return sorted(self._manifests.values(), key=lambda item: item.name)

    def get(self, name: str) -> AppManifest | None:
        """Return one app by name."""
        return self._manifests.get(name)
