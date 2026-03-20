"""Kernel runtime plugin registry and SPI contracts."""

from __future__ import annotations

from typing import Protocol, runtime_checkable


@runtime_checkable
class KernelPlugin(Protocol):
    """Minimal runtime SPI exposed by kernel.plugins."""

    plugin_id: str


class KernelPluginRegistry:
    """Typed registry for runtime plugins."""

    def __init__(self) -> None:
        self._plugins: dict[str, KernelPlugin] = {}

    def register(self, plugin_id: str, plugin: KernelPlugin) -> None:
        """Register a runtime plugin under a stable id."""
        self._plugins[plugin_id] = plugin

    def get(self, plugin_id: str) -> KernelPlugin:
        """Return a registered runtime plugin."""
        return self._plugins[plugin_id]

    def list_ids(self) -> list[str]:
        """Return sorted runtime plugin ids."""
        return sorted(self._plugins.keys())


__all__ = ["KernelPlugin", "KernelPluginRegistry"]
