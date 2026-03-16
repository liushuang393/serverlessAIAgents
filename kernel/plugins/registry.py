"""Kernel plugin registry."""

from __future__ import annotations

from typing import Any


class KernelPluginRegistry:
    """Kernel extension point を管理する簡易 registry。"""

    def __init__(self) -> None:
        self._plugins: dict[str, Any] = {}

    def register(self, plugin_id: str, plugin: Any) -> None:
        """plugin を登録する。"""
        self._plugins[plugin_id] = plugin

    def get(self, plugin_id: str) -> Any:
        """plugin を取得する。"""
        return self._plugins[plugin_id]

    def list_ids(self) -> list[str]:
        """登録済み plugin ID 一覧を返す."""
        return sorted(self._plugins.keys())
