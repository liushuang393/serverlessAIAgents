"""Control-plane DB compatibility facade."""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "Base": ("control_plane.operations.db", "Base"),
    "LLMEngineDeployment": ("control_plane.operations.db", "LLMEngineDeployment"),
    "LLMProviderSecret": ("control_plane.operations.db", "LLMProviderSecret"),
    "close_platform_db": ("control_plane.operations.db", "close_platform_db"),
    "ensure_platform_db_ready": ("control_plane.operations.db", "ensure_platform_db_ready"),
    "get_platform_database_url": ("control_plane.operations.db", "get_platform_database_url"),
    "get_platform_db_session": ("control_plane.operations.db", "get_platform_db_session"),
}


def __getattr__(name: str) -> Any:
    """互換 DB API を遅延解決する。"""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module 'control_plane.db' has no attribute {name!r}"
        raise AttributeError(msg)

    module_path, symbol_name = target
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


__all__ = [
    "Base",
    "LLMEngineDeployment",
    "LLMProviderSecret",
    "close_platform_db",
    "ensure_platform_db_ready",
    "get_platform_database_url",
    "get_platform_db_session",
]
