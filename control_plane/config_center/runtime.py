"""Control-plane config center 実装の互換窓口."""

from __future__ import annotations

from control_plane._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "ConfigWatcherService": ("control_plane.services.config_watcher", "ConfigWatcherService"),
    "get_default_llm_management_service": (
        "control_plane.services.llm_management",
        "get_default_llm_management_service",
    ),
    "init_app_config_event_store": (
        "control_plane.services.app_config_event_store",
        "init_app_config_event_store",
    ),
    "init_rag_config_store": ("control_plane.services.rag_config_store", "init_rag_config_store"),
    "resolve_platform_cached_secret": (
        "control_plane.services.llm_management_persistence",
        "resolve_platform_cached_secret",
    ),
    "sync_runtime_secret_cache_from_db": (
        "control_plane.services.llm_management_persistence",
        "sync_runtime_secret_cache_from_db",
    ),
}


def __getattr__(name: str) -> Any:
    """設定系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
