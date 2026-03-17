"""Layer 5 config center 実装の互換窓口."""

from __future__ import annotations

from platform._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "ConfigWatcherService": ("apps.platform.services.config_watcher", "ConfigWatcherService"),
    "get_default_llm_management_service": (
        "apps.platform.services.llm_management",
        "get_default_llm_management_service",
    ),
    "init_app_config_event_store": (
        "apps.platform.services.app_config_event_store",
        "init_app_config_event_store",
    ),
    "init_rag_config_store": ("apps.platform.services.rag_config_store", "init_rag_config_store"),
    "resolve_platform_cached_secret": (
        "apps.platform.services.llm_management_persistence",
        "resolve_platform_cached_secret",
    ),
    "sync_runtime_secret_cache_from_db": (
        "apps.platform.services.llm_management_persistence",
        "sync_runtime_secret_cache_from_db",
    ),
}


def __getattr__(name: str) -> Any:
    """設定系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
