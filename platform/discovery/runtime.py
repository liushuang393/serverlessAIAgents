"""Layer 5 discovery 実装の互換窓口."""

from __future__ import annotations

from platform._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "AppDiscoveryService": ("apps.platform.services.app_discovery", "AppDiscoveryService"),
    "AppScaffolderService": ("apps.platform.services.app_scaffolder", "AppScaffolderService"),
}


def __getattr__(name: str) -> Any:
    """discovery 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
