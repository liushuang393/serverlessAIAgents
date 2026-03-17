"""Layer 5 publish 実装の互換窓口."""

from __future__ import annotations

from platform._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "ComponentEntry": ("platform.services.component_library", "ComponentEntry"),
    "ComponentLibrary": ("platform.services.component_library", "ComponentLibrary"),
    "ComponentType": ("platform.services.component_library", "ComponentType"),
    "ComponentVisibility": ("platform.services.component_library", "ComponentVisibility"),
    "GalleryService": ("platform.services.gallery_service", "GalleryService"),
    "PublishOrchestrator": ("platform.services.publish_orchestrator", "PublishOrchestrator"),
    "get_component_library": ("platform.services.component_library", "get_component_library"),
}


def __getattr__(name: str) -> Any:
    """publish 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
