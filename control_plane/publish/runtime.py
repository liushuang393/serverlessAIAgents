"""Control-plane publish 実装の互換窓口."""

from __future__ import annotations

from control_plane._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "ComponentEntry": ("control_plane.services.component_library", "ComponentEntry"),
    "ComponentLibrary": ("control_plane.services.component_library", "ComponentLibrary"),
    "ComponentType": ("control_plane.services.component_library", "ComponentType"),
    "ComponentVisibility": ("control_plane.services.component_library", "ComponentVisibility"),
    "GalleryService": ("control_plane.services.gallery_service", "GalleryService"),
    "PublishOrchestrator": ("control_plane.services.publish_orchestrator", "PublishOrchestrator"),
    "get_component_library": ("control_plane.services.component_library", "get_component_library"),
}


def __getattr__(name: str) -> Any:
    """publish 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
