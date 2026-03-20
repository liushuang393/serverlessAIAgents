"""Control-plane publish public facade."""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "ComponentEntry": ("control_plane.publish.runtime", "ComponentEntry"),
    "ComponentLibrary": ("control_plane.publish.runtime", "ComponentLibrary"),
    "ComponentType": ("control_plane.publish.runtime", "ComponentType"),
    "ComponentVisibility": ("control_plane.publish.runtime", "ComponentVisibility"),
    "GalleryService": ("control_plane.publish.runtime", "GalleryService"),
    "PublishOrchestrator": ("control_plane.publish.runtime", "PublishOrchestrator"),
    "get_component_library": ("control_plane.publish.runtime", "get_component_library"),
    "PublishRequest": ("control_plane.publish.service", "PublishRequest"),
    "PublishService": ("control_plane.publish.service", "PublishService"),
}


def __getattr__(name: str) -> Any:
    """Resolve publish exports lazily to avoid circular imports."""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module {__name__!r} has no attribute {name!r}"
        raise AttributeError(msg)
    module_path, symbol_name = target
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
