"""Platform 公開エンジンの互換 facade.

最終的な実装は top-level ``control_plane/`` へ集約するが、
移行期間中は既存実装を段階的に吸収する。
このモジュールは公開面だけを top-level に固定する。
"""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "ComponentEntry": ("control_plane.publish.runtime", "ComponentEntry"),
    "ComponentLibrary": ("control_plane.publish.runtime", "ComponentLibrary"),
    "ComponentType": ("control_plane.publish.runtime", "ComponentType"),
    "ComponentVisibility": ("control_plane.publish.runtime", "ComponentVisibility"),
    "GalleryService": ("control_plane.publish.runtime", "GalleryService"),
    "PlatformEngine": ("control_plane.runtime_engine", "PlatformEngine"),
    "PublishOrchestrator": ("control_plane.publish.runtime", "PublishOrchestrator"),
}


def __getattr__(name: str) -> Any:
    """公開シンボルを遅延解決する。"""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module 'control_plane.engine' has no attribute {name!r}"
        raise AttributeError(msg)

    module_path, symbol_name = target
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
