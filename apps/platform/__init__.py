"""Platform App shell 公開 API.

旧 ``apps.platform`` の公開面は互換維持しつつ、
正規実装は top-level ``platform`` へ移す。
"""

from __future__ import annotations

import importlib
from typing import Any


__version__ = "1.0.0"
__author__ = "AgentFlow Team"

_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "ComponentEntry": ("platform.engine", "ComponentEntry"),
    "ComponentLibrary": ("platform.engine", "ComponentLibrary"),
    "ComponentType": ("platform.engine", "ComponentType"),
    "ComponentVisibility": ("platform.engine", "ComponentVisibility"),
    "GalleryService": ("platform.engine", "GalleryService"),
    "PlatformEngine": ("platform.engine", "PlatformEngine"),
    "PublishOrchestrator": ("platform.engine", "PublishOrchestrator"),
}


def __getattr__(name: str) -> Any:
    """互換公開シンボルを遅延解決する。"""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module 'apps.platform' has no attribute {name!r}"
        raise AttributeError(msg)

    module_path, symbol_name = target
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


__all__ = [
    "ComponentEntry",
    "ComponentLibrary",
    "ComponentType",
    "ComponentVisibility",
    "GalleryService",
    "PlatformEngine",
    "PublishOrchestrator",
]
