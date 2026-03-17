"""Platform 公開エンジンの互換 facade.

最終的な実装は top-level ``platform/`` へ集約するが、
移行期間中は既存 ``apps.platform`` 実装を段階的に吸収する。
このモジュールは公開面だけを top-level に固定する。
"""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "ComponentEntry": ("platform.publish.runtime", "ComponentEntry"),
    "ComponentLibrary": ("platform.publish.runtime", "ComponentLibrary"),
    "ComponentType": ("platform.publish.runtime", "ComponentType"),
    "ComponentVisibility": ("platform.publish.runtime", "ComponentVisibility"),
    "GalleryService": ("platform.publish.runtime", "GalleryService"),
    "PlatformEngine": ("platform.runtime_engine", "PlatformEngine"),
    "PublishOrchestrator": ("platform.publish.runtime", "PublishOrchestrator"),
}


def __getattr__(name: str) -> Any:
    """公開シンボルを遅延解決する。"""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module 'platform.engine' has no attribute {name!r}"
        raise AttributeError(msg)

    module_path, symbol_name = target
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
