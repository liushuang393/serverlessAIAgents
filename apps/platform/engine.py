"""Platform Engine shell.

正規実装は top-level ``platform.runtime_engine`` に移し、
このモジュールは旧 import 互換だけを保持する。
"""

from platform.engine import (
    ComponentEntry,
    ComponentLibrary,
    ComponentType,
    ComponentVisibility,
    GalleryService,
    PlatformEngine,
    PublishOrchestrator,
)


__all__ = [
    "ComponentEntry",
    "ComponentLibrary",
    "ComponentType",
    "ComponentVisibility",
    "GalleryService",
    "PlatformEngine",
    "PublishOrchestrator",
]
