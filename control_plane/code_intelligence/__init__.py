"""Compatibility facade for the moved dev_studio code intelligence package."""

from __future__ import annotations

import importlib
from types import ModuleType

import warnings as _warnings

_warnings.warn(
    "control_plane.code_intelligence は apps.dev_studio.code_intelligence に移動しました。"
    " 新しいインポートパスを使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

_TARGET_MODULE = "apps.dev_studio.code_intelligence"


def _load_target_module() -> ModuleType:
    return importlib.import_module(_TARGET_MODULE)


def __getattr__(name: str) -> object:
    return getattr(_load_target_module(), name)


def __dir__() -> list[str]:
    return sorted(set(globals()) | set(dir(_load_target_module())))


__all__ = [name for name in dir(_load_target_module()) if not name.startswith("_")]
