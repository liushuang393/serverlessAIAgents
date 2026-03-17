"""Compatibility facade for the moved dev_studio codegen package."""

from __future__ import annotations

import importlib
from types import ModuleType

import warnings as _warnings

_warnings.warn(
    "control_plane.codegen は apps.dev_studio.codegen に移動しました。"
    " 新しいインポートパスを使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

_TARGET_MODULE = "apps.dev_studio.codegen"


def _load_target_module() -> ModuleType:
    return importlib.import_module(_TARGET_MODULE)


def __getattr__(name: str) -> object:
    return getattr(_load_target_module(), name)


def __dir__() -> list[str]:
    return sorted(set(globals()) | set(dir(_load_target_module())))


__all__ = ["CodeGenerator"]
