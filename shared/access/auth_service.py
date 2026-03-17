"""共有 access 層の auth_service 互換窓口."""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP = {
    "AuthService": ("shared.auth_service.service", "AuthService"),
    "UserInfo": ("shared.auth_service.api.schemas", "UserInfo"),
    "get_auth_service": ("shared.auth_service.service", "get_auth_service"),
    "reset_auth_service": ("shared.auth_service.service", "reset_auth_service"),
}


def __getattr__(name: str) -> Any:
    """auth_service 公開面を遅延解決する。"""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module {__name__!r} has no attribute {name!r}"
        raise AttributeError(msg)
    module_path, symbol_name = target
    mod = importlib.import_module(module_path)
    return getattr(mod, symbol_name)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
