"""Control-plane 移行用の遅延読み込みヘルパー."""

from __future__ import annotations

import importlib
from typing import Any


def load_symbol(module_path: str, symbol_name: str) -> Any:
    """指定モジュールからシンボルを遅延取得する。"""
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


def resolve_export(export_map: dict[str, tuple[str, str]], name: str, module_name: str) -> Any:
    """公開名から実装シンボルを解決する。"""
    target = export_map.get(name)
    if target is None:
        msg = f"module {module_name!r} has no attribute {name!r}"
        raise AttributeError(msg)

    module_path, symbol_name = target
    return load_symbol(module_path, symbol_name)
