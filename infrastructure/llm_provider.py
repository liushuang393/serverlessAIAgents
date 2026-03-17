"""旧 `infrastructure.llm_provider` の互換 facade."""

from __future__ import annotations

import importlib
from typing import Any


_TARGET_MODULE = "infrastructure.llm.providers.llm_provider"


def __getattr__(name: str) -> Any:
    """公開シンボルを遅延解決する。"""
    module = importlib.import_module(_TARGET_MODULE)
    return getattr(module, name)


def __dir__() -> list[str]:
    """公開シンボル一覧を返す。"""
    module = importlib.import_module(_TARGET_MODULE)
    exports = getattr(module, "__all__", None)
    if isinstance(exports, list | tuple):
        return sorted(str(name) for name in exports)
    return sorted(name for name in vars(module) if not name.startswith("_"))


__all__ = list(__dir__())
