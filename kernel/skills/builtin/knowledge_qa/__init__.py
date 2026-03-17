"""後方互換: apps/faq_system/skills/knowledge_qa/ からの re-export."""

from __future__ import annotations

import importlib
from types import ModuleType

from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.knowledge_qa",
    "apps.faq_system.skills.knowledge_qa",
    ["answer_generator", "doc_ingester", "gap_analyzer", "retriever"],
)

_TARGET_MODULE = "apps.faq_system.skills.knowledge_qa"


def _load_target_module() -> ModuleType:
    return importlib.import_module(_TARGET_MODULE)


def __getattr__(name: str) -> object:
    return getattr(_load_target_module(), name)


def __dir__() -> list[str]:
    return sorted(set(globals()) | set(dir(_load_target_module())))


__all__ = [name for name in dir(_load_target_module()) if not name.startswith("_")]
