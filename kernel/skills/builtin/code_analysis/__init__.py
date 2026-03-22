"""後方互換: apps/code_migration_assistant/skills/code_analysis/ からの re-export."""

from __future__ import annotations

import importlib
from typing import TYPE_CHECKING

from kernel.skills.builtin._compat import register_submodule_alias


if TYPE_CHECKING:
    from types import ModuleType


register_submodule_alias(
    "kernel.skills.builtin.code_analysis",
    "apps.code_migration_assistant.skills.code_analysis",
    [
        "complexity_scorer",
        "dependency_mapper",
        "migration_planner",
        "repo_connector",
        "security_scanner",
        "static_analyzer",
    ],
)

_TARGET_MODULE = "apps.code_migration_assistant.skills.code_analysis"


def _load_target_module() -> ModuleType:
    return importlib.import_module(_TARGET_MODULE)


def __getattr__(name: str) -> object:
    return getattr(_load_target_module(), name)


def __dir__() -> list[str]:
    return sorted(set(globals()) | set(dir(_load_target_module())))


__all__ = [name for name in dir(_load_target_module()) if not name.startswith("_")]
