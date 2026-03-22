"""後方互換: apps/market_trend_monitor/agents/data_analytics_agent からの re-export."""

from __future__ import annotations

import importlib
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from types import ModuleType


_TARGET_MODULE = "apps.market_trend_monitor.agents.data_analytics_agent"


def _load_target_module() -> ModuleType:
    return importlib.import_module(_TARGET_MODULE)


def __getattr__(name: str) -> object:
    return getattr(_load_target_module(), name)


def __dir__() -> list[str]:
    return sorted(set(globals()) | set(dir(_load_target_module())))


__all__ = [name for name in dir(_load_target_module()) if not name.startswith("_")]
