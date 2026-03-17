"""後方互換: specialized agents は各 app / shared に移動済み.

各 Agent の正規パス:
- FAQAgent → apps.faq_system.backend.agents.faq_agent
- DataAnalyticsAgent → apps.market_trend_monitor.agents.data_analytics_agent
- SalesAgent → shared.agents.sales_agent
"""

from __future__ import annotations

import importlib
from typing import Any

_LAZY_IMPORTS: dict[str, tuple[str, str]] = {
    "FAQAgent": ("apps.faq_system.backend.agents.faq_agent", "FAQAgent"),
    "FAQAgentConfig": ("apps.faq_system.backend.agents.faq_agent", "FAQAgentConfig"),
    "FAQInput": ("apps.faq_system.backend.agents.faq_agent", "FAQInput"),
    "FAQOutput": ("apps.faq_system.backend.agents.faq_agent", "FAQOutput"),
    "DataAnalyticsAgent": (
        "apps.market_trend_monitor.agents.data_analytics_agent",
        "DataAnalyticsAgent",
    ),
    "DataAnalyticsConfig": (
        "apps.market_trend_monitor.agents.data_analytics_agent",
        "DataAnalyticsConfig",
    ),
    "DataAnalyticsInput": (
        "apps.market_trend_monitor.agents.data_analytics_agent",
        "DataAnalyticsInput",
    ),
    "DataAnalyticsOutput": (
        "apps.market_trend_monitor.agents.data_analytics_agent",
        "DataAnalyticsOutput",
    ),
    "SalesAgent": ("shared.agents.sales_agent", "SalesAgent"),
    "SalesAgentConfig": ("shared.agents.sales_agent", "SalesAgentConfig"),
    "SalesInput": ("shared.agents.sales_agent", "SalesInput"),
    "SalesOutput": ("shared.agents.sales_agent", "SalesOutput"),
}


def __getattr__(name: str) -> Any:
    if name in _LAZY_IMPORTS:
        module_path, attr_name = _LAZY_IMPORTS[name]
        module = importlib.import_module(module_path)
        return getattr(module, attr_name)
    msg = f"module {__name__!r} has no attribute {name!r}"
    raise AttributeError(msg)


__all__ = list(_LAZY_IMPORTS.keys())
