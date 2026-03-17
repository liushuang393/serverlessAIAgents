"""後方互換: apps/market_trend_monitor/skills/intelligence/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.intelligence",
    "apps.market_trend_monitor.skills.intelligence",
    ["report_builder", "trend_analyzer", "web_crawler"],
)

from apps.market_trend_monitor.skills.intelligence import *  # noqa: F401,F403
