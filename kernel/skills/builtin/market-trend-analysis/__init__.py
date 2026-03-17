"""後方互換: plugins/infrastructure-skills/market-trend-analysis/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.market_trend_analysis",
    "plugins.infrastructure_skills.market_trend_analysis",
    ["scripts"],
)

