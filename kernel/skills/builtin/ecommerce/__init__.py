"""後方互換: plugins/ecommerce-skills/ecommerce/ からの re-export."""

from kernel.skills.builtin._compat import register_submodule_alias


register_submodule_alias(
    "kernel.skills.builtin.ecommerce",
    "plugins.ecommerce_skills.ecommerce",
    ["ad_monitor", "competitor_scraper", "daily_report", "inventory_adjuster", "listing_generator", "price_analyzer"],
)

from plugins.ecommerce_skills.ecommerce import *  # noqa: F403
