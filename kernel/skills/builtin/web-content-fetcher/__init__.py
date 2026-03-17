"""後方互換: plugins/infrastructure-skills/web-content-fetcher/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.web_content_fetcher",
    "plugins.infrastructure_skills.web_content_fetcher",
    ["scripts"],
)

