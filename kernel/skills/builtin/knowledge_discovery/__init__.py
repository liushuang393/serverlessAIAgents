"""後方互換: apps/faq_system/skills/knowledge_discovery/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.knowledge_discovery",
    "apps.faq_system.skills.knowledge_discovery",
    ["manager"],
)

from apps.faq_system.skills.knowledge_discovery import *  # noqa: F401,F403
