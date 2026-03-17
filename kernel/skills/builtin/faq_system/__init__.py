"""後方互換: apps/faq_system/skills/faq_system/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.faq_system",
    "apps.faq_system.skills.faq_system",
    ["manager"],
)

from apps.faq_system.skills.faq_system import *  # noqa: F401,F403
