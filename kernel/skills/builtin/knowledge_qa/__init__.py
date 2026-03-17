"""後方互換: apps/faq_system/skills/knowledge_qa/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.knowledge_qa",
    "apps.faq_system.skills.knowledge_qa",
    ["answer_generator", "doc_ingester", "gap_analyzer", "retriever"],
)

from apps.faq_system.skills.knowledge_qa import *  # noqa: F401,F403
