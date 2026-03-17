"""後方互換: shared/skills/builtin/rag/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.rag",
    "shared.skills.builtin.rag",
    ["rag"],
)

from shared.skills.builtin.rag import *  # noqa: F401,F403
from shared.skills.builtin.rag.rag import *  # noqa: F401,F403
