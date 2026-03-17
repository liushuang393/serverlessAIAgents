"""後方互換: shared/skills/builtin/conversation_export/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.conversation_export",
    "shared.skills.builtin.conversation_export",
    ["conversation_export"],
)

from shared.skills.builtin.conversation_export import *  # noqa: F401,F403
