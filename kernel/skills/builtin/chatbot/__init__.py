"""後方互換: shared/skills/builtin/chatbot/ からの re-export."""

from kernel.skills.builtin._compat import register_submodule_alias


register_submodule_alias(
    "kernel.skills.builtin.chatbot",
    "shared.skills.builtin.chatbot",
    ["chatbot"],
)

from shared.skills.builtin.chatbot import *  # noqa: F403
