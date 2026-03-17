"""後方互換: shared/skills/builtin/voice/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.voice",
    "shared.skills.builtin.voice",
    ["voice"],
)

from shared.skills.builtin.voice import *  # noqa: F401,F403
