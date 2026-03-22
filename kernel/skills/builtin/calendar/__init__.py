"""後方互換: shared/skills/builtin/calendar/ からの re-export."""

from kernel.skills.builtin._compat import register_submodule_alias


register_submodule_alias(
    "kernel.skills.builtin.calendar",
    "shared.skills.builtin.calendar",
    ["calendar"],
)

from shared.skills.builtin.calendar import *  # noqa: F403
