"""後方互換: shared/skills/builtin/bi_analytics/ からの re-export."""

from kernel.skills.builtin._compat import register_submodule_alias


register_submodule_alias(
    "kernel.skills.builtin.bi_analytics",
    "shared.skills.builtin.bi_analytics",
    ["analyzer", "connector", "visualizer"],
)

from shared.skills.builtin.bi_analytics import *  # noqa: F403
