"""後方互換: plugins/infrastructure-skills/database-manager/ からの re-export."""

from kernel.skills.builtin._compat import register_submodule_alias


register_submodule_alias(
    "kernel.skills.builtin.database_manager",
    "plugins.infrastructure_skills.database_manager",
    ["exceptions", "manager"],
)
