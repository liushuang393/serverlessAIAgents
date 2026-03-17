"""後方互換: plugins/infrastructure-skills/deployment-manager/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.deployment_manager",
    "plugins.infrastructure_skills.deployment_manager",
    ["exceptions", "manager"],
)

