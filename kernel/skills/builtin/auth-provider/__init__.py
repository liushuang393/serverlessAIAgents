"""後方互換: plugins/infrastructure-skills/auth-provider/ からの re-export."""

from kernel.skills.builtin._compat import register_submodule_alias


register_submodule_alias(
    "kernel.skills.builtin.auth_provider",
    "plugins.infrastructure_skills.auth_provider",
    ["exceptions", "provider"],
)
