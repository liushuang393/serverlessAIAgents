"""後方互換: apps/code_migration_assistant/skills/code_analysis/ からの re-export."""
from kernel.skills.builtin._compat import register_submodule_alias

register_submodule_alias(
    "kernel.skills.builtin.code_analysis",
    "apps.code_migration_assistant.skills.code_analysis",
    ["complexity_scorer", "dependency_mapper", "migration_planner",
     "repo_connector", "security_scanner", "static_analyzer"],
)

from apps.code_migration_assistant.skills.code_analysis import *  # noqa: F401,F403
