"""Legacy config.settings compatibility shim. 実体は infrastructure.config.settings."""

from infrastructure.config.settings import (  # noqa: F401
    AgentFlowSettings,
    clear_settings_cache,
    get_settings,
)

__all__ = ["AgentFlowSettings", "clear_settings_cache", "get_settings"]
