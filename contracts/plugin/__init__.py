"""Plugin 契約."""

from contracts.plugin.contracts import (
    PluginBinding,
    PluginDescriptor,
    PluginRegistryProtocol,
    PluginRuntimeAssessment,
)


__all__ = [
    "PluginBinding",
    "PluginDescriptor",
    "PluginRegistryProtocol",
    "PluginRuntimeAssessment",
]
