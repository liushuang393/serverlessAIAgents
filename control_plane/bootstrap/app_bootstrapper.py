"""Compatibility wrapper for the runtime app capability bootstrapper."""

from kernel.runtime.bootstrap import AppCapabilityBootstrapper
from shared.config.manifest import load_app_manifest_dict_text


__all__ = ["AppCapabilityBootstrapper", "load_app_manifest_dict_text"]
