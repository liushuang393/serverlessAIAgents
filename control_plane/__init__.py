"""BizCore control plane public API."""

from __future__ import annotations

import importlib
from typing import Any


_SERVICE_MODULES = {
    "AppRegistryService": "control_plane.registry",
    "DiscoveryService": "control_plane.discovery",
    "LifecycleService": "control_plane.lifecycle",
}


def __getattr__(name: str) -> Any:
    """Resolve public control-plane services lazily."""
    module_path = _SERVICE_MODULES.get(name)
    if module_path is None:
        msg = f"module 'control_plane' has no attribute {name!r}"
        raise AttributeError(msg)

    module = importlib.import_module(module_path)
    return getattr(module, name)


__all__ = ("AppRegistryService", "DiscoveryService", "LifecycleService")
