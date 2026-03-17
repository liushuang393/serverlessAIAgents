"""Layer 5 lifecycle 実装の互換窓口."""

from __future__ import annotations

from platform._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "AppLifecycleManager": ("platform.services.app_lifecycle", "AppLifecycleManager"),
    "PortAllocatorService": ("platform.services.port_allocator", "PortAllocatorService"),
    "ResolvedRuntimeCommands": (
        "platform.services.runtime_command_resolver",
        "ResolvedRuntimeCommands",
    ),
    "RuntimeCommandResolver": (
        "platform.services.runtime_command_resolver",
        "RuntimeCommandResolver",
    ),
}


def __getattr__(name: str) -> Any:
    """lifecycle 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
