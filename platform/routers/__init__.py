"""Platform router shell.

正規公開面は top-level ``platform.api.routers`` に集約する。
"""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "agents_router": ("platform.api.routers", "agents_router"),
    "apps_router": ("platform.api.routers", "apps_router"),
    "components_router": ("platform.api.routers", "components_router"),
    "dashboard_router": ("platform.api.routers", "dashboard_router"),
    "gallery_router": ("platform.api.routers", "gallery_router"),
    "llm_management_router": ("platform.api.routers", "llm_management_router"),
    "mcp_router": ("platform.api.routers", "mcp_router"),
    "publish_router": ("platform.api.routers", "publish_router"),
    "rag_router": ("platform.api.routers", "rag_router"),
    "skills_router": ("platform.api.routers", "skills_router"),
    "studios_router": ("platform.api.routers", "studios_router"),
    "tenant_invitations_router": ("platform.api.routers", "tenant_invitations_router"),
}


def __getattr__(name: str) -> Any:
    """互換 router を遅延解決する。"""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module 'platform.routers' has no attribute {name!r}"
        raise AttributeError(msg)

    module_path, symbol_name = target
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


__all__ = [
    "agents_router",
    "apps_router",
    "components_router",
    "dashboard_router",
    "gallery_router",
    "llm_management_router",
    "mcp_router",
    "publish_router",
    "rag_router",
    "skills_router",
    "studios_router",
    "tenant_invitations_router",
]
