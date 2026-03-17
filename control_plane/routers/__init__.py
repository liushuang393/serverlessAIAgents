"""Platform router shell.

正規公開面は top-level ``control_plane.api.routers`` に集約する。
"""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "agents_router": ("control_plane.api.routers", "agents_router"),
    "apps_router": ("control_plane.api.routers", "apps_router"),
    "components_router": ("control_plane.api.routers", "components_router"),
    "dashboard_router": ("control_plane.api.routers", "dashboard_router"),
    "gallery_router": ("control_plane.api.routers", "gallery_router"),
    "llm_management_router": ("control_plane.api.routers", "llm_management_router"),
    "mcp_router": ("control_plane.api.routers", "mcp_router"),
    "publish_router": ("control_plane.api.routers", "publish_router"),
    "rag_router": ("control_plane.api.routers", "rag_router"),
    "skills_router": ("control_plane.api.routers", "skills_router"),
    "studios_router": ("control_plane.api.routers", "studios_router"),
    "tenant_invitations_router": ("control_plane.api.routers", "tenant_invitations_router"),
}


def __getattr__(name: str) -> Any:
    """互換 router を遅延解決する。"""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module 'control_plane.routers' has no attribute {name!r}"
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
