"""Control-plane API router の正規窓口."""

from __future__ import annotations

import importlib
from typing import Any

from control_plane._legacy import resolve_export


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "agents_router": ("control_plane.routers.agents", "router"),
    "apps_router": ("control_plane.routers.apps", "router"),
    "components_router": ("control_plane.routers.components", "router"),
    "dashboard_router": ("control_plane.routers.dashboard", "router"),
    "gallery_router": ("control_plane.routers.gallery", "router"),
    "init_agent_services": ("control_plane.routers.agents", "init_agent_services"),
    "init_app_services": ("control_plane.routers.apps", "init_app_services"),
    "init_llm_management_service": ("control_plane.routers.llm_management", "init_llm_management_service"),
    "init_mcp_services": ("control_plane.routers.mcp", "init_mcp_services"),
    "init_rag_services": ("control_plane.routers.rag", "init_rag_services"),
    "init_skill_services": ("control_plane.routers.skills", "init_skill_services"),
    "init_studio_services": ("control_plane.routers.studios", "init_studio_services"),
    "init_tenant_invitation_services": (
        "control_plane.routers.tenant_invitations",
        "init_tenant_invitation_services",
    ),
    "llm_management_router": ("control_plane.routers.llm_management", "router"),
    "mcp_router": ("control_plane.routers.mcp", "router"),
    "publish_router": ("control_plane.routers.publish", "router"),
    "rag_router": ("control_plane.routers.rag", "router"),
    "skills_router": ("control_plane.routers.skills", "router"),
    "studios_router": ("control_plane.routers.studios", "router"),
    "tenant_invitations_router": ("control_plane.routers.tenant_invitations", "router"),
}

_ROUTER_EXPORTS: tuple[str, ...] = (
    "gallery_router",
    "components_router",
    "publish_router",
    "dashboard_router",
    "apps_router",
    "agents_router",
    "skills_router",
    "rag_router",
    "mcp_router",
    "llm_management_router",
    "studios_router",
    "tenant_invitations_router",
)


def iter_default_routers() -> list[Any]:
    """標準で公開する router 一覧を返す。"""
    module = importlib.import_module(__name__)
    return [getattr(module, name) for name in _ROUTER_EXPORTS]


def __getattr__(name: str) -> Any:
    """router または初期化関数を遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted((*_ROUTER_EXPORTS, *(name for name in _EXPORT_MAP if name.startswith("init_")))))
