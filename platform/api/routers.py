"""Layer 5 API router の正規窓口."""

from __future__ import annotations

import importlib
from platform._legacy import resolve_export
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "agents_router": ("platform.routers.agents", "router"),
    "apps_router": ("platform.routers.apps", "router"),
    "components_router": ("platform.routers.components", "router"),
    "dashboard_router": ("platform.routers.dashboard", "router"),
    "gallery_router": ("platform.routers.gallery", "router"),
    "init_agent_services": ("platform.routers.agents", "init_agent_services"),
    "init_app_services": ("platform.routers.apps", "init_app_services"),
    "init_llm_management_service": ("platform.routers.llm_management", "init_llm_management_service"),
    "init_mcp_services": ("platform.routers.mcp", "init_mcp_services"),
    "init_rag_services": ("platform.routers.rag", "init_rag_services"),
    "init_skill_services": ("platform.routers.skills", "init_skill_services"),
    "init_studio_services": ("platform.routers.studios", "init_studio_services"),
    "init_tenant_invitation_services": (
        "platform.routers.tenant_invitations",
        "init_tenant_invitation_services",
    ),
    "llm_management_router": ("platform.routers.llm_management", "router"),
    "mcp_router": ("platform.routers.mcp", "router"),
    "publish_router": ("platform.routers.publish", "router"),
    "rag_router": ("platform.routers.rag", "router"),
    "skills_router": ("platform.routers.skills", "router"),
    "studios_router": ("platform.routers.studios", "router"),
    "tenant_invitations_router": ("platform.routers.tenant_invitations", "router"),
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
