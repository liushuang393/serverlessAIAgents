"""Platform App Routers.

FastAPI ルーター定義。
"""

from apps.platform.routers.agents import router as agents_router
from apps.platform.routers.apps import router as apps_router
from apps.platform.routers.components import router as components_router
from apps.platform.routers.dashboard import router as dashboard_router
from apps.platform.routers.gallery import router as gallery_router
from apps.platform.routers.mcp import router as mcp_router
from apps.platform.routers.publish import router as publish_router
from apps.platform.routers.rag import router as rag_router
from apps.platform.routers.skills import router as skills_router
from apps.platform.routers.studios import router as studios_router
from apps.platform.routers.tenant_invitations import router as tenant_invitations_router


__all__ = [
    "agents_router",
    "apps_router",
    "components_router",
    "dashboard_router",
    "gallery_router",
    "mcp_router",
    "publish_router",
    "rag_router",
    "skills_router",
    "studios_router",
    "tenant_invitations_router",
]
