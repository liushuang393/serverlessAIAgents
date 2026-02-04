# -*- coding: utf-8 -*-
"""Platform App Routers.

FastAPI ルーター定義。
"""

from apps.platform.routers.gallery import router as gallery_router
from apps.platform.routers.components import router as components_router
from apps.platform.routers.publish import router as publish_router
from apps.platform.routers.dashboard import router as dashboard_router

__all__ = [
    "gallery_router",
    "components_router",
    "publish_router",
    "dashboard_router",
]
