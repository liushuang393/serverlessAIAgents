"""FAQ System ルーターパッケージ.

各ルーターは単一責務で API エンドポイントを提供する。
main.py から app.include_router() で登録される。
"""

from apps.faq_system.routers.agents import router as agents_router
from apps.faq_system.routers.chat import router as chat_router
from apps.faq_system.routers.kb_settings import router as kb_settings_router
from apps.faq_system.routers.misc import router as misc_router
from apps.faq_system.routers.rag import router as rag_router
from apps.faq_system.routers.sql import router as sql_router
from apps.faq_system.routers.websocket import router as ws_router

__all__ = [
    "agents_router",
    "chat_router",
    "kb_settings_router",
    "misc_router",
    "rag_router",
    "sql_router",
    "ws_router",
]
