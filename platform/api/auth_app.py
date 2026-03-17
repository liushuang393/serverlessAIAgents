"""Layer 5 auth_service API の正規エントリーポイント."""

from __future__ import annotations

import logging
from contextlib import asynccontextmanager
from platform._legacy import load_symbol
from typing import TYPE_CHECKING, Any

import uvicorn
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


def _get_settings() -> Any:
    """auth_service 設定を取得する。"""
    return load_symbol("apps.auth_service.config", "get_settings")()


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncIterator[None]:
    """auth_service の起動・終了処理."""
    del app
    settings = _get_settings()
    logger = logging.getLogger(__name__)
    logger.info("auth_service 起動中... PORT=%d", settings.AUTH_SERVICE_PORT)

    ensure_database_ready = load_symbol("apps.auth_service.db.session", "ensure_database_ready")
    close_db = load_symbol("apps.auth_service.db.session", "close_db")
    seed_authorization = load_symbol("apps.auth_service.db.seed_authorization", "seed_authorization")
    seed_faq_resource_definitions = load_symbol(
        "apps.auth_service.db.seed_authorization",
        "seed_faq_resource_definitions",
    )

    await ensure_database_ready()
    logger.info("データベース準備完了")

    await seed_authorization()
    await seed_faq_resource_definitions()
    logger.info("シードデータ準備完了")

    yield

    await close_db()
    logger.info("auth_service シャットダウン完了")


def create_app() -> FastAPI:
    """auth_service FastAPI アプリを生成する。"""
    settings = _get_settings()
    router = load_symbol("apps.auth_service.api.router", "router")
    admin_router = load_symbol("apps.auth_service.api.router_admin", "router")
    authorization_router = load_symbol("apps.auth_service.api.router_authorization", "router")

    app = FastAPI(
        title="Auth Service",
        description="スタンドアロン認証サービス - JWT・OAuth2・LDAP・SAML・MFA 対応",
        version="1.0.0",
        docs_url="/docs",
        redoc_url="/redoc",
        lifespan=lifespan,
    )
    app.add_middleware(
        CORSMiddleware,
        allow_origins=settings.ALLOWED_ORIGINS,
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

    app.include_router(router)
    app.include_router(authorization_router)
    app.include_router(admin_router)

    @app.get("/health", tags=["ヘルス"])
    async def health() -> dict[str, Any]:
        """サービスヘルスチェック."""
        return {
            "status": "ok",
            "service": "auth_service",
            "version": "1.0.0",
            "provider": settings.AUTH_PROVIDER,
        }

    return app


def main() -> None:
    """auth_service サーバーを起動する。"""
    settings = _get_settings()
    logging.basicConfig(level=logging.INFO)
    uvicorn.run(
        "platform.api.auth_app:app",
        host=settings.AUTH_SERVICE_HOST,
        port=settings.AUTH_SERVICE_PORT,
        reload=True,
    )


app = create_app()
