"""Standalone auth_service entrypoint owned by the shared layer."""

from __future__ import annotations

import logging
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING, Any

import uvicorn
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from shared.auth_service.api.router import router
from shared.auth_service.api.router_admin import router as admin_router
from shared.auth_service.api.router_authorization import router as authorization_router
from shared.auth_service.config import get_settings
from shared.auth_service.db.seed_authorization import (
    seed_authorization,
    seed_faq_resource_definitions,
)
from shared.auth_service.db.session import close_db, ensure_database_ready


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncIterator[None]:
    """Run auth_service startup and shutdown hooks."""
    del app
    settings = get_settings()
    logger = logging.getLogger(__name__)
    logger.info("auth_service 起動中... PORT=%d", settings.AUTH_SERVICE_PORT)

    await ensure_database_ready()
    logger.info("データベース準備完了")

    await seed_authorization()
    await seed_faq_resource_definitions()
    logger.info("シードデータ準備完了")

    yield

    await close_db()
    logger.info("auth_service シャットダウン完了")


def create_app() -> FastAPI:
    """Create the standalone auth_service FastAPI app."""
    settings = get_settings()
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
        return {
            "status": "ok",
            "service": "auth_service",
            "version": "1.0.0",
            "provider": settings.AUTH_PROVIDER,
        }

    return app


def main() -> None:
    """Run the standalone auth_service server."""
    settings = get_settings()
    logging.basicConfig(level=logging.INFO)
    uvicorn.run(
        "shared.auth_service.main:app",
        host=settings.AUTH_SERVICE_HOST,
        port=settings.AUTH_SERVICE_PORT,
        reload=True,
    )


def _get_app() -> FastAPI:
    """Platform entrypoint が存在する場合はそちらを返す."""
    try:
        from control_plane.api.auth_app import app as _platform_app

        return _platform_app
    except ImportError:
        return create_app()


app = _get_app()


__all__ = ["app", "create_app", "lifespan", "main"]


if __name__ == "__main__":
    main()
