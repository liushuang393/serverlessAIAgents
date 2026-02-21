"""auth_service FastAPI アプリケーションエントリーポイント.

起動:
    uvicorn apps.auth_service.main:app --port 8010 --reload

または:
    python -m apps.auth_service.main
"""

from __future__ import annotations

import logging
from contextlib import asynccontextmanager
from typing import Any

from apps.auth_service.api.router import router
from apps.auth_service.config import get_settings
from apps.auth_service.db.session import close_db, ensure_database_ready
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware


logger = logging.getLogger(__name__)


@asynccontextmanager
async def lifespan(app: FastAPI):  # type: ignore[type-arg]
    """アプリライフサイクル管理."""
    settings = get_settings()
    logger.info("auth_service 起動中... PORT=%d", settings.AUTH_SERVICE_PORT)

    # DB 初期化
    await ensure_database_ready()
    logger.info("データベース準備完了")

    yield

    # シャットダウン
    await close_db()
    logger.info("auth_service シャットダウン完了")


def create_app() -> FastAPI:
    """FastAPI アプリを生成."""
    settings = get_settings()

    app = FastAPI(
        title="Auth Service",
        description="スタンドアロン認証サービス - JWT・OAuth2・LDAP・SAML・MFA 対応",
        version="1.0.0",
        docs_url="/docs",
        redoc_url="/redoc",
        lifespan=lifespan,
    )

    # CORS 設定
    app.add_middleware(
        CORSMiddleware,
        allow_origins=settings.ALLOWED_ORIGINS,
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

    # 認証ルーター
    app.include_router(router)

    # グローバルヘルスチェック
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


app = create_app()


if __name__ == "__main__":
    import uvicorn

    settings = get_settings()
    logging.basicConfig(level=logging.INFO)
    uvicorn.run(
        "apps.auth_service.main:app",
        host=settings.AUTH_SERVICE_HOST,
        port=settings.AUTH_SERVICE_PORT,
        reload=True,
    )
