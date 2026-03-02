"""FastAPI アプリケーションファクトリ.

Migration Studio バックエンドのメインアプリ。

使用方法:
  cd /path/to/serverlessAIAgents
  uvicorn apps.migration_studio.backend.app:app --reload --port 8010
"""

from __future__ import annotations

import logging
from pathlib import Path

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles

from apps.migration_studio.backend.router import router

logger = logging.getLogger(__name__)


def create_app() -> FastAPI:
    """FastAPIアプリケーションを生成して返す."""
    app = FastAPI(
        title="Migration Studio API",
        description="COBOL → Java Spring Boot 自動移行パイプライン",
        version="1.0.0",
        docs_url="/docs",
        redoc_url="/redoc",
    )

    # CORS設定（フロントエンド開発サーバーからのアクセスを許可）
    # CORS: 開発時は全オリジン許可（credentials不要）
    # 本番環境では環境変数 MIGRATION_CORS_ORIGINS で制限すること
    import os
    cors_origins = os.environ.get("MIGRATION_CORS_ORIGINS", "*").split(",")
    app.add_middleware(
        CORSMiddleware,
        allow_origins=cors_origins,
        allow_credentials=False,
        allow_methods=["GET", "POST"],
        allow_headers=["Content-Type", "Accept"],
    )

    # APIルーター（StaticFiles より先に登録）
    app.include_router(router)

    @app.get("/health")
    async def health_check() -> dict[str, str]:
        """ヘルスチェックエンドポイント."""
        return {"status": "ok", "service": "migration-studio"}

    # フロントエンド静的ファイルをサーブ（必ず最後に mount）
    frontend_dir = Path(__file__).parent.parent / "frontend"
    if frontend_dir.exists():
        app.mount("/", StaticFiles(directory=str(frontend_dir), html=True), name="frontend")
        logger.info("フロントエンドをサーブ: %s", frontend_dir)
    else:
        logger.warning("フロントエンドディレクトリが存在しません: %s", frontend_dir)

    return app


app = create_app()


if __name__ == "__main__":
    import uvicorn

    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )
    uvicorn.run(
        "apps.migration_studio.backend.app:app",
        host="127.0.0.1",
        port=8010,
        reload=True,
        log_level="info",
    )
