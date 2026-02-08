"""FastAPI メインアプリケーション.

REST API と WebSocket エンドポイントを提供します。
"""

import logging
from contextlib import asynccontextmanager
from typing import AsyncGenerator

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from apps.market_trend_monitor.backend.api.routes import (
    collect_router,
    evidence_router,
    predictions_router,
    settings_router,
    signals_router,
    sources_router,
    trends_router,
)
from apps.market_trend_monitor.backend.api.state import source_registry_service
from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.db import init_db
from apps.market_trend_monitor.backend.workflow import workflow

# ロギング設定
logging.basicConfig(
    level=getattr(logging, config.log_level),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None, None]:
    """アプリケーションライフサイクル管理.

    Args:
        app: FastAPI アプリケーション

    Yields:
        None
    """
    # 起動時処理
    logger.info("Market Trend Monitor API を起動中")
    await init_db()
    await source_registry_service.ensure_defaults()
    await workflow.initialize()

    yield

    # 終了時処理
    logger.info("Market Trend Monitor API を終了中")
    await workflow.cleanup()


# FastAPI アプリケーション作成
app = FastAPI(
    title="Market Trend Monitor API",
    description="市場動向監視システム REST API",
    version="1.0.0",
    lifespan=lifespan,
)

# CORS 設定
app.add_middleware(
    CORSMiddleware,
    allow_origins=config.api.cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/")
async def root() -> dict[str, str]:
    """ルートエンドポイント.

    Returns:
        ウェルカムメッセージ
    """
    return {
        "message": "Market Trend Monitor API",
        "version": "1.0.0",
        "status": "running",
    }


@app.get("/health")
async def health() -> dict[str, str]:
    """ヘルスチェックエンドポイント.

    Returns:
        ヘルスステータス
    """
    return {"status": "healthy"}


app.include_router(collect_router)
app.include_router(trends_router)
app.include_router(evidence_router)
app.include_router(signals_router)
app.include_router(predictions_router)
app.include_router(sources_router)
app.include_router(settings_router)


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(
        "apps.market_trend_monitor.backend.api.main:app",
        host=config.api.host,
        port=config.api.port,
        reload=True,
        reload_excludes=["**/__pycache__/**", "**/data/**", "**/*.pyc"],
    )
