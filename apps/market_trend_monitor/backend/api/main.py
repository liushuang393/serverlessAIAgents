"""FastAPI メインアプリケーション.

REST API と WebSocket エンドポイントを提供します。
"""

import logging
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager

from apps.market_trend_monitor.backend.api.routes import (
    collect_router,
    competitors_router,
    evidence_router,
    metrics_router,
    predictions_router,
    settings_router,
    signals_router,
    sources_router,
    trends_router,
)
from apps.market_trend_monitor.backend.api.state import (
    prediction_service,
    signal_service,
    source_registry_service,
)
from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.db import init_db
from apps.market_trend_monitor.backend.workflow import workflow
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware


# ロギング設定
logging.basicConfig(
    level=getattr(logging, config.log_level),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None]:
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
    await prediction_service.initialize()
    await signal_service.initialize()
    from apps.market_trend_monitor.backend.api.state import store
    await store.initialize()
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
app.include_router(competitors_router)
app.include_router(metrics_router)
app.include_router(settings_router)


if __name__ == "__main__":
    import argparse

    import uvicorn
    from pathlib import Path

    parser = argparse.ArgumentParser(
        description="Market Trend Monitor - FastAPI Backend"
    )
    parser.add_argument(
        "--reload",
        action="store_true",
        help="開発モード（ホットリロード有効）",
    )
    parser.add_argument(
        "--host",
        default=None,
        help="ホスト（省略時: config / デフォルト 0.0.0.0）",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=None,
        help="ポート（省略時: config / app_config.json）",
    )
    args = parser.parse_args()

    _host = args.host or config.api.host
    _port = args.port or config.api.port

    print(f"[Market Trend Monitor] Starting on {_host}:{_port} (reload={args.reload})")

    if args.reload:
        # バックエンドソースディレクトリのみを監視対象にして高速リロード
        _backend_dir = str(Path(__file__).resolve().parents[2])
        uvicorn.run(
            "apps.market_trend_monitor.backend.api.main:app",
            host=_host,
            port=_port,
            reload=True,
            reload_dirs=[_backend_dir],
            reload_excludes=["**/__pycache__/**", "**/data/**", "**/*.pyc"],
        )
    else:
        uvicorn.run(app, host=_host, port=_port)
