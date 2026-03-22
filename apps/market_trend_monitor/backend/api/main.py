"""FastAPI メインアプリケーション.

REST API と WebSocket エンドポイントを提供します。
"""

import logging
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from pathlib import Path
from typing import Any

from apps.market_trend_monitor.backend.api.routes import (
    collect_router,
    competitors_router,
    evidence_router,
    jobs_router,
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
from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware

from harness.gating.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig


# ロギング設定
logging.basicConfig(
    level=getattr(logging, config.log_level),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# ContractAuthGuard 設定（自動トリガー: HTTPミドルウェアとして全リクエストに適用）
# auth.enabled=false の間は匿名アクセスを許可、有効化後はAPIキー認証が自動適用される
# ---------------------------------------------------------------------------
_APP_ROOT = Path(__file__).resolve().parents[2]  # apps/market_trend_monitor/
_APP_CONFIG_PATH = _APP_ROOT / "app_config.json"
_PUBLIC_HTTP_PATHS = {"/", "/health", "/docs", "/redoc", "/openapi.json"}

_auth_guard = ContractAuthGuard(
    ContractAuthGuardConfig(
        app_config_path=_APP_CONFIG_PATH,
        public_http_paths=_PUBLIC_HTTP_PATHS,
        auth_header_name="x-api-key",
        ws_query_key="api_key",
        api_key_env_selector_var="MARKET_TREND_API_KEY_ENV",
        default_api_key_env_var="MARKET_TREND_API_KEY",
    ),
)


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

    # サーバー再起動時に中断された "running" ジョブを "failed" にリセット
    from datetime import datetime

    from apps.market_trend_monitor.backend.db import async_session as _session_factory
    from apps.market_trend_monitor.backend.db.models import CollectJobModel
    from sqlalchemy import update as _update

    async with _session_factory() as _sess, _sess.begin():
        await _sess.execute(
            _update(CollectJobModel)
            .where(CollectJobModel.status == "running")
            .values(
                status="failed",
                current_step="中断（サーバー再起動）",
                completed_at=datetime.now(),
                error="サーバーが再起動されたため処理が中断されました",
            )
        )
    logger.info("中断ジョブのリセット完了")

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


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """ContractAuthGuard による HTTP 認証（自動トリガー）.

    app_config.json の contracts.auth.enabled=true かつ allow_anonymous=false の場合に認証を強制。
    現在は enabled=false のため匿名アクセスを許可（将来の有効化に備えた準備）。
    """
    return await _auth_guard.http_middleware(request, call_next)


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
app.include_router(jobs_router)
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
    from pathlib import Path

    import uvicorn

    parser = argparse.ArgumentParser(description="Market Trend Monitor - FastAPI Backend")
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
