"""Decision Governance Engine - FastAPI REST API.

モジュール化されたルーター構成:
    - routers/auth.py: 認証関連
    - routers/decision.py: 決策処理
    - routers/knowledge.py: 知識ベース管理（通用工厂）
    - routers/report.py: レポート関連
    - routers/workflow.py: ワークフロー設定

エンドポイント一覧:
- POST /api/auth/login, /logout, GET /me
- POST /api/decision, GET /api/decision/stream
- GET/POST/DELETE /api/knowledge/{shu|qi}
- GET /api/report/{id}/pdf, /components, /sign
- GET /api/health, /api/agents, /api/flows/{id}/definition
"""

# 環境変数をロード（最初に実行）
from dotenv import load_dotenv


load_dotenv()

import json
import logging
import os
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from pathlib import Path

from apps.decision_governance_engine.flow_config import (
    register_flow_definition,
    setup_result_store,
)

# ルーターインポート
from apps.decision_governance_engine.routers.auth import router as auth_router
from apps.decision_governance_engine.routers.config import router as config_router
from apps.decision_governance_engine.routers.decision import router as decision_router
from apps.decision_governance_engine.routers.human_review import router as human_review_router
from apps.decision_governance_engine.routers.knowledge import router as knowledge_router
from apps.decision_governance_engine.routers.product_launch import router as product_launch_router
from apps.decision_governance_engine.routers.report import router as report_router
from apps.decision_governance_engine.routers.workflow import router as workflow_router
from apps.decision_governance_engine.startup import log_startup_info
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from agentflow import LogLevel, setup_logging


logger = logging.getLogger("decision_api")


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None]:
    """アプリケーションライフサイクル管理.

    起動時: DB初期化 + 設定情報をログ出力
    終了時: DB接続クローズ + クリーンアップ処理
    """
    log_level = LogLevel.DEBUG if os.getenv("DEBUG") else LogLevel.INFO
    setup_logging(level=log_level, format="text")

    # データベース接続を確立（履歴保存に必須）
    from apps.decision_governance_engine.repositories.database import close_db, init_db

    try:
        await init_db()
        logger.info("Database connection established")
    except Exception as e:
        logger.warning(f"Database initialization failed (history will use fallback): {e}")

    register_flow_definition()
    setup_result_store()
    log_startup_info()
    logger.info("Decision Governance Engine API started")

    yield

    # DB接続をクリーンアップ
    try:
        await close_db()
    except Exception as e:
        logger.warning(f"Database cleanup error: {e}")
    logger.info("Decision Governance Engine API shutting down")


# FastAPIアプリ
app = FastAPI(
    title="Decision Governance Engine API",
    description="企業級決策Agent平台 - 意思決定支援システム",
    version="2.0.0",
    docs_url="/docs",
    redoc_url="/redoc",
    lifespan=lifespan,
)

# CORS設定
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # 本番環境では適切に設定
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ========================================
# ルーター登録
# ========================================
app.include_router(auth_router)
app.include_router(config_router)
app.include_router(decision_router)
app.include_router(human_review_router)
app.include_router(knowledge_router)
app.include_router(product_launch_router)
app.include_router(report_router)
app.include_router(workflow_router)


# ========================================
# アプリ起動（直接実行用）
# ========================================
if __name__ == "__main__":
    import argparse

    import uvicorn

    parser = argparse.ArgumentParser(description="Decision Governance Engine - FastAPI Backend")
    parser.add_argument(
        "--reload",
        action="store_true",
        help="開発モード（ホットリロード有効）",
    )
    parser.add_argument(
        "--host",
        default=None,
        help="ホスト（省略時: 環境変数 DGE_HOST / デフォルト 0.0.0.0）",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=None,
        help="ポート（省略時: 環境変数 DGE_PORT / app_config.json）",
    )
    args = parser.parse_args()

    config_path = Path(__file__).resolve().parent / "app_config.json"
    config_raw: dict = {}
    if config_path.is_file():
        try:
            config_raw = json.loads(config_path.read_text("utf-8"))
        except json.JSONDecodeError:
            config_raw = {}

    _default_port = config_raw.get("ports", {}).get("api", 8001)
    _host = args.host or os.getenv("DGE_HOST", "0.0.0.0")
    _port = args.port or int(os.getenv("DGE_PORT", str(_default_port)))

    print(f"[DGE] Starting on {_host}:{_port} (reload={args.reload})")

    if args.reload:
        uvicorn.run(
            "apps.decision_governance_engine.api:app",
            host=_host,
            port=_port,
            reload=True,
            reload_dirs=["apps/decision_governance_engine", "agentflow"],
        )
    else:
        uvicorn.run(app, host=_host, port=_port)
