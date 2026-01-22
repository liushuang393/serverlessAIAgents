# -*- coding: utf-8 -*-
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

import logging
import os
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager

from agentflow import setup_logging, LogLevel
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from apps.decision_governance_engine.flow_config import (
    register_flow_definition,
    setup_result_store,
)
from apps.decision_governance_engine.startup import log_startup_info

# ルーターインポート
from apps.decision_governance_engine.routers.auth import router as auth_router
from apps.decision_governance_engine.routers.config import router as config_router
from apps.decision_governance_engine.routers.decision import router as decision_router
from apps.decision_governance_engine.routers.knowledge import router as knowledge_router
from apps.decision_governance_engine.routers.product_launch import router as product_launch_router
from apps.decision_governance_engine.routers.report import router as report_router
from apps.decision_governance_engine.routers.workflow import router as workflow_router

logger = logging.getLogger("decision_api")


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None, None]:
    """アプリケーションライフサイクル管理.

    起動時: 設定情報をログ出力
    終了時: クリーンアップ処理
    """
    log_level = LogLevel.DEBUG if os.getenv("DEBUG") else LogLevel.INFO
    setup_logging(level=log_level, format="text")

    register_flow_definition()
    setup_result_store()
    log_startup_info()
    logger.info("Decision Governance Engine API started")

    yield

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
app.include_router(knowledge_router)
app.include_router(product_launch_router)
app.include_router(report_router)
app.include_router(workflow_router)


# ========================================
# アプリ起動（直接実行用）
# ========================================
if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000)
