# -*- coding: utf-8 -*-
"""{{ app_title }} - FastAPI エントリポイント.

{{ app_description }}
"""
from contextlib import asynccontextmanager
from typing import AsyncGenerator

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from config import get_settings
from repositories.database import init_db, close_db
from routers import health_router, api_router


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None, None]:
    """アプリケーションライフサイクル管理.
    
    起動時: DB 接続初期化
    終了時: DB 接続クローズ
    """
    # 起動処理
    await init_db()
    yield
    # 終了処理
    await close_db()


# FastAPI アプリケーション
app = FastAPI(
    title="{{ app_title }}",
    description="{{ app_description }}",
    version="1.0.0",
    lifespan=lifespan,
)

# CORS 設定
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # 本番環境では制限すること
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ルーター登録
app.include_router(health_router, tags=["Health"])
app.include_router(api_router, prefix="/api", tags=["API"])


@app.get("/")
async def root() -> dict:
    """ルートエンドポイント."""
    settings = get_settings()
    return {
        "app": "{{ app_name }}",
        "title": "{{ app_title }}",
        "version": "1.0.0",
        "env": settings.app_env,
    }

