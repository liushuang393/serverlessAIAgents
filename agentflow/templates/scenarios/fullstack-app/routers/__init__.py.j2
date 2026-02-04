# -*- coding: utf-8 -*-
"""{{ app_name }} API ルーター.

エンドポイント定義を提供する。
"""
from fastapi import APIRouter

# ヘルスチェックルーター
health_router = APIRouter()


@health_router.get("/health")
async def health_check() -> dict:
    """ヘルスチェック.
    
    Kubernetes liveness/readiness probe 用。
    """
    return {
        "status": "healthy",
        "service": "{{ app_name }}",
    }


# メイン API ルーター
api_router = APIRouter()


@api_router.get("/info")
async def api_info() -> dict:
    """API 情報."""
    return {
        "app": "{{ app_name }}",
        "version": "1.0.0",
        "endpoints": [
            "GET /api/info",
            # TODO: エンドポイント一覧を追加
        ],
    }


# TODO: 機能別ルーターを追加
# from .users import router as users_router
# api_router.include_router(users_router, prefix="/users", tags=["Users"])


__all__ = [
    "health_router",
    "api_router",
]

