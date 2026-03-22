"""Developer Studio Main Server.

harness 統合:
    - ContractAuthGuard: HTTP ミドルウェア認証（自動トリガー）
    - AISafetyGuard: 入力プロンプトインジェクション防御（手動）
    - ContextEngineer: コンテキスト最適化・トークン予算管理（手動）
"""
from __future__ import annotations

import logging
from contextlib import asynccontextmanager
from pathlib import Path
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

import uvicorn
from fastapi import FastAPI, Request

from harness.context.context_engineer import BuiltContext, ContextEngineer
from harness.gating.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig
from harness.guardrails.ai_safety_guard import AISafetyGuard


_logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# ContractAuthGuard 設定（自動トリガー: HTTPミドルウェアとして全リクエストに適用）
# ---------------------------------------------------------------------------
_APP_ROOT = Path(__file__).resolve().parent
_APP_CONFIG_PATH = _APP_ROOT / "app_config.json"
_PUBLIC_HTTP_PATHS = {"/health", "/", "/docs", "/redoc", "/openapi.json"}

_auth_guard = ContractAuthGuard(
    ContractAuthGuardConfig(
        app_config_path=_APP_CONFIG_PATH,
        public_http_paths=_PUBLIC_HTTP_PATHS,
        auth_header_name="x-api-key",
        ws_query_key="api_key",
        api_key_env_selector_var="DEV_STUDIO_API_KEY_ENV",
        default_api_key_env_var="DEV_STUDIO_API_KEY",
    ),
)

# ---------------------------------------------------------------------------
# AISafetyGuard（手動トリガー: コード生成・補完エンドポイントで使用）
# ---------------------------------------------------------------------------
_safety_guard = AISafetyGuard()

# ---------------------------------------------------------------------------
# ContextEngineer（手動トリガー: コード生成セッションで呼び出す）
# ---------------------------------------------------------------------------
_context_engineer: ContextEngineer | None = None


@asynccontextmanager
async def _lifespan(app_instance: FastAPI) -> AsyncIterator[None]:
    """アプリケーションライフサイクル管理.

    起動時: ContextEngineer を初期化・開始
    終了時: ContextEngineer を停止
    """
    global _context_engineer
    _context_engineer = ContextEngineer(llm_client=None)
    await _context_engineer.start()
    _logger.info("dev_studio: ContextEngineer 開始")
    app_instance.state.context_engineer = _context_engineer
    app_instance.state.safety_guard = _safety_guard
    app_instance.state.auth_guard = _auth_guard

    yield

    if _context_engineer is not None:
        await _context_engineer.stop()
        _logger.info("dev_studio: ContextEngineer 停止")


# ---------------------------------------------------------------------------
# FastAPI アプリケーション
# ---------------------------------------------------------------------------
app = FastAPI(
    title="Developer Studio API",
    version="1.0.0",
    lifespan=_lifespan,
)


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """ContractAuthGuard による HTTP 認証（自動トリガー）."""
    return await _auth_guard.http_middleware(request, call_next)


@app.get("/health")
async def health_check() -> dict[str, Any]:
    """ヘルスチェックエンドポイント."""
    return {
        "status": "healthy",
        "service": "dev_studio",
        "version": "1.0.0",
        "capabilities": ["code_intelligence", "codegen", "wizard"],
        "harness": {
            "auth_guard": "enabled",
            "safety_guard": "enabled",
            "context_engineer": "enabled" if _context_engineer is not None else "not_started",
        },
    }


@app.get("/")
async def root() -> dict[str, str]:
    """ルートエンドポイント."""
    return {"message": "Developer Studio API is running."}


async def get_optimized_context(
    query: str,
    base_prompt: str = "",
) -> BuiltContext | None:
    """ContextEngineer でコンテキストを最適化するヘルパー.

    コード生成・補完エンドポイントから呼び出す（手動トリガー）。

    Args:
        query: ユーザーのコード生成クエリ
        base_prompt: ベースシステムプロンプト

    Returns:
        最適化されたコンテキスト（ContextEngineer 未起動時は None）
    """
    if _context_engineer is None:
        return None
    return await _context_engineer.build_context(
        query=query,
        base_prompt=base_prompt,
    )


# 将来的にルーターを追加するためのプレースホルダー
# app.include_router(code_intelligence.router, prefix="/code")
# app.include_router(codegen.router, prefix="/codegen")
# app.include_router(wizard.router, prefix="/wizard")

if __name__ == "__main__":
    uvicorn.run("apps.dev_studio.main:app", host="0.0.0.0", port=8011, reload=True)
