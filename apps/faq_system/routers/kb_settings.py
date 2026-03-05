"""KB設定ルーター.

/api/kb/settings (GET / PATCH)
/api/rag/settings (GET / PATCH)  統一 RAG 設定 API（認証不要・パターン自動展開）
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import httpx
from apps.faq_system.backend.auth.dependencies import require_auth, require_role
from apps.faq_system.backend.config import KnowledgeBaseUpdateRequest, kb_registry
from apps.faq_system.backend.services.rag_runtime_config import load_rag_runtime_config
from apps.faq_system.routers.dependencies import (
    invalidate_service_cache,
    resolve_default_collection,
)
from fastapi import APIRouter, Depends, HTTPException
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo
else:
    UserInfo = Any


router = APIRouter(tags=["KB設定"])

# ---------------------------------------------------------------------------
# RAG パターン定義（業界最良プラクティス）
# ---------------------------------------------------------------------------

_PATTERNS: dict[str, dict[str, Any]] = {
    "faq_precision": {
        "chunk_strategy": "sentence",
        "chunk_size": 500,
        "chunk_overlap": 80,
        "retrieval_method": "hybrid",
        "reranker": "cross_encoder",
        "top_k": 8,
        "score_threshold": 0.25,
    },
    "balanced_knowledge": {
        "chunk_strategy": "recursive",
        "chunk_size": 800,
        "chunk_overlap": 120,
        "retrieval_method": "hybrid",
        "reranker": "bm25",
        "top_k": 6,
        "score_threshold": 0.20,
    },
    "long_doc_reasoning": {
        "chunk_strategy": "markdown",
        "chunk_size": 1200,
        "chunk_overlap": 180,
        "retrieval_method": "multi_query",
        "reranker": "cross_encoder",
        "top_k": 10,
        "score_threshold": 0.30,
    },
}

KNOWN_PATTERNS = set(_PATTERNS.keys()) | {"custom"}


class RAGSettingsPatch(BaseModel):
    """RAG 設定更新リクエスト."""

    pattern: str | None = Field(None, description="パターン名（自動パラメータセット）")
    enabled: bool | None = None
    vector_provider: str | None = None
    vector_collection: str | None = None
    chunk_strategy: str | None = None
    chunk_size: int | None = Field(None, ge=100, le=4000)
    chunk_overlap: int | None = Field(None, ge=0, le=500)
    retrieval_method: str | None = None
    reranker: str | None = None
    top_k: int | None = Field(None, ge=1, le=20)
    score_threshold: float | None = Field(None, ge=0.0, le=1.0)
    auto_create_collection: bool = Field(False, description="コレクション自動作成フラグ")


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.get("/api/kb/settings")
async def get_kb_settings(
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """知識ベース設定を取得 (認証必須)."""
    await kb_registry.ensure_initialized()
    settings = kb_registry.to_dict()
    settings["resolved_default_collection"] = resolve_default_collection()
    return settings


@router.get("/api/rag/settings")
async def get_rag_settings() -> dict[str, Any]:
    """現在の RAG 設定を返す（認証不要）."""
    try:
        config = load_rag_runtime_config()
    except Exception:
        config = None

    return {
        "enabled": config.rag_enabled if config else False,
        "pattern": None,
        "chunk_strategy": config.rag_chunk_strategy if config else "recursive",
        "reranker": config.rag_reranker if config else "bm25",
        "top_k": config.rag_top_k if config else 5,
        "vector_provider": "qdrant",
        "available_patterns": list(_PATTERNS.keys()),
    }


@router.patch("/api/rag/settings")
async def patch_rag_settings(patch: RAGSettingsPatch) -> JSONResponse:
    """RAG 設定を更新する。パターン選択時は関連パラメータを自動セット（認証不要）."""
    patch_dict = patch.model_dump(exclude_none=True)
    patch_dict.pop("auto_create_collection", None)

    # パターン検証
    pattern_name: str | None = patch_dict.get("pattern")
    if pattern_name and pattern_name not in KNOWN_PATTERNS:
        raise HTTPException(
            status_code=422,
            detail=f"Unknown pattern: {pattern_name!r}. Valid: {sorted(KNOWN_PATTERNS)}",
        )

    # パターンから自動パラメータ展開（手動指定値が上書き）
    if pattern_name and pattern_name in _PATTERNS:
        auto_params = dict(_PATTERNS[pattern_name])
        merged = {
            **auto_params,
            **{k: v for k, v in patch_dict.items() if k != "pattern"},
        }
        patch_dict.update(merged)

    # Platform API へ転送（失敗時は警告のみ）
    warnings: list[str] = []
    platform_url = "http://localhost:8001/api/studios/framework/rag/apps/faq_system/config"
    try:
        async with httpx.AsyncClient(timeout=5.0) as client:
            resp = await client.patch(platform_url, json=patch_dict)
            if resp.status_code >= 400:
                warnings.append(f"Platform 更新失敗 ({resp.status_code}) - ローカル設定のみ更新")
    except Exception as exc:
        warnings.append(f"Platform 接続失敗: {exc} - ローカル設定のみ更新")

    result: dict[str, Any] = {**patch_dict}
    if warnings:
        result["warnings"] = warnings

    return JSONResponse(content=result, status_code=207 if warnings else 200)


@router.patch("/api/kb/settings")
async def update_kb_settings(
    request: KnowledgeBaseUpdateRequest,
    _user: UserInfo = Depends(require_role("admin", "manager")),
) -> dict[str, Any]:
    """知識ベース設定を更新 (admin/manager 限定)."""
    settings = await kb_registry.update(request)

    # 既存キャッシュ破棄（新設定を即時反映）
    invalidate_service_cache("rag:", "faq_agent")

    return {
        "success": True,
        "message": "Knowledge base settings updated",
        "settings": settings.model_dump(),
    }
