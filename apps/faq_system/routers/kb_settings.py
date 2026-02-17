"""KB設定ルーター.

/api/kb/settings (GET / PATCH)
"""

from __future__ import annotations

from typing import Any

from apps.faq_system.backend.auth.dependencies import require_auth, require_role
from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.backend.config import KnowledgeBaseUpdateRequest, kb_registry
from apps.faq_system.routers.dependencies import (
    invalidate_service_cache,
    resolve_default_collection,
)
from fastapi import APIRouter, Depends


router = APIRouter(tags=["KB設定"])


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
