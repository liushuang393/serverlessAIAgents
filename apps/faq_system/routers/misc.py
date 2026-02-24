"""その他ルーター.

/api/feedback, /api/health, /api/a2a/card, /api/nodes/service,
/api/assets/{artifact_id}/download, /favicon.svg, /favicon.ico, /
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.db import ensure_database_ready
from apps.faq_system.backend.db.session import get_database_url, get_db_session
from apps.faq_system.routers.dependencies import (
    get_artifact_path,
    get_faq_agent,
    get_feedback_service,
)
from fastapi import APIRouter, Depends, HTTPException
from fastapi.responses import FileResponse
from pydantic import BaseModel, Field
from sqlalchemy import text


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo
else:
    UserInfo = Any


logger = logging.getLogger(__name__)

router = APIRouter()

_APP_ROOT = Path(__file__).resolve().parent.parent
_FAVICON_PATH = _APP_ROOT / "public" / "favicon.svg"
_ENHANCED_HTML_PATH = _APP_ROOT / "public" / "index.html"


# ---------------------------------------------------------------------------
# リクエストモデル
# ---------------------------------------------------------------------------


class FeedbackRequest(BaseModel):
    """フィードバックリクエスト."""

    message_id: str = Field(..., description="メッセージID")
    helpful: bool = Field(..., description="役に立ったか")
    comment: str | None = Field(None, description="コメント")


# ---------------------------------------------------------------------------
# フィードバック
# ---------------------------------------------------------------------------


@router.post("/api/feedback")
async def submit_feedback(
    request: FeedbackRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, str]:
    """フィードバック送信 (認証必須)."""
    logger.info(
        "Feedback: user=%s, message_id=%s, helpful=%s, comment=%s",
        user.username,
        request.message_id,
        request.helpful,
        request.comment,
    )
    service = get_feedback_service()
    await service.submit_feedback(
        query_id=request.message_id,
        helpful=request.helpful,
        user_id=user.user_id,
        comment=request.comment or "",
    )
    return {"status": "ok", "message": "フィードバックを受け付けました"}


# ---------------------------------------------------------------------------
# アセットダウンロード
# ---------------------------------------------------------------------------


@router.get("/api/assets/{artifact_id}/download")
async def download_artifact(
    artifact_id: str,
    _user: UserInfo = Depends(require_auth),
) -> FileResponse:
    """生成アセットをダウンロード (認証必須)."""
    path_obj = get_artifact_path(artifact_id)
    if path_obj is None:
        raise HTTPException(status_code=404, detail="artifact not found")
    if not path_obj.exists() or not path_obj.is_file():
        raise HTTPException(status_code=404, detail="artifact file missing")
    return FileResponse(
        path=str(path_obj),
        filename=path_obj.name,
        media_type="application/octet-stream",
    )


# ---------------------------------------------------------------------------
# A2A / サービスノード
# ---------------------------------------------------------------------------


@router.get("/api/a2a/card")
async def get_a2a_card() -> dict[str, Any]:
    """A2A AgentCard 相当の情報を取得."""
    agent = get_faq_agent()
    card = agent.get_a2a_card()
    if card is not None and hasattr(card, "to_a2a_format"):
        return card.to_a2a_format()

    return {
        "name": "faq-system-maq-router",
        "description": "社内FAQ/SQL分析/営業資料画像生成を振り分けるマルチ機能Agent",
        "version": "1.1.0",
        "skills": [
            {"name": "knowledge_search", "description": "社内知識検索と回答生成"},
            {"name": "sql_analytics", "description": "自然言語からSQL生成し、表とチャートを返却"},
            {"name": "design_skills", "description": "営業資料向け画像セットを生成"},
        ],
    }


@router.get("/api/nodes/service")
async def list_service_nodes() -> dict[str, Any]:
    """利用可能なサービスノード一覧を取得.

    Studio UI からノーコードで使用する際のサービス定義を返す。
    """
    return {
        "nodes": [
            {
                "type": "rag",
                "label": "RAGノード",
                "description": "ナレッジベース検索と回答生成",
                "config_fields": ["collection", "chunk_strategy", "top_k"],
            },
            {
                "type": "text2sql",
                "label": "Text2SQLノード",
                "description": "自然言語からSQLを生成して実行",
                "config_fields": ["dialect", "schema", "max_rows"],
            },
            {
                "type": "chart",
                "label": "チャートノード",
                "description": "データの可視化",
                "config_fields": ["chart_type", "enable_drill_down"],
            },
            {
                "type": "suggestion",
                "label": "提案ノード",
                "description": "フォローアップ質問を提案",
                "config_fields": ["max_suggestions", "types"],
            },
        ],
    }


# ---------------------------------------------------------------------------
# ヘルスチェック
# ---------------------------------------------------------------------------


@router.get("/api/health")
async def health_check() -> dict[str, Any]:
    """ヘルスチェック.

    正常時は ``{"status": "healthy"}``、DB 異常時は ``{"status": "degraded"}`` を返す。
    platform のヘルスチェック判定基準と統一した応答フォーマット。
    """
    status = "healthy"
    db_status = "healthy"
    db_error: str | None = None
    resolved_db_url = get_database_url()
    masked_db_url = (
        f"***@{resolved_db_url.split('@', 1)[1]}" if "@" in resolved_db_url else resolved_db_url
    )

    try:
        await ensure_database_ready()
        async with get_db_session() as session:
            await session.execute(text("SELECT 1"))
    except Exception as exc:
        status = "degraded"
        db_status = "error"
        db_error = str(exc)
        logger.exception("Health check DB error: %s", exc)

    payload: dict[str, Any] = {
        "status": status,
        "service": "faq-system",
        "version": "2.0.0",
        "timestamp": datetime.now(tz=UTC).isoformat(),
        "db": {
            "status": db_status,
            "url": masked_db_url,
        },
    }
    if db_error:
        payload["db"]["error"] = db_error
    return payload
