"""チャット関連ルーター.

/api/chat, /api/chat/stream, /api/chat/history, /api/maq/chat
"""

from __future__ import annotations

import json
import logging
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.routers.dependencies import (
    extract_assistant_content,
    get_chat_history_service,
    get_faq_agent,
    register_artifacts,
    resolve_session_id,
)
from fastapi import APIRouter, Depends
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field

if TYPE_CHECKING:
    from collections.abc import AsyncIterator

logger = logging.getLogger(__name__)

router = APIRouter(tags=["チャット"])


# ---------------------------------------------------------------------------
# リクエストモデル
# ---------------------------------------------------------------------------


class ChatRequest(BaseModel):
    """チャットリクエスト."""

    message: str = Field(..., description="ユーザーメッセージ")
    session_id: str | None = Field(None, description="セッションID")
    options: dict[str, Any] = Field(default_factory=dict, description="オプション")


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.post("/api/chat")
async def chat(
    request: ChatRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """チャット API.

    FAQAgent を呼び出し、質問に回答します。
    FAQAgent が内部でクエリタイプを判定し、適切なサービスを使用します。
    認証必須。
    """
    session_id = resolve_session_id(request.session_id)
    history_svc = get_chat_history_service()
    await history_svc.save_message(
        session_id=session_id,
        role="user",
        content=request.message,
        transport="api",
        user=user,
        metadata={"options": request.options},
    )

    agent = get_faq_agent()
    result = await agent.run(
        {
            "question": request.message,
            "context": {
                "user": {
                    "user_id": user.user_id,
                    "username": user.username,
                    "role": user.role,
                    "department": user.department,
                },
                "session_id": session_id,
                "options": request.options,
            },
        }
    )

    result = register_artifacts(result)
    result["session_id"] = session_id

    await history_svc.save_message(
        session_id=session_id,
        role="assistant",
        content=extract_assistant_content(result),
        transport="api",
        user=user,
        metadata={"query_type": result.get("query_type")},
    )

    return result


@router.post("/api/maq/chat")
async def maq_chat(
    request: ChatRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """MAQ統合チャット API.

    社内FAQ・SQL分析・営業資料画像生成を単一入口で処理する。
    認証必須。
    """
    return await chat(request, user)


@router.post("/api/chat/stream")
async def chat_stream(
    request: ChatRequest,
    user: UserInfo = Depends(require_auth),
) -> StreamingResponse:
    """チャット API (ストリーム版).

    SSE でリアルタイムに進捗を返します。認証必須。
    """
    session_id = resolve_session_id(request.session_id)
    history_svc = get_chat_history_service()
    await history_svc.save_message(
        session_id=session_id,
        role="user",
        content=request.message,
        transport="sse",
        user=user,
        metadata={"options": request.options},
    )
    agent = get_faq_agent()

    async def event_generator() -> AsyncIterator[str]:
        try:
            async for event in agent.run_stream(
                {
                    "question": request.message,
                    "context": {
                        "user": {
                            "user_id": user.user_id,
                            "username": user.username,
                            "role": user.role,
                            "department": user.department,
                        },
                        "session_id": session_id,
                        "options": request.options,
                    },
                }
            ):
                if event.get("type") == "result" and isinstance(event.get("data"), dict):
                    event["data"] = register_artifacts(event["data"])
                    event["data"]["session_id"] = session_id
                    await history_svc.save_message(
                        session_id=session_id,
                        role="assistant",
                        content=extract_assistant_content(event["data"]),
                        transport="sse",
                        user=user,
                        metadata={"query_type": event["data"].get("query_type")},
                    )
                yield f"data: {json.dumps(event)}\n\n"
        except Exception as exc:
            logger.exception("SSE chat stream failed: %s", exc)
            await history_svc.save_message(
                session_id=session_id,
                role="system",
                content=str(exc),
                transport="sse",
                user=user,
                metadata={"error": True},
            )
            yield f"data: {json.dumps({'type': 'error', 'message': str(exc)})}\n\n"

    return StreamingResponse(event_generator(), media_type="text/event-stream")


@router.get("/api/chat/history")
async def get_chat_history(
    session_id: str,
    limit: int = 100,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """チャット履歴を取得 (認証必須)."""
    history_svc = get_chat_history_service()
    messages = await history_svc.list_messages(
        session_id=session_id,
        limit=limit,
        user=user,
    )
    return {
        "session_id": session_id,
        "count": len(messages),
        "messages": messages,
    }


@router.get("/api/chat/sessions")
async def list_sessions(
    limit: int = 50,
    offset: int = 0,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """チャットセッション一覧を取得 (認証必須)."""
    history_svc = get_chat_history_service()
    sessions = await history_svc.list_sessions(user, limit=limit, offset=offset)
    return {
        "count": len(sessions),
        "sessions": sessions,
    }


@router.delete("/api/chat/sessions/{session_id}")
async def delete_session(
    session_id: str,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """チャットセッションを削除 (認証必須)."""
    history_svc = get_chat_history_service()
    deleted = await history_svc.delete_session(session_id, user)
    if not deleted:
        return {"success": False, "message": "セッションが見つかりません"}
    return {"success": True, "message": "セッションを削除しました"}

