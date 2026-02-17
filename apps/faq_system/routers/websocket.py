"""WebSocket ルーター.

/ws/{client_id} - リアルタイム双方向通信
"""

from __future__ import annotations

import logging
from typing import Any

from apps.faq_system.backend.auth.dependencies import resolve_user
from apps.faq_system.routers.dependencies import (
    extract_assistant_content,
    get_chat_history_service,
    get_faq_agent,
    register_artifacts,
    resolve_session_id,
)
from fastapi import APIRouter, WebSocket, WebSocketDisconnect


logger = logging.getLogger(__name__)

router = APIRouter(tags=["WebSocket"])


# ---------------------------------------------------------------------------
# 接続管理
# ---------------------------------------------------------------------------


class ConnectionManager:
    """WebSocket接続マネージャ."""

    def __init__(self) -> None:
        """初期化."""
        self.active_connections: dict[str, WebSocket] = {}

    async def connect(self, websocket: WebSocket, client_id: str) -> None:
        """接続."""
        await websocket.accept()
        self.active_connections[client_id] = websocket
        logger.info("WebSocket connected: %s", client_id)

    def disconnect(self, client_id: str) -> None:
        """切断."""
        if client_id in self.active_connections:
            del self.active_connections[client_id]
            logger.info("WebSocket disconnected: %s", client_id)

    async def send_message(self, client_id: str, message: dict[str, Any]) -> None:
        """メッセージ送信."""
        if client_id in self.active_connections:
            await self.active_connections[client_id].send_json(message)

    async def broadcast(self, message: dict[str, Any]) -> None:
        """ブロードキャスト."""
        for ws in self.active_connections.values():
            await ws.send_json(message)


_ws_manager = ConnectionManager()


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.websocket("/ws/{client_id}")
async def websocket_endpoint(websocket: WebSocket, client_id: str) -> None:
    """WebSocket エンドポイント - リアルタイム双方向通信."""
    access_token = websocket.query_params.get("access_token")
    authorization_header = websocket.headers.get("authorization")
    authorization = authorization_header or (
        f"Bearer {access_token}" if access_token else None
    )
    session_token = websocket.cookies.get("session_token")

    proxy_user = websocket.headers.get("x-forwarded-user") or websocket.headers.get("x-auth-user")
    proxy_name = (
        websocket.headers.get("x-forwarded-preferred-username")
        or websocket.headers.get("x-auth-name")
    )
    proxy_role = websocket.headers.get("x-forwarded-groups") or websocket.headers.get("x-auth-role")
    proxy_department = (
        websocket.headers.get("x-forwarded-department")
        or websocket.headers.get("x-auth-department")
    )
    proxy_position = (
        websocket.headers.get("x-forwarded-title")
        or websocket.headers.get("x-auth-position")
    )

    user = await resolve_user(
        authorization=authorization,
        session_token=session_token,
        x_auth_user=proxy_user,
        x_auth_name=proxy_name,
        x_auth_role=proxy_role,
        x_auth_department=proxy_department,
        x_auth_position=proxy_position,
        x_auth_timestamp=websocket.headers.get("x-auth-timestamp"),
        x_auth_nonce=websocket.headers.get("x-auth-nonce"),
        x_auth_signature=websocket.headers.get("x-auth-signature"),
        request_method="GET",
        request_path=websocket.url.path,
    )
    if not user:
        await websocket.close(code=4401, reason="Unauthorized")
        return

    await _ws_manager.connect(websocket, client_id)
    history_svc = get_chat_history_service()

    try:
        while True:
            data = await websocket.receive_json()

            if data.get("type") == "chat":
                message = data.get("message", "")
                session_id = resolve_session_id(data.get("sessionId", client_id))
                await history_svc.save_message(
                    session_id=session_id,
                    role="user",
                    content=message,
                    transport="ws",
                    user=user,
                    metadata={"options": data.get("options", {})},
                )

                agent = get_faq_agent()

                async for event in agent.run_stream(
                    {
                        "question": message,
                        "context": {
                            "user": {
                                "user_id": user.user_id,
                                "username": user.username,
                                "role": user.role,
                                "department": user.department,
                            },
                            "session_id": session_id,
                            "options": data.get("options", {}),
                        },
                    }
                ):
                    if event.get("type") == "result" and isinstance(
                        event.get("data"),
                        dict,
                    ):
                        event["data"] = register_artifacts(event["data"])
                        event["data"]["session_id"] = session_id
                        await history_svc.save_message(
                            session_id=session_id,
                            role="assistant",
                            content=extract_assistant_content(event["data"]),
                            transport="ws",
                            user=user,
                            metadata={"query_type": event["data"].get("query_type")},
                        )
                    await _ws_manager.send_message(client_id, event)

    except WebSocketDisconnect:
        _ws_manager.disconnect(client_id)
    except Exception as e:
        logger.exception("WebSocket error: %s", e)
        await history_svc.save_message(
            session_id=client_id,
            role="system",
            content=str(e),
            transport="ws",
            user=user if "user" in locals() else None,
            metadata={"error": True},
        )
        await _ws_manager.send_message(
            client_id,
            {
                "type": "error",
                "message": str(e),
            },
        )
