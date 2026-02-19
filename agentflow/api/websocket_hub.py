"""WebSocket Hub - 統一 WebSocket 接続管理.

設計原則:
- 中央管理: 全接続を一元管理
- ルーム対応: グループブロードキャスト
- ミドルウェア: 認証・ログ等の横断的処理
- 型安全: Pydantic によるメッセージ定義

使用例:
    >>> from agentflow.api import WebSocketHub, WSMessage
    >>>
    >>> hub = WebSocketHub()
    >>>
    >>> # 接続管理
    >>> await hub.connect(websocket, client_id="user-123")
    >>> await hub.join_room(client_id="user-123", room="project-1")
    >>>
    >>> # メッセージ送信
    >>> await hub.send(client_id="user-123", message={...})
    >>> await hub.broadcast_room(room="project-1", message={...})
    >>>
    >>> # ハンドラ登録
    >>> @hub.on("chat")
    >>> async def handle_chat(client_id: str, data: dict):
    ...     await hub.broadcast_room(data["room"], {"text": data["text"]})
"""

from __future__ import annotations

import asyncio
import json
import logging
from collections.abc import Awaitable, Callable
from datetime import datetime
from enum import Enum
from typing import Any, Protocol

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class WSMessageType(str, Enum):
    """WebSocket メッセージ種別."""

    # クライアント → サーバー
    CONNECT = "connect"
    DISCONNECT = "disconnect"
    SUBSCRIBE = "subscribe"
    UNSUBSCRIBE = "unsubscribe"
    MESSAGE = "message"
    COMMAND = "command"

    # サーバー → クライアント
    CONNECTED = "connected"
    SUBSCRIBED = "subscribed"
    UNSUBSCRIBED = "unsubscribed"
    DATA = "data"
    PROGRESS = "progress"
    ERROR = "error"
    PING = "ping"
    PONG = "pong"


class WSMessage(BaseModel):
    """WebSocket メッセージ."""

    type: WSMessageType = Field(..., description="メッセージ種別")
    data: dict[str, Any] = Field(default_factory=dict, description="メッセージデータ")
    room: str | None = Field(None, description="対象ルーム")
    client_id: str | None = Field(None, description="クライアントID")
    timestamp: str = Field(
        default_factory=lambda: datetime.now().isoformat(),
        description="タイムスタンプ",
    )

    def to_json(self) -> str:
        """JSON文字列に変換."""
        return json.dumps(self.model_dump(), ensure_ascii=False, default=str)

    @classmethod
    def from_json(cls, data: str | bytes) -> WSMessage:
        """JSONから作成."""
        if isinstance(data, bytes):
            data = data.decode("utf-8")
        return cls(**json.loads(data))


class WebSocketProtocol(Protocol):
    """WebSocket プロトコル（FastAPI/Starlette 互換）."""

    async def accept(self) -> None: ...
    async def close(self, code: int = 1000) -> None: ...
    async def send_text(self, data: str) -> None: ...
    async def send_json(self, data: Any) -> None: ...
    async def receive_text(self) -> str: ...
    async def receive_json(self) -> Any: ...


class WSClient(BaseModel):
    """WebSocket クライアント情報."""

    model_config = {"arbitrary_types_allowed": True}

    client_id: str = Field(..., description="クライアントID")
    rooms: set[str] = Field(default_factory=set, description="参加ルーム")
    connected_at: datetime = Field(
        default_factory=datetime.now,
        description="接続時刻",
    )
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


# 型定義
WSHandler = Callable[[str, dict[str, Any]], Awaitable[None]]
WSMiddleware = Callable[[str, WSMessage, Callable], Awaitable[None]]


class WebSocketHub:
    """WebSocket Hub - 統一接続管理.

    全アプリケーションで共有可能な WebSocket 接続マネージャ。

    特徴:
    - 接続の一元管理
    - ルームベースのグループ通信
    - ミドルウェアサポート
    - イベントハンドラ登録
    """

    def __init__(self) -> None:
        """初期化."""
        self._connections: dict[str, WebSocketProtocol] = {}
        self._clients: dict[str, WSClient] = {}
        self._rooms: dict[str, set[str]] = {}  # room -> client_ids
        self._handlers: dict[str, list[WSHandler]] = {}
        self._middlewares: list[WSMiddleware] = []
        self._logger = logging.getLogger("websocket_hub")

    # =========================================================================
    # 接続管理
    # =========================================================================

    async def connect(
        self,
        websocket: WebSocketProtocol,
        client_id: str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """クライアント接続.

        Args:
            websocket: WebSocket インスタンス
            client_id: クライアントID
            metadata: メタデータ
        """
        await websocket.accept()
        self._connections[client_id] = websocket
        self._clients[client_id] = WSClient(
            client_id=client_id,
            metadata=metadata or {},
        )
        self._logger.info("Client connected: %s", client_id)

        # 接続確認メッセージ送信
        await self.send(
            client_id,
            WSMessage(
                type=WSMessageType.CONNECTED,
                data={"client_id": client_id},
            ),
        )

    async def disconnect(self, client_id: str) -> None:
        """クライアント切断.

        Args:
            client_id: クライアントID
        """
        if client_id in self._clients:
            # 全ルームから退出
            client = self._clients[client_id]
            for room in list(client.rooms):
                await self.leave_room(client_id, room)

            del self._clients[client_id]

        if client_id in self._connections:
            del self._connections[client_id]

        self._logger.info("Client disconnected: %s", client_id)

    def is_connected(self, client_id: str) -> bool:
        """接続中かどうか."""
        return client_id in self._connections

    def get_client(self, client_id: str) -> WSClient | None:
        """クライアント情報取得."""
        return self._clients.get(client_id)

    @property
    def client_count(self) -> int:
        """接続数."""
        return len(self._connections)

    # =========================================================================
    # ルーム管理
    # =========================================================================

    async def join_room(self, client_id: str, room: str) -> None:
        """ルーム参加.

        Args:
            client_id: クライアントID
            room: ルーム名
        """
        if room not in self._rooms:
            self._rooms[room] = set()

        self._rooms[room].add(client_id)

        if client_id in self._clients:
            self._clients[client_id].rooms.add(room)

        self._logger.debug("Client %s joined room: %s", client_id, room)

        # 参加通知
        await self.send(
            client_id,
            WSMessage(
                type=WSMessageType.SUBSCRIBED,
                data={"room": room},
                room=room,
            ),
        )

    async def leave_room(self, client_id: str, room: str) -> None:
        """ルーム退出.

        Args:
            client_id: クライアントID
            room: ルーム名
        """
        if room in self._rooms:
            self._rooms[room].discard(client_id)
            if not self._rooms[room]:
                del self._rooms[room]

        if client_id in self._clients:
            self._clients[client_id].rooms.discard(room)

        self._logger.debug("Client %s left room: %s", client_id, room)

    def get_room_clients(self, room: str) -> set[str]:
        """ルームのクライアントID一覧."""
        return self._rooms.get(room, set())

    # =========================================================================
    # メッセージ送信
    # =========================================================================

    async def send(
        self,
        client_id: str,
        message: WSMessage | dict[str, Any],
    ) -> bool:
        """単一クライアントにメッセージ送信.

        Args:
            client_id: クライアントID
            message: メッセージ

        Returns:
            送信成功かどうか
        """
        if client_id not in self._connections:
            return False

        ws = self._connections[client_id]
        try:
            if isinstance(message, WSMessage):
                await ws.send_text(message.to_json())
            else:
                await ws.send_json(message)
            return True
        except Exception as e:
            self._logger.warning("Failed to send to %s: %s", client_id, e)
            await self.disconnect(client_id)
            return False

    async def broadcast(
        self,
        message: WSMessage | dict[str, Any],
        exclude: set[str] | None = None,
    ) -> int:
        """全クライアントにブロードキャスト.

        Args:
            message: メッセージ
            exclude: 除外するクライアントID

        Returns:
            送信成功数
        """
        exclude = exclude or set()
        tasks = [
            self.send(client_id, message)
            for client_id in self._connections
            if client_id not in exclude
        ]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        return sum(1 for r in results if r is True)

    async def broadcast_room(
        self,
        room: str,
        message: WSMessage | dict[str, Any],
        exclude: set[str] | None = None,
    ) -> int:
        """ルームにブロードキャスト.

        Args:
            room: ルーム名
            message: メッセージ
            exclude: 除外するクライアントID

        Returns:
            送信成功数
        """
        exclude = exclude or set()
        client_ids = self._rooms.get(room, set())
        tasks = [
            self.send(client_id, message) for client_id in client_ids if client_id not in exclude
        ]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        return sum(1 for r in results if r is True)

    # =========================================================================
    # イベントハンドラ
    # =========================================================================

    def on(self, event_type: str) -> Callable[[WSHandler], WSHandler]:
        """イベントハンドラデコレータ.

        Args:
            event_type: イベント種別

        Returns:
            デコレータ
        """

        def decorator(handler: WSHandler) -> WSHandler:
            if event_type not in self._handlers:
                self._handlers[event_type] = []
            self._handlers[event_type].append(handler)
            return handler

        return decorator

    def add_handler(self, event_type: str, handler: WSHandler) -> None:
        """イベントハンドラ追加."""
        if event_type not in self._handlers:
            self._handlers[event_type] = []
        self._handlers[event_type].append(handler)

    async def emit(
        self,
        event_type: str,
        client_id: str,
        data: dict[str, Any],
    ) -> None:
        """イベント発火.

        Args:
            event_type: イベント種別
            client_id: クライアントID
            data: イベントデータ
        """
        handlers = self._handlers.get(event_type, [])
        for handler in handlers:
            try:
                await handler(client_id, data)
            except Exception as e:
                self._logger.exception("Handler error for %s: %s", event_type, e)

    # =========================================================================
    # ミドルウェア
    # =========================================================================

    def use(self, middleware: WSMiddleware) -> None:
        """ミドルウェア追加.

        Args:
            middleware: ミドルウェア関数
        """
        self._middlewares.append(middleware)

    # =========================================================================
    # メッセージ受信ループ
    # =========================================================================

    async def handle_connection(
        self,
        websocket: WebSocketProtocol,
        client_id: str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """接続ハンドリング（メインループ）.

        Args:
            websocket: WebSocket インスタンス
            client_id: クライアントID
            metadata: メタデータ
        """
        await self.connect(websocket, client_id, metadata)

        try:
            while True:
                try:
                    raw_data = await websocket.receive_text()
                    message = WSMessage.from_json(raw_data)

                    # イベント発火
                    await self.emit(message.type.value, client_id, message.data)

                except json.JSONDecodeError:
                    await self.send(
                        client_id,
                        WSMessage(
                            type=WSMessageType.ERROR,
                            data={"message": "Invalid JSON"},
                        ),
                    )

        except Exception as e:
            self._logger.debug("Connection closed: %s - %s", client_id, e)
        finally:
            await self.disconnect(client_id)


__all__ = [
    "WSClient",
    "WSHandler",
    "WSMessage",
    "WSMessageType",
    "WSMiddleware",
    "WebSocketHub",
]
