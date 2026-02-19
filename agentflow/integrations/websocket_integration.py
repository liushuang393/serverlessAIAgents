"""WebSocket Integration - 双向実時間通信.

このモジュールは、Agent との双方向リアルタイム通信を提供します。

接口設計原則:
- FastAPI WebSocket API 互換
- Socket.IO 風のイベント駆動
- 将来の Socket.IO / Pusher 差し替え可能
- AG-UI プロトコル互換

使用例:
    >>> # FastAPI 統合
    >>> manager = WebSocketManager()
    >>>
    >>> @app.websocket("/ws/{session_id}")
    >>> async def websocket_endpoint(websocket: WebSocket, session_id: str):
    ...     await manager.handle_connection(websocket, session_id)
    >>>
    >>> # Agent からイベント送信
    >>> await manager.send_event(session_id, AgentEvent(...))
    >>>
    >>> # ブロードキャスト
    >>> await manager.broadcast(AgentEvent(...))

参考:
- FastAPI WebSocket
- Socket.IO Protocol
- AG-UI Events
"""

from __future__ import annotations

import asyncio
import contextlib
import json
import logging
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from collections.abc import Callable


# =============================================================================
# イベントモデル（AG-UI + カスタム拡張）
# =============================================================================


class WSEventType(str, Enum):
    """WebSocket イベントタイプ."""

    # 接続管理
    CONNECT = "connect"
    DISCONNECT = "disconnect"
    RECONNECT = "reconnect"
    PING = "ping"
    PONG = "pong"

    # Agent イベント（AG-UI互換）
    FLOW_START = "flow.start"
    FLOW_COMPLETE = "flow.complete"
    FLOW_ERROR = "flow.error"
    NODE_START = "node.start"
    NODE_COMPLETE = "node.complete"
    PROGRESS = "progress"

    # HITL イベント
    APPROVAL_REQUIRED = "approval.required"
    APPROVAL_RESPONSE = "approval.response"

    # カスタムイベント
    MESSAGE = "message"
    COMMAND = "command"
    STATE_UPDATE = "state.update"


class WSEvent(BaseModel):
    """WebSocket イベント（AG-UI互換 + 拡張）.

    AG-UI プロトコルと互換性を持ちながら、
    WebSocket 固有の機能を拡張。
    """

    id: str = Field(default_factory=lambda: f"evt_{uuid.uuid4().hex[:12]}")
    type: WSEventType = Field(...)
    timestamp: float = Field(default_factory=time.time)
    session_id: str | None = Field(default=None)
    flow_id: str | None = Field(default=None)
    data: dict[str, Any] = Field(default_factory=dict)

    def to_json(self) -> str:
        """JSON文字列に変換."""
        return json.dumps(self.model_dump(), ensure_ascii=False, default=str)

    @classmethod
    def from_json(cls, data: str | bytes) -> WSEvent:
        """JSONから作成."""
        if isinstance(data, bytes):
            data = data.decode("utf-8")
        parsed = json.loads(data)
        return cls(**parsed)


class WSCommand(BaseModel):
    """クライアントからのコマンド.

    HITL の承認/拒否や、フロー制御などに使用。
    """

    id: str = Field(default_factory=lambda: f"cmd_{uuid.uuid4().hex[:12]}")
    type: str = Field(..., description="コマンドタイプ")
    session_id: str = Field(...)
    flow_id: str | None = Field(default=None)
    payload: dict[str, Any] = Field(default_factory=dict)
    timestamp: float = Field(default_factory=time.time)


# =============================================================================
# 接続管理
# =============================================================================


@dataclass
class ConnectionState:
    """接続状態."""

    session_id: str
    connected_at: datetime = field(default_factory=datetime.now)
    last_activity: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)
    subscriptions: set[str] = field(default_factory=set)

    def is_stale(self, timeout_seconds: int = 300) -> bool:
        """タイムアウト判定."""
        elapsed = (datetime.now() - self.last_activity).total_seconds()
        return elapsed > timeout_seconds

    def touch(self) -> None:
        """最終アクティビティを更新."""
        self.last_activity = datetime.now()


class ConnectionManager(ABC):
    """接続マネージャー抽象接口.

    将来の実装差し替えを考慮した抽象接口。
    """

    @abstractmethod
    async def connect(self, session_id: str, connection: Any) -> None:
        """接続を登録."""

    @abstractmethod
    async def disconnect(self, session_id: str) -> None:
        """接続を解除."""

    @abstractmethod
    async def send(self, session_id: str, event: WSEvent) -> bool:
        """特定セッションにイベント送信."""

    @abstractmethod
    async def broadcast(self, event: WSEvent, exclude: set[str] | None = None) -> int:
        """全セッションにブロードキャスト."""

    @abstractmethod
    def get_connection_count(self) -> int:
        """接続数を取得."""

    @abstractmethod
    def get_sessions(self) -> list[str]:
        """セッションID一覧を取得."""


# =============================================================================
# WebSocket マネージャー（自研実装、標準接口）
# =============================================================================


class WebSocketManager(ConnectionManager):
    """WebSocket 接続マネージャー（FastAPI互換）.

    FastAPI WebSocket を使用したリアルタイム通信管理。

    使用例:
        >>> manager = WebSocketManager()
        >>>
        >>> # FastAPI エンドポイント
        >>> @app.websocket("/ws/{session_id}")
        >>> async def ws_endpoint(websocket: WebSocket, session_id: str):
        ...     await manager.handle_connection(websocket, session_id)
    """

    def __init__(
        self,
        heartbeat_interval: float = 30.0,
        connection_timeout: float = 300.0,
        max_connections: int = 1000,
        on_connect: Callable[[str], None] | None = None,
        on_disconnect: Callable[[str], None] | None = None,
        on_command: Callable[[WSCommand], None] | None = None,
    ) -> None:
        """初期化.

        Args:
            heartbeat_interval: ハートビート間隔（秒）
            connection_timeout: 接続タイムアウト（秒）
            max_connections: 最大接続数
            on_connect: 接続時コールバック
            on_disconnect: 切断時コールバック
            on_command: コマンド受信コールバック
        """
        self._connections: dict[str, Any] = {}  # session_id -> WebSocket
        self._states: dict[str, ConnectionState] = {}
        self._command_handlers: dict[str, Callable[[WSCommand], Any]] = {}
        self._heartbeat_interval = heartbeat_interval
        self._connection_timeout = connection_timeout
        self._max_connections = max_connections
        self._on_connect = on_connect
        self._on_disconnect = on_disconnect
        self._on_command = on_command
        self._logger = logging.getLogger(__name__)
        self._heartbeat_task: asyncio.Task | None = None
        self._command_queue: asyncio.Queue[WSCommand] = asyncio.Queue()

    async def connect(self, session_id: str, connection: Any) -> None:
        """接続を登録."""
        if len(self._connections) >= self._max_connections:
            self._logger.warning(f"Max connections reached: {self._max_connections}")
            msg = "Max connections reached"
            raise ConnectionError(msg)

        self._connections[session_id] = connection
        self._states[session_id] = ConnectionState(session_id=session_id)

        self._logger.info(f"WebSocket connected: {session_id}")

        if self._on_connect:
            self._on_connect(session_id)

        # 接続イベント送信
        await self.send(
            session_id,
            WSEvent(
                type=WSEventType.CONNECT,
                session_id=session_id,
                data={"message": "Connected successfully"},
            ),
        )

    async def disconnect(self, session_id: str) -> None:
        """接続を解除."""
        if session_id in self._connections:
            del self._connections[session_id]
        if session_id in self._states:
            del self._states[session_id]

        self._logger.info(f"WebSocket disconnected: {session_id}")

        if self._on_disconnect:
            self._on_disconnect(session_id)

    async def send(self, session_id: str, event: WSEvent) -> bool:
        """特定セッションにイベント送信."""
        connection = self._connections.get(session_id)
        if not connection:
            self._logger.warning(f"Connection not found: {session_id}")
            return False

        try:
            # FastAPI WebSocket の場合
            if hasattr(connection, "send_text"):
                await connection.send_text(event.to_json())
            elif hasattr(connection, "send"):
                await connection.send(event.to_json())
            else:
                self._logger.error(f"Unknown connection type: {type(connection)}")
                return False

            # アクティビティ更新
            if session_id in self._states:
                self._states[session_id].touch()

            return True

        except Exception as e:
            self._logger.exception(f"Send error to {session_id}: {e}")
            await self.disconnect(session_id)
            return False

    async def broadcast(self, event: WSEvent, exclude: set[str] | None = None) -> int:
        """全セッションにブロードキャスト."""
        exclude = exclude or set()
        sent_count = 0

        tasks = []
        for session_id in list(self._connections.keys()):
            if session_id not in exclude:
                tasks.append(self.send(session_id, event))

        results = await asyncio.gather(*tasks, return_exceptions=True)
        sent_count = sum(1 for r in results if r is True)

        self._logger.debug(f"Broadcast sent to {sent_count} sessions")
        return sent_count

    def get_connection_count(self) -> int:
        """接続数を取得."""
        return len(self._connections)

    def get_sessions(self) -> list[str]:
        """セッションID一覧を取得."""
        return list(self._connections.keys())

    async def handle_connection(
        self,
        websocket: Any,
        session_id: str,
    ) -> None:
        """WebSocket 接続を処理（FastAPI用）.

        FastAPI WebSocket エンドポイントから呼び出す。

        Args:
            websocket: FastAPI WebSocket オブジェクト
            session_id: セッションID
        """
        # 接続を受け入れ
        if hasattr(websocket, "accept"):
            await websocket.accept()

        await self.connect(session_id, websocket)

        try:
            while True:
                # メッセージ受信
                if hasattr(websocket, "receive_text"):
                    data = await websocket.receive_text()
                elif hasattr(websocket, "receive"):
                    data = await websocket.receive()
                else:
                    break

                # コマンドとして処理
                try:
                    await self._handle_message(session_id, data)
                except Exception as e:
                    self._logger.exception(f"Message handling error: {e}")

        except Exception as e:
            self._logger.info(f"WebSocket closed: {session_id} - {e}")
        finally:
            await self.disconnect(session_id)

    async def _handle_message(self, session_id: str, data: str) -> None:
        """受信メッセージを処理."""
        try:
            parsed = json.loads(data)
            command = WSCommand(session_id=session_id, **parsed)

            # コマンドハンドラーがあれば実行
            if command.type in self._command_handlers:
                handler = self._command_handlers[command.type]
                result = handler(command)
                if asyncio.iscoroutine(result):
                    await result

            # コールバック
            if self._on_command:
                self._on_command(command)

            # キューに追加
            await self._command_queue.put(command)

        except json.JSONDecodeError:
            self._logger.warning(f"Invalid JSON from {session_id}: {data[:100]}")
        except Exception as e:
            self._logger.exception(f"Command handling error: {e}")

    def register_command_handler(
        self,
        command_type: str,
        handler: Callable[[WSCommand], Any],
    ) -> None:
        """コマンドハンドラーを登録.

        Args:
            command_type: コマンドタイプ
            handler: ハンドラー関数
        """
        self._command_handlers[command_type] = handler

    async def get_next_command(self, timeout: float | None = None) -> WSCommand | None:
        """次のコマンドを取得.

        Args:
            timeout: タイムアウト（秒）

        Returns:
            コマンド、またはタイムアウト時は None
        """
        try:
            if timeout:
                return await asyncio.wait_for(
                    self._command_queue.get(),
                    timeout=timeout,
                )
            return await self._command_queue.get()
        except TimeoutError:
            return None

    async def start_heartbeat(self) -> None:
        """ハートビートを開始."""
        if self._heartbeat_task is not None:
            return

        self._heartbeat_task = asyncio.create_task(self._heartbeat_loop())
        self._logger.info("Heartbeat started")

    async def stop_heartbeat(self) -> None:
        """ハートビートを停止."""
        if self._heartbeat_task:
            self._heartbeat_task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._heartbeat_task
            self._heartbeat_task = None
            self._logger.info("Heartbeat stopped")

    async def _heartbeat_loop(self) -> None:
        """ハートビートループ."""
        while True:
            try:
                await asyncio.sleep(self._heartbeat_interval)

                # ハートビート送信
                event = WSEvent(type=WSEventType.PING, data={"ts": time.time()})
                await self.broadcast(event)

                # タイムアウトした接続をクリーンアップ
                stale_sessions = [
                    sid
                    for sid, state in self._states.items()
                    if state.is_stale(int(self._connection_timeout))
                ]
                for sid in stale_sessions:
                    self._logger.info(f"Disconnecting stale session: {sid}")
                    await self.disconnect(sid)

            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.exception(f"Heartbeat error: {e}")

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "total_connections": len(self._connections),
            "active_sessions": list(self._connections.keys()),
            "command_queue_size": self._command_queue.qsize(),
        }


# =============================================================================
# FastAPI ルーター作成（ユーティリティ）
# =============================================================================


def create_websocket_router(
    manager: WebSocketManager | None = None,
    path: str = "/ws/{session_id}",
) -> Any:
    """WebSocket ルーターを作成.

    Args:
        manager: WebSocketManager インスタンス
        path: WebSocket パス

    Returns:
        FastAPI APIRouter
    """
    try:
        from fastapi import APIRouter, WebSocket, WebSocketDisconnect
    except ImportError:
        msg = "FastAPI is required for create_websocket_router"
        raise ImportError(msg)

    router = APIRouter()
    ws_manager = manager or WebSocketManager()

    @router.websocket(path)
    async def websocket_endpoint(websocket: WebSocket, session_id: str) -> None:
        """WebSocket エンドポイント."""
        await ws_manager.handle_connection(websocket, session_id)

    @router.get("/ws/stats")
    async def get_ws_stats() -> dict[str, Any]:
        """WebSocket 統計情報."""
        return ws_manager.get_stats()

    return router


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    "ConnectionManager",
    # 接続管理
    "ConnectionState",
    "WSCommand",
    "WSEvent",
    # イベント
    "WSEventType",
    "WebSocketManager",
    # ユーティリティ
    "create_websocket_router",
]
