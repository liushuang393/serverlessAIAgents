"""Stable runtime websocket hub abstractions."""

from __future__ import annotations

import asyncio
import json
import logging
from collections.abc import Awaitable, Callable
from datetime import datetime
from enum import StrEnum
from typing import Any, Protocol

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class WSMessageType(StrEnum):
    """WebSocket message types."""

    CONNECT = "connect"
    DISCONNECT = "disconnect"
    SUBSCRIBE = "subscribe"
    UNSUBSCRIBE = "unsubscribe"
    MESSAGE = "message"
    COMMAND = "command"
    CONNECTED = "connected"
    SUBSCRIBED = "subscribed"
    UNSUBSCRIBED = "unsubscribed"
    DATA = "data"
    PROGRESS = "progress"
    ERROR = "error"
    PING = "ping"
    PONG = "pong"


class WSMessage(BaseModel):
    """WebSocket message payload."""

    type: WSMessageType = Field(..., description="message type")
    data: dict[str, Any] = Field(default_factory=dict, description="message data")
    room: str | None = Field(default=None, description="room")
    client_id: str | None = Field(default=None, description="client id")
    timestamp: str = Field(default_factory=lambda: datetime.now().isoformat(), description="timestamp")

    def to_json(self) -> str:
        """Serialize message as JSON text."""
        return json.dumps(self.model_dump(), ensure_ascii=False, default=str)

    @classmethod
    def from_json(cls, data: str | bytes) -> WSMessage:
        """Deserialize JSON payload."""
        if isinstance(data, bytes):
            data = data.decode("utf-8")
        return cls(**json.loads(data))


class WebSocketProtocol(Protocol):
    """FastAPI/Starlette-compatible websocket protocol."""

    async def accept(self) -> None: ...
    async def close(self, code: int = 1000) -> None: ...
    async def send_text(self, data: str) -> None: ...
    async def send_json(self, data: Any) -> None: ...
    async def receive_text(self) -> str: ...
    async def receive_json(self) -> Any: ...


class WSClient(BaseModel):
    """Connected websocket client metadata."""

    model_config = {"arbitrary_types_allowed": True}

    client_id: str = Field(..., description="client id")
    rooms: set[str] = Field(default_factory=set, description="joined rooms")
    connected_at: datetime = Field(default_factory=datetime.now, description="connected at")
    metadata: dict[str, Any] = Field(default_factory=dict, description="metadata")


WSHandler = Callable[[str, dict[str, Any]], Awaitable[None]]
WSNext = Callable[[WSMessage], Awaitable[None]]
WSMiddleware = Callable[[str, WSMessage, WSNext], Awaitable[None]]


class WebSocketHub:
    """Shared websocket hub for runtime-facing apps."""

    def __init__(self) -> None:
        self._connections: dict[str, WebSocketProtocol] = {}
        self._clients: dict[str, WSClient] = {}
        self._rooms: dict[str, set[str]] = {}
        self._handlers: dict[str, list[WSHandler]] = {}
        self._middlewares: list[WSMiddleware] = []
        self._logger = logging.getLogger("websocket_hub")

    async def connect(
        self,
        websocket: WebSocketProtocol,
        client_id: str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """Accept and register a client connection."""
        await websocket.accept()
        self._connections[client_id] = websocket
        self._clients[client_id] = WSClient(client_id=client_id, metadata=metadata or {})
        self._logger.info("Client connected: %s", client_id)
        await self.send(
            client_id,
            WSMessage(
                type=WSMessageType.CONNECTED,
                data={"client_id": client_id},
                client_id=client_id,
            ),
        )

    async def disconnect(self, client_id: str) -> None:
        """Disconnect a client and remove room memberships."""
        if client_id in self._clients:
            client = self._clients[client_id]
            for room in list(client.rooms):
                await self.leave_room(client_id, room)
            del self._clients[client_id]
        self._connections.pop(client_id, None)
        self._logger.info("Client disconnected: %s", client_id)

    def is_connected(self, client_id: str) -> bool:
        """Return whether the client is connected."""
        return client_id in self._connections

    def get_client(self, client_id: str) -> WSClient | None:
        """Return client metadata."""
        return self._clients.get(client_id)

    @property
    def client_count(self) -> int:
        """Return the number of active connections."""
        return len(self._connections)

    async def join_room(self, client_id: str, room: str) -> None:
        """Join a room."""
        if room not in self._rooms:
            self._rooms[room] = set()
        self._rooms[room].add(client_id)
        if client_id in self._clients:
            self._clients[client_id].rooms.add(room)
        self._logger.debug("Client %s joined room %s", client_id, room)
        await self.send(
            client_id,
            WSMessage(
                type=WSMessageType.SUBSCRIBED,
                data={"room": room},
                room=room,
                client_id=client_id,
            ),
        )

    async def leave_room(self, client_id: str, room: str) -> None:
        """Leave a room."""
        if room in self._rooms:
            self._rooms[room].discard(client_id)
            if not self._rooms[room]:
                del self._rooms[room]
        if client_id in self._clients:
            self._clients[client_id].rooms.discard(room)
        self._logger.debug("Client %s left room %s", client_id, room)

    def get_room_clients(self, room: str) -> set[str]:
        """Return client ids for a room."""
        return set(self._rooms.get(room, set()))

    @property
    def room_count(self) -> int:
        """Return the number of active rooms."""
        return len(self._rooms)

    def list_rooms(self) -> list[str]:
        """Return sorted room names."""
        return sorted(self._rooms.keys())

    async def send(self, client_id: str, message: WSMessage | dict[str, Any]) -> bool:
        """Send a message to one client."""
        websocket = self._connections.get(client_id)
        if websocket is None:
            return False
        try:
            if isinstance(message, WSMessage):
                await websocket.send_text(message.to_json())
            else:
                await websocket.send_json(message)
            return True
        except Exception as exc:
            self._logger.warning("WebSocket send failed for %s: %s", client_id, exc)
            await self.disconnect(client_id)
            return False

    async def broadcast(
        self,
        message: WSMessage | dict[str, Any],
        *,
        exclude: set[str] | None = None,
    ) -> int:
        """Broadcast to all connected clients."""
        target_clients = set(self._connections.keys())
        if exclude:
            target_clients -= exclude
        return await self._broadcast_to_clients(target_clients, message)

    async def broadcast_room(
        self,
        room: str,
        message: WSMessage | dict[str, Any],
        *,
        exclude: set[str] | None = None,
    ) -> int:
        """Broadcast to all clients in a room."""
        target_clients = self.get_room_clients(room)
        if exclude:
            target_clients -= exclude
        return await self._broadcast_to_clients(target_clients, message)

    async def _broadcast_to_clients(
        self,
        client_ids: set[str],
        message: WSMessage | dict[str, Any],
    ) -> int:
        if not client_ids:
            return 0
        if not isinstance(message, WSMessage):
            message = WSMessage(**message)
        results = await asyncio.gather(
            *(self.send(client_id, message) for client_id in client_ids),
            return_exceptions=True,
        )
        delivered = 0
        for result in results:
            if result is True:
                delivered += 1
        return delivered

    def on(self, event_type: str) -> Callable[[WSHandler], WSHandler]:
        """Register an event handler."""

        def decorator(handler: WSHandler) -> WSHandler:
            self._handlers.setdefault(event_type, []).append(handler)
            return handler

        return decorator

    def add_handler(self, event_type: str, handler: WSHandler) -> None:
        """Register an event handler without decorator syntax."""
        self._handlers.setdefault(event_type, []).append(handler)

    def add_middleware(self, middleware: WSMiddleware) -> None:
        """Register middleware."""
        self._middlewares.append(middleware)

    def use(self, middleware: WSMiddleware) -> None:
        """Compatibility alias for middleware registration."""
        self.add_middleware(middleware)

    async def emit(self, event_type: str, client_id: str, data: dict[str, Any]) -> None:
        """Emit an event to registered handlers."""
        handlers = self._handlers.get(event_type, [])
        for handler in handlers:
            try:
                await handler(client_id, data)
            except Exception as exc:
                self._logger.exception("WebSocket handler error for %s: %s", event_type, exc)

    async def handle_message(self, client_id: str, payload: str | bytes | dict[str, Any]) -> None:
        """Handle one incoming message payload."""
        message = self._normalize_message(payload)
        await self._apply_middlewares(client_id, message)

    async def handle_connection(
        self,
        websocket: WebSocketProtocol,
        client_id: str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """Accept a connection and process inbound messages until disconnect."""
        await self.connect(websocket, client_id, metadata)
        try:
            while True:
                raw_data = await websocket.receive_text()
                await self.handle_message(client_id, raw_data)
        except json.JSONDecodeError:
            await self.send(
                client_id,
                WSMessage(
                    type=WSMessageType.ERROR,
                    data={"message": "Invalid JSON"},
                    client_id=client_id,
                ),
            )
        except Exception as exc:
            self._logger.debug("Connection closed: %s - %s", client_id, exc)
        finally:
            await self.disconnect(client_id)

    def _normalize_message(self, payload: str | bytes | dict[str, Any]) -> WSMessage:
        if isinstance(payload, (str, bytes)):
            return WSMessage.from_json(payload)
        return WSMessage(**payload)

    async def _apply_middlewares(self, client_id: str, message: WSMessage) -> None:
        async def final_handler(msg: WSMessage) -> None:
            await self._dispatch_message(client_id, msg)

        handler = final_handler
        for middleware in reversed(self._middlewares):
            next_handler = handler

            async def wrapped(msg: WSMessage, mw: WSMiddleware = middleware, nxt: WSNext = next_handler) -> None:
                await mw(client_id, msg, nxt)

            handler = wrapped

        await handler(message)

    async def _dispatch_message(self, client_id: str, message: WSMessage) -> None:
        await self.emit(message.type.value, client_id, message.data)
        if message.type == WSMessageType.SUBSCRIBE and message.room:
            await self.join_room(client_id, message.room)
        elif message.type == WSMessageType.UNSUBSCRIBE and message.room:
            await self.leave_room(client_id, message.room)
        elif message.type == WSMessageType.PING:
            await self.send(client_id, {"type": WSMessageType.PONG, "data": {}})


__all__ = [
    "WSClient",
    "WSHandler",
    "WSMessage",
    "WSMessageType",
    "WSMiddleware",
    "WSNext",
    "WebSocketHub",
    "WebSocketProtocol",
]
