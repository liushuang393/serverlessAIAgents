# -*- coding: utf-8 -*-
"""Event Provider - 統一イベント通知インターフェース.

このモジュールは、SSE/WebSocket/Callbackの統一イベント通知を提供します。
AG-UIプロトコルに準拠。

使用例:
    >>> events = EventProvider.create()
    >>> await events.emit("progress", {"step": 1, "total": 3})
    >>> await events.emit("node_start", {"node_id": "agent1"})
    >>> # SSE用ジェネレータ
    >>> async for event in events.stream():
    ...     yield f"data: {event}\\n\\n"
"""

import asyncio
import json
import logging
import time
from collections.abc import AsyncIterator
from typing import Any, Callable

from pydantic import BaseModel, Field


class EventData(BaseModel):
    """イベントデータ.

    Args:
        event_type: イベントタイプ
        timestamp: タイムスタンプ
        data: イベントデータ
        flow_id: フローID
        node_id: ノードID
    """

    event_type: str = Field(..., description="イベントタイプ")
    timestamp: float = Field(default_factory=time.time, description="タイムスタンプ")
    data: dict[str, Any] = Field(default_factory=dict, description="イベントデータ")
    flow_id: str | None = Field(default=None, description="フローID")
    node_id: str | None = Field(default=None, description="ノードID")


class EventProvider:
    """イベント統一プロバイダー.

    SSE/WebSocket/Callbackへの統一イベント通知を提供。
    AG-UIプロトコル準拠のイベント発火をサポート。

    使用例:
        >>> events = EventProvider.create()
        >>> await events.emit("progress", {"step": 1})
    """

    def __init__(
        self,
        mode: str = "queue",
        queue_size: int = 1000,
    ) -> None:
        """初期化.

        Args:
            mode: モード (queue/callback/broadcast)
            queue_size: キューサイズ
        """
        self._mode = mode
        self._logger = logging.getLogger(__name__)
        self._queue: asyncio.Queue[EventData] = asyncio.Queue(maxsize=queue_size)
        self._callbacks: list[Callable[[EventData], Any]] = []
        self._flow_id: str | None = None
        self._closed = False

    @classmethod
    def create(
        cls,
        mode: str = "queue",
        flow_id: str | None = None,
    ) -> "EventProvider":
        """インスタンスを作成.

        Args:
            mode: モード
            flow_id: フローID

        Returns:
            EventProvider
        """
        provider = cls(mode=mode)
        provider._flow_id = flow_id
        return provider

    def set_flow_id(self, flow_id: str) -> None:
        """フローIDを設定.

        Args:
            flow_id: フローID
        """
        self._flow_id = flow_id

    def add_callback(self, callback: Callable[[EventData], Any]) -> None:
        """コールバックを追加.

        Args:
            callback: コールバック関数
        """
        self._callbacks.append(callback)

    async def emit(
        self,
        event_type: str,
        data: dict[str, Any] | None = None,
        node_id: str | None = None,
    ) -> None:
        """イベントを発火.

        Args:
            event_type: イベントタイプ
            data: イベントデータ
            node_id: ノードID
        """
        if self._closed:
            return

        event = EventData(
            event_type=event_type,
            timestamp=time.time(),
            data=data or {},
            flow_id=self._flow_id,
            node_id=node_id,
        )

        # キューモード
        if self._mode == "queue":
            try:
                await asyncio.wait_for(self._queue.put(event), timeout=1.0)
            except TimeoutError:
                self._logger.warning("Event queue full, dropping event")

        # コールバックモード
        for callback in self._callbacks:
            try:
                result = callback(event)
                if asyncio.iscoroutine(result):
                    await result
            except Exception as e:
                self._logger.warning(f"Callback error: {e}")

    async def emit_flow_start(self, data: dict[str, Any] | None = None) -> None:
        """フロー開始イベントを発火."""
        await self.emit("flow_start", data)

    async def emit_flow_complete(self, data: dict[str, Any] | None = None) -> None:
        """フロー完了イベントを発火."""
        await self.emit("flow_complete", data)

    async def emit_flow_error(
        self,
        error_message: str,
        error_type: str = "Error",
    ) -> None:
        """フローエラーイベントを発火."""
        await self.emit("flow_error", {
            "error_message": error_message,
            "error_type": error_type,
        })

    async def emit_node_start(
        self,
        node_id: str,
        node_name: str,
        data: dict[str, Any] | None = None,
    ) -> None:
        """ノード開始イベントを発火."""
        await self.emit(
            "node_start",
            {"node_name": node_name, **(data or {})},
            node_id=node_id,
        )

    async def emit_node_complete(
        self,
        node_id: str,
        node_name: str,
        data: dict[str, Any] | None = None,
    ) -> None:
        """ノード完了イベントを発火."""
        await self.emit(
            "node_complete",
            {"node_name": node_name, **(data or {})},
            node_id=node_id,
        )

    async def emit_progress(
        self,
        current: int,
        total: int,
        message: str | None = None,
    ) -> None:
        """プログレスイベントを発火."""
        await self.emit("progress", {
            "current": current,
            "total": total,
            "percentage": (current / total) * 100 if total > 0 else 0,
            "message": message,
        })

    async def emit_log(
        self,
        message: str,
        level: str = "INFO",
        source: str | None = None,
    ) -> None:
        """ログイベントを発火."""
        await self.emit("log", {
            "message": message,
            "level": level,
            "source": source,
        })

    async def stream(self) -> AsyncIterator[EventData]:
        """イベントストリームを取得.

        Yields:
            EventData: イベント
        """
        while not self._closed:
            try:
                event = await asyncio.wait_for(self._queue.get(), timeout=0.1)
                yield event
            except TimeoutError:
                continue
            except asyncio.CancelledError:
                break

    async def stream_sse(self) -> AsyncIterator[str]:
        """SSE形式のストリームを取得.

        Yields:
            SSE形式の文字列
        """
        async for event in self.stream():
            data = event.model_dump()
            yield f"event: {event.event_type}\ndata: {json.dumps(data)}\n\n"

    def drain(self) -> list[EventData]:
        """キューから全イベントを取り出し.

        Returns:
            イベントリスト
        """
        events = []
        while not self._queue.empty():
            try:
                events.append(self._queue.get_nowait())
            except asyncio.QueueEmpty:
                break
        return events

    def close(self) -> None:
        """プロバイダーをクローズ."""
        self._closed = True

    @property
    def flow_id(self) -> str | None:
        """フローIDを取得."""
        return self._flow_id

