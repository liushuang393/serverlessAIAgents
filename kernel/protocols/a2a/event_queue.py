"""A2A イベントキュー.

タスクごとの asyncio.Queue でストリーミングイベントを管理する。
"""

from __future__ import annotations

import asyncio
import logging
from typing import TYPE_CHECKING

from kernel.protocols.a2a.types import (
    TaskArtifactUpdateEvent,
    TaskStatusUpdateEvent,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


# イベントの Union 型
A2AEvent = TaskStatusUpdateEvent | TaskArtifactUpdateEvent

# キュー終端を示すセンチネル
_SENTINEL: object = object()


class A2AEventQueue:
    """タスク単位のイベントキュー."""

    def __init__(self, task_id: str, *, maxsize: int = 0) -> None:
        """初期化.

        Args:
            task_id: 対象タスク ID
            maxsize: キューの最大サイズ（0 = 無制限）
        """
        self.task_id = task_id
        self._queue: asyncio.Queue[A2AEvent | object] = asyncio.Queue(maxsize=maxsize)
        self._closed = False

    async def put(self, event: A2AEvent) -> None:
        """イベントをキューに追加.

        Args:
            event: 追加するイベント
        """
        if not self._closed:
            await self._queue.put(event)

    async def close(self) -> None:
        """キューを終了（センチネルを送信）."""
        if not self._closed:
            self._closed = True
            await self._queue.put(_SENTINEL)

    async def consume(self) -> AsyncIterator[A2AEvent]:
        """イベントをストリーム消費.

        Yields:
            A2AEvent（センチネル受信で終了）
        """
        while True:
            item = await self._queue.get()
            if item is _SENTINEL:
                break
            yield item  # type: ignore[misc]

    @property
    def is_closed(self) -> bool:
        """キューが閉じられたか."""
        return self._closed


class A2AQueueManager:
    """複数タスクのイベントキューを管理."""

    def __init__(self) -> None:
        """初期化."""
        self._queues: dict[str, A2AEventQueue] = {}
        self._logger = logging.getLogger("kernel.a2a.queue_manager")

    def create_queue(self, task_id: str, *, maxsize: int = 0) -> A2AEventQueue:
        """タスク用のイベントキューを作成.

        Args:
            task_id: タスク ID
            maxsize: キューの最大サイズ

        Returns:
            作成されたイベントキュー
        """
        if task_id in self._queues:
            self._logger.warning(f"キュー既存: task_id={task_id}、既存キューを返却")
            return self._queues[task_id]

        queue = A2AEventQueue(task_id, maxsize=maxsize)
        self._queues[task_id] = queue
        self._logger.debug(f"キュー作成: task_id={task_id}")
        return queue

    def get_queue(self, task_id: str) -> A2AEventQueue | None:
        """タスクのイベントキューを取得."""
        return self._queues.get(task_id)

    async def close_queue(self, task_id: str) -> None:
        """タスクのイベントキューを閉じて削除."""
        queue = self._queues.pop(task_id, None)
        if queue is not None:
            await queue.close()
            self._logger.debug(f"キュー閉鎖: task_id={task_id}")

    async def put_event(self, task_id: str, event: A2AEvent) -> bool:
        """指定タスクのキューにイベントを送信.

        Returns:
            送信成功なら True。キューが存在しない場合は False。
        """
        queue = self._queues.get(task_id)
        if queue is None:
            return False
        await queue.put(event)
        return True

    @property
    def queue_count(self) -> int:
        """管理中のキュー数."""
        return len(self._queues)

    async def close_all(self) -> None:
        """全キューを閉鎖（シャットダウン用）."""
        for task_id in list(self._queues.keys()):
            await self.close_queue(task_id)
