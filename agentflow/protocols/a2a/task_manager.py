"""A2A タスクライフサイクル管理.

タスクの作成・状態遷移・アーティファクト追加・イベント発火を統合する。
オブザーバーパターンで AG-UI/A2UI ブリッジに通知する。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, Protocol

from agentflow.protocols.a2a.event_queue import A2AEvent, A2AQueueManager
from agentflow.protocols.a2a.task_store import A2ATaskStore, InMemoryA2ATaskStore
from agentflow.protocols.a2a.types import (
    A2ATask,
    A2ATaskState,
    Artifact,
    Message,
    Role,
    TaskArtifactUpdateEvent,
    TaskStatus,
    TaskStatusUpdateEvent,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


class TaskEventObserver(Protocol):
    """タスクイベントのオブザーバーインターフェース.

    AG-UI/A2UI ブリッジがこのインターフェースを実装する。
    """

    async def on_status_update(self, task_id: str, status: TaskStatus) -> None:
        """状態更新通知."""
        ...

    async def on_artifact_update(self, task_id: str, artifact: Artifact) -> None:
        """アーティファクト追加通知."""
        ...


class A2ATaskManager:
    """A2A タスクライフサイクルマネージャー.

    タスクの生成から完了まで、状態遷移・イベント発火・永続化を一元管理する。
    """

    def __init__(
        self,
        *,
        store: A2ATaskStore | None = None,
        queue_manager: A2AQueueManager | None = None,
        observer: TaskEventObserver | None = None,
    ) -> None:
        """初期化.

        Args:
            store: タスクストア（None の場合は InMemory）
            queue_manager: イベントキューマネージャー（None の場合は自動生成）
            observer: イベントオブザーバー（AG-UI/A2UI ブリッジ等）
        """
        self._store = store or InMemoryA2ATaskStore()
        self._queue_manager = queue_manager or A2AQueueManager()
        self._observer = observer
        self._logger = logging.getLogger("agentflow.a2a.task_manager")

    async def create_task(
        self,
        message: Message | None = None,
        *,
        context_id: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> A2ATask:
        """タスクを新規作成.

        Args:
            message: 初期メッセージ（オプション）
            context_id: セッション/会話 ID（オプション）
            metadata: 追加メタデータ

        Returns:
            作成された A2ATask
        """
        task = A2ATask(metadata=metadata or {})
        if context_id:
            task.context_id = context_id
        if message:
            task.history.append(message)
            message.task_id = task.id

        await self._store.save(task)
        self._queue_manager.create_queue(task.id)
        self._logger.info(f"タスク作成: id={task.id}")
        return task

    async def update_task_status(
        self,
        task_id: str,
        state: A2ATaskState,
        text: str | None = None,
    ) -> A2ATask | None:
        """タスクの状態を更新しイベントを発火.

        Args:
            task_id: タスク ID
            state: 新しい状態
            text: 状態メッセージ（オプション）

        Returns:
            更新後のタスク。タスク不在なら None。
        """
        # メッセージを構築
        msg: Message | None = None
        if text:
            msg = Message.from_text(text, role=Role.AGENT, task_id=task_id)

        status = TaskStatus(state=state, message=msg)
        task = await self._store.update_status(task_id, status)
        if task is None:
            self._logger.warning(f"タスク不在: task_id={task_id}")
            return None

        # ストリーミングイベント発火
        is_final = task.is_terminal
        event = TaskStatusUpdateEvent(task_id=task_id, status=status, final=is_final)
        await self._queue_manager.put_event(task_id, event)

        # 終端状態ならキューを閉鎖
        if is_final:
            await self._queue_manager.close_queue(task_id)

        # オブザーバーに通知（AG-UI/A2UI ブリッジ）
        if self._observer:
            await self._observer.on_status_update(task_id, status)

        self._logger.debug(f"状態更新: task_id={task_id}, state={state.value}")
        return task

    async def add_artifact(
        self,
        task_id: str,
        artifact: Artifact,
    ) -> A2ATask | None:
        """タスクにアーティファクトを追加しイベントを発火.

        Args:
            task_id: タスク ID
            artifact: 追加するアーティファクト

        Returns:
            更新後のタスク。タスク不在なら None。
        """
        task = await self._store.add_artifact(task_id, artifact)
        if task is None:
            self._logger.warning(f"タスク不在: task_id={task_id}")
            return None

        # ストリーミングイベント発火
        event = TaskArtifactUpdateEvent(task_id=task_id, artifact=artifact)
        await self._queue_manager.put_event(task_id, event)

        # オブザーバーに通知
        if self._observer:
            await self._observer.on_artifact_update(task_id, artifact)

        self._logger.debug(f"アーティファクト追加: task_id={task_id}")
        return task

    async def add_text_artifact(self, task_id: str, text: str, **kwargs: Any) -> A2ATask | None:
        """テキストアーティファクトを簡易追加."""
        return await self.add_artifact(task_id, Artifact.from_text(text, **kwargs))

    async def add_data_artifact(self, task_id: str, data: dict[str, Any], **kwargs: Any) -> A2ATask | None:
        """構造化データアーティファクトを簡易追加."""
        return await self.add_artifact(task_id, Artifact.from_data(data, **kwargs))

    async def cancel_task(self, task_id: str) -> A2ATask | None:
        """タスクをキャンセル.

        Args:
            task_id: タスク ID

        Returns:
            更新後のタスク。
        """
        return await self.update_task_status(task_id, A2ATaskState.CANCELED, "キャンセルされました")

    async def get_task(self, task_id: str) -> A2ATask | None:
        """タスクを取得."""
        return await self._store.get(task_id)

    async def subscribe(self, task_id: str) -> AsyncIterator[A2AEvent]:
        """タスクのイベントストリームを購読.

        Args:
            task_id: タスク ID

        Returns:
            イベントの AsyncIterator

        Raises:
            ValueError: タスクのキューが存在しない場合
        """
        queue = self._queue_manager.get_queue(task_id)
        if queue is None:
            msg = f"イベントキュー不在: task_id={task_id}"
            raise ValueError(msg)
        async for event in queue.consume():
            yield event

    @property
    def store(self) -> A2ATaskStore:
        """タスクストアを取得."""
        return self._store

    @property
    def queue_manager(self) -> A2AQueueManager:
        """キューマネージャーを取得."""
        return self._queue_manager
