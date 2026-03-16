"""A2A タスク永続化.

タスクの保存・取得・更新を抽象化するインターフェースと InMemory 実装。
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.protocols.a2a.types import A2ATask, Artifact, TaskStatus


class A2ATaskStore(ABC):
    """タスク永続化インターフェース."""

    @abstractmethod
    async def get(self, task_id: str) -> A2ATask | None:
        """タスクを取得."""

    @abstractmethod
    async def save(self, task: A2ATask) -> None:
        """タスクを保存."""

    @abstractmethod
    async def update_status(self, task_id: str, status: TaskStatus) -> A2ATask | None:
        """タスクの状態を更新.

        Returns:
            更新後のタスク。タスクが存在しない場合は None。
        """

    @abstractmethod
    async def add_artifact(self, task_id: str, artifact: Artifact) -> A2ATask | None:
        """タスクにアーティファクトを追加.

        Returns:
            更新後のタスク。タスクが存在しない場合は None。
        """

    @abstractmethod
    async def delete(self, task_id: str) -> bool:
        """タスクを削除.

        Returns:
            削除成功なら True。
        """

    @abstractmethod
    async def list_by_context(self, context_id: str) -> list[A2ATask]:
        """コンテキスト ID でタスクを検索."""


class InMemoryA2ATaskStore(A2ATaskStore):
    """インメモリタスクストア（開発・テスト用）."""

    def __init__(self) -> None:
        """初期化."""
        self._tasks: dict[str, A2ATask] = {}

    async def get(self, task_id: str) -> A2ATask | None:
        """タスクを取得."""
        return self._tasks.get(task_id)

    async def save(self, task: A2ATask) -> None:
        """タスクを保存."""
        self._tasks[task.id] = task

    async def update_status(self, task_id: str, status: TaskStatus) -> A2ATask | None:
        """タスクの状態を更新."""
        task = self._tasks.get(task_id)
        if task is None:
            return None
        task.status = status
        return task

    async def add_artifact(self, task_id: str, artifact: Artifact) -> A2ATask | None:
        """タスクにアーティファクトを追加."""
        task = self._tasks.get(task_id)
        if task is None:
            return None
        task.artifacts.append(artifact)
        return task

    async def delete(self, task_id: str) -> bool:
        """タスクを削除."""
        if task_id in self._tasks:
            del self._tasks[task_id]
            return True
        return False

    async def list_by_context(self, context_id: str) -> list[A2ATask]:
        """コンテキスト ID でタスクを検索."""
        return [t for t in self._tasks.values() if t.context_id == context_id]

    @property
    def task_count(self) -> int:
        """保存されているタスク数."""
        return len(self._tasks)

    def clear(self) -> None:
        """全タスクを削除（テスト用）."""
        self._tasks.clear()
