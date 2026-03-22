"""Migration UI 用タスク状態管理モジュール."""

from __future__ import annotations

import asyncio
import uuid
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from pathlib import Path


class TaskStatus(str, Enum):
    """タスクの実行状態."""

    PENDING = "pending"
    RUNNING = "running"
    COMPLETE = "complete"
    ERROR = "error"


@dataclass
class HITLRequest:
    """HITL（Human-In-The-Loop）要求."""

    request_id: str
    stage: str
    artifact: dict[str, Any]
    unknowns: list[str]
    question: str
    response_event: asyncio.Event = field(default_factory=asyncio.Event)
    response: dict[str, Any] | None = None


@dataclass
class MigrationTask:
    """移行タスクの状態."""

    task_id: str
    status: TaskStatus = TaskStatus.PENDING
    current_stage: str | None = None
    events: asyncio.Queue = field(default_factory=asyncio.Queue)
    event_history: list[tuple[int, dict[str, Any]]] = field(default_factory=list)
    event_counter: int = 0
    download_path: Path | None = None
    error_message: str | None = None
    pending_hitl: HITLRequest | None = None

    def to_status_dict(self) -> dict[str, Any]:
        """ステータス情報をdictに変換."""
        return {
            "task_id": self.task_id,
            "status": self.status.value,
            "current_stage": self.current_stage,
            "download_available": self.download_path is not None,
            "error": self.error_message,
        }


class TaskStore:
    """タスク状態ストア."""

    def __init__(self) -> None:
        self._tasks: dict[str, MigrationTask] = {}
        self._lock = asyncio.Lock()

    async def create(self) -> MigrationTask:
        task_id = str(uuid.uuid4())
        task = MigrationTask(task_id=task_id)
        async with self._lock:
            self._tasks[task_id] = task
        return task

    async def get(self, task_id: str) -> MigrationTask | None:
        return self._tasks.get(task_id)

    async def update_status(
        self,
        task_id: str,
        status: TaskStatus,
        current_stage: str | None = None,
        error_message: str | None = None,
    ) -> None:
        task = self._tasks.get(task_id)
        if task is None:
            return
        task.status = status
        if current_stage is not None:
            task.current_stage = current_stage
        if error_message is not None:
            task.error_message = error_message

    async def set_download_path(self, task_id: str, path: Path) -> None:
        task = self._tasks.get(task_id)
        if task is not None:
            task.download_path = path

    async def push_event(self, task_id: str, event: dict[str, Any]) -> None:
        task = self._tasks.get(task_id)
        if task is not None:
            task.event_counter += 1
            event_id = task.event_counter
            task.event_history.append((event_id, event))
            await task.events.put((event_id, event))

    async def close_events(self, task_id: str) -> None:
        task = self._tasks.get(task_id)
        if task is not None:
            await task.events.put(None)

    async def get_events_since(
        self,
        task_id: str,
        last_event_id: int,
    ) -> list[tuple[int, dict[str, Any]]]:
        task = self._tasks.get(task_id)
        if task is None:
            return []
        return [(event_id, event) for event_id, event in task.event_history if event_id > last_event_id]

    async def submit_hitl_response(self, task_id: str, request_id: str, response: dict[str, Any]) -> bool:
        task = self._tasks.get(task_id)
        if task is None or task.pending_hitl is None:
            return False
        if task.pending_hitl.request_id != request_id:
            return False
        task.pending_hitl.response = response
        task.pending_hitl.response_event.set()
        task.pending_hitl = None
        return True

    async def delete(self, task_id: str) -> None:
        async with self._lock:
            self._tasks.pop(task_id, None)


_store: TaskStore | None = None


def get_task_store() -> TaskStore:
    """グローバルタスクストアを返す（FastAPI DI用）."""
    global _store
    if _store is None:
        _store = TaskStore()
    return _store
