"""A2A TaskStore のユニットテスト."""

import pytest

from agentflow.protocols.a2a.task_store import InMemoryA2ATaskStore
from agentflow.protocols.a2a.types import (
    A2ATask,
    A2ATaskState,
    Artifact,
    TaskStatus,
    TextPart,
)


@pytest.fixture
def store() -> InMemoryA2ATaskStore:
    """テスト用ストアを作成."""
    return InMemoryA2ATaskStore()


@pytest.fixture
def sample_task() -> A2ATask:
    """テスト用タスクを作成."""
    return A2ATask(id="task-1", context_id="ctx-1")


class TestInMemoryA2ATaskStore:
    """InMemoryA2ATaskStore のテスト."""

    async def test_save_and_get(self, store: InMemoryA2ATaskStore, sample_task: A2ATask) -> None:
        """保存と取得."""
        await store.save(sample_task)
        result = await store.get("task-1")
        assert result is not None
        assert result.id == "task-1"
        assert result.context_id == "ctx-1"

    async def test_get_存在しないタスク(self, store: InMemoryA2ATaskStore) -> None:
        """存在しないタスクの取得は None."""
        result = await store.get("nonexistent")
        assert result is None

    async def test_update_status(self, store: InMemoryA2ATaskStore, sample_task: A2ATask) -> None:
        """状態更新."""
        await store.save(sample_task)
        new_status = TaskStatus(state=A2ATaskState.WORKING)
        result = await store.update_status("task-1", new_status)
        assert result is not None
        assert result.status.state == A2ATaskState.WORKING

    async def test_update_status_タスク不在(self, store: InMemoryA2ATaskStore) -> None:
        """存在しないタスクの状態更新は None."""
        new_status = TaskStatus(state=A2ATaskState.WORKING)
        result = await store.update_status("nonexistent", new_status)
        assert result is None

    async def test_add_artifact(self, store: InMemoryA2ATaskStore, sample_task: A2ATask) -> None:
        """アーティファクト追加."""
        await store.save(sample_task)
        artifact = Artifact.from_text("結果テキスト")
        result = await store.add_artifact("task-1", artifact)
        assert result is not None
        assert len(result.artifacts) == 1
        assert isinstance(result.artifacts[0].parts[0], TextPart)

    async def test_add_artifact_タスク不在(self, store: InMemoryA2ATaskStore) -> None:
        """存在しないタスクへのアーティファクト追加は None."""
        artifact = Artifact.from_text("テスト")
        result = await store.add_artifact("nonexistent", artifact)
        assert result is None

    async def test_delete(self, store: InMemoryA2ATaskStore, sample_task: A2ATask) -> None:
        """タスク削除."""
        await store.save(sample_task)
        assert await store.delete("task-1") is True
        assert await store.get("task-1") is None

    async def test_delete_存在しないタスク(self, store: InMemoryA2ATaskStore) -> None:
        """存在しないタスクの削除は False."""
        assert await store.delete("nonexistent") is False

    async def test_list_by_context(self, store: InMemoryA2ATaskStore) -> None:
        """コンテキスト ID で検索."""
        task1 = A2ATask(id="t-1", context_id="ctx-A")
        task2 = A2ATask(id="t-2", context_id="ctx-A")
        task3 = A2ATask(id="t-3", context_id="ctx-B")
        await store.save(task1)
        await store.save(task2)
        await store.save(task3)

        results = await store.list_by_context("ctx-A")
        assert len(results) == 2
        ids = {t.id for t in results}
        assert ids == {"t-1", "t-2"}

    async def test_task_count(self, store: InMemoryA2ATaskStore) -> None:
        """タスク数カウント."""
        assert store.task_count == 0
        await store.save(A2ATask(id="t-1"))
        await store.save(A2ATask(id="t-2"))
        assert store.task_count == 2

    async def test_clear(self, store: InMemoryA2ATaskStore) -> None:
        """全タスク削除."""
        await store.save(A2ATask(id="t-1"))
        await store.save(A2ATask(id="t-2"))
        store.clear()
        assert store.task_count == 0
