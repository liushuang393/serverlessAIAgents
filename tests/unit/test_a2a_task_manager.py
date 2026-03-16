"""A2A TaskManager のユニットテスト."""

import pytest

from agentflow.protocols.a2a.task_manager import A2ATaskManager
from agentflow.protocols.a2a.task_store import InMemoryA2ATaskStore
from agentflow.protocols.a2a.types import (
    A2ATaskState,
    Artifact,
    Message,
    TaskStatus,
    TextPart,
)


@pytest.fixture
def store() -> InMemoryA2ATaskStore:
    """テスト用ストア."""
    return InMemoryA2ATaskStore()


@pytest.fixture
def manager(store: InMemoryA2ATaskStore) -> A2ATaskManager:
    """テスト用マネージャー."""
    return A2ATaskManager(store=store)


class TestA2ATaskManager:
    """A2ATaskManager のテスト."""

    async def test_タスク作成(self, manager: A2ATaskManager) -> None:
        """タスクを新規作成."""
        task = await manager.create_task()
        assert task.id
        assert task.status.state == A2ATaskState.SUBMITTED

    async def test_タスク作成_メッセージ付き(self, manager: A2ATaskManager) -> None:
        """初期メッセージ付きでタスクを作成."""
        msg = Message.from_text("入力テキスト")
        task = await manager.create_task(msg, context_id="ctx-1")
        assert len(task.history) == 1
        assert task.context_id == "ctx-1"
        assert task.history[0].task_id == task.id

    async def test_タスク作成_メタデータ付き(self, manager: A2ATaskManager) -> None:
        """メタデータ付きでタスクを作成."""
        task = await manager.create_task(metadata={"source": "test"})
        assert task.metadata.get("source") == "test"

    async def test_状態更新(self, manager: A2ATaskManager) -> None:
        """タスク状態を更新."""
        task = await manager.create_task()
        updated = await manager.update_task_status(task.id, A2ATaskState.WORKING)
        assert updated is not None
        assert updated.status.state == A2ATaskState.WORKING

    async def test_状態更新_テキスト付き(self, manager: A2ATaskManager) -> None:
        """テキストメッセージ付きで状態更新."""
        task = await manager.create_task()
        updated = await manager.update_task_status(task.id, A2ATaskState.WORKING, "処理中...")
        assert updated is not None
        assert updated.status.message is not None
        assert updated.status.message.text == "処理中..."

    async def test_状態更新_タスク不在(self, manager: A2ATaskManager) -> None:
        """存在しないタスクの状態更新は None."""
        result = await manager.update_task_status("nonexistent", A2ATaskState.WORKING)
        assert result is None

    async def test_アーティファクト追加(self, manager: A2ATaskManager) -> None:
        """アーティファクトを追加."""
        task = await manager.create_task()
        artifact = Artifact.from_text("結果テキスト")
        updated = await manager.add_artifact(task.id, artifact)
        assert updated is not None
        assert len(updated.artifacts) == 1

    async def test_テキストアーティファクト簡易追加(self, manager: A2ATaskManager) -> None:
        """テキストアーティファクトを簡易追加."""
        task = await manager.create_task()
        updated = await manager.add_text_artifact(task.id, "テスト結果")
        assert updated is not None
        assert len(updated.artifacts) == 1
        assert isinstance(updated.artifacts[0].parts[0], TextPart)

    async def test_データアーティファクト簡易追加(self, manager: A2ATaskManager) -> None:
        """構造化データアーティファクトを簡易追加."""
        task = await manager.create_task()
        updated = await manager.add_data_artifact(task.id, {"score": 0.9})
        assert updated is not None
        assert len(updated.artifacts) == 1

    async def test_キャンセル(self, manager: A2ATaskManager) -> None:
        """タスクをキャンセル."""
        task = await manager.create_task()
        canceled = await manager.cancel_task(task.id)
        assert canceled is not None
        assert canceled.status.state == A2ATaskState.CANCELED
        assert canceled.is_terminal

    async def test_タスク取得(self, manager: A2ATaskManager) -> None:
        """タスクをIDで取得."""
        task = await manager.create_task()
        fetched = await manager.get_task(task.id)
        assert fetched is not None
        assert fetched.id == task.id

    async def test_終端状態でキューが閉じる(self, manager: A2ATaskManager) -> None:
        """終端状態への遷移でイベントキューが閉鎖される."""
        task = await manager.create_task()
        queue = manager.queue_manager.get_queue(task.id)
        assert queue is not None
        assert not queue.is_closed

        await manager.update_task_status(task.id, A2ATaskState.COMPLETED)
        # キューは閉鎖後に QueueManager から削除される
        assert manager.queue_manager.get_queue(task.id) is None


class TestA2ATaskManagerWithObserver:
    """オブザーバー付き A2ATaskManager のテスト."""

    async def test_状態更新がオブザーバーに通知される(self) -> None:
        """状態更新時にオブザーバーが呼ばれる."""
        notifications: list[tuple[str, TaskStatus]] = []

        class MockObserver:
            async def on_status_update(self, task_id: str, status: TaskStatus) -> None:
                notifications.append((task_id, status))

            async def on_artifact_update(self, task_id: str, artifact: Artifact) -> None:
                pass

        observer = MockObserver()
        manager = A2ATaskManager(observer=observer)
        task = await manager.create_task()
        await manager.update_task_status(task.id, A2ATaskState.WORKING, "進行中")

        assert len(notifications) == 1
        assert notifications[0][0] == task.id
        assert notifications[0][1].state == A2ATaskState.WORKING

    async def test_アーティファクト追加がオブザーバーに通知される(self) -> None:
        """アーティファクト追加時にオブザーバーが呼ばれる."""
        artifacts_notified: list[tuple[str, Artifact]] = []

        class MockObserver:
            async def on_status_update(self, task_id: str, status: TaskStatus) -> None:
                pass

            async def on_artifact_update(self, task_id: str, artifact: Artifact) -> None:
                artifacts_notified.append((task_id, artifact))

        observer = MockObserver()
        manager = A2ATaskManager(observer=observer)
        task = await manager.create_task()
        await manager.add_text_artifact(task.id, "テスト")

        assert len(artifacts_notified) == 1
        assert artifacts_notified[0][0] == task.id


class TestA2ATaskManagerSubscribe:
    """イベント購読のテスト."""

    async def test_存在しないキューで購読するとエラー(self, manager: A2ATaskManager) -> None:
        """存在しないキューへの購読は ValueError."""
        with pytest.raises(ValueError, match="イベントキュー不在"):
            async for _ in manager.subscribe("nonexistent"):
                pass

    async def test_storeプロパティ(self, manager: A2ATaskManager, store: InMemoryA2ATaskStore) -> None:
        """store プロパティでストアを取得."""
        assert manager.store is store
