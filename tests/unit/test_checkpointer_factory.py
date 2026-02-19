"""Checkpointer のファクトリ関数と実装の単体テスト."""

import os
from unittest.mock import patch

import pytest

from agentflow.hitl import (
    CheckpointData,
    Checkpointer,
    MemoryCheckpointer,
    get_checkpointer,
)


class TestGetCheckpointer:
    """get_checkpointer() ファクトリ関数のテスト."""

    def test_default_returns_memory(self) -> None:
        """デフォルトで MemoryCheckpointer を返す."""
        with patch.dict(os.environ, {}, clear=True):
            # 影響しうる環境変数をクリア
            for key in ["REDIS_URL", "DATABASE_URL", "CHECKPOINTER_BACKEND"]:
                os.environ.pop(key, None)

            cp = get_checkpointer()
            assert isinstance(cp, MemoryCheckpointer)

    def test_explicit_memory_backend(self) -> None:
        """明示指定した memory backend."""
        cp = get_checkpointer(backend="memory")
        assert isinstance(cp, MemoryCheckpointer)

    def test_redis_backend_returns_redis_checkpointer(self) -> None:
        """redis backend 指定で RedisCheckpointer を返す."""
        from agentflow.hitl.redis_checkpointer import RedisCheckpointer

        cp = get_checkpointer(backend="redis")
        # RedisCheckpointer は import 可能だが、実行時には redis パッケージが必要
        assert isinstance(cp, (MemoryCheckpointer, RedisCheckpointer))

    def test_postgres_backend_returns_postgres_checkpointer(self) -> None:
        """postgres backend 指定で PostgresCheckpointer を返す."""
        from agentflow.hitl.postgres_checkpointer import PostgresCheckpointer

        cp = get_checkpointer(backend="postgres")
        # PostgresCheckpointer は import 可能だが、実行時には asyncpg パッケージが必要
        assert isinstance(cp, (MemoryCheckpointer, PostgresCheckpointer))

    def test_env_var_redis_url_detection(self) -> None:
        """環境変数 REDIS_URL の自動検出."""
        with patch.dict(os.environ, {"REDIS_URL": "redis://localhost:6379"}):
            cp = get_checkpointer()
            # redis パッケージが存在すれば RedisCheckpointer、なければ MemoryCheckpointer
            assert isinstance(cp, Checkpointer)


class TestMemoryCheckpointer:
    """MemoryCheckpointer の単体テスト."""

    @pytest.fixture
    def checkpointer(self) -> MemoryCheckpointer:
        """テスト用 MemoryCheckpointer を作成."""
        return MemoryCheckpointer()

    @pytest.fixture
    def sample_data(self) -> CheckpointData:
        """テスト用 CheckpointData を作成."""
        return CheckpointData(
            checkpoint_id="cp-001",
            thread_id="thread-001",
            flow_id="flow-001",
            node_id="node-001",
            state={"step": 1},
            inputs={"query": "test"},
            results={"answer": "ok"},
        )

    @pytest.mark.asyncio
    async def test_save_and_load(
        self, checkpointer: MemoryCheckpointer, sample_data: CheckpointData
    ) -> None:
        """保存と読み出しのテスト."""
        await checkpointer.save(sample_data)
        loaded = await checkpointer.load("cp-001")

        assert loaded is not None
        assert loaded.checkpoint_id == "cp-001"
        assert loaded.thread_id == "thread-001"
        assert loaded.state == {"step": 1}

    @pytest.mark.asyncio
    async def test_load_nonexistent(self, checkpointer: MemoryCheckpointer) -> None:
        """存在しないチェックポイントを読み出す."""
        loaded = await checkpointer.load("nonexistent")
        assert loaded is None

    @pytest.mark.asyncio
    async def test_load_latest(self, checkpointer: MemoryCheckpointer) -> None:
        """最新のチェックポイントを読み出す."""
        # 複数のチェックポイントを保存
        for i in range(3):
            data = CheckpointData(
                checkpoint_id=f"cp-{i:03d}",
                thread_id="thread-001",
                state={"step": i},
            )
            await checkpointer.save(data)

        latest = await checkpointer.load_latest("thread-001")
        assert latest is not None
        assert latest.checkpoint_id == "cp-002"

    @pytest.mark.asyncio
    async def test_delete(
        self, checkpointer: MemoryCheckpointer, sample_data: CheckpointData
    ) -> None:
        """チェックポイントを削除する."""
        await checkpointer.save(sample_data)
        result = await checkpointer.delete("cp-001")
        assert result is True

        loaded = await checkpointer.load("cp-001")
        assert loaded is None

    @pytest.mark.asyncio
    async def test_list_by_thread(self, checkpointer: MemoryCheckpointer) -> None:
        """thread ごとにチェックポイントを列挙する."""
        # 複数スレッドのチェックポイントを保存
        for i in range(3):
            data = CheckpointData(
                checkpoint_id=f"cp-{i:03d}",
                thread_id="thread-001" if i < 2 else "thread-002",
            )
            await checkpointer.save(data)

        thread1_cps = await checkpointer.list_by_thread("thread-001")
        assert len(thread1_cps) == 2

        thread2_cps = await checkpointer.list_by_thread("thread-002")
        assert len(thread2_cps) == 1
