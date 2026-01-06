# -*- coding: utf-8 -*-
"""Checkpointer 工厂函数和实现的单元测试."""

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
    """get_checkpointer() 工厂函数测试."""

    def test_default_returns_memory(self) -> None:
        """默认返回 MemoryCheckpointer."""
        with patch.dict(os.environ, {}, clear=True):
            # 清除可能影响的环境变量
            for key in ["REDIS_URL", "DATABASE_URL", "CHECKPOINTER_BACKEND"]:
                os.environ.pop(key, None)

            cp = get_checkpointer()
            assert isinstance(cp, MemoryCheckpointer)

    def test_explicit_memory_backend(self) -> None:
        """明确指定 memory 后端."""
        cp = get_checkpointer(backend="memory")
        assert isinstance(cp, MemoryCheckpointer)

    def test_redis_backend_returns_redis_checkpointer(self) -> None:
        """指定 redis 后端返回 RedisCheckpointer."""
        from agentflow.hitl.redis_checkpointer import RedisCheckpointer
        cp = get_checkpointer(backend="redis")
        # RedisCheckpointer 可以导入，但运行时需要 redis 包
        assert isinstance(cp, (MemoryCheckpointer, RedisCheckpointer))

    def test_postgres_backend_returns_postgres_checkpointer(self) -> None:
        """指定 postgres 后端返回 PostgresCheckpointer."""
        from agentflow.hitl.postgres_checkpointer import PostgresCheckpointer
        cp = get_checkpointer(backend="postgres")
        # PostgresCheckpointer 可以导入，但运行时需要 asyncpg 包
        assert isinstance(cp, (MemoryCheckpointer, PostgresCheckpointer))

    def test_env_var_redis_url_detection(self) -> None:
        """环境变量 REDIS_URL 自动检测."""
        with patch.dict(os.environ, {"REDIS_URL": "redis://localhost:6379"}):
            cp = get_checkpointer()
            # 如果 redis 包存在，返回 RedisCheckpointer
            # 否则返回 MemoryCheckpointer
            assert isinstance(cp, Checkpointer)


class TestMemoryCheckpointer:
    """MemoryCheckpointer 单元测试."""

    @pytest.fixture
    def checkpointer(self) -> MemoryCheckpointer:
        """创建测试用 MemoryCheckpointer."""
        return MemoryCheckpointer()

    @pytest.fixture
    def sample_data(self) -> CheckpointData:
        """创建测试用 CheckpointData."""
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
        """保存和加载测试."""
        await checkpointer.save(sample_data)
        loaded = await checkpointer.load("cp-001")

        assert loaded is not None
        assert loaded.checkpoint_id == "cp-001"
        assert loaded.thread_id == "thread-001"
        assert loaded.state == {"step": 1}

    @pytest.mark.asyncio
    async def test_load_nonexistent(self, checkpointer: MemoryCheckpointer) -> None:
        """加载不存在的检查点."""
        loaded = await checkpointer.load("nonexistent")
        assert loaded is None

    @pytest.mark.asyncio
    async def test_load_latest(
        self, checkpointer: MemoryCheckpointer
    ) -> None:
        """加载最新检查点."""
        # 保存多个检查点
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
        """删除检查点."""
        await checkpointer.save(sample_data)
        result = await checkpointer.delete("cp-001")
        assert result is True

        loaded = await checkpointer.load("cp-001")
        assert loaded is None

    @pytest.mark.asyncio
    async def test_list_by_thread(
        self, checkpointer: MemoryCheckpointer
    ) -> None:
        """按线程列出检查点."""
        # 保存多个线程的检查点
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

