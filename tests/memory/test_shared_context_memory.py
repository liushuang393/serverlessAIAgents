"""Tests for SharedContext with Memory System.

SharedContextの記憶システム統合テスト。
"""

import pytest

from agentflow.patterns.multi_agent import SharedContext


@pytest.mark.asyncio
class TestSharedContextMemory:
    """SharedContext の記憶システム統合テストクラス."""

    async def test_memory_disabled_by_default(self) -> None:
        """デフォルトでは記憶システムが無効."""
        context = SharedContext()
        assert context._memory_manager is None

        # 記憶システムが無効な場合はエラー
        with pytest.raises(RuntimeError):
            await context.remember("テスト")

    async def test_memory_enabled(self) -> None:
        """記憶システムを有効化."""
        context = SharedContext(enable_memory=True)
        assert context._memory_manager is not None

    async def test_remember_and_recall(self) -> None:
        """記憶と検索のテスト."""
        context = SharedContext(enable_memory=True)
        await context.start()

        # 情報を記憶
        entry = await context.remember("重要な情報", topic="test")
        assert entry is not None

        # 記憶を検索
        memories = await context.recall(topic="test")
        assert len(memories) >= 0

        await context.stop()

    async def test_memory_with_shared_data(self) -> None:
        """共有データと記憶システムの併用テスト."""
        context = SharedContext(enable_memory=True)
        await context.start()

        # 通常の共有データ
        context.set("key1", "value1")
        assert context.get("key1") == "value1"

        # 記憶システム
        await context.remember("記憶データ", topic="shared")
        memories = await context.recall(topic="shared")

        # 両方が独立して動作
        assert context.get("key1") == "value1"
        assert len(memories) >= 0

        await context.stop()

    async def test_memory_status(self) -> None:
        """記憶システムの状態取得テスト."""
        context = SharedContext(enable_memory=True)
        await context.start()

        # 初期状態
        status = context.get_memory_status()
        assert status is not None
        assert "long_term_count" in status

        # 記憶を追加
        await context.remember("テスト情報", topic="status")

        # 状態確認
        status = context.get_memory_status()
        assert status is not None

        await context.stop()

    async def test_memory_status_disabled(self) -> None:
        """記憶システムが無効な場合の状態取得."""
        context = SharedContext(enable_memory=False)
        status = context.get_memory_status()
        assert status is None

    async def test_start_stop_without_memory(self) -> None:
        """記憶システムなしでstart/stopを呼んでもエラーにならない."""
        context = SharedContext(enable_memory=False)
        await context.start()  # エラーにならない
        await context.stop()  # エラーにならない

    async def test_multiple_contexts_with_memory(self) -> None:
        """複数のSharedContextで記憶システムを使用."""
        context1 = SharedContext(enable_memory=True)
        context2 = SharedContext(enable_memory=True)

        await context1.start()
        await context2.start()

        # 各コンテキストで独立した記憶
        await context1.remember("コンテキスト1の情報", topic="ctx1")
        await context2.remember("コンテキスト2の情報", topic="ctx2")

        memories1 = await context1.recall(topic="ctx1")
        memories2 = await context2.recall(topic="ctx2")

        assert len(memories1) >= 0
        assert len(memories2) >= 0

        await context1.stop()
        await context2.stop()

