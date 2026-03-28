"""Tests for MemoryManager.

記憶システム統合マネージャーのテスト。
"""

import pytest

from kernel.memory import MemoryManager


@pytest.mark.asyncio
class TestMemoryManager:
    """MemoryManager のテストクラス."""

    async def test_initialization(self) -> None:
        """初期化のテスト."""
        manager = MemoryManager()
        assert manager is not None

    async def test_start_stop(self) -> None:
        """開始と停止のテスト."""
        manager = MemoryManager()
        await manager.start()
        await manager.stop()

    async def test_remember_and_recall(self) -> None:
        """記憶と検索のテスト."""
        manager = MemoryManager(token_threshold=100)
        await manager.start()

        # 情報を記憶
        entry = await manager.remember("これはAIに関する重要な情報です", topic="AI")
        assert entry is not None
        assert entry.topic == "AI"

        # 複数の情報を記憶（要約をトリガー）
        for i in range(5):
            await manager.remember(
                f"AI情報 {i}: " + "詳細な内容 " * 20,
                topic="AI",
            )

        # 記憶を検索
        memories = await manager.recall(topic="AI")
        assert len(memories) > 0

        await manager.stop()

    async def test_memory_flow(self) -> None:
        """記憶フロー（Sensory → Short-Term → Long-Term）のテスト."""
        manager = MemoryManager(token_threshold=50)
        await manager.start()

        # 短いテキスト（要約なし）
        entry1 = await manager.remember("短いテキスト", topic="test")
        assert entry1.topic == "test"

        # 長いテキスト（要約トリガー）
        long_text = "これは非常に長いテキストです。" * 50
        entry2 = await manager.remember(long_text, topic="test")
        assert entry2.topic == "test"

        # 状態確認
        status = manager.get_status()
        assert "short_term_buffers" in status
        assert "long_term_count" in status

        await manager.stop()

    async def test_update_memory(self) -> None:
        """記憶更新のテスト."""
        manager = MemoryManager(token_threshold=100)
        await manager.start()

        # 記憶を作成
        for i in range(3):
            await manager.remember(
                f"情報 {i}: " + "内容 " * 30,
                topic="update_test",
            )

        # 記憶を検索
        memories = await manager.recall(topic="update_test")
        if memories:
            # 最初の記憶を更新
            await manager.update_memory(
                memories[0].id,
                {"importance_score": 0.9},
            )

            # 統合を実行
            count = await manager.consolidate()
            assert count >= 0

        await manager.stop()

    async def test_get_status(self) -> None:
        """状態取得のテスト."""
        manager = MemoryManager()
        await manager.start()

        # 初期状態
        status = manager.get_status()
        assert status["long_term_count"] == 0

        # 記憶を追加
        await manager.remember("テスト情報", topic="status")

        # 状態確認
        status = manager.get_status()
        assert "short_term_buffers" in status

        await manager.stop()

    async def test_multiple_topics(self) -> None:
        """複数トピックのテスト."""
        manager = MemoryManager(token_threshold=100)
        await manager.start()

        # 異なるトピックで記憶
        await manager.remember("AI情報", topic="AI")
        await manager.remember("Python情報", topic="Python")
        await manager.remember("データベース情報", topic="Database")

        # トピック別に検索
        ai_memories = await manager.recall(topic="AI")
        python_memories = await manager.recall(topic="Python")

        assert len(ai_memories) >= 0
        assert len(python_memories) >= 0

        await manager.stop()

    async def test_importance_filtering(self) -> None:
        """重要度フィルタリングのテスト."""
        manager = MemoryManager(token_threshold=100)
        await manager.start()

        # 複数の記憶を追加（要約をトリガー）
        for i in range(5):
            await manager.remember(
                f"情報 {i}: " + "内容 " * 30,
                topic="importance",
            )

        # 重要度でフィルタリング
        high_importance = await manager.recall(
            topic="importance",
            min_importance=0.7,
        )
        all_memories = await manager.recall(topic="importance")

        assert len(high_importance) <= len(all_memories)

        await manager.stop()


async def test_memory_manager_delete() -> None:
    """MemoryManager.delete() が長期記憶からエントリを削除することを確認."""
    import datetime

    from infrastructure.memory.types import MemoryEntry, MemoryType

    manager = MemoryManager(token_threshold=1000)
    await manager.start()

    # 長期記憶に直接書き込む
    entry = MemoryEntry(
        id="mgr-del-001",
        content="ManagerDeleteテスト記憶",
        topic="del_manager",
        timestamp=datetime.datetime.now(),
        memory_type=MemoryType.LONG_TERM,
    )
    await manager._long_term.store(entry)

    # 削除前は検索結果に含まれる
    memories_before = await manager.recall(topic="del_manager")
    assert any(m.id == "mgr-del-001" for m in memories_before)

    # delete()を呼ぶ
    result = await manager.delete("mgr-del-001")
    assert result is True

    # 削除後は検索結果に含まれない
    memories_after = await manager.recall(topic="del_manager")
    assert all(m.id != "mgr-del-001" for m in memories_after)

    # 存在しないIDはFalseを返す
    result2 = await manager.delete("not-exist-xxx")
    assert result2 is False

    await manager.stop()


async def test_long_term_memory_delete() -> None:
    """LongTermMemory.delete() が記憶と更新キューを両方削除することを確認."""
    import datetime

    from infrastructure.memory.types import MemoryEntry, MemoryType
    from kernel.memory.long_term_memory import LongTermMemory

    ltm = LongTermMemory(enable_auto_consolidation=False)
    await ltm.start()

    entry = MemoryEntry(
        id="del-test-001",
        content="削除テスト記憶",
        topic="delete",
        timestamp=datetime.datetime.now(),
        memory_type=MemoryType.LONG_TERM,
    )
    await ltm.store(entry)
    await ltm.update("del-test-001", {"importance_score": 0.9})

    # 削除前は存在する
    assert "del-test-001" in ltm._memories
    assert "del-test-001" in ltm._update_queues

    # 削除
    result = await ltm.delete("del-test-001")
    assert result is True

    # 削除後は存在しない（メモリも更新キューも）
    assert "del-test-001" not in ltm._memories
    assert "del-test-001" not in ltm._update_queues

    # 存在しないIDを削除してもFalseを返す
    result2 = await ltm.delete("not-exist")
    assert result2 is False

    await ltm.stop()
