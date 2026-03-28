"""EnhancedMemoryManager.forget_low_importance() の真正削除テスト."""

import datetime

import pytest

from kernel.memory.enhanced_memory import EnhancedMemoryManager, MemoryConfig


@pytest.mark.asyncio
class TestForgetLowImportance:
    """forget_low_importance が底層データも削除することを確認."""

    async def test_forget_removes_from_long_term(self) -> None:
        """忘却が長期記憶（_long_term._memories）からも削除することを確認."""
        config = MemoryConfig(
            min_importance=0.3,
            auto_distillation=False,
            auto_forgetting=False,
        )
        manager = EnhancedMemoryManager(config=config)
        await manager.start()

        # 低重要度でメモリを書き込む
        entry = await manager.remember(
            "忘却されるべき低重要度記憶",
            topic="forget_test",
            importance=0.05,  # min_importance=0.3 より低い
        )
        memory_id = entry.id

        # 書き込み後はtrackerに存在する
        assert memory_id in manager._tracker._importance
        long_term_before = list(manager._base._long_term._memories.keys())

        # 忘却実行
        forgotten_count = await manager.forget_low_importance()

        # trackerからは削除されている
        assert memory_id not in manager._tracker._importance

        # 長期記憶にあった場合は、そこからも削除されている
        for mid in long_term_before:
            if mid == memory_id:
                assert mid not in manager._base._long_term._memories, (
                    "forget_low_importance() はtracker削除だけでなく長期記憶からも削除する必要がある"
                )

        assert forgotten_count >= 0

        await manager.stop()

    async def test_forget_tracker_only_was_broken(self) -> None:
        """旧実装のバグ確認: trackerのみ削除で長期記憶に残留しないことを保証."""
        config = MemoryConfig(
            min_importance=0.3,
            auto_distillation=False,
            auto_forgetting=False,
            forgetting_threshold=1,  # 1件でも忘却チェック
        )
        manager = EnhancedMemoryManager(config=config)
        await manager.start()

        # 低重要度エントリをTrackerに強制登録
        entry = await manager.remember(
            "低重要度テスト記憶",
            topic="broken_test",
            importance=0.01,
        )
        memory_id = entry.id

        # 長期記憶に強制書き込み（内部テスト用）
        from infrastructure.memory.types import MemoryEntry, MemoryType

        lt_entry = MemoryEntry(
            id=memory_id,
            content="低重要度テスト記憶",
            topic="broken_test",
            timestamp=datetime.datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.01,
        )
        manager._base._long_term._memories[memory_id] = lt_entry

        # 忘却実行
        await manager.forget_low_importance()

        # 長期記憶からも削除されているべき
        assert memory_id not in manager._base._long_term._memories, (
            "forget_low_importance()修正後: 長期記憶からも削除されているべき"
        )

        await manager.stop()
