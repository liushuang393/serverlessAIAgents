"""重要度自動調整のテスト."""

import pytest
from datetime import datetime, timedelta

from agentflow.memory.importance_adjuster import ImportanceAdjuster
from agentflow.memory.types import MemoryEntry, MemoryType


class TestImportanceAdjuster:
    """ImportanceAdjusterクラスのテスト."""

    @pytest.fixture
    def adjuster(self) -> ImportanceAdjuster:
        """ImportanceAdjusterインスタンスを作成."""
        return ImportanceAdjuster(decay_constant=30.0, access_boost_factor=0.1)

    @pytest.mark.asyncio
    async def test_start_stop(self, adjuster: ImportanceAdjuster) -> None:
        """開始・停止のテスト."""
        await adjuster.start()
        assert adjuster._running is True
        assert adjuster._recalculation_task is not None

        await adjuster.stop()
        assert adjuster._running is False

    def test_record_access(self, adjuster: ImportanceAdjuster) -> None:
        """アクセス記録のテスト."""
        entry_id = "test_entry_1"

        # 初回アクセス
        adjuster.record_access(entry_id)
        assert adjuster._access_counts[entry_id] == 1
        assert entry_id in adjuster._last_access

        # 2回目アクセス
        adjuster.record_access(entry_id)
        assert adjuster._access_counts[entry_id] == 2

    def test_calculate_importance_base(self, adjuster: ImportanceAdjuster) -> None:
        """基本的な重要度計算のテスト."""
        entry = MemoryEntry(
            id="test_1",
            content="Test content",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
        )

        # アクセスなし、時間経過なし
        importance = adjuster.calculate_importance(entry)
        assert 0.0 <= importance <= 1.0
        assert abs(importance - 0.5) < 0.1  # 基本スコアに近い

    def test_calculate_importance_with_access(self, adjuster: ImportanceAdjuster) -> None:
        """アクセス回数を考慮した重要度計算のテスト."""
        entry = MemoryEntry(
            id="test_2",
            content="Test content",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
        )

        # アクセスなし
        importance_no_access = adjuster.calculate_importance(entry)

        # 複数回アクセス
        for _ in range(10):
            adjuster.record_access(entry.id)

        importance_with_access = adjuster.calculate_importance(entry)

        # アクセスありの方が重要度が高い
        assert importance_with_access > importance_no_access

    def test_calculate_importance_with_decay(self, adjuster: ImportanceAdjuster) -> None:
        """時間減衰を考慮した重要度計算のテスト."""
        # 最近の記憶
        recent_entry = MemoryEntry(
            id="recent",
            content="Recent content",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
        )

        # 古い記憶（30日前）
        old_entry = MemoryEntry(
            id="old",
            content="Old content",
            topic="test",
            timestamp=datetime.now() - timedelta(days=30),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
        )

        importance_recent = adjuster.calculate_importance(recent_entry)
        importance_old = adjuster.calculate_importance(old_entry)

        # 最近の記憶の方が重要度が高い
        assert importance_recent > importance_old

    @pytest.mark.asyncio
    async def test_adjust_importance(self, adjuster: ImportanceAdjuster) -> None:
        """重要度調整のテスト."""
        entry = MemoryEntry(
            id="test_3",
            content="Test content",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
        )

        original_score = entry.importance_score
        new_score = await adjuster.adjust_importance(entry)

        assert entry.importance_score == new_score
        assert 0.0 <= new_score <= 1.0

    @pytest.mark.asyncio
    async def test_adjust_batch(self, adjuster: ImportanceAdjuster) -> None:
        """一括重要度調整のテスト."""
        entries = [
            MemoryEntry(
                id=f"test_{i}",
                content=f"Test content {i}",
                topic="test",
                timestamp=datetime.now(),
                memory_type=MemoryType.LONG_TERM,
                importance_score=0.5,
            )
            for i in range(5)
        ]

        adjusted_entries = await adjuster.adjust_batch(entries)

        assert len(adjusted_entries) == 5
        for entry in adjusted_entries:
            assert 0.0 <= entry.importance_score <= 1.0

    def test_get_statistics(self, adjuster: ImportanceAdjuster) -> None:
        """統計情報取得のテスト."""
        # アクセスを記録
        adjuster.record_access("entry_1")
        adjuster.record_access("entry_1")
        adjuster.record_access("entry_2")

        stats = adjuster.get_statistics()

        assert stats["total_accesses"] == 3
        assert stats["unique_entries"] == 2
        assert stats["average_accesses"] == 1.5
        assert stats["decay_constant"] == 30.0
        assert stats["access_boost_factor"] == 0.1

