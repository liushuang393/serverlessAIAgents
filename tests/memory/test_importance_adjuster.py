"""重要度自動調整のテスト."""

from datetime import datetime, timedelta

import pytest

from agentflow.memory.importance_adjuster import ImportanceAdjuster
from agentflow.memory.types import (
    MemoryEntry,
    MemoryStability,
    MemoryType,
)


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
        assert abs(stats["average_accesses"] - 1.5) < 0.001
        assert abs(stats["decay_constant"] - 30.0) < 0.001
        assert abs(stats["access_boost_factor"] - 0.1) < 0.001


class TestForgetPolicy:
    """忘却ポリシーのテスト."""

    @pytest.fixture
    def adjuster(self) -> ImportanceAdjuster:
        """ImportanceAdjusterインスタンスを作成."""
        return ImportanceAdjuster(
            decay_constant=30.0,
            access_boost_factor=0.1,
        )

    def test_identify_forgettable_low_importance(self, adjuster: ImportanceAdjuster) -> None:
        """低重要度記憶の忘却判定テスト."""
        # 低重要度、古い、負の強化
        entry = MemoryEntry(
            id="forget_1",
            content="低価値な記憶",
            topic="test",
            timestamp=datetime.now() - timedelta(days=90),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.05,
            reinforcement_score=-0.5,
            stability=MemoryStability.VOLATILE,
        )

        forgettable = adjuster.identify_forgettable([entry])
        assert "forget_1" in forgettable

    def test_identify_forgettable_not_high_importance(self, adjuster: ImportanceAdjuster) -> None:
        """高重要度記憶は忘却されないテスト."""
        entry = MemoryEntry(
            id="keep_1",
            content="重要な記憶",
            topic="test",
            timestamp=datetime.now() - timedelta(days=90),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.8,  # 高重要度
            reinforcement_score=0.5,
        )

        forgettable = adjuster.identify_forgettable([entry])
        assert "keep_1" not in forgettable

    def test_identify_forgettable_not_crystallized(self, adjuster: ImportanceAdjuster) -> None:
        """結晶化された記憶は忘却されないテスト."""
        entry = MemoryEntry(
            id="crystal_1",
            content="結晶化された記憶",
            topic="test",
            timestamp=datetime.now() - timedelta(days=180),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.05,  # 低重要度でも
            reinforcement_score=-0.5,  # 負の強化でも
            stability=MemoryStability.CRYSTALLIZED,  # 結晶化されていれば保持
        )

        forgettable = adjuster.identify_forgettable([entry])
        assert "crystal_1" not in forgettable

    def test_identify_forgettable_not_recent(self, adjuster: ImportanceAdjuster) -> None:
        """最近の記憶は忘却されないテスト."""
        entry = MemoryEntry(
            id="recent_1",
            content="最近の記憶",
            topic="test",
            timestamp=datetime.now() - timedelta(days=10),  # 最近
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.05,
            reinforcement_score=-0.5,
            stability=MemoryStability.VOLATILE,
        )

        forgettable = adjuster.identify_forgettable([entry])
        assert "recent_1" not in forgettable


class TestReinforcementLearning:
    """強化学習フィードバックのテスト."""

    @pytest.fixture
    def adjuster(self) -> ImportanceAdjuster:
        """ImportanceAdjusterインスタンスを作成."""
        return ImportanceAdjuster(decay_constant=30.0, access_boost_factor=0.1)

    @pytest.mark.asyncio
    async def test_apply_reinforcement_positive(self, adjuster: ImportanceAdjuster) -> None:
        """正の強化フィードバックのテスト."""
        entry = MemoryEntry(
            id="reinforce_1",
            content="強化対象の記憶",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
            reinforcement_score=0.0,
        )

        original_score = entry.reinforcement_score
        await adjuster.apply_reinforcement(entry, reward=1.0)

        assert entry.reinforcement_score > original_score

    @pytest.mark.asyncio
    async def test_apply_reinforcement_negative(self, adjuster: ImportanceAdjuster) -> None:
        """負の強化フィードバックのテスト."""
        entry = MemoryEntry(
            id="reinforce_2",
            content="強化対象の記憶",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
            reinforcement_score=0.0,
        )

        original_score = entry.reinforcement_score
        await adjuster.apply_reinforcement(entry, reward=-0.5)

        assert entry.reinforcement_score < original_score

    @pytest.mark.asyncio
    async def test_apply_reinforcement_stability_upgrade(
        self, adjuster: ImportanceAdjuster
    ) -> None:
        """強化による安定性アップグレードのテスト."""
        entry = MemoryEntry(
            id="upgrade_1",
            content="強化対象の記憶",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
            reinforcement_score=0.4,  # 閾値付近
            stability=MemoryStability.VOLATILE,
        )

        # 高い報酬を複数回適用
        await adjuster.apply_reinforcement(entry, reward=1.0)
        await adjuster.apply_reinforcement(entry, reward=1.0)

        # 安定性がアップグレードされる
        assert entry.stability in [
            MemoryStability.CONSOLIDATED,
            MemoryStability.CRYSTALLIZED,
        ]

    @pytest.mark.asyncio
    async def test_apply_reinforcement_clamps_score(self, adjuster: ImportanceAdjuster) -> None:
        """強化スコアが範囲内に収まるテスト."""
        entry = MemoryEntry(
            id="clamp_1",
            content="強化対象の記憶",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=0.5,
            reinforcement_score=0.9,  # 高い初期値
        )

        # 大きな報酬を適用
        await adjuster.apply_reinforcement(entry, reward=1.0)

        # スコアは1.0を超えない
        assert entry.reinforcement_score <= 1.0
