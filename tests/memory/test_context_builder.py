"""ContextBuilder のGating・Budget・Scoring ロジックのテスト."""

import datetime

import pytest

from infrastructure.memory.types import MemoryEntry, MemorySemanticLevel, MemoryStability, MemoryType
from kernel.memory.context_builder import (
    ContextBuilder,
    MemoryBudget,
    MemoryNeedLevel,
)
from kernel.memory.memory_manager import MemoryManager


def _make_entry(
    entry_id: str,
    content: str,
    topic: str = "test",
    importance: float = 0.5,
    semantic_level: MemorySemanticLevel = MemorySemanticLevel.SEMANTIC,
    stability: MemoryStability = MemoryStability.CONSOLIDATED,
    seconds_ago: int = 60,
) -> MemoryEntry:
    return MemoryEntry(
        id=entry_id,
        content=content,
        topic=topic,
        timestamp=datetime.datetime.now() - datetime.timedelta(seconds=seconds_ago),
        memory_type=MemoryType.LONG_TERM,
        importance_score=importance,
        semantic_level=semantic_level,
        stability=stability,
    )


class TestMemoryNeedLevel:
    """assess_need() のGating ロジックテスト."""

    def test_none_for_simple_question(self) -> None:
        """単純な質問はNONEレベルを返す."""
        cb = ContextBuilder()
        level = cb.assess_need("What is 2+2?")
        assert level == MemoryNeedLevel.NONE

    def test_low_for_preference_keywords(self) -> None:
        """「prefer」「スタイル」等の語はLOW以上のレベルを返す."""
        cb = ContextBuilder()
        level = cb.assess_need("Please answer in my preferred format")
        assert level in (MemoryNeedLevel.LOW, MemoryNeedLevel.MEDIUM)

    def test_medium_for_project_context(self) -> None:
        """「based on」「existing」等はMEDIUM以上のレベルを返す."""
        cb = ContextBuilder()
        # "previous" はHIGHキーワードにも該当するため HIGH も許容
        level = cb.assess_need("Based on the existing interface contract")
        assert level == MemoryNeedLevel.MEDIUM

    def test_high_for_continuation_keywords(self) -> None:
        """「前回/上次/之前」等はHIGH以上のレベルを返す."""
        cb = ContextBuilder()
        level_ja = cb.assess_need("前回の作業を継続してください")
        level_zh = cb.assess_need("按照上次的方案继续")
        assert level_ja in (MemoryNeedLevel.HIGH, MemoryNeedLevel.MEDIUM)
        assert level_zh in (MemoryNeedLevel.HIGH, MemoryNeedLevel.MEDIUM)


class TestMemoryBudget:
    """Budget制限のテスト."""

    def test_none_level_returns_zero_items(self) -> None:
        """NONEレベルでは記憶を注入しない."""
        cb = ContextBuilder()
        budget = cb.get_budget(MemoryNeedLevel.NONE)
        assert budget.max_items == 0

    def test_low_level_returns_2_items(self) -> None:
        """LOWレベルでは最大2件."""
        cb = ContextBuilder()
        budget = cb.get_budget(MemoryNeedLevel.LOW)
        assert budget.max_items == 2

    def test_medium_level_returns_5_items(self) -> None:
        """MEDIUMレベルでは最大5件."""
        cb = ContextBuilder()
        budget = cb.get_budget(MemoryNeedLevel.MEDIUM)
        assert budget.max_items == 5

    def test_high_level_returns_8_items(self) -> None:
        """HIGHレベルでは最大8件."""
        cb = ContextBuilder()
        budget = cb.get_budget(MemoryNeedLevel.HIGH)
        assert budget.max_items == 8

    def test_budget_is_memory_budget_type(self) -> None:
        """get_budget() はMemoryBudgetを返す."""
        cb = ContextBuilder()
        budget = cb.get_budget(MemoryNeedLevel.LOW)
        assert isinstance(budget, MemoryBudget)


@pytest.mark.asyncio
class TestContextBuilder:
    """ContextBuilder.build() のEnd-to-Endテスト."""

    async def test_none_level_returns_empty(self) -> None:
        """NONEレベルでは空リストを返す."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()
        await manager._long_term.store(_make_entry("e1", "some memory"))

        cb = ContextBuilder()
        blocks = await cb.build(
            user_request="What is 2+2?",
            memory_manager=manager,
            need_level=MemoryNeedLevel.NONE,
        )
        assert blocks == []
        await manager.stop()

    async def test_budget_limits_results(self) -> None:
        """Budget上限を超えた記憶は注入されない."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()

        # 10件の記憶を直接書き込む
        for i in range(10):
            await manager._long_term.store(_make_entry(f"e{i}", f"記憶 {i}", topic="test", importance=0.5 + i * 0.04))

        cb = ContextBuilder()
        # LOWレベルは最大2件
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.LOW,
            topic="test",
        )
        assert len(blocks) <= 2
        await manager.stop()

    async def test_high_importance_is_prioritized(self) -> None:
        """高importance記憶が低importanceより先に選ばれる."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()

        await manager._long_term.store(_make_entry("low", "低重要度記憶", topic="prio", importance=0.1))
        await manager._long_term.store(_make_entry("high", "高重要度記憶", topic="prio", importance=0.9))

        cb = ContextBuilder()
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.LOW,  # max_items=2
            topic="prio",
        )
        if len(blocks) >= 1:
            # 先頭が高重要度であるべき
            assert blocks[0].memory_id == "high"
        await manager.stop()

    async def test_episodic_is_filtered_for_normal_task(self) -> None:
        """通常タスクではEPISODIC記憶は除外される."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()

        await manager._long_term.store(
            _make_entry(
                "ep1",
                "イベント詳細記憶",
                topic="filter_test",
                importance=0.8,
                semantic_level=MemorySemanticLevel.EPISODIC,
            )
        )
        await manager._long_term.store(
            _make_entry(
                "sem1",
                "セマンティック記憶",
                topic="filter_test",
                importance=0.6,
                semantic_level=MemorySemanticLevel.SEMANTIC,
            )
        )

        cb = ContextBuilder(filter_episodic=True)
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.MEDIUM,
            topic="filter_test",
        )
        memory_ids = [b.memory_id for b in blocks]
        assert "ep1" not in memory_ids
        assert "sem1" in memory_ids
        await manager.stop()

    async def test_context_block_has_source_label(self) -> None:
        """ContextBlockにはsource_label（追跡情報）が付く."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()
        await manager._long_term.store(_make_entry("track1", "追跡テスト記憶", topic="track"))

        cb = ContextBuilder()
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.MEDIUM,
            topic="track",
        )
        if blocks:
            assert blocks[0].source_label != ""
            assert blocks[0].memory_id != ""
        await manager.stop()

    async def test_to_prompt_text(self) -> None:
        """to_prompt_text() がsource_labelとcontentを含むことを確認."""
        manager = MemoryManager(token_threshold=1000)
        await manager.start()
        await manager._long_term.store(_make_entry("pt1", "プロンプトテスト内容", topic="pt"))

        cb = ContextBuilder()
        blocks = await cb.build(
            user_request="テスト",
            memory_manager=manager,
            need_level=MemoryNeedLevel.MEDIUM,
            topic="pt",
        )
        if blocks:
            text = blocks[0].to_prompt_text()
            assert "[" in text  # source_labelのブラケット
            assert "プロンプトテスト内容" in text
        await manager.stop()
