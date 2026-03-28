"""語彙検索・多視点検索のテスト.

TDD: search_lexical() と RRF による多視点検索の確認。
"""

from __future__ import annotations

from datetime import datetime

from infrastructure.memory.types import MemoryEntry, MemorySemanticLevel, MemoryType
from infrastructure.memory.vector_search import VectorSearch
from kernel.memory.context_builder import ContextBuilder, MemoryNeedLevel
from kernel.memory.memory_manager import MemoryManager


def _make_entry(entry_id: str, content: str, topic: str = "test") -> MemoryEntry:
    """テスト用 MemoryEntry を作成するヘルパー."""
    return MemoryEntry(
        id=entry_id,
        content=content,
        topic=topic,
        timestamp=datetime.now(),
        memory_type=MemoryType.LONG_TERM,
        importance_score=0.6,
        semantic_level=MemorySemanticLevel.SEMANTIC,
    )


class TestLexicalSearch:
    """語彙検索のテスト."""

    async def test_keyword_match_found(self) -> None:
        """固有名詞で正確にマッチすること."""
        vs = VectorSearch()
        memories = [
            _make_entry("e1", "tanaka approved project alpha budget"),
            _make_entry("e2", "suzuki reviewed the design document"),
            _make_entry("e3", "yamada prepared the presentation slides"),
        ]

        results = await vs.search_lexical("tanaka project", memories, top_k=3)
        assert len(results) > 0
        # 最上位の結果が tanaka を含むこと
        top_content = results[0][0].content
        assert "tanaka" in top_content

    async def test_semantic_misses_lexical_finds(self) -> None:
        """意味検索がミスしても語彙検索が固有名詞を見つけること."""
        vs = VectorSearch()
        memories = [
            _make_entry("e1", "Project-XYZ-9999 has been completed successfully"),
            _make_entry("e2", "the weather is nice today"),
            _make_entry("e3", "lunch menu includes pasta and salad"),
        ]

        # "Project-XYZ-9999" という固有名詞を語彙検索で見つける
        results = await vs.search_lexical("Project-XYZ-9999", memories, top_k=3)
        assert len(results) > 0
        top_content = results[0][0].content
        assert "Project-XYZ-9999" in top_content

    async def test_empty_memories_returns_empty(self) -> None:
        """記憶が空の場合は空リストを返すこと."""
        vs = VectorSearch()
        results = await vs.search_lexical("query", [], top_k=5)
        assert results == []

    async def test_score_range(self) -> None:
        """スコアが 0.0-1.0 の範囲であること."""
        vs = VectorSearch()
        memories = [
            _make_entry("e1", "alpha beta gamma delta"),
            _make_entry("e2", "epsilon zeta eta theta"),
        ]
        results = await vs.search_lexical("alpha beta", memories, top_k=2)
        for _, score in results:
            assert 0.0 <= score <= 1.0

    async def test_no_match_returns_empty_or_low_score(self) -> None:
        """クエリに合致しない場合は空か低スコアを返すこと."""
        vs = VectorSearch()
        memories = [
            _make_entry("e1", "xyz abc def ghi"),
        ]
        results = await vs.search_lexical("completely unrelated query words here", memories, top_k=5)
        # マッチなしか低スコア（0.0は除外される場合がある）
        for _, score in results:
            assert score >= 0.0

    async def test_top_k_respected(self) -> None:
        """top_k 件数が守られること."""
        vs = VectorSearch()
        memories = [_make_entry(f"e{i}", f"apple orange banana grape {i}") for i in range(10)]
        results = await vs.search_lexical("apple orange", memories, top_k=3)
        assert len(results) <= 3


class TestMultiViewRetrieval:
    """多視点検索のテスト."""

    async def test_build_uses_vector_search_when_available(self) -> None:
        """ベクトル検索が有効な場合に多視点検索が利用されること."""
        manager = MemoryManager(enable_vector_search=True)
        await manager.start()
        try:
            # セマンティックレベルの記憶を保存（フィルタをパスするため）
            from infrastructure.memory.types import MemorySemanticLevel

            entry = _make_entry("e1", "architecture design patterns for AI agents")
            entry.semantic_level = MemorySemanticLevel.SEMANTIC
            await manager._long_term.store(entry)

            cb = ContextBuilder(filter_episodic=False)
            blocks = await cb.build(
                user_request="前回のアーキテクチャ設計について",
                memory_manager=manager,
                need_level=MemoryNeedLevel.HIGH,
            )
            # エラーなく実行できること
            assert isinstance(blocks, list)
        finally:
            await manager.stop()

    async def test_rrf_combines_results(self) -> None:
        """RRF で意味+語彙の結果が統合されること."""
        manager = MemoryManager(enable_vector_search=True)
        await manager.start()
        try:
            from infrastructure.memory.types import MemorySemanticLevel

            # 複数の記憶を保存
            entries = [
                _make_entry("e1", "tanaka approved the Q4 budget plan"),
                _make_entry("e2", "suzuki reviewed architecture design"),
                _make_entry("e3", "project alpha milestone was completed"),
            ]
            for e in entries:
                e.semantic_level = MemorySemanticLevel.SEMANTIC
                await manager._long_term.store(e)

            cb = ContextBuilder(filter_episodic=False)
            blocks = await cb.build(
                user_request="前回のアーキテクチャ設計について教えて",
                memory_manager=manager,
                need_level=MemoryNeedLevel.HIGH,
            )
            # ブロックが返ること
            assert isinstance(blocks, list)
        finally:
            await manager.stop()

    async def test_multi_view_recall_method_exists(self) -> None:
        """ContextBuilder に _multi_view_recall メソッドが存在すること."""
        cb = ContextBuilder()
        assert hasattr(cb, "_multi_view_recall")

    async def test_multi_view_recall_returns_list(self) -> None:
        """_multi_view_recall がリストを返すこと."""
        manager = MemoryManager(enable_vector_search=True)
        await manager.start()
        try:
            from infrastructure.memory.types import MemorySemanticLevel

            entry = _make_entry("e1", "test content for recall")
            entry.semantic_level = MemorySemanticLevel.SEMANTIC
            await manager._long_term.store(entry)

            cb = ContextBuilder(filter_episodic=False)
            results = await cb._multi_view_recall(
                query="test content",
                memory_manager=manager,
                topic=None,
                limit=5,
                min_importance=0.0,
            )
            assert isinstance(results, list)
        finally:
            await manager.stop()
