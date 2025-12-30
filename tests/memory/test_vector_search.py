"""ベクトル検索のテスト."""

import pytest

from agentflow.memory.types import MemoryEntry, MemoryType
from agentflow.memory.vector_search import VectorSearch


class TestVectorSearch:
    """VectorSearchクラスのテスト."""

    @pytest.fixture
    def vector_search(self) -> VectorSearch:
        """VectorSearchインスタンスを作成."""
        return VectorSearch(embedding_dim=384)

    @pytest.mark.asyncio
    async def test_embed_text(self, vector_search: VectorSearch) -> None:
        """テキスト埋め込みのテスト."""
        text = "AgentFlowは軽量なAIエージェント開発フレームワークです"
        embedding = await vector_search.embed_text(text)

        assert isinstance(embedding, list)
        assert len(embedding) == 384
        assert all(isinstance(x, float) for x in embedding)

        # L2正規化されているか確認
        norm = sum(x * x for x in embedding) ** 0.5
        assert abs(norm - 1.0) < 0.01

    @pytest.mark.asyncio
    async def test_embed_text_cache(self, vector_search: VectorSearch) -> None:
        """埋め込みキャッシュのテスト."""
        text = "テストテキスト"

        # 初回埋め込み
        embedding1 = await vector_search.embed_text(text)

        # キャッシュから取得
        embedding2 = await vector_search.embed_text(text)

        assert embedding1 == embedding2
        assert text in vector_search._embedding_cache

    @pytest.mark.asyncio
    async def test_cosine_similarity(self, vector_search: VectorSearch) -> None:
        """コサイン類似度のテスト."""
        vec1 = [1.0, 0.0, 0.0]
        vec2 = [1.0, 0.0, 0.0]
        vec3 = [0.0, 1.0, 0.0]

        # 同じベクトル
        similarity1 = await vector_search.cosine_similarity(vec1, vec2)
        assert abs(similarity1 - 1.0) < 0.01

        # 直交ベクトル
        similarity2 = await vector_search.cosine_similarity(vec1, vec3)
        assert abs(similarity2 - 0.0) < 0.01

    @pytest.mark.asyncio
    async def test_cosine_similarity_error(self, vector_search: VectorSearch) -> None:
        """異なる次元のベクトルでエラーが発生することを確認."""
        vec1 = [1.0, 0.0]
        vec2 = [1.0, 0.0, 0.0]

        with pytest.raises(ValueError, match="same dimension"):
            await vector_search.cosine_similarity(vec1, vec2)

    @pytest.mark.asyncio
    async def test_search_similar(self, vector_search: VectorSearch) -> None:
        """意味的検索のテスト."""
        from datetime import datetime

        # テスト用の記憶エントリを作成
        memories = [
            MemoryEntry(
                id="1",
                content="AgentFlowはAIエージェント開発フレームワークです",
                topic="AgentFlow",
                timestamp=datetime.now(),
                memory_type=MemoryType.LONG_TERM,
            ),
            MemoryEntry(
                id="2",
                content="Pythonは人気のあるプログラミング言語です",
                topic="Python",
                timestamp=datetime.now(),
                memory_type=MemoryType.LONG_TERM,
            ),
            MemoryEntry(
                id="3",
                content="AIエージェントは自律的にタスクを実行します",
                topic="AI",
                timestamp=datetime.now(),
                memory_type=MemoryType.LONG_TERM,
            ),
        ]

        # "AIエージェント"に関連する記憶を検索
        query = "AIエージェント開発"
        results = await vector_search.search_similar(query, memories, top_k=2)

        assert len(results) == 2
        assert all(isinstance(r, tuple) for r in results)
        assert all(isinstance(r[0], MemoryEntry) for r in results)
        assert all(isinstance(r[1], float) for r in results)

        # 類似度が降順にソートされているか確認
        assert results[0][1] >= results[1][1]

        # 最も類似度が高いのはAgentFlowまたはAIに関する記憶
        assert results[0][0].id in ["1", "3"]

    @pytest.mark.asyncio
    async def test_search_similar_with_threshold(self, vector_search: VectorSearch) -> None:
        """最小類似度閾値のテスト."""
        from datetime import datetime

        memories = [
            MemoryEntry(
                id="1",
                content="AgentFlow",
                topic="test",
                timestamp=datetime.now(),
                memory_type=MemoryType.LONG_TERM,
            ),
            MemoryEntry(
                id="2",
                content="完全に異なる内容",
                topic="test",
                timestamp=datetime.now(),
                memory_type=MemoryType.LONG_TERM,
            ),
        ]

        # 高い閾値で検索
        results = await vector_search.search_similar("AgentFlow", memories, top_k=10, min_similarity=0.5)

        # 類似度が低い記憶は除外される
        assert len(results) <= len(memories)
        assert all(similarity >= 0.5 for _, similarity in results)

    def test_clear_cache(self, vector_search: VectorSearch) -> None:
        """キャッシュクリアのテスト."""
        vector_search._embedding_cache["test"] = [1.0, 2.0, 3.0]
        assert len(vector_search._embedding_cache) > 0

        vector_search.clear_cache()
        assert len(vector_search._embedding_cache) == 0

