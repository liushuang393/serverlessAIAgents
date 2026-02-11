"""セマンティック検索ユニットテスト.

SemanticSearchService / QueryExpansionService のテスト。
Mock embedding/vectordb を使用（外部依存なし）。
"""

from __future__ import annotations

from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from apps.market_trend_monitor.backend.models import Article, SourceType
from apps.market_trend_monitor.backend.services.query_expansion_service import (
    QueryExpansionService,
)
from apps.market_trend_monitor.backend.services.semantic_search_service import (
    SemanticSearchService,
)


# ============================================================
# Helpers
# ============================================================


def _make_article(
    article_id: str = "a-1",
    title: str = "COBOL to Java Migration",
    content: str = "Legacy modernization using AI tools",
    url: str = "https://example.com/article-1",
    source: SourceType = SourceType.NEWS,
) -> Article:
    """テスト用 Article を生成."""
    return Article(
        id=article_id,
        title=title,
        url=url,
        source=source,
        published_at=datetime(2026, 1, 15),
        content=content,
        keywords=["COBOL", "Java"],
    )


def _make_mock_embedding(dimension: int = 4):
    """テスト用 Mock Embedding を生成."""
    mock = AsyncMock()
    mock.embed_text = AsyncMock(return_value=[0.1, 0.2, 0.3, 0.4])
    mock.embed_batch = AsyncMock(
        side_effect=lambda texts: [[0.1 * (i + 1), 0.2, 0.3, 0.4] for i in range(len(texts))]
    )
    mock.get_dimension = MagicMock(return_value=dimension)
    return mock


def _make_mock_vectordb():
    """テスト用 Mock VectorDB を生成."""
    mock = AsyncMock()
    mock.connect = AsyncMock()
    mock.add = AsyncMock()
    mock.search = AsyncMock(return_value=[])
    mock.delete = AsyncMock(return_value=0)
    mock.clear = AsyncMock()
    return mock


# ============================================================
# SemanticSearchService Tests
# ============================================================


class TestSemanticSearchService:
    """SemanticSearchService のテスト."""

    async def test_index_article(self) -> None:
        """記事のインデックス登録テスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        article = _make_article()
        doc_id = await service.index_article(article)

        assert doc_id == "a-1"
        emb.embed_text.assert_called_once()
        vdb.add.assert_called_once()

    async def test_index_articles_batch(self) -> None:
        """一括インデックス登録テスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        articles = [_make_article(article_id=f"a-{i}") for i in range(3)]
        ids = await service.index_articles_batch(articles)

        assert len(ids) == 3
        emb.embed_batch.assert_called_once()
        vdb.add.assert_called_once()

    async def test_index_articles_batch_empty(self) -> None:
        """空リストの一括インデックス登録テスト."""
        service = SemanticSearchService(
            embedding=_make_mock_embedding(), vectordb=_make_mock_vectordb(),
        )
        service._initialized = True
        ids = await service.index_articles_batch([])
        assert ids == []

    async def test_semantic_search(self) -> None:
        """セマンティック検索テスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        vdb.search = AsyncMock(return_value=[
            {"id": "a-1", "score": 0.95, "document": "test"},
            {"id": "a-2", "score": 0.85, "document": "test2"},
        ])
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        results = await service.semantic_search("COBOL migration", top_k=5)
        assert len(results) == 2
        emb.embed_text.assert_called_once()
        vdb.search.assert_called_once()

    async def test_semantic_search_with_filter(self) -> None:
        """メタデータフィルタ付き検索テスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        vdb.search = AsyncMock(return_value=[{"id": "a-1", "score": 0.9}])
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        results = await service.semantic_search(
            "test", filter_metadata={"source": "arxiv"},
        )
        assert len(results) == 1
        call_kwargs = vdb.search.call_args[1]
        assert call_kwargs["filter_metadata"] == {"source": "arxiv"}

    async def test_find_similar(self) -> None:
        """類似記事検索テスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        vdb.search = AsyncMock(return_value=[
            {"id": "a-1", "score": 1.0},  # 自分自身
            {"id": "a-2", "score": 0.95},
            {"id": "a-3", "score": 0.80},
        ])
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        article = _make_article(article_id="a-1")
        similar = await service.find_similar(article, threshold=0.92)

        # a-1 (自分自身) は除外、a-2 (0.95 >= 0.92) のみ
        assert len(similar) == 1
        assert similar[0]["id"] == "a-2"

    async def test_find_similar_no_results(self) -> None:
        """類似記事なしテスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        vdb.search = AsyncMock(return_value=[
            {"id": "a-1", "score": 1.0},  # 自分自身のみ
        ])
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        article = _make_article(article_id="a-1")
        similar = await service.find_similar(article)
        assert similar == []

    async def test_deduplicate_batch(self) -> None:
        """バッチ重複排除テスト."""
        emb = AsyncMock()
        emb.embed_batch = AsyncMock(return_value=[
            [1.0, 0.0, 0.0, 0.0],  # a-1
            [0.99, 0.01, 0.0, 0.0],  # a-2: a-1と非常に類似
            [0.0, 1.0, 0.0, 0.0],  # a-3: a-1とは異なる
        ])
        vdb = _make_mock_vectordb()
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        articles = [
            _make_article(article_id="a-1", title="COBOL migration"),
            _make_article(article_id="a-2", title="COBOL migration update"),
            _make_article(article_id="a-3", title="Java Spring Boot"),
        ]

        unique = await service.deduplicate_batch(articles, threshold=0.95)
        # a-2は a-1に類似 → 除外。a-1, a-3が残る
        assert len(unique) == 2
        assert unique[0].id == "a-1"
        assert unique[1].id == "a-3"

    async def test_deduplicate_batch_empty(self) -> None:
        """空リスト重複排除テスト."""
        service = SemanticSearchService(
            embedding=_make_mock_embedding(), vectordb=_make_mock_vectordb(),
        )
        service._initialized = True
        unique = await service.deduplicate_batch([])
        assert unique == []

    async def test_deduplicate_batch_single(self) -> None:
        """1件の重複排除テスト."""
        emb = AsyncMock()
        emb.embed_batch = AsyncMock(return_value=[[1.0, 0.0, 0.0, 0.0]])
        vdb = _make_mock_vectordb()
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        articles = [_make_article()]
        unique = await service.deduplicate_batch(articles)
        assert len(unique) == 1

    async def test_mmr_search(self) -> None:
        """MMR検索テスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        vdb.search = AsyncMock(return_value=[
            {"id": "a-1", "score": 0.95, "embedding": [1.0, 0.0, 0.0, 0.0]},
            {"id": "a-2", "score": 0.90, "embedding": [0.9, 0.1, 0.0, 0.0]},
            {"id": "a-3", "score": 0.80, "embedding": [0.0, 1.0, 0.0, 0.0]},
        ])
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        results = await service.mmr_search("test query", top_k=2)
        assert len(results) == 2

    async def test_mmr_search_empty(self) -> None:
        """MMR検索: 候補なしテスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        vdb.search = AsyncMock(return_value=[])
        service = SemanticSearchService(embedding=emb, vectordb=vdb)
        service._initialized = True

        results = await service.mmr_search("test query")
        assert results == []

    def test_cosine_similarity_identical(self) -> None:
        """コサイン類似度: 同一ベクトル."""
        result = SemanticSearchService._cosine_similarity(
            [1.0, 0.0, 0.0], [1.0, 0.0, 0.0],
        )
        assert result == pytest.approx(1.0)

    def test_cosine_similarity_orthogonal(self) -> None:
        """コサイン類似度: 直交ベクトル."""
        result = SemanticSearchService._cosine_similarity(
            [1.0, 0.0, 0.0], [0.0, 1.0, 0.0],
        )
        assert result == pytest.approx(0.0)

    def test_cosine_similarity_empty(self) -> None:
        """コサイン類似度: 空ベクトル."""
        assert SemanticSearchService._cosine_similarity([], []) == 0.0

    def test_cosine_similarity_different_length(self) -> None:
        """コサイン類似度: 異なる長さ."""
        assert SemanticSearchService._cosine_similarity([1.0], [1.0, 0.0]) == 0.0

    def test_cosine_similarity_zero_vector(self) -> None:
        """コサイン類似度: ゼロベクトル."""
        assert SemanticSearchService._cosine_similarity(
            [0.0, 0.0], [1.0, 0.0],
        ) == 0.0

    async def test_ensure_initialized_lazy(self) -> None:
        """遅延初期化テスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        service = SemanticSearchService(embedding=emb, vectordb=vdb)

        assert not service._initialized
        await service._ensure_initialized()
        assert service._initialized
        vdb.connect.assert_called_once()

    async def test_ensure_initialized_idempotent(self) -> None:
        """初期化の冪等性テスト."""
        emb = _make_mock_embedding()
        vdb = _make_mock_vectordb()
        service = SemanticSearchService(embedding=emb, vectordb=vdb)

        await service._ensure_initialized()
        await service._ensure_initialized()
        # connect は1回だけ呼ばれる
        vdb.connect.assert_called_once()


# ============================================================
# QueryExpansionService Tests
# ============================================================


class TestQueryExpansionService:
    """QueryExpansionService のテスト."""

    def test_expand_with_synonyms_cobol(self) -> None:
        """COBOL同義語拡張テスト."""
        service = QueryExpansionService()
        expanded = service.expand_with_synonyms("COBOL migration")

        assert "COBOL migration" in expanded
        assert len(expanded) > 1
        # 同義語が含まれている
        assert any("mainframe" in q for q in expanded)

    def test_expand_with_synonyms_java(self) -> None:
        """Java同義語拡張テスト."""
        service = QueryExpansionService()
        expanded = service.expand_with_synonyms("Java modernization")

        assert "Java modernization" in expanded
        assert len(expanded) > 1

    def test_expand_with_synonyms_no_match(self) -> None:
        """マッチなし同義語拡張テスト."""
        service = QueryExpansionService()
        expanded = service.expand_with_synonyms("quantum computing")

        assert expanded == ["quantum computing"]

    def test_expand_with_synonyms_multiple_terms(self) -> None:
        """複数ドメイン語を含むクエリの拡張テスト."""
        service = QueryExpansionService()
        expanded = service.expand_with_synonyms("AI legacy modernization")
        assert len(expanded) > 1

    def test_expand_with_synonyms_case_insensitive(self) -> None:
        """大小文字非依存の同義語拡張テスト."""
        service = QueryExpansionService()
        expanded = service.expand_with_synonyms("cobol to java")
        assert len(expanded) > 1

    def test_domain_synonyms_structure(self) -> None:
        """ドメイン同義語マッピングの構造テスト."""
        synonyms = QueryExpansionService.DOMAIN_SYNONYMS
        assert "COBOL" in synonyms
        assert "Java" in synonyms
        assert "migration" in synonyms
        assert "AI" in synonyms
        assert "LLM" in synonyms
        for key, values in synonyms.items():
            assert isinstance(values, list)
            assert len(values) > 0

    async def test_expand_query_with_llm(self) -> None:
        """LLMベースクエリ拡張テスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(return_value="COBOL modernization\nLegacy migration tools")
        service = QueryExpansionService(llm=mock_llm)

        expanded = await service.expand_query("COBOL migration", max_expansions=3)
        assert "COBOL migration" in expanded
        assert len(expanded) > 1

    async def test_expand_query_llm_failure(self) -> None:
        """LLM失敗時のフォールバックテスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(side_effect=Exception("LLM error"))
        service = QueryExpansionService(llm=mock_llm)

        expanded = await service.expand_query("COBOL migration")
        # 同義語拡張のみが返される
        assert "COBOL migration" in expanded
        assert len(expanded) >= 1

    async def test_generate_multilingual_english_input(self) -> None:
        """英語入力の多言語クエリ生成テスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(return_value="COBOL移行")
        service = QueryExpansionService(llm=mock_llm)

        result = await service.generate_multilingual_queries("COBOL migration")
        assert "en" in result
        assert "ja" in result
        assert "COBOL migration" in result["en"]
        assert len(result["ja"]) > 0

    async def test_generate_multilingual_japanese_input(self) -> None:
        """日本語入力の多言語クエリ生成テスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(return_value="COBOL migration")
        service = QueryExpansionService(llm=mock_llm)

        result = await service.generate_multilingual_queries("COBOL移行")
        assert "en" in result
        assert "ja" in result
        assert "COBOL移行" in result["ja"]

    async def test_generate_multilingual_llm_failure(self) -> None:
        """多言語生成: LLM失敗テスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(side_effect=Exception("LLM error"))
        service = QueryExpansionService(llm=mock_llm)

        result = await service.generate_multilingual_queries("COBOL migration")
        assert "en" in result
        assert "COBOL migration" in result["en"]

    def test_expand_with_synonyms_deduplication(self) -> None:
        """拡張結果の重複排除テスト."""
        service = QueryExpansionService()
        expanded = service.expand_with_synonyms("COBOL")
        assert len(expanded) == len(set(expanded))
