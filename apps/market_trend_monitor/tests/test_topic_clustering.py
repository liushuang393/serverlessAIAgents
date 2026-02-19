"""トピッククラスタリングユニットテスト.

TopicClusteringService のテスト。
Mock Embedding を使用（外部依存なし）。
"""

from __future__ import annotations

from datetime import datetime
from unittest.mock import AsyncMock, MagicMock

import pytest
from apps.market_trend_monitor.backend.models import Article, SourceType
from apps.market_trend_monitor.backend.services.topic_clustering_service import (
    TopicCluster,
    TopicClusteringService,
)


# ============================================================
# Helpers
# ============================================================


def _make_article(
    article_id: str = "a-1",
    title: str = "COBOL Migration",
    content: str = "Legacy modernization",
    keywords: list[str] | None = None,
) -> Article:
    """テスト用 Article を生成."""
    return Article(
        id=article_id,
        title=title,
        url=f"https://example.com/{article_id}",
        source=SourceType.NEWS,
        published_at=datetime(2026, 1, 15),
        content=content,
        keywords=keywords or ["COBOL", "Java"],
    )


def _make_mock_embedding():
    """テスト用 Mock Embedding を生成."""
    mock = AsyncMock()
    mock.embed_text = AsyncMock(return_value=[0.5, 0.5, 0.0, 0.0])
    mock.embed_batch = AsyncMock(
        side_effect=lambda texts: [
            [1.0, 0.0, 0.0, 0.0]
            if i == 0
            else [0.95, 0.05, 0.0, 0.0]
            if i == 1
            else [0.0, 1.0, 0.0, 0.0]
            for i in range(len(texts))
        ]
    )
    mock.get_dimension = MagicMock(return_value=4)
    return mock


# ============================================================
# TopicCluster Model Tests
# ============================================================


class TestTopicClusterModel:
    """TopicCluster データモデルのテスト."""

    def test_cluster_creation(self) -> None:
        """クラスタ生成テスト."""
        cluster = TopicCluster(
            id="c-1",
            label="COBOL Migration",
            keywords=["COBOL", "Java"],
            article_ids=["a-1", "a-2"],
            centroid_embedding=[0.5, 0.5, 0.0, 0.0],
            coherence_score=0.85,
        )
        assert cluster.id == "c-1"
        assert len(cluster.article_ids) == 2

    def test_cluster_to_dict(self) -> None:
        """to_dict 変換テスト."""
        cluster = TopicCluster(
            id="c-1",
            label="Test",
            keywords=["test"],
            article_ids=["a-1"],
            centroid_embedding=[0.5, 0.5],
        )
        d = cluster.to_dict()
        assert d["id"] == "c-1"
        assert d["article_count"] == 1
        assert "centroid_embedding" not in d  # 大きなベクトルは含めない

    def test_cluster_default_metadata(self) -> None:
        """デフォルトメタデータテスト."""
        cluster = TopicCluster(
            id="c-1",
            label="Test",
            keywords=[],
            article_ids=[],
            centroid_embedding=[],
        )
        assert cluster.metadata == {}


# ============================================================
# Service Tests
# ============================================================


class TestTopicClusteringService:
    """TopicClusteringService のテスト."""

    async def test_cluster_articles_basic(self) -> None:
        """基本クラスタリングテスト."""
        emb = _make_mock_embedding()
        service = TopicClusteringService(embedding=emb, similarity_threshold=0.8)

        articles = [
            _make_article("a-1", "COBOL Migration", "Legacy to modern"),
            _make_article("a-2", "COBOL Modernization", "Mainframe update"),
            _make_article("a-3", "Spring Boot Tutorial", "Java web framework"),
        ]

        clusters = await service.cluster_articles(articles)
        # a-1とa-2は類似 (0.95+) → 1クラスタ, a-3は別 → 計2クラスタ
        assert len(clusters) == 2

    async def test_cluster_articles_empty(self) -> None:
        """空リストのクラスタリングテスト."""
        service = TopicClusteringService(embedding=_make_mock_embedding())
        clusters = await service.cluster_articles([])
        assert clusters == []

    async def test_cluster_articles_single(self) -> None:
        """1記事のクラスタリングテスト."""
        emb = AsyncMock()
        emb.embed_batch = AsyncMock(return_value=[[1.0, 0.0, 0.0, 0.0]])
        service = TopicClusteringService(embedding=emb)

        articles = [_make_article()]
        clusters = await service.cluster_articles(articles)
        assert len(clusters) == 1
        assert len(clusters[0].article_ids) == 1

    async def test_cluster_articles_all_similar(self) -> None:
        """全記事類似のクラスタリングテスト."""
        emb = AsyncMock()
        emb.embed_batch = AsyncMock(
            side_effect=lambda texts: [[1.0, 0.0, 0.0, 0.0]] * len(texts),
        )
        service = TopicClusteringService(embedding=emb, similarity_threshold=0.9)

        articles = [_make_article(f"a-{i}") for i in range(5)]
        clusters = await service.cluster_articles(articles)
        assert len(clusters) == 1
        assert len(clusters[0].article_ids) == 5

    async def test_cluster_articles_all_different(self) -> None:
        """全記事異なるクラスタリングテスト."""
        emb = AsyncMock()
        # 直交するベクトルを生成
        emb.embed_batch = AsyncMock(
            side_effect=lambda texts: [
                [1.0 if j == i else 0.0 for j in range(4)] for i in range(len(texts))
            ],
        )
        service = TopicClusteringService(embedding=emb, similarity_threshold=0.5)

        articles = [_make_article(f"a-{i}") for i in range(3)]
        clusters = await service.cluster_articles(articles)
        assert len(clusters) == 3

    async def test_cluster_sorted_by_size(self) -> None:
        """クラスタがサイズ降順にソートされるテスト."""
        emb = _make_mock_embedding()
        service = TopicClusteringService(embedding=emb, similarity_threshold=0.8)

        articles = [
            _make_article("a-1"),
            _make_article("a-2"),
            _make_article("a-3"),
        ]
        clusters = await service.cluster_articles(articles)
        for i in range(len(clusters) - 1):
            assert len(clusters[i].article_ids) >= len(clusters[i + 1].article_ids)

    async def test_assign_to_cluster(self) -> None:
        """既存クラスタへの割り当てテスト."""
        emb = _make_mock_embedding()
        service = TopicClusteringService(embedding=emb, similarity_threshold=0.8)

        # クラスタを作成
        articles = [_make_article("a-1"), _make_article("a-2"), _make_article("a-3")]
        await service.cluster_articles(articles)

        # 新しい記事を割り当て
        emb.embed_text = AsyncMock(return_value=[1.0, 0.0, 0.0, 0.0])
        new_article = _make_article("a-4", "New COBOL article")
        cluster = await service.assign_to_cluster(new_article)

        # 類似クラスタが見つかるはず
        assert cluster is not None

    async def test_assign_to_cluster_no_match(self) -> None:
        """クラスタマッチなしテスト."""
        emb = AsyncMock()
        emb.embed_batch = AsyncMock(return_value=[[1.0, 0.0, 0.0, 0.0]])
        emb.embed_text = AsyncMock(return_value=[0.0, 0.0, 0.0, 1.0])
        service = TopicClusteringService(embedding=emb, similarity_threshold=0.9)

        await service.cluster_articles([_make_article()])

        new_article = _make_article("a-99", "Completely different")
        cluster = await service.assign_to_cluster(new_article)
        assert cluster is None

    async def test_assign_to_cluster_empty(self) -> None:
        """クラスタ空時の割り当てテスト."""
        service = TopicClusteringService(embedding=_make_mock_embedding())
        article = _make_article()
        cluster = await service.assign_to_cluster(article)
        assert cluster is None

    async def test_get_trending_topics(self) -> None:
        """トレンドトピック取得テスト."""
        emb = _make_mock_embedding()
        service = TopicClusteringService(embedding=emb, similarity_threshold=0.8)

        articles = [_make_article("a-1"), _make_article("a-2"), _make_article("a-3")]
        await service.cluster_articles(articles)

        trending = await service.get_trending_topics(window_days=30, min_articles=1)
        assert len(trending) > 0

    async def test_get_trending_topics_min_articles(self) -> None:
        """最小記事数フィルタテスト."""
        emb = AsyncMock()
        emb.embed_batch = AsyncMock(
            side_effect=lambda texts: [
                [1.0 if j == i else 0.0 for j in range(4)] for i in range(len(texts))
            ],
        )
        service = TopicClusteringService(embedding=emb, similarity_threshold=0.5)
        articles = [_make_article(f"a-{i}") for i in range(3)]
        await service.cluster_articles(articles)

        # 各クラスタは1記事 → min_articles=2 で0件
        trending = await service.get_trending_topics(min_articles=2)
        assert len(trending) == 0

    def test_get_cluster(self) -> None:
        """クラスタ取得テスト."""
        service = TopicClusteringService(embedding=_make_mock_embedding())
        cluster = TopicCluster(
            id="c-1",
            label="Test",
            keywords=[],
            article_ids=[],
            centroid_embedding=[],
        )
        service._clusters["c-1"] = cluster
        assert service.get_cluster("c-1") is cluster
        assert service.get_cluster("nonexistent") is None

    def test_list_clusters(self) -> None:
        """クラスタ一覧テスト."""
        service = TopicClusteringService(embedding=_make_mock_embedding())
        service._clusters["c-1"] = TopicCluster(
            id="c-1",
            label="A",
            keywords=[],
            article_ids=["a-1"],
            centroid_embedding=[],
        )
        service._clusters["c-2"] = TopicCluster(
            id="c-2",
            label="B",
            keywords=[],
            article_ids=["a-2", "a-3"],
            centroid_embedding=[],
        )
        clusters = service.list_clusters()
        assert len(clusters) == 2
        # サイズ降順
        assert len(clusters[0].article_ids) >= len(clusters[1].article_ids)

    def test_cosine_similarity(self) -> None:
        """コサイン類似度テスト."""
        assert TopicClusteringService._cosine_similarity(
            [1.0, 0.0],
            [1.0, 0.0],
        ) == pytest.approx(1.0)
        assert TopicClusteringService._cosine_similarity(
            [1.0, 0.0],
            [0.0, 1.0],
        ) == pytest.approx(0.0)

    def test_compute_centroid(self) -> None:
        """重心計算テスト."""
        centroid = TopicClusteringService._compute_centroid(
            [
                [1.0, 0.0],
                [0.0, 1.0],
            ]
        )
        assert centroid == pytest.approx([0.5, 0.5])

    def test_compute_centroid_empty(self) -> None:
        """空リストの重心計算テスト."""
        centroid = TopicClusteringService._compute_centroid([])
        assert centroid == []

    def test_compute_coherence(self) -> None:
        """凝集度計算テスト."""
        embeddings = [[1.0, 0.0], [1.0, 0.0]]
        centroid = [1.0, 0.0]
        coherence = TopicClusteringService._compute_coherence(embeddings, centroid)
        assert coherence == pytest.approx(1.0)

    def test_extract_cluster_keywords(self) -> None:
        """クラスタキーワード抽出テスト."""
        articles = [
            _make_article("a-1", keywords=["COBOL", "Java"]),
            _make_article("a-2", keywords=["COBOL", "migration"]),
        ]
        keywords = TopicClusteringService._extract_cluster_keywords(articles, max_keywords=2)
        assert keywords[0] == "COBOL"  # 最頻出
        assert len(keywords) == 2
