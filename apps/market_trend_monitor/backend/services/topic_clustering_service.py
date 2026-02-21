"""トピッククラスタリングサービス.

Embeddingベースのトピッククラスタリングを提供します。
記事群をトピックごとにグルーピングし、トレンドを追跡します。
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import TYPE_CHECKING, Any

from agentflow import get_embedding


if TYPE_CHECKING:
    from apps.market_trend_monitor.backend.models import Article



@dataclass
class TopicCluster:
    """トピッククラスタデータモデル."""

    id: str
    label: str
    keywords: list[str]
    article_ids: list[str]
    centroid_embedding: list[float]
    coherence_score: float = 0.0
    created_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "label": self.label,
            "keywords": self.keywords,
            "article_ids": self.article_ids,
            "coherence_score": self.coherence_score,
            "article_count": len(self.article_ids),
            "created_at": self.created_at.isoformat(),
            "metadata": self.metadata,
        }


class TopicClusteringService:
    """トピッククラスタリングサービス.

    - Embeddingベースの記事クラスタリング
    - 距離ベースの階層的クラスタリング
    - トピックトレンドの時系列追跡
    """

    def __init__(
        self,
        *,
        embedding: Any | None = None,
        similarity_threshold: float = 0.75,
    ) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._embedding = embedding
        self._similarity_threshold = similarity_threshold
        self._clusters: dict[str, TopicCluster] = {}

    def _get_embedding(self) -> Any:
        """Embeddingインスタンスを取得."""
        if self._embedding is None:
            self._embedding = get_embedding()
        return self._embedding

    async def cluster_articles(
        self,
        articles: list[Article],
        threshold: float | None = None,
    ) -> list[TopicCluster]:
        """記事群をクラスタリング.

        Args:
            articles: クラスタリング対象記事リスト
            threshold: 類似度閾値

        Returns:
            トピッククラスタリスト
        """
        if not articles:
            return []

        threshold = threshold if threshold is not None else self._similarity_threshold
        emb = self._get_embedding()

        texts = [f"{a.title}\n{a.content}" for a in articles]
        embeddings = await emb.embed_batch(texts)

        clusters: list[TopicCluster] = []
        assigned = [False] * len(articles)

        for i, (article, embedding) in enumerate(zip(articles, embeddings, strict=False)):
            if assigned[i]:
                continue

            cluster_articles = [article]
            cluster_embeddings = [embedding]
            assigned[i] = True

            for j in range(i + 1, len(articles)):
                if assigned[j]:
                    continue
                similarity = self._cosine_similarity(embedding, embeddings[j])
                if similarity >= threshold:
                    cluster_articles.append(articles[j])
                    cluster_embeddings.append(embeddings[j])
                    assigned[j] = True

            centroid = self._compute_centroid(cluster_embeddings)
            coherence = self._compute_coherence(cluster_embeddings, centroid)
            keywords = self._extract_cluster_keywords(cluster_articles)
            label = keywords[0] if keywords else cluster_articles[0].title[:50]

            cluster = TopicCluster(
                id=str(uuid.uuid4()),
                label=label,
                keywords=keywords,
                article_ids=[a.id for a in cluster_articles],
                centroid_embedding=centroid,
                coherence_score=coherence,
            )
            clusters.append(cluster)
            self._clusters[cluster.id] = cluster

        self._logger.info(
            "クラスタリング完了: %d記事 → %d クラスタ",
            len(articles),
            len(clusters),
        )
        return sorted(clusters, key=lambda c: len(c.article_ids), reverse=True)

    async def assign_to_cluster(
        self,
        article: Article,
        threshold: float | None = None,
    ) -> TopicCluster | None:
        """記事を既存クラスタに割り当て.

        Args:
            article: 割り当て対象記事
            threshold: 類似度閾値

        Returns:
            割り当て先クラスタ（マッチなしの場合None）
        """
        if not self._clusters:
            return None

        threshold = threshold if threshold is not None else self._similarity_threshold
        emb = self._get_embedding()
        text = f"{article.title}\n{article.content}"
        article_embedding = await emb.embed_text(text)

        best_cluster = None
        best_similarity = 0.0

        for cluster in self._clusters.values():
            similarity = self._cosine_similarity(
                article_embedding,
                cluster.centroid_embedding,
            )
            if similarity >= threshold and similarity > best_similarity:
                best_similarity = similarity
                best_cluster = cluster

        if best_cluster and article.id not in best_cluster.article_ids:
            best_cluster.article_ids.append(article.id)
            self._logger.debug(
                "記事 %s をクラスタ %s に割り当て (similarity=%.3f)",
                article.id,
                best_cluster.id,
                best_similarity,
            )

        return best_cluster

    async def get_trending_topics(
        self,
        window_days: int = 7,
        min_articles: int = 2,
    ) -> list[TopicCluster]:
        """トレンドトピックを取得.

        Args:
            window_days: 対象ウィンドウ日数
            min_articles: 最小記事数

        Returns:
            トレンドトピックリスト（記事数降順）
        """
        cutoff = datetime.now() - timedelta(days=window_days)
        trending = [
            cluster
            for cluster in self._clusters.values()
            if cluster.created_at >= cutoff and len(cluster.article_ids) >= min_articles
        ]

        return sorted(trending, key=lambda c: len(c.article_ids), reverse=True)

    def get_cluster(self, cluster_id: str) -> TopicCluster | None:
        """クラスタを取得."""
        return self._clusters.get(cluster_id)

    def list_clusters(self) -> list[TopicCluster]:
        """全クラスタを取得."""
        return sorted(
            self._clusters.values(),
            key=lambda c: len(c.article_ids),
            reverse=True,
        )

    @staticmethod
    def _cosine_similarity(a: list[float], b: list[float]) -> float:
        """コサイン類似度を計算."""
        if not a or not b or len(a) != len(b):
            return 0.0
        dot = sum(x * y for x, y in zip(a, b, strict=False))
        norm_a = sum(x * x for x in a) ** 0.5
        norm_b = sum(x * x for x in b) ** 0.5
        if norm_a == 0 or norm_b == 0:
            return 0.0
        return dot / (norm_a * norm_b)

    @staticmethod
    def _compute_centroid(embeddings: list[list[float]]) -> list[float]:
        """重心ベクトルを計算."""
        if not embeddings:
            return []
        dim = len(embeddings[0])
        centroid = [0.0] * dim
        for emb in embeddings:
            for i in range(dim):
                centroid[i] += emb[i]
        n = len(embeddings)
        return [c / n for c in centroid]

    @staticmethod
    def _compute_coherence(
        embeddings: list[list[float]],
        centroid: list[float],
    ) -> float:
        """クラスタ凝集度を計算."""
        if not embeddings or not centroid:
            return 0.0
        similarities = []
        for emb in embeddings:
            dot = sum(x * y for x, y in zip(emb, centroid, strict=False))
            norm_a = sum(x * x for x in emb) ** 0.5
            norm_b = sum(x * x for x in centroid) ** 0.5
            if norm_a > 0 and norm_b > 0:
                similarities.append(dot / (norm_a * norm_b))
        return sum(similarities) / len(similarities) if similarities else 0.0

    @staticmethod
    def _extract_cluster_keywords(
        articles: list[Article],
        max_keywords: int = 5,
    ) -> list[str]:
        """クラスタ内記事からキーワードを抽出."""
        counts: dict[str, int] = {}
        for article in articles:
            for kw in article.keywords:
                counts[kw] = counts.get(kw, 0) + 1
        sorted_kw = sorted(counts.items(), key=lambda x: x[1], reverse=True)
        return [kw for kw, _ in sorted_kw[:max_keywords]]
