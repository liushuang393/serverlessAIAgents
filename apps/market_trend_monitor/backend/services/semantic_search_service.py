"""セマンティック検索サービス.

ベクトル類似度検索による意味的な記事検索と重複排除を提供します。
AgentFlowの get_embedding() / get_vectordb() を使用します。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from apps.market_trend_monitor.backend.config import config

from agentflow import get_embedding, get_vectordb


if TYPE_CHECKING:
    from apps.market_trend_monitor.backend.models import Article


class SemanticSearchService:
    """セマンティック検索サービス.

    - 記事のベクトル化・保存・類似検索
    - コサイン類似度による意味的重複排除
    - MMR検索でダイバーシティ確保
    """

    def __init__(
        self,
        *,
        embedding: Any | None = None,
        vectordb: Any | None = None,
    ) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._embedding = embedding
        self._vectordb = vectordb
        self._similarity_threshold = config.vectordb.similarity_threshold
        self._mmr_lambda = config.vectordb.mmr_lambda
        self._initialized = False

    async def _ensure_initialized(self) -> None:
        """遅延初期化."""
        if self._initialized:
            return
        if self._embedding is None:
            self._embedding = get_embedding()
        if self._vectordb is None:
            collection = config.vectordb.collection_name
            self._vectordb = get_vectordb(collection=collection)
        await self._vectordb.connect()
        self._initialized = True

    async def index_article(self, article: Article) -> str:
        """記事をベクトルインデックスに登録.

        Args:
            article: インデックス対象の記事

        Returns:
            登録されたドキュメントID
        """
        await self._ensure_initialized()
        text = f"{article.title}\n{article.content}"
        embedding = await self._embedding.embed_text(text)
        doc_id = article.id

        await self._vectordb.add(
            documents=[text],
            ids=[doc_id],
            embeddings=[embedding],
            metadatas=[
                {
                    "title": article.title,
                    "url": article.url,
                    "source": article.source.value,
                    "published_at": article.published_at.isoformat(),
                }
            ],
        )

        self._logger.debug("記事をインデックスに登録: %s", doc_id)
        return doc_id

    async def index_articles_batch(self, articles: list[Article]) -> list[str]:
        """複数記事を一括インデックス登録.

        Args:
            articles: インデックス対象記事リスト

        Returns:
            登録されたドキュメントIDリスト
        """
        if not articles:
            return []

        await self._ensure_initialized()
        batch_size = config.embedding.batch_size

        all_ids: list[str] = []
        for i in range(0, len(articles), batch_size):
            batch = articles[i : i + batch_size]
            texts = [f"{a.title}\n{a.content}" for a in batch]
            embeddings = await self._embedding.embed_batch(texts)
            ids = [a.id for a in batch]
            metadatas = [
                {
                    "title": a.title,
                    "url": a.url,
                    "source": a.source.value,
                    "published_at": a.published_at.isoformat(),
                }
                for a in batch
            ]

            await self._vectordb.add(
                documents=texts,
                ids=ids,
                embeddings=embeddings,
                metadatas=metadatas,
            )
            all_ids.extend(ids)

        self._logger.info("一括インデックス登録: %d件", len(all_ids))
        return all_ids

    async def semantic_search(
        self,
        query: str,
        top_k: int = 20,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """セマンティック検索.

        Args:
            query: 検索クエリ
            top_k: 返却する最大件数
            filter_metadata: メタデータフィルタ

        Returns:
            検索結果のリスト
        """
        await self._ensure_initialized()
        query_embedding = await self._embedding.embed_text(query)

        results = await self._vectordb.search(
            query=query,
            query_embedding=query_embedding,
            top_k=top_k,
            filter_metadata=filter_metadata,
        )

        self._logger.debug("セマンティック検索: query=%s, results=%d", query[:50], len(results))
        return results

    async def find_similar(
        self,
        article: Article,
        threshold: float | None = None,
        top_k: int = 10,
    ) -> list[dict[str, Any]]:
        """類似記事を検索.

        Args:
            article: 検索元記事
            threshold: 類似度閾値（指定なしの場合はデフォルト値）
            top_k: 返却する最大件数

        Returns:
            類似記事のリスト（閾値以上のもの）
        """
        await self._ensure_initialized()
        threshold = threshold if threshold is not None else self._similarity_threshold
        text = f"{article.title}\n{article.content}"
        query_embedding = await self._embedding.embed_text(text)

        results = await self._vectordb.search(
            query=text,
            query_embedding=query_embedding,
            top_k=top_k + 1,
        )

        similar = []
        for r in results:
            doc_id = r.get("id", "")
            if doc_id == article.id:
                continue
            score = r.get("score", 0.0)
            if score >= threshold:
                similar.append(r)

        return similar[:top_k]

    async def deduplicate_batch(
        self,
        articles: list[Article],
        threshold: float | None = None,
    ) -> list[Article]:
        """記事リストのセマンティック重複排除.

        Args:
            articles: 重複排除対象の記事リスト
            threshold: 類似度閾値

        Returns:
            重複排除後の記事リスト
        """
        if not articles:
            return []

        threshold = threshold if threshold is not None else self._similarity_threshold
        await self._ensure_initialized()

        texts = [f"{a.title}\n{a.content}" for a in articles]
        embeddings = await self._embedding.embed_batch(texts)

        unique: list[Article] = []
        unique_embeddings: list[list[float]] = []

        for article, emb in zip(articles, embeddings, strict=False):
            is_duplicate = False
            for existing_emb in unique_embeddings:
                similarity = self._cosine_similarity(emb, existing_emb)
                if similarity >= threshold:
                    is_duplicate = True
                    break

            if not is_duplicate:
                unique.append(article)
                unique_embeddings.append(emb)

        removed = len(articles) - len(unique)
        if removed > 0:
            self._logger.info("重複排除: %d件を除外 (%d → %d)", removed, len(articles), len(unique))
        return unique

    async def mmr_search(
        self,
        query: str,
        top_k: int = 10,
        candidates_k: int = 50,
        lambda_param: float | None = None,
    ) -> list[dict[str, Any]]:
        """MMR (Maximal Marginal Relevance) 検索.

        関連性とダイバーシティのバランスを取る検索。

        Args:
            query: 検索クエリ
            top_k: 返却する件数
            candidates_k: 候補数
            lambda_param: MMRラムダパラメータ (0-1)

        Returns:
            MMR選択された結果リスト
        """
        await self._ensure_initialized()
        lambda_param = lambda_param if lambda_param is not None else self._mmr_lambda
        query_embedding = await self._embedding.embed_text(query)

        candidates = await self._vectordb.search(
            query=query,
            query_embedding=query_embedding,
            top_k=candidates_k,
        )

        if not candidates:
            return []

        selected: list[dict[str, Any]] = []
        selected_embeddings: list[list[float]] = []
        remaining = list(candidates)

        for _ in range(min(top_k, len(candidates))):
            if not remaining:
                break

            best_score = -float("inf")
            best_idx = 0

            for idx, candidate in enumerate(remaining):
                candidate_emb = candidate.get("embedding", [])
                if not candidate_emb:
                    relevance = candidate.get("score", 0.0)
                else:
                    relevance = self._cosine_similarity(query_embedding, candidate_emb)

                max_sim = 0.0
                if selected_embeddings and candidate_emb:
                    max_sim = max(self._cosine_similarity(candidate_emb, sel_emb) for sel_emb in selected_embeddings)

                mmr_score = lambda_param * relevance - (1 - lambda_param) * max_sim
                if mmr_score > best_score:
                    best_score = mmr_score
                    best_idx = idx

            chosen = remaining.pop(best_idx)
            selected.append(chosen)
            emb = chosen.get("embedding", [])
            if emb:
                selected_embeddings.append(emb)

        return selected

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
