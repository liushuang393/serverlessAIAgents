"""セマンティック検索サービス.

ベクトル類似度検索による意味的な記事検索と重複排除を提供します。
AgentFlowの get_embedding() / get_vectordb() を使用します。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from apps.market_trend_monitor.backend.config import config

from infrastructure.providers.embedding_provider import EmbeddingProvider, get_embedding
from infrastructure.providers.vectordb_provider import VectorDBProvider, get_vectordb


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
        embedding: EmbeddingProvider | None = None,
        vectordb: VectorDBProvider | None = None,
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
        await self._require_vectordb().connect()
        self._initialized = True

    def _require_embedding(self) -> EmbeddingProvider:
        """Embedding インスタンスを返す."""
        if self._embedding is None:
            msg = "Embedding provider is not initialized"
            raise RuntimeError(msg)
        return self._embedding

    def _require_vectordb(self) -> VectorDBProvider:
        """Vector DB インスタンスを返す."""
        if self._vectordb is None:
            msg = "Vector database provider is not initialized"
            raise RuntimeError(msg)
        return self._vectordb

    async def index_article(self, article: Article) -> str:
        """記事をベクトルインデックスに登録.

        Args:
            article: インデックス対象の記事

        Returns:
            登録されたドキュメントID
        """
        await self._ensure_initialized()
        text = f"{article.title}\n{article.content}"
        embedding_provider = self._require_embedding()
        vectordb = self._require_vectordb()
        embedding = await embedding_provider.embed_query(text)
        doc_id = article.id

        await vectordb.add_documents(
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
        embedding_provider = self._require_embedding()
        vectordb = self._require_vectordb()

        all_ids: list[str] = []
        for i in range(0, len(articles), batch_size):
            batch = articles[i : i + batch_size]
            texts = [f"{a.title}\n{a.content}" for a in batch]
            embeddings = await embedding_provider.embed_documents(texts)
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

            await vectordb.add_documents(
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
        embedding_provider = self._require_embedding()
        vectordb = self._require_vectordb()
        query_embedding = await embedding_provider.embed_query(query)

        results = await vectordb.similarity_search(
            query=query,
            k=top_k,
            filter=filter_metadata,
            query_embedding=query_embedding,
        )

        normalized_results = [item for item in results if isinstance(item, dict)]
        self._logger.debug("セマンティック検索: query=%s, results=%d", query[:50], len(normalized_results))
        return normalized_results

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
        embedding_provider = self._require_embedding()
        vectordb = self._require_vectordb()
        query_embedding = await embedding_provider.embed_query(text)

        results = await vectordb.similarity_search(
            query=text,
            k=top_k + 1,
            query_embedding=query_embedding,
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
        embedding_provider = self._require_embedding()

        texts = [f"{a.title}\n{a.content}" for a in articles]
        embeddings = await embedding_provider.embed_documents(texts)

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
        embedding_provider = self._require_embedding()
        vectordb = self._require_vectordb()
        query_embedding = await embedding_provider.embed_query(query)

        candidates = await vectordb.similarity_search(
            query=query,
            k=candidates_k,
            query_embedding=query_embedding,
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
        return float(dot / (norm_a * norm_b))
