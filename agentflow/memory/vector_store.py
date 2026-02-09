"""Vector Store - 標準化ベクトルストア接口.

このモジュールは、LlamaIndex/LangChain 互換のベクトルストア接口を提供します。

接口設計原則:
- LlamaIndex VectorStore API 互換
- LangChain VectorStore API 互換
- 将来の Pinecone/Qdrant/Milvus 差し替え可能
- 内部実装は自研、接口は業界標準

使用例:
    >>> # 1. 抽象接口（推奨）
    >>> store: VectorStore = InMemoryVectorStore(embedding_model=my_embedder)
    >>> await store.add_documents(docs)
    >>> results = await store.similarity_search("query", k=5)
    >>>
    >>> # 2. 将来の差し替え
    >>> store: VectorStore = QdrantVectorStore(...)  # 接口同じ
    >>> store: VectorStore = PineconeVectorStore(...)  # 接口同じ

参考:
- LlamaIndex: VectorStore Protocol
- LangChain: VectorStore ABC
- Qdrant/Pinecone/Milvus APIs
"""

from __future__ import annotations

import asyncio
import hashlib
import logging
import math
import uuid
from abc import ABC, abstractmethod
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from collections.abc import Sequence


# =============================================================================
# 標準化データモデル（LlamaIndex/LangChain 互換）
# =============================================================================


class Document(BaseModel):
    """ドキュメント（LangChain Document 互換）.

    Attributes:
        page_content: テキスト内容
        metadata: メタデータ
        id: ドキュメントID（自動生成可能）
    """

    page_content: str = Field(..., description="テキスト内容")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")
    id: str | None = Field(default=None, description="ドキュメントID")

    def __init__(self, **data: Any) -> None:
        """初期化（ID自動生成）."""
        super().__init__(**data)
        if self.id is None:
            # 内容ベースのIDを生成（重複防止）
            content_hash = hashlib.md5(self.page_content.encode()).hexdigest()[:12]
            self.id = f"doc_{content_hash}"


class Node(BaseModel):
    """ノード（LlamaIndex Node 互換）.

    LlamaIndex の TextNode に相当。
    Document より詳細な制御が必要な場合に使用。

    Attributes:
        id_: ノードID
        text: テキスト内容
        metadata: メタデータ
        embedding: 埋め込みベクトル
        relationships: 関連ノード
    """

    id_: str = Field(default_factory=lambda: f"node_{uuid.uuid4().hex[:12]}")
    text: str = Field(..., description="テキスト内容")
    metadata: dict[str, Any] = Field(default_factory=dict)
    embedding: list[float] | None = Field(default=None)
    relationships: dict[str, str] = Field(default_factory=dict)
    start_char_idx: int | None = Field(default=None)
    end_char_idx: int | None = Field(default=None)

    def get_content(self) -> str:
        """テキスト内容を取得（LlamaIndex互換）."""
        return self.text

    def get_embedding(self) -> list[float] | None:
        """埋め込みを取得."""
        return self.embedding

    @classmethod
    def from_document(cls, doc: Document, embedding: list[float] | None = None) -> Node:
        """Documentからノードを作成."""
        return cls(
            id_=doc.id or f"node_{uuid.uuid4().hex[:12]}",
            text=doc.page_content,
            metadata=doc.metadata,
            embedding=embedding,
        )


class SearchResult(BaseModel):
    """検索結果."""

    document: Document = Field(...)
    score: float = Field(..., ge=0.0, le=1.0, description="類似度スコア")
    node: Node | None = Field(default=None)


class SearchType(str, Enum):
    """検索タイプ."""

    SIMILARITY = "similarity"  # コサイン類似度
    MMR = "mmr"  # Maximal Marginal Relevance
    HYBRID = "hybrid"  # ハイブリッド検索


# =============================================================================
# 埋め込みモデル接口（LlamaIndex BaseEmbedding 互換）
# =============================================================================


class EmbeddingModel(ABC):
    """埋め込みモデル接口（LlamaIndex/LangChain 互換）.

    将来の OpenAI Embeddings / Sentence Transformers 差し替え可能。
    """

    @abstractmethod
    async def embed_query(self, text: str) -> list[float]:
        """クエリテキストを埋め込み.

        Args:
            text: クエリテキスト

        Returns:
            埋め込みベクトル
        """

    @abstractmethod
    async def embed_documents(self, texts: list[str]) -> list[list[float]]:
        """複数ドキュメントを埋め込み.

        Args:
            texts: テキストリスト

        Returns:
            埋め込みベクトルリスト
        """

    @property
    @abstractmethod
    def dimension(self) -> int:
        """埋め込み次元数."""


class SimpleEmbedding(EmbeddingModel):
    """シンプルな埋め込みモデル（自研実装）.

    TF-IDF風の軽量埋め込み。
    本番環境では OpenAI/Sentence Transformers への差し替え推奨。
    """

    def __init__(self, dim: int = 384) -> None:
        """初期化.

        Args:
            dim: 埋め込み次元数
        """
        self._dim = dim
        self._vocabulary: dict[str, int] = {}
        self._cache: dict[str, list[float]] = {}

    @property
    def dimension(self) -> int:
        """埋め込み次元数."""
        return self._dim

    async def embed_query(self, text: str) -> list[float]:
        """クエリを埋め込み."""
        return await self._embed_single(text)

    async def embed_documents(self, texts: list[str]) -> list[list[float]]:
        """ドキュメントを埋め込み."""
        return await asyncio.gather(*[self._embed_single(t) for t in texts])

    async def _embed_single(self, text: str) -> list[float]:
        """単一テキストを埋め込み."""
        if text in self._cache:
            return self._cache[text]

        # トークン化
        tokens = text.lower().split()

        # 語彙更新
        for token in tokens:
            if token not in self._vocabulary:
                self._vocabulary[token] = len(self._vocabulary)

        # TF計算
        tf: dict[str, float] = {}
        for token in tokens:
            tf[token] = tf.get(token, 0) + 1

        # 正規化
        max_tf = max(tf.values()) if tf else 1
        for token in tf:
            tf[token] = tf[token] / max_tf

        # ベクトル生成
        embedding = [0.0] * self._dim
        for token, value in tf.items():
            idx = self._vocabulary[token] % self._dim
            embedding[idx] += value

        # L2正規化
        norm = math.sqrt(sum(x * x for x in embedding))
        if norm > 0:
            embedding = [x / norm for x in embedding]

        self._cache[text] = embedding
        return embedding


# =============================================================================
# ベクトルストア接口（LlamaIndex/LangChain 互換）
# =============================================================================


class VectorStore(ABC):
    """ベクトルストア抽象接口（LlamaIndex/LangChain 互換）.

    この接口に従えば、将来的に以下への差し替えが容易:
    - Pinecone
    - Qdrant
    - Milvus
    - Weaviate
    - Chroma
    - FAISS

    全てのベクトルストア実装はこの接口を実装する必要があります。
    """

    @abstractmethod
    async def add_documents(
        self,
        documents: Sequence[Document],
        **kwargs: Any,
    ) -> list[str]:
        """ドキュメントを追加.

        Args:
            documents: 追加するドキュメント
            **kwargs: 追加オプション

        Returns:
            追加されたドキュメントIDリスト
        """

    @abstractmethod
    async def add_nodes(
        self,
        nodes: Sequence[Node],
        **kwargs: Any,
    ) -> list[str]:
        """ノードを追加（LlamaIndex互換）.

        Args:
            nodes: 追加するノード
            **kwargs: 追加オプション

        Returns:
            追加されたノードIDリスト
        """

    @abstractmethod
    async def delete(
        self,
        ids: list[str],
        **kwargs: Any,
    ) -> bool:
        """ドキュメント/ノードを削除.

        Args:
            ids: 削除するIDリスト
            **kwargs: 削除オプション

        Returns:
            削除成功かどうか
        """

    @abstractmethod
    async def similarity_search(
        self,
        query: str,
        k: int = 4,
        filter: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> list[SearchResult]:
        """類似度検索.

        Args:
            query: 検索クエリ
            k: 返却件数
            filter: メタデータフィルター
            **kwargs: 追加オプション

        Returns:
            検索結果リスト
        """

    @abstractmethod
    async def similarity_search_with_score(
        self,
        query: str,
        k: int = 4,
        filter: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> list[tuple[Document, float]]:
        """スコア付き類似度検索（LangChain互換）.

        Args:
            query: 検索クエリ
            k: 返却件数
            filter: メタデータフィルター
            **kwargs: 追加オプション

        Returns:
            (ドキュメント, スコア) のタプルリスト
        """

    async def max_marginal_relevance_search(
        self,
        query: str,
        k: int = 4,
        fetch_k: int = 20,
        lambda_mult: float = 0.5,
        filter: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> list[SearchResult]:
        """MMR検索（多様性考慮）.

        Maximal Marginal Relevance による検索。
        関連性と多様性のバランスを取る。

        Args:
            query: 検索クエリ
            k: 返却件数
            fetch_k: 候補取得件数
            lambda_mult: 多様性重み（0=多様性優先、1=関連性優先）
            filter: メタデータフィルター
            **kwargs: 追加オプション

        Returns:
            検索結果リスト
        """
        # デフォルト実装（サブクラスでオーバーライド推奨）
        return await self.similarity_search(query, k, filter, **kwargs)


# =============================================================================
# インメモリ実装（自研、接口は標準）
# =============================================================================


class InMemoryVectorStore(VectorStore):
    """インメモリベクトルストア（自研実装、標準接口）.

    開発/テスト環境向け。
    本番環境では Qdrant/Pinecone への差し替え推奨。

    使用例:
        >>> store = InMemoryVectorStore(embedding_model=SimpleEmbedding())
        >>> await store.add_documents([Document(page_content="Hello")])
        >>> results = await store.similarity_search("Hi", k=3)
    """

    def __init__(
        self,
        embedding_model: EmbeddingModel | None = None,
    ) -> None:
        """初期化.

        Args:
            embedding_model: 埋め込みモデル（Noneの場合はSimpleEmbedding）
        """
        self._embedder = embedding_model or SimpleEmbedding()
        self._nodes: dict[str, Node] = {}
        self._logger = logging.getLogger(__name__)

    async def add_documents(
        self,
        documents: Sequence[Document],
        **kwargs: Any,
    ) -> list[str]:
        """ドキュメントを追加."""
        # テキストを抽出
        texts = [doc.page_content for doc in documents]

        # バッチ埋め込み
        embeddings = await self._embedder.embed_documents(texts)

        # ノードに変換して保存
        ids = []
        for doc, embedding in zip(documents, embeddings, strict=False):
            node = Node.from_document(doc, embedding)
            self._nodes[node.id_] = node
            ids.append(node.id_)

        self._logger.debug(f"Added {len(ids)} documents")
        return ids

    async def add_nodes(
        self,
        nodes: Sequence[Node],
        **kwargs: Any,
    ) -> list[str]:
        """ノードを追加."""
        ids = []
        for node in nodes:
            # 埋め込みがない場合は生成
            if node.embedding is None:
                node.embedding = await self._embedder.embed_query(node.text)
            self._nodes[node.id_] = node
            ids.append(node.id_)

        self._logger.debug(f"Added {len(ids)} nodes")
        return ids

    async def delete(
        self,
        ids: list[str],
        **kwargs: Any,
    ) -> bool:
        """ノードを削除."""
        deleted = 0
        for id_ in ids:
            if id_ in self._nodes:
                del self._nodes[id_]
                deleted += 1

        self._logger.debug(f"Deleted {deleted} nodes")
        return deleted > 0

    async def similarity_search(
        self,
        query: str,
        k: int = 4,
        filter: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> list[SearchResult]:
        """類似度検索."""
        results = await self.similarity_search_with_score(query, k, filter, **kwargs)
        return [
            SearchResult(
                document=doc,
                score=score,
                node=self._nodes.get(doc.id) if doc.id else None,
            )
            for doc, score in results
        ]

    async def similarity_search_with_score(
        self,
        query: str,
        k: int = 4,
        filter: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> list[tuple[Document, float]]:
        """スコア付き類似度検索."""
        if not self._nodes:
            return []

        # クエリ埋め込み
        query_embedding = await self._embedder.embed_query(query)

        # 全ノードとの類似度を計算
        scores: list[tuple[str, float]] = []
        for node_id, node in self._nodes.items():
            # フィルター適用
            if filter and not self._match_filter(node.metadata, filter):
                continue

            if node.embedding:
                score = self._cosine_similarity(query_embedding, node.embedding)
                scores.append((node_id, score))

        # スコア順にソート
        scores.sort(key=lambda x: x[1], reverse=True)

        # 上位k件を返却
        results: list[tuple[Document, float]] = []
        for node_id, score in scores[:k]:
            node = self._nodes[node_id]
            doc = Document(
                page_content=node.text,
                metadata=node.metadata,
                id=node_id,
            )
            results.append((doc, score))

        return results

    async def max_marginal_relevance_search(
        self,
        query: str,
        k: int = 4,
        fetch_k: int = 20,
        lambda_mult: float = 0.5,
        filter: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> list[SearchResult]:
        """MMR検索（多様性考慮）."""
        if not self._nodes:
            return []

        # クエリ埋め込み
        query_embedding = await self._embedder.embed_query(query)

        # 候補を取得
        candidates: list[tuple[str, float, list[float]]] = []
        for node_id, node in self._nodes.items():
            if filter and not self._match_filter(node.metadata, filter):
                continue
            if node.embedding:
                score = self._cosine_similarity(query_embedding, node.embedding)
                candidates.append((node_id, score, node.embedding))

        # 関連度順にソート
        candidates.sort(key=lambda x: x[1], reverse=True)
        candidates = candidates[:fetch_k]

        # MMR選択
        selected: list[str] = []
        selected_embeddings: list[list[float]] = []

        while len(selected) < k and candidates:
            best_idx = -1
            best_score = -float("inf")

            for i, (node_id, rel_score, embedding) in enumerate(candidates):
                if node_id in selected:
                    continue

                # 既選択との最大類似度
                max_sim = 0.0
                for sel_emb in selected_embeddings:
                    sim = self._cosine_similarity(embedding, sel_emb)
                    max_sim = max(max_sim, sim)

                # MMRスコア
                mmr_score = lambda_mult * rel_score - (1 - lambda_mult) * max_sim

                if mmr_score > best_score:
                    best_score = mmr_score
                    best_idx = i

            if best_idx >= 0:
                node_id, score, embedding = candidates[best_idx]
                selected.append(node_id)
                selected_embeddings.append(embedding)
                candidates.pop(best_idx)
            else:
                break

        # 結果を構築
        results: list[SearchResult] = []
        for node_id in selected:
            node = self._nodes[node_id]
            doc = Document(
                page_content=node.text,
                metadata=node.metadata,
                id=node_id,
            )
            # スコアは元の類似度を使用
            score = self._cosine_similarity(query_embedding, node.embedding or [])
            results.append(SearchResult(document=doc, score=score, node=node))

        return results

    def _cosine_similarity(self, vec1: list[float], vec2: list[float]) -> float:
        """コサイン類似度を計算."""
        if len(vec1) != len(vec2):
            return 0.0
        dot_product = sum(a * b for a, b in zip(vec1, vec2, strict=False))
        return max(0.0, min(1.0, dot_product))

    def _match_filter(self, metadata: dict[str, Any], filter: dict[str, Any]) -> bool:
        """フィルターマッチングをチェック."""
        for key, value in filter.items():
            if key not in metadata:
                return False
            if metadata[key] != value:
                return False
        return True

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "total_nodes": len(self._nodes),
            "embedding_dim": self._embedder.dimension,
        }

    def clear(self) -> None:
        """全データをクリア."""
        self._nodes.clear()


# =============================================================================
# ファクトリー関数（将来の差し替えを容易に）
# =============================================================================


def create_vector_store(
    store_type: str = "memory",
    embedding_model: EmbeddingModel | None = None,
    **kwargs: Any,
) -> VectorStore:
    """ベクトルストアを作成（ファクトリー）.

    将来の差し替えを考慮したファクトリー関数。
    設定に基づいて適切な実装を返す。

    Args:
        store_type: ストアタイプ
            - "memory": InMemoryVectorStore
            - "qdrant": (将来) QdrantVectorStore
            - "pinecone": (将来) PineconeVectorStore
        embedding_model: 埋め込みモデル
        **kwargs: ストア固有のオプション

    Returns:
        VectorStore インスタンス

    使用例:
        >>> # 開発環境
        >>> store = create_vector_store("memory")
        >>>
        >>> # 本番環境（将来）
        >>> store = create_vector_store("qdrant", url="...", api_key="...")
    """
    if store_type == "memory":
        return InMemoryVectorStore(embedding_model=embedding_model)
    if store_type == "qdrant":
        # 将来の実装
        msg = "Qdrant support coming soon. Use 'memory' for now."
        raise NotImplementedError(msg)
    if store_type == "pinecone":
        # 将来の実装
        msg = "Pinecone support coming soon. Use 'memory' for now."
        raise NotImplementedError(msg)
    msg = f"Unknown store type: {store_type}"
    raise ValueError(msg)


def create_embedding_model(
    model_type: str = "simple",
    **kwargs: Any,
) -> EmbeddingModel:
    """埋め込みモデルを作成（ファクトリー）.

    Args:
        model_type: モデルタイプ
            - "simple": SimpleEmbedding（軽量、開発用）
            - "openai": (将来) OpenAI Embeddings
            - "sentence-transformers": (将来) Sentence Transformers
        **kwargs: モデル固有のオプション

    Returns:
        EmbeddingModel インスタンス
    """
    if model_type == "simple":
        return SimpleEmbedding(dim=kwargs.get("dim", 384))
    if model_type == "openai":
        # 将来の実装
        msg = "OpenAI embeddings coming soon. Use 'simple' for now."
        raise NotImplementedError(msg)
    if model_type == "sentence-transformers":
        # 将来の実装
        msg = "Sentence Transformers coming soon. Use 'simple' for now."
        raise NotImplementedError(msg)
    msg = f"Unknown model type: {model_type}"
    raise ValueError(msg)


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    # データモデル
    "Document",
    # 埋め込み
    "EmbeddingModel",
    "InMemoryVectorStore",
    "Node",
    "SearchResult",
    "SearchType",
    "SimpleEmbedding",
    # ベクトルストア
    "VectorStore",
    "create_embedding_model",
    # ファクトリー
    "create_vector_store",
]
