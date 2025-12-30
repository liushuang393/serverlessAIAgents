# -*- coding: utf-8 -*-
"""VectorDBProvider - 松耦合ベクトルデータベースアクセス.

Agent/サービスは具体的なVector DB実装を知る必要がありません。
環境変数から自動検出してVector DBインスタンスを提供します。

使用例:
    >>> from agentflow import get_vectordb
    >>> vdb = get_vectordb()  # 環境変数から自動検出
    >>> await vdb.connect()
    >>> await vdb.add(["doc1", "doc2"], ids=["1", "2"])
    >>> results = await vdb.search("query text", top_k=5)

環境変数優先順位:
    1. PINECONE_API_KEY → Pinecone
    2. QDRANT_URL → Qdrant
    3. CHROMA_PERSIST_DIR → ChromaDB（ローカル）
    4. なし → メモリ内 Mock（開発用）
"""

import logging
import os
from typing import Any, Protocol, runtime_checkable

logger = logging.getLogger(__name__)

# グローバルシングルトン
_vectordb_instance: "VectorDBProvider | None" = None


@runtime_checkable
class VectorDBProvider(Protocol):
    """ベクトルDBプロバイダーの統一インターフェース."""

    async def connect(self) -> None:
        """接続."""
        ...

    async def disconnect(self) -> None:
        """切断."""
        ...

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        embeddings: list[list[float]] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加."""
        ...

    async def search(
        self,
        query: str,
        query_embedding: list[float] | None = None,
        top_k: int = 5,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """類似検索."""
        ...

    async def delete(self, ids: list[str]) -> int:
        """ドキュメントを削除."""
        ...

    async def clear(self) -> None:
        """全ドキュメントをクリア."""
        ...

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        ...


class MockVectorDBProvider:
    """Mock Vector DB Provider（開発・テスト用）.

    API キーがない場合のインメモリ実装。
    """

    def __init__(self, collection: str = "default") -> None:
        """初期化."""
        self._collection = collection
        self._data: dict[str, dict[str, Any]] = {}

    async def connect(self) -> None:
        """接続（no-op）."""
        pass

    async def disconnect(self) -> None:
        """切断（no-op）."""
        pass

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        embeddings: list[list[float]] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加."""
        if ids is None:
            ids = [f"doc_{i}" for i in range(len(documents))]
        for i, (doc_id, doc) in enumerate(zip(ids, documents)):
            self._data[doc_id] = {
                "id": doc_id,
                "document": doc,
                "embedding": embeddings[i] if embeddings else None,
                "metadata": metadatas[i] if metadatas else {},
            }

    async def search(
        self,
        query: str,
        query_embedding: list[float] | None = None,
        top_k: int = 5,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """類似検索（Mock: 単純なテキストマッチ）."""
        results = []
        for item in self._data.values():
            # 簡易マッチ：クエリがドキュメントに含まれるか
            if query.lower() in item["document"].lower():
                results.append({
                    "id": item["id"],
                    "document": item["document"],
                    "distance": 0.1,
                    "metadata": item.get("metadata", {}),
                })
        return results[:top_k]

    async def delete(self, ids: list[str]) -> int:
        """削除."""
        count = 0
        for doc_id in ids:
            if doc_id in self._data:
                del self._data[doc_id]
                count += 1
        return count

    async def clear(self) -> None:
        """全クリア."""
        self._data.clear()

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "mock"


class ChromaDBProvider:
    """ChromaDB Provider（ローカル向量DB）."""

    def __init__(self, persist_dir: str | None = None, collection: str = "default") -> None:
        """初期化."""
        self._persist_dir = persist_dir
        self._collection_name = collection
        self._client: Any = None
        self._collection: Any = None

    async def connect(self) -> None:
        """ChromaDB に接続."""
        try:
            import chromadb
            if self._persist_dir:
                self._client = chromadb.PersistentClient(path=self._persist_dir)
            else:
                self._client = chromadb.Client()
            self._collection = self._client.get_or_create_collection(
                name=self._collection_name
            )
            logger.info(f"Connected to ChromaDB: {self._collection_name}")
        except ImportError:
            raise ImportError("chromadb package required: pip install chromadb")

    async def disconnect(self) -> None:
        """切断."""
        self._client = None
        self._collection = None

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        embeddings: list[list[float]] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加."""
        if ids is None:
            ids = [f"doc_{i}" for i in range(len(documents))]
        kwargs: dict[str, Any] = {"documents": documents, "ids": ids}
        if embeddings:
            kwargs["embeddings"] = embeddings
        if metadatas:
            kwargs["metadatas"] = metadatas
        self._collection.add(**kwargs)

    async def search(
        self,
        query: str,
        query_embedding: list[float] | None = None,
        top_k: int = 5,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """類似検索."""
        kwargs: dict[str, Any] = {"n_results": top_k}
        if query_embedding:
            kwargs["query_embeddings"] = [query_embedding]
        else:
            kwargs["query_texts"] = [query]
        if filter_metadata:
            kwargs["where"] = filter_metadata

        results = self._collection.query(**kwargs)
        output = []
        if results and "documents" in results:
            for i, doc in enumerate(results["documents"][0]):
                output.append({
                    "id": results["ids"][0][i] if "ids" in results else f"doc_{i}",
                    "document": doc,
                    "distance": results["distances"][0][i] if "distances" in results else 0,
                    "metadata": results["metadatas"][0][i] if "metadatas" in results else {},
                })
        return output

    async def delete(self, ids: list[str]) -> int:
        """削除."""
        self._collection.delete(ids=ids)
        return len(ids)

    async def clear(self) -> None:
        """全クリア."""
        # ChromaDB: コレクション再作成で全削除
        self._client.delete_collection(self._collection_name)
        self._collection = self._client.create_collection(name=self._collection_name)

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "chromadb"


def get_vectordb(collection: str = "default") -> VectorDBProvider:
    """ベクトルDBプロバイダーを取得（松耦合）.

    環境変数から自動検出して最適なVector DBを返します。
    Agent/サービスは具体的な実装を知る必要がありません。

    Args:
        collection: コレクション名

    Returns:
        VectorDBProvider インスタンス

    環境変数優先順位:
        1. PINECONE_API_KEY → Pinecone
        2. QDRANT_URL → Qdrant
        3. CHROMA_PERSIST_DIR → ChromaDB（ローカル永続化）
        4. なし → MockVectorDBProvider
    """
    global _vectordb_instance

    if _vectordb_instance is not None:
        return _vectordb_instance

    # Pinecone (TODO: 実装)
    if os.getenv("PINECONE_API_KEY"):
        logger.warning("Pinecone detected but not implemented via get_vectordb()")

    # Qdrant (TODO: 実装)
    if os.getenv("QDRANT_URL"):
        logger.warning("Qdrant detected but not implemented via get_vectordb()")

    # ChromaDB
    chroma_dir = os.getenv("CHROMA_PERSIST_DIR")
    if chroma_dir or os.getenv("USE_CHROMADB"):
        logger.info(f"Using ChromaDB provider: {chroma_dir or 'in-memory'}")
        _vectordb_instance = ChromaDBProvider(persist_dir=chroma_dir, collection=collection)
        return _vectordb_instance

    # フォールバック: Mock
    logger.info("No VectorDB config found. Using mock provider.")
    _vectordb_instance = MockVectorDBProvider(collection=collection)
    return _vectordb_instance


def reset_vectordb() -> None:
    """VectorDBインスタンスをリセット（テスト用）."""
    global _vectordb_instance
    _vectordb_instance = None
