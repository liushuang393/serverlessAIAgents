"""VectorDBProvider - 松耦合ベクトルデータベースアクセス.

Agent/サービスは具体的なVector DB実装を知る必要がありません。
環境変数から自動検出してVector DBインスタンスを提供します。

使用例:
    >>> from agentflow import get_vectordb
    >>> vdb = get_vectordb()  # 環境変数から自動検出
    >>> await vdb.connect()
    >>> await vdb.add(["doc1", "doc2"], ids=["1", "2"])
    >>> results = await vdb.search("query text", top_k=5)

サポートするVector DB:
    - faiss: Facebook AI Similarity Search（ローカル高速検索）
    - qdrant: Qdrant（クラウド/ローカル、スケーラブル）
    - weaviate: Weaviate（グラフベースセマンティック検索）
    - supabase: Supabase Vector（PostgreSQL pgvector）
    - chromadb: ChromaDB（ローカル開発向け）

環境変数:
    VECTOR_DATABASE_TYPE: "faiss", "qdrant", "weaviate", "supabase", "chromadb"
    QDRANT_URL: Qdrant エンドポイント
    WEAVIATE_URL: Weaviate エンドポイント
    SUPABASE_URL: Supabase プロジェクト URL
    SUPABASE_KEY: Supabase API キー
"""

import logging
import os
from pathlib import Path
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


if TYPE_CHECKING:
    from agentflow.runtime import RuntimeContext

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

    async def disconnect(self) -> None:
        """切断（no-op）."""

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
        for i, (doc_id, doc) in enumerate(zip(ids, documents, strict=False)):
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
                results.append(
                    {
                        "id": item["id"],
                        "document": item["document"],
                        "distance": 0.1,
                        "metadata": item.get("metadata", {}),
                    }
                )
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
            self._collection = self._client.get_or_create_collection(name=self._collection_name)
            logger.info(f"Connected to ChromaDB: {self._collection_name}")
        except ImportError:
            msg = "chromadb package required: pip install chromadb"
            raise ImportError(msg)

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
                output.append(
                    {
                        "id": results["ids"][0][i] if "ids" in results else f"doc_{i}",
                        "document": doc,
                        "distance": results["distances"][0][i] if "distances" in results else 0,
                        "metadata": results["metadatas"][0][i] if "metadatas" in results else {},
                    }
                )
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


class QdrantProvider:
    """Qdrant Provider（クラウド/ローカル向量DB）.

    特徴:
        - 高性能なベクトル検索
        - スケーラブル
        - 豊富なフィルタリング機能
    """

    def __init__(
        self,
        url: str | None = None,
        api_key: str | None = None,
        collection: str = "default",
        vector_size: int = 1536,  # OpenAI embedding size
    ) -> None:
        """初期化.

        Args:
            url: Qdrant URL (例: http://localhost:6333)
            api_key: API キー（Qdrant Cloud 用）
            collection: コレクション名
            vector_size: ベクトルの次元数
        """
        self._url: str = url or os.getenv("QDRANT_URL", "http://localhost:6333") or "http://localhost:6333"
        self._api_key = api_key or os.getenv("QDRANT_API_KEY")
        self._collection_name = collection
        self._vector_size = vector_size
        self._client: Any = None

    async def connect(self) -> None:
        """Qdrant に接続."""
        try:
            from qdrant_client import QdrantClient
            from qdrant_client.models import Distance, VectorParams

            # URL が https で始まる場合はクラウド、そうでなければローカル
            if self._url.startswith("https://"):
                self._client = QdrantClient(
                    url=self._url,
                    api_key=self._api_key,
                )
            else:
                self._client = QdrantClient(url=self._url)

            # コレクションが存在しない場合は作成
            collections = self._client.get_collections().collections
            exists = any(c.name == self._collection_name for c in collections)
            if not exists:
                self._client.create_collection(
                    collection_name=self._collection_name,
                    vectors_config=VectorParams(
                        size=self._vector_size,
                        distance=Distance.COSINE,
                    ),
                )
            logger.info(f"Connected to Qdrant: {self._url}, collection: {self._collection_name}")
        except ImportError:
            msg = "qdrant-client package required: pip install qdrant-client"
            raise ImportError(msg)

    async def disconnect(self) -> None:
        """切断."""
        self._client = None

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        embeddings: list[list[float]] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加."""
        from qdrant_client.models import PointStruct

        if embeddings is None:
            msg = "Qdrant requires embeddings. Use get_embedding() to generate."
            raise ValueError(msg)

        if ids is None:
            import uuid

            ids = [str(uuid.uuid4()) for _ in documents]

        points = []
        for i, (doc_id, doc, emb) in enumerate(zip(ids, documents, embeddings, strict=False)):
            payload = {"document": doc}
            if metadatas and i < len(metadatas):
                payload.update(metadatas[i])
            points.append(PointStruct(id=doc_id, vector=emb, payload=payload))

        self._client.upsert(collection_name=self._collection_name, points=points)

    async def search(
        self,
        query: str,
        query_embedding: list[float] | None = None,
        top_k: int = 5,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """類似検索."""
        if query_embedding is None:
            msg = "Qdrant requires query_embedding for search."
            raise ValueError(msg)

        search_params = {
            "collection_name": self._collection_name,
            "query_vector": query_embedding,
            "limit": top_k,
        }

        if filter_metadata:
            from qdrant_client.models import FieldCondition, Filter, MatchValue

            conditions = [FieldCondition(key=k, match=MatchValue(value=v)) for k, v in filter_metadata.items()]
            search_params["query_filter"] = Filter(must=conditions)

        results = self._client.search(**search_params)

        return [
            {
                "id": str(r.id),
                "document": r.payload.get("document", ""),
                "distance": 1 - r.score,  # Qdrant returns similarity, convert to distance
                "metadata": {k: v for k, v in r.payload.items() if k != "document"},
            }
            for r in results
        ]

    async def delete(self, ids: list[str]) -> int:
        """削除."""
        from qdrant_client.models import PointIdsList

        self._client.delete(
            collection_name=self._collection_name,
            points_selector=PointIdsList(points=ids),
        )
        return len(ids)

    async def clear(self) -> None:
        """全クリア."""
        from qdrant_client.models import Distance, VectorParams

        self._client.delete_collection(self._collection_name)
        self._client.create_collection(
            collection_name=self._collection_name,
            vectors_config=VectorParams(size=self._vector_size, distance=Distance.COSINE),
        )

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "qdrant"


class FAISSProvider:
    """FAISS Provider（Facebook AI Similarity Search）.

    特徴:
        - ローカル高速ベクトル検索
        - GPUサポート
        - 大規模データセット対応
    """

    def __init__(
        self,
        collection: str = "default",
        index_path: str | None = None,
        vector_size: int = 1536,
    ) -> None:
        """初期化.

        Args:
            collection: コレクション名（インデックス識別用）
            index_path: インデックスファイルのパス
            vector_size: ベクトルの次元数
        """
        self._collection_name = collection
        self._index_path = index_path or os.getenv("FAISS_INDEX_PATH")
        self._vector_size = vector_size
        self._index: Any = None
        self._documents: dict[int, dict[str, Any]] = {}
        self._next_id = 0

    async def connect(self) -> None:
        """FAISSインデックスを初期化/ロード."""
        try:
            import faiss

            if self._index_path and Path(self._index_path).exists():
                self._index = faiss.read_index(self._index_path)
                logger.info(f"Loaded FAISS index from: {self._index_path}")
            else:
                # L2距離の平坦インデックス（正確な検索）
                self._index = faiss.IndexFlatL2(self._vector_size)
                logger.info(f"Created new FAISS index: dimension={self._vector_size}")
        except ImportError:
            msg = "faiss-cpu package required: pip install faiss-cpu"
            raise ImportError(msg)

    async def disconnect(self) -> None:
        """インデックスを保存して切断."""
        if self._index and self._index_path:
            import faiss

            faiss.write_index(self._index, self._index_path)
            logger.info(f"Saved FAISS index to: {self._index_path}")
        self._index = None

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        embeddings: list[list[float]] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加."""
        import numpy as np

        if embeddings is None:
            msg = "FAISS requires embeddings. Use get_embedding() to generate."
            raise ValueError(msg)

        vectors = np.array(embeddings, dtype=np.float32)
        self._index.add(vectors)

        # ドキュメントとメタデータを保存
        for i, doc in enumerate(documents):
            doc_id = ids[i] if ids else str(self._next_id)
            self._documents[self._next_id] = {
                "id": doc_id,
                "document": doc,
                "metadata": metadatas[i] if metadatas and i < len(metadatas) else {},
            }
            self._next_id += 1

    async def search(
        self,
        query: str,
        query_embedding: list[float] | None = None,
        top_k: int = 5,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """類似検索."""
        import numpy as np

        if query_embedding is None:
            msg = "FAISS requires query_embedding for search."
            raise ValueError(msg)

        query_vector = np.array([query_embedding], dtype=np.float32)
        distances, indices = self._index.search(query_vector, top_k)

        results = []
        for i, idx in enumerate(indices[0]):
            if idx >= 0 and idx in self._documents:
                doc = self._documents[idx]
                # メタデータフィルタリング
                if filter_metadata:
                    if not all(doc["metadata"].get(k) == v for k, v in filter_metadata.items()):
                        continue
                results.append(
                    {
                        "id": doc["id"],
                        "document": doc["document"],
                        "distance": float(distances[0][i]),
                        "metadata": doc["metadata"],
                    }
                )
        return results

    async def delete(self, ids: list[str]) -> int:
        """削除（FAISSは直接削除非対応、マーク削除）."""
        count = 0
        for doc_id in ids:
            for idx, doc in list(self._documents.items()):
                if doc["id"] == doc_id:
                    del self._documents[idx]
                    count += 1
        return count

    async def clear(self) -> None:
        """全クリア."""
        import faiss

        self._index = faiss.IndexFlatL2(self._vector_size)
        self._documents.clear()
        self._next_id = 0

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "faiss"


class WeaviateProvider:
    """Weaviate Provider（グラフベースセマンティック検索）.

    特徴:
        - セマンティック検索
        - GraphQL API
        - モジュール拡張（テキスト埋め込み、分類等）
    """

    def __init__(
        self,
        url: str | None = None,
        api_key: str | None = None,
        collection: str = "Document",
        vector_size: int = 1536,
    ) -> None:
        """初期化.

        Args:
            url: Weaviate URL
            api_key: API キー
            collection: クラス名（Weaviateではクラス）
            vector_size: ベクトルの次元数
        """
        self._url = url or os.getenv("WEAVIATE_URL", "http://localhost:8080")
        self._api_key = api_key or os.getenv("WEAVIATE_API_KEY")
        self._class_name = collection
        self._vector_size = vector_size
        self._client: Any = None

    async def connect(self) -> None:
        """Weaviate に接続."""
        try:
            import weaviate

            auth_config = None
            if self._api_key:
                auth_config = weaviate.AuthApiKey(api_key=self._api_key)

            self._client = weaviate.Client(
                url=self._url,
                auth_client_secret=auth_config,
            )

            # クラスが存在しない場合は作成
            if not self._client.schema.exists(self._class_name):
                class_obj = {
                    "class": self._class_name,
                    "vectorizer": "none",  # 外部埋め込み使用
                    "properties": [
                        {"name": "content", "dataType": ["text"]},
                        {"name": "doc_id", "dataType": ["string"]},
                    ],
                }
                self._client.schema.create_class(class_obj)
            logger.info(f"Connected to Weaviate: {self._url}, class: {self._class_name}")
        except ImportError:
            msg = "weaviate-client package required: pip install weaviate-client"
            raise ImportError(msg)

    async def disconnect(self) -> None:
        """切断."""
        self._client = None

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        embeddings: list[list[float]] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加."""
        if embeddings is None:
            msg = "Weaviate requires embeddings when vectorizer is 'none'."
            raise ValueError(msg)

        if ids is None:
            import uuid

            ids = [str(uuid.uuid4()) for _ in documents]

        with self._client.batch as batch:
            for i, (doc_id, doc, emb) in enumerate(zip(ids, documents, embeddings, strict=False)):
                properties = {
                    "content": doc,
                    "doc_id": doc_id,
                }
                if metadatas and i < len(metadatas):
                    properties.update(metadatas[i])
                batch.add_data_object(
                    data_object=properties,
                    class_name=self._class_name,
                    vector=emb,
                )

    async def search(
        self,
        query: str,
        query_embedding: list[float] | None = None,
        top_k: int = 5,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """類似検索."""
        if query_embedding is None:
            msg = "Weaviate requires query_embedding for vector search."
            raise ValueError(msg)

        near_vector = {"vector": query_embedding}
        query_builder = (
            self._client.query.get(self._class_name, ["content", "doc_id"])
            .with_near_vector(near_vector)
            .with_limit(top_k)
            .with_additional(["distance"])
        )

        if filter_metadata:
            # Weaviate フィルター構築
            where_filter: dict[str, Any] = {"operator": "And", "operands": []}
            for k, v in filter_metadata.items():
                where_filter["operands"].append(
                    {
                        "path": [k],
                        "operator": "Equal",
                        "valueString": str(v),
                    }
                )
            query_builder = query_builder.with_where(where_filter)

        response = query_builder.do()
        results = []
        if response and "data" in response and "Get" in response["data"]:
            items = response["data"]["Get"].get(self._class_name, [])
            for item in items:
                results.append(
                    {
                        "id": item.get("doc_id", ""),
                        "document": item.get("content", ""),
                        "distance": item.get("_additional", {}).get("distance", 0),
                        "metadata": {k: v for k, v in item.items() if k not in ["content", "doc_id", "_additional"]},
                    }
                )
        return results

    async def delete(self, ids: list[str]) -> int:
        """削除."""
        count = 0
        for doc_id in ids:
            where_filter = {
                "path": ["doc_id"],
                "operator": "Equal",
                "valueString": doc_id,
            }
            self._client.batch.delete_objects(
                class_name=self._class_name,
                where=where_filter,
            )
            count += 1
        return count

    async def clear(self) -> None:
        """全クリア（クラス再作成）."""
        self._client.schema.delete_class(self._class_name)
        class_obj = {
            "class": self._class_name,
            "vectorizer": "none",
            "properties": [
                {"name": "content", "dataType": ["text"]},
                {"name": "doc_id", "dataType": ["string"]},
            ],
        }
        self._client.schema.create_class(class_obj)

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "weaviate"


class SupabaseVectorProvider:
    """Supabase Vector Provider（PostgreSQL pgvector）.

    特徴:
        - PostgreSQL ベース
        - SQL でクエリ可能
        - Supabase エコシステム統合
    """

    def __init__(
        self,
        url: str | None = None,
        key: str | None = None,
        table: str = "documents",
        vector_size: int = 1536,
    ) -> None:
        """初期化.

        Args:
            url: Supabase プロジェクト URL
            key: Supabase API キー
            table: テーブル名
            vector_size: ベクトルの次元数
        """
        self._url = url or os.getenv("SUPABASE_URL")
        self._key = key or os.getenv("SUPABASE_KEY")
        self._table = table
        self._vector_size = vector_size
        self._client: Any = None

    async def connect(self) -> None:
        """Supabase に接続."""
        try:
            from supabase import create_client

            if not self._url or not self._key:
                msg = "SUPABASE_URL and SUPABASE_KEY required"
                raise ValueError(msg)

            self._client = create_client(self._url, self._key)
            logger.info(f"Connected to Supabase: {self._url}, table: {self._table}")
        except ImportError:
            msg = "supabase package required: pip install supabase"
            raise ImportError(msg)

    async def disconnect(self) -> None:
        """切断."""
        self._client = None

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        embeddings: list[list[float]] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加."""
        if embeddings is None:
            msg = "Supabase Vector requires embeddings."
            raise ValueError(msg)

        if ids is None:
            import uuid

            ids = [str(uuid.uuid4()) for _ in documents]

        rows = []
        for i, (doc_id, doc, emb) in enumerate(zip(ids, documents, embeddings, strict=False)):
            row: dict[str, Any] = {
                "id": doc_id,
                "content": doc,
                "embedding": emb,
            }
            if metadatas and i < len(metadatas):
                row["metadata"] = metadatas[i]
            rows.append(row)

        self._client.table(self._table).upsert(rows).execute()

    async def search(
        self,
        query: str,
        query_embedding: list[float] | None = None,
        top_k: int = 5,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """類似検索（RPC関数使用）."""
        if query_embedding is None:
            msg = "Supabase Vector requires query_embedding for search."
            raise ValueError(msg)

        # Supabase の match_documents RPC 関数を呼び出し
        # 事前にこの関数をSupabaseに作成しておく必要あり
        response = self._client.rpc(
            "match_documents",
            {
                "query_embedding": query_embedding,
                "match_count": top_k,
                "filter": filter_metadata or {},
            },
        ).execute()

        results = []
        if response.data:
            for item in response.data:
                results.append(
                    {
                        "id": item.get("id", ""),
                        "document": item.get("content", ""),
                        "distance": 1 - item.get("similarity", 0),
                        "metadata": item.get("metadata", {}),
                    }
                )
        return results

    async def delete(self, ids: list[str]) -> int:
        """削除."""
        self._client.table(self._table).delete().in_("id", ids).execute()
        return len(ids)

    async def clear(self) -> None:
        """全クリア."""
        # 注意: テーブル全削除は危険なので、truncate ではなく条件なし delete
        self._client.table(self._table).delete().neq("id", "").execute()

    def get_provider_name(self) -> str:
        """プロバイダー名."""
        return "supabase"


def get_vectordb(
    collection: str = "default",
    *,
    context: "RuntimeContext | None" = None,
    _new_instance: bool = False,
) -> VectorDBProvider:
    """ベクトルDBプロバイダーを取得（松耦合・黒盒設計）.

    環境変数から自動検出して最適なVector DBを返します。
    Agent/サービスは具体的な実装を知る必要がありません（黒盒）。

    Args:
        collection: コレクション名
        context: RuntimeContext（テナント/設定の分離用）
        _new_instance: 新しいインスタンスを強制作成（テスト用）

    Returns:
        VectorDBProvider インスタンス

    環境変数:
        VECTOR_DATABASE_TYPE: "faiss", "qdrant", "weaviate", "supabase", "chromadb"
        （指定がない場合は他の環境変数から自動検出）
    """
    global _vectordb_instance

    if _vectordb_instance is not None and context is None and not _new_instance:
        return _vectordb_instance

    from agentflow.runtime import get_env, resolve_settings

    settings = resolve_settings(context) if context is not None else None

    # 1. 明示的な VECTOR_DATABASE_TYPE 指定を優先
    db_type = (get_env("VECTOR_DATABASE_TYPE", context=context) or "").lower()
    provider: VectorDBProvider

    if db_type == "faiss":
        try:
            logger.info("Using FAISS provider")
            provider = FAISSProvider(collection=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            logger.warning("faiss-cpu not installed. Falling back.")

    if db_type == "qdrant":
        try:
            url = (settings.qdrant_url if settings else None) or get_env(
                "QDRANT_URL", "http://localhost:6333", context=context
            )
            logger.info(f"Using Qdrant provider: {url}")
            provider = QdrantProvider(url=url, collection=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            logger.warning("qdrant-client not installed. Falling back.")

    if db_type == "weaviate":
        try:
            url = get_env("WEAVIATE_URL", "http://localhost:8080", context=context)
            logger.info(f"Using Weaviate provider: {url}")
            provider = WeaviateProvider(url=url, collection=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            logger.warning("weaviate-client not installed. Falling back.")

    if db_type == "supabase":
        try:
            logger.info("Using Supabase Vector provider")
            provider = SupabaseVectorProvider(table=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            logger.warning("supabase package not installed. Falling back.")

    if db_type == "chromadb":
        try:
            chroma_dir = (settings.chroma_persist_dir if settings else None) or get_env(
                "CHROMA_PERSIST_DIR", context=context
            )
            logger.info(f"Using ChromaDB provider: {chroma_dir or 'in-memory'}")
            provider = ChromaDBProvider(persist_dir=chroma_dir, collection=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            logger.warning("chromadb not installed. Falling back.")

    # 2. 環境変数から自動検出（後方互換性）
    if (settings.qdrant_url if settings else None) or get_env("QDRANT_URL", context=context):
        try:
            url = (settings.qdrant_url if settings else None) or get_env("QDRANT_URL", context=context)
            logger.info(f"Auto-detected Qdrant: {url}")
            provider = QdrantProvider(url=url, collection=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            pass

    if get_env("WEAVIATE_URL", context=context):
        try:
            url = get_env("WEAVIATE_URL", context=context)
            logger.info(f"Auto-detected Weaviate: {url}")
            provider = WeaviateProvider(url=url, collection=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            pass

    if ((settings.supabase_url if settings else None) or get_env("SUPABASE_URL", context=context)) and (
        (settings.supabase_key if settings else None) or get_env("SUPABASE_KEY", context=context)
    ):
        try:
            logger.info("Auto-detected Supabase Vector")
            provider = SupabaseVectorProvider(table=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            pass

    if get_env("FAISS_INDEX_PATH", context=context):
        try:
            logger.info("Auto-detected FAISS")
            provider = FAISSProvider(collection=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            pass

    if get_env("CHROMA_PERSIST_DIR", context=context) or get_env("USE_CHROMADB", context=context):
        try:
            chroma_dir = get_env("CHROMA_PERSIST_DIR", context=context)
            logger.info(f"Auto-detected ChromaDB: {chroma_dir or 'in-memory'}")
            provider = ChromaDBProvider(persist_dir=chroma_dir, collection=collection)
            if context is None and not _new_instance:
                _vectordb_instance = provider
            return provider
        except ImportError:
            pass

    # 3. フォールバック: Mock（開発/テスト用）
    logger.info("No VectorDB config found. Using mock provider.")
    provider = MockVectorDBProvider(collection=collection)
    if context is None and not _new_instance:
        _vectordb_instance = provider
    return provider


def reset_vectordb() -> None:
    """VectorDBインスタンスをリセット（テスト用）."""
    global _vectordb_instance
    _vectordb_instance = None
