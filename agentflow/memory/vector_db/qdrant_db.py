"""Qdrant Vector Database実装.

Qdrantを使用してベクトル埋め込みを保存・検索します。
"""

import json
import logging
from datetime import datetime
from typing import Any

from agentflow.memory.types import MemoryEntry, MemoryType
from agentflow.memory.vector_db.vector_db_interface import VectorDatabase


# オプション依存: テストでのモック（patch）が効くようにモジュールレベルで import
try:
    from qdrant_client import QdrantClient  # type: ignore[import-untyped]
except ImportError:
    QdrantClient = None  # type: ignore[assignment, misc]


class QdrantDB(VectorDatabase):
    """Qdrant Vector Database実装.

    機能:
    - オープンソースのベクトルデータベース
    - ローカルまたはクラウドで動作
    - 高速な類似度検索

    注意:
    - Qdrantサーバーが必要（ローカルまたはクラウド）
    - コレクションの事前作成が必要
    """

    def __init__(
        self,
        host: str = "localhost",
        port: int = 6333,
        collection_name: str = "memories",
        dimension: int = 384,
    ) -> None:
        """初期化.

        Args:
            host: Qdrantホスト
            port: Qdrantポート
            collection_name: コレクション名
            dimension: ベクトルの次元数
        """
        self._host = host
        self._port = port
        self._collection_name = collection_name
        self._dimension = dimension
        self._logger = logging.getLogger(__name__)
        self._client: Any = None
        self._connected = False

    async def connect(self) -> None:
        """Qdrantに接続."""
        if QdrantClient is None:
            msg = "qdrant-client package is required. Install with: pip install qdrant-client"
            raise ImportError(msg)
        try:
            # qdrant_client.models は QdrantClient がある場合のみ利用可能
            from qdrant_client.models import Distance, VectorParams  # type: ignore[import-untyped]

            self._client = QdrantClient(host=self._host, port=self._port)

            # コレクションを作成（存在しない場合）
            collections = self._client.get_collections().collections
            if not any(c.name == self._collection_name for c in collections):
                self._client.create_collection(
                    collection_name=self._collection_name,
                    vectors_config=VectorParams(size=self._dimension, distance=Distance.COSINE),
                )
                self._logger.info(f"Created Qdrant collection: {self._collection_name}")

            self._connected = True
            self._logger.info(f"Connected to Qdrant at {self._host}:{self._port}")
        except Exception as e:
            msg = f"Failed to connect to Qdrant: {e}"
            raise ConnectionError(msg)

    async def disconnect(self) -> None:
        """Qdrantから切断."""
        self._connected = False
        self._logger.info("Disconnected from Qdrant")

    async def upsert(self, entry: MemoryEntry, embedding: list[float]) -> None:
        """ベクトルを挿入/更新."""
        if not self._connected:
            msg = "Not connected to Qdrant"
            raise ConnectionError(msg)

        from qdrant_client.models import PointStruct

        payload = {
            "content": entry.content,
            "topic": entry.topic or "",
            "timestamp": entry.timestamp.isoformat(),
            "memory_type": entry.memory_type.value,
            "importance_score": entry.importance_score,
            "metadata": json.dumps(entry.metadata, ensure_ascii=False),
        }

        point = PointStruct(id=entry.id, vector=embedding, payload=payload)

        self._client.upsert(collection_name=self._collection_name, points=[point])
        self._logger.debug(f"Upserted vector {entry.id} to Qdrant")

    async def search(
        self,
        query_embedding: list[float],
        limit: int = 10,
        min_similarity: float = 0.0,
        topic: str | None = None,
    ) -> list[tuple[MemoryEntry, float]]:
        """ベクトル類似度検索."""
        if not self._connected:
            msg = "Not connected to Qdrant"
            raise ConnectionError(msg)

        from qdrant_client.models import FieldCondition, Filter, MatchValue

        # フィルタを構築
        query_filter = None
        if topic:
            query_filter = Filter(must=[FieldCondition(key="topic", match=MatchValue(value=topic))])

        # 検索実行
        results = self._client.search(
            collection_name=self._collection_name,
            query_vector=query_embedding,
            limit=limit,
            query_filter=query_filter,
            score_threshold=min_similarity,
        )

        # 結果を変換
        memories = []
        for result in results:
            payload = result.payload
            entry = MemoryEntry(
                id=str(result.id),
                content=payload["content"],
                topic=str(payload.get("topic") or "general"),
                timestamp=datetime.fromisoformat(payload["timestamp"]),
                memory_type=MemoryType(payload["memory_type"]),
                importance_score=payload["importance_score"],
                metadata=json.loads(payload["metadata"]) if payload.get("metadata") else {},
            )
            memories.append((entry, result.score))

        return memories

    async def delete(self, entry_id: str) -> bool:
        """ベクトルを削除."""
        if not self._connected:
            msg = "Not connected to Qdrant"
            raise ConnectionError(msg)

        self._client.delete(collection_name=self._collection_name, points_selector=[entry_id])
        return True

    async def clear(self, topic: str | None = None) -> int:
        """ベクトルをクリア."""
        if not self._connected:
            msg = "Not connected to Qdrant"
            raise ConnectionError(msg)

        if topic:
            from qdrant_client.models import FieldCondition, Filter, MatchValue

            # トピックでフィルタして削除
            self._client.delete(
                collection_name=self._collection_name,
                points_selector=Filter(must=[FieldCondition(key="topic", match=MatchValue(value=topic))]),
            )
        else:
            # 全削除（コレクションを再作成）
            self._client.delete_collection(collection_name=self._collection_name)
            from qdrant_client.models import Distance, VectorParams

            self._client.create_collection(
                collection_name=self._collection_name,
                vectors_config=VectorParams(size=self._dimension, distance=Distance.COSINE),
            )

        return 0  # Qdrantは削除数を返さない

    def get_status(self) -> dict[str, Any]:
        """データベースの状態を取得."""
        return {
            "database": "qdrant",
            "host": self._host,
            "port": self._port,
            "collection_name": self._collection_name,
            "dimension": self._dimension,
            "connected": self._connected,
        }
