"""Pinecone Vector Database実装.

Pineconeを使用してベクトル埋め込みを保存・検索します。
"""

import json
import logging
from typing import Any

from agentflow.memory.types import MemoryEntry, MemoryType
from agentflow.memory.vector_db.vector_db_interface import VectorDatabase

# オプション依存: テストでのモック（patch）が効くようにモジュールレベルで import
try:
    from pinecone import Pinecone  # type: ignore[import-untyped]
except ImportError:
    Pinecone = None  # type: ignore[assignment, misc]


class PineconeDB(VectorDatabase):
    """Pinecone Vector Database実装.

    機能:
    - クラウドベースのベクトルデータベース
    - 高速な類似度検索
    - スケーラブルなアーキテクチャ

    注意:
    - Pinecone APIキーが必要
    - インデックスの事前作成が必要
    """

    def __init__(
        self,
        api_key: str,
        environment: str,
        index_name: str,
    ) -> None:
        """初期化.

        Args:
            api_key: Pinecone APIキー
            environment: Pinecone環境（例: "us-west1-gcp"）
            index_name: インデックス名
        """
        self._api_key = api_key
        self._environment = environment
        self._index_name = index_name
        self._logger = logging.getLogger(__name__)
        self._index: Any = None
        self._connected = False

    async def connect(self) -> None:
        """Pineconeに接続."""
        if Pinecone is None:
            msg = "pinecone-client package is required. Install with: pip install pinecone-client"
            raise ImportError(msg)
        try:
            pc = Pinecone(api_key=self._api_key)
            self._index = pc.Index(self._index_name)
            self._connected = True
            self._logger.info(f"Connected to Pinecone index: {self._index_name}")
        except ImportError:
            # テスト時に side_effect=ImportError でパッチされた場合も正しいメッセージを返す
            msg = "pinecone-client package is required. Install with: pip install pinecone-client"
            raise ImportError(msg)
        except Exception as e:
            msg = f"Failed to connect to Pinecone: {e}"
            raise ConnectionError(msg)

    async def disconnect(self) -> None:
        """Pineconeから切断."""
        self._connected = False
        self._logger.info("Disconnected from Pinecone")

    async def upsert(self, entry: MemoryEntry, embedding: list[float]) -> None:
        """ベクトルを挿入/更新."""
        if not self._connected:
            msg = "Not connected to Pinecone"
            raise ConnectionError(msg)

        metadata = {
            "content": entry.content,
            "topic": entry.topic or "",
            "timestamp": entry.timestamp.isoformat(),
            "memory_type": entry.memory_type.value,
            "importance_score": entry.importance_score,
            "metadata": json.dumps(entry.metadata, ensure_ascii=False),
        }

        self._index.upsert(vectors=[(entry.id, embedding, metadata)])
        self._logger.debug(f"Upserted vector {entry.id} to Pinecone")

    async def search(
        self,
        query_embedding: list[float],
        limit: int = 10,
        min_similarity: float = 0.0,
        topic: str | None = None,
    ) -> list[tuple[MemoryEntry, float]]:
        """ベクトル類似度検索."""
        if not self._connected:
            msg = "Not connected to Pinecone"
            raise ConnectionError(msg)

        # フィルタを構築
        filter_dict = {}
        if topic:
            filter_dict["topic"] = topic

        # 検索実行
        results = self._index.query(
            vector=query_embedding,
            top_k=limit,
            include_metadata=True,
            filter=filter_dict if filter_dict else None,
        )

        # 結果を変換
        memories = []
        for match in results.matches:
            if match.score < min_similarity:
                continue

            metadata = match.metadata
            entry = MemoryEntry(
                id=match.id,
                content=metadata["content"],
                topic=str(metadata.get("topic") or "general"),
                timestamp=metadata["timestamp"],
                memory_type=MemoryType(metadata["memory_type"]),
                importance_score=metadata["importance_score"],
                metadata=json.loads(metadata["metadata"]) if metadata.get("metadata") else {},
            )
            memories.append((entry, match.score))

        return memories

    async def delete(self, entry_id: str) -> bool:
        """ベクトルを削除."""
        if not self._connected:
            msg = "Not connected to Pinecone"
            raise ConnectionError(msg)

        self._index.delete(ids=[entry_id])
        return True

    async def clear(self, topic: str | None = None) -> int:
        """ベクトルをクリア."""
        if not self._connected:
            msg = "Not connected to Pinecone"
            raise ConnectionError(msg)

        if topic:
            # トピックでフィルタして削除
            self._index.delete(filter={"topic": topic})
        else:
            # 全削除
            self._index.delete(delete_all=True)

        return 0  # Pineconeは削除数を返さない

    def get_status(self) -> dict[str, Any]:
        """データベースの状態を取得."""
        return {
            "database": "pinecone",
            "environment": self._environment,
            "index_name": self._index_name,
            "connected": self._connected,
        }
