"""Redisバックエンド実装.

高速キャッシュとして使用し、短期記憶と中期記憶を保存します。
"""

import json
import logging
from datetime import datetime
from typing import Any

from agentflow.memory.distributed.backend_interface import MemoryBackend
from agentflow.memory.types import MemoryEntry, MemoryType


class RedisBackend(MemoryBackend):
    """Redisバックエンド実装.

    機能:
    - 高速キャッシュとして短期記憶を保存
    - TTL（Time To Live）による自動削除
    - Pub/Subによる分散同期

    データ構造:
    - Key: memory:{entry_id}
    - Value: JSON形式の記憶エントリ
    - Index: topic:{topic} -> Set of entry_ids
    """

    def __init__(
        self,
        host: str = "localhost",
        port: int = 6379,
        db: int = 0,
        password: str | None = None,
        ttl: int = 3600,
    ) -> None:
        """初期化.

        Args:
            host: Redisホスト
            port: Redisポート
            db: データベース番号
            password: パスワード
            ttl: デフォルトTTL（秒）
        """
        self._host = host
        self._port = port
        self._db = db
        self._password = password
        self._ttl = ttl
        self._logger = logging.getLogger(__name__)
        self._client: Any = None
        self._connected = False

    async def connect(self) -> None:
        """Redisに接続."""
        try:
            import redis.asyncio as redis

            self._client = redis.Redis(
                host=self._host,
                port=self._port,
                db=self._db,
                password=self._password,
                decode_responses=True,
            )
            # 接続テスト
            await self._client.ping()
            self._connected = True
            self._logger.info(f"Connected to Redis at {self._host}:{self._port}")
        except ImportError:
            raise ImportError("redis package is required. Install with: pip install redis")
        except Exception as e:
            raise ConnectionError(f"Failed to connect to Redis: {e}")

    async def disconnect(self) -> None:
        """Redisから切断."""
        if self._client:
            await self._client.close()
            self._connected = False
            self._logger.info("Disconnected from Redis")

    async def save(self, entry: MemoryEntry) -> None:
        """記憶を保存."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        # 記憶エントリをJSON化
        entry_data = {
            "id": entry.id,
            "content": entry.content,
            "topic": entry.topic,
            "timestamp": entry.timestamp.isoformat(),
            "memory_type": entry.memory_type.value,
            "importance_score": entry.importance_score,
            "metadata": entry.metadata,
        }

        # 記憶を保存
        key = f"memory:{entry.id}"
        await self._client.setex(key, self._ttl, json.dumps(entry_data, ensure_ascii=False))

        # トピックインデックスに追加
        if entry.topic:
            topic_key = f"topic:{entry.topic}"
            await self._client.sadd(topic_key, entry.id)
            await self._client.expire(topic_key, self._ttl)

        self._logger.debug(f"Saved memory {entry.id} to Redis")

    async def load(self, entry_id: str) -> MemoryEntry | None:
        """記憶を読み込み."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        key = f"memory:{entry_id}"
        data = await self._client.get(key)

        if not data:
            return None

        # JSONをパース
        entry_data = json.loads(data)

        return MemoryEntry(
            id=entry_data["id"],
            content=entry_data["content"],
            topic=entry_data["topic"],
            timestamp=datetime.fromisoformat(entry_data["timestamp"]),
            memory_type=MemoryType(entry_data["memory_type"]),
            importance_score=entry_data["importance_score"],
            metadata=entry_data.get("metadata", {}),
        )

    async def delete(self, entry_id: str) -> bool:
        """記憶を削除."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        key = f"memory:{entry_id}"
        result = await self._client.delete(key)
        return result > 0

    async def search(
        self,
        topic: str | None = None,
        limit: int = 10,
        min_importance: float = 0.0,
    ) -> list[MemoryEntry]:
        """記憶を検索."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        memories: list[MemoryEntry] = []

        if topic:
            # トピックインデックスから検索
            topic_key = f"topic:{topic}"
            entry_ids = await self._client.smembers(topic_key)
        else:
            # 全記憶を検索
            keys = await self._client.keys("memory:*")
            entry_ids = [key.split(":", 1)[1] for key in keys]

        # 記憶を読み込み
        for entry_id in entry_ids:
            entry = await self.load(entry_id)
            if entry and entry.importance_score >= min_importance:
                memories.append(entry)

        # 重要度でソート
        memories.sort(key=lambda e: e.importance_score, reverse=True)

        return memories[:limit]

    async def exists(self, entry_id: str) -> bool:
        """記憶の存在を確認."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        key = f"memory:{entry_id}"
        return await self._client.exists(key) > 0

    async def count(self, topic: str | None = None) -> int:
        """記憶の数を取得."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        if topic:
            topic_key = f"topic:{topic}"
            return await self._client.scard(topic_key)
        else:
            keys = await self._client.keys("memory:*")
            return len(keys)

    async def clear(self, topic: str | None = None) -> int:
        """記憶をクリア."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        if topic:
            # トピック内の記憶を削除
            topic_key = f"topic:{topic}"
            entry_ids = await self._client.smembers(topic_key)
            count = 0
            for entry_id in entry_ids:
                if await self.delete(entry_id):
                    count += 1
            await self._client.delete(topic_key)
            return count
        else:
            # 全記憶を削除
            keys = await self._client.keys("memory:*")
            if keys:
                await self._client.delete(*keys)
            topic_keys = await self._client.keys("topic:*")
            if topic_keys:
                await self._client.delete(*topic_keys)
            return len(keys)

    def get_status(self) -> dict[str, Any]:
        """バックエンドの状態を取得."""
        return {
            "backend": "redis",
            "host": self._host,
            "port": self._port,
            "db": self._db,
            "connected": self._connected,
            "ttl": self._ttl,
        }

