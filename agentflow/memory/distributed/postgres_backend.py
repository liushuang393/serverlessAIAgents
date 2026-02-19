"""PostgreSQLバックエンド実装.

永続化ストレージとして使用し、長期記憶を保存します。
"""

import json
import logging
from typing import Any

from agentflow.memory.distributed.backend_interface import MemoryBackend
from agentflow.memory.types import MemoryEntry, MemoryType

# オプション依存: テストでのモック（patch）が効くようにモジュールレベルで import
try:
    import asyncpg  # type: ignore[import-untyped]
except ImportError:
    asyncpg = None  # type: ignore[assignment]


class PostgresBackend(MemoryBackend):
    """PostgreSQLバックエンド実装.

    機能:
    - 永続化ストレージとして長期記憶を保存
    - インデックスによる高速検索
    - トランザクションによるデータ整合性

    テーブル構造:
    - memories: 記憶エントリ
        - id (TEXT PRIMARY KEY)
        - content (TEXT)
        - topic (TEXT)
        - timestamp (TIMESTAMP)
        - memory_type (TEXT)
        - importance_score (REAL)
        - metadata (JSONB)
    """

    def __init__(
        self,
        host: str = "localhost",
        port: int = 5432,
        database: str = "agentflow",
        user: str = "postgres",
        password: str | None = None,
    ) -> None:
        """初期化.

        Args:
            host: PostgreSQLホスト
            port: PostgreSQLポート
            database: データベース名
            user: ユーザー名
            password: パスワード
        """
        self._host = host
        self._port = port
        self._database = database
        self._user = user
        self._password = password
        self._logger = logging.getLogger(__name__)
        self._pool: Any = None
        self._connected = False

    async def connect(self) -> None:
        """PostgreSQLに接続."""
        if asyncpg is None:
            msg = "asyncpg package is required. Install with: pip install asyncpg"
            raise ImportError(msg)
        try:
            self._pool = await asyncpg.create_pool(
                host=self._host,
                port=self._port,
                database=self._database,
                user=self._user,
                password=self._password,
            )

            # テーブルを作成
            await self._create_tables()

            self._connected = True
            self._logger.info(
                f"Connected to PostgreSQL at {self._host}:{self._port}/{self._database}"
            )
        except Exception as e:
            msg = f"Failed to connect to PostgreSQL: {e}"
            raise ConnectionError(msg)

    async def disconnect(self) -> None:
        """PostgreSQLから切断."""
        if self._pool:
            await self._pool.close()
            self._connected = False
            self._logger.info("Disconnected from PostgreSQL")

    async def _create_tables(self) -> None:
        """テーブルを作成."""
        async with self._pool.acquire() as conn:
            await conn.execute(
                """
                CREATE TABLE IF NOT EXISTS memories (
                    id TEXT PRIMARY KEY,
                    content TEXT NOT NULL,
                    topic TEXT,
                    timestamp TIMESTAMP NOT NULL,
                    memory_type TEXT NOT NULL,
                    importance_score REAL NOT NULL,
                    metadata JSONB
                )
                """
            )

            # インデックスを作成
            await conn.execute("CREATE INDEX IF NOT EXISTS idx_topic ON memories(topic)")
            await conn.execute("CREATE INDEX IF NOT EXISTS idx_timestamp ON memories(timestamp)")
            await conn.execute(
                "CREATE INDEX IF NOT EXISTS idx_importance ON memories(importance_score)"
            )

    async def save(self, entry: MemoryEntry) -> None:
        """記憶を保存."""
        if not self._connected:
            msg = "Not connected to PostgreSQL"
            raise ConnectionError(msg)

        async with self._pool.acquire() as conn:
            await conn.execute(
                """
                INSERT INTO memories (id, content, topic, timestamp, memory_type, importance_score, metadata)
                VALUES ($1, $2, $3, $4, $5, $6, $7)
                ON CONFLICT (id) DO UPDATE SET
                    content = EXCLUDED.content,
                    topic = EXCLUDED.topic,
                    timestamp = EXCLUDED.timestamp,
                    memory_type = EXCLUDED.memory_type,
                    importance_score = EXCLUDED.importance_score,
                    metadata = EXCLUDED.metadata
                """,
                entry.id,
                entry.content,
                entry.topic,
                entry.timestamp,
                entry.memory_type.value,
                entry.importance_score,
                json.dumps(entry.metadata, ensure_ascii=False),
            )

        self._logger.debug(f"Saved memory {entry.id} to PostgreSQL")

    async def load(self, entry_id: str) -> MemoryEntry | None:
        """記憶を読み込み."""
        if not self._connected:
            msg = "Not connected to PostgreSQL"
            raise ConnectionError(msg)

        async with self._pool.acquire() as conn:
            row = await conn.fetchrow("SELECT * FROM memories WHERE id = $1", entry_id)

            if not row:
                return None

            return MemoryEntry(
                id=row["id"],
                content=row["content"],
                topic=row["topic"],
                timestamp=row["timestamp"],
                memory_type=MemoryType(row["memory_type"]),
                importance_score=row["importance_score"],
                metadata=json.loads(row["metadata"]) if row["metadata"] else {},
            )

    async def delete(self, entry_id: str) -> bool:
        """記憶を削除."""
        if not self._connected:
            msg = "Not connected to PostgreSQL"
            raise ConnectionError(msg)

        async with self._pool.acquire() as conn:
            result = await conn.execute("DELETE FROM memories WHERE id = $1", entry_id)
            return result.split()[-1] != "0"

    async def search(
        self,
        topic: str | None = None,
        limit: int = 10,
        min_importance: float = 0.0,
    ) -> list[MemoryEntry]:
        """記憶を検索."""
        if not self._connected:
            msg = "Not connected to PostgreSQL"
            raise ConnectionError(msg)

        async with self._pool.acquire() as conn:
            if topic:
                rows = await conn.fetch(
                    """
                    SELECT * FROM memories
                    WHERE topic = $1 AND importance_score >= $2
                    ORDER BY importance_score DESC, timestamp DESC
                    LIMIT $3
                    """,
                    topic,
                    min_importance,
                    limit,
                )
            else:
                rows = await conn.fetch(
                    """
                    SELECT * FROM memories
                    WHERE importance_score >= $1
                    ORDER BY importance_score DESC, timestamp DESC
                    LIMIT $2
                    """,
                    min_importance,
                    limit,
                )

            memories = []
            for row in rows:
                memories.append(
                    MemoryEntry(
                        id=row["id"],
                        content=row["content"],
                        topic=row["topic"],
                        timestamp=row["timestamp"],
                        memory_type=MemoryType(row["memory_type"]),
                        importance_score=row["importance_score"],
                        metadata=json.loads(row["metadata"]) if row["metadata"] else {},
                    )
                )

            return memories

    async def exists(self, entry_id: str) -> bool:
        """記憶の存在を確認."""
        if not self._connected:
            msg = "Not connected to PostgreSQL"
            raise ConnectionError(msg)

        async with self._pool.acquire() as conn:
            result = await conn.fetchval("SELECT COUNT(*) FROM memories WHERE id = $1", entry_id)
            return result > 0

    async def count(self, topic: str | None = None) -> int:
        """記憶の数を取得."""
        if not self._connected:
            msg = "Not connected to PostgreSQL"
            raise ConnectionError(msg)

        async with self._pool.acquire() as conn:
            if topic:
                result = await conn.fetchval(
                    "SELECT COUNT(*) FROM memories WHERE topic = $1", topic
                )
            else:
                result = await conn.fetchval("SELECT COUNT(*) FROM memories")
            return result

    async def clear(self, topic: str | None = None) -> int:
        """記憶をクリア."""
        if not self._connected:
            msg = "Not connected to PostgreSQL"
            raise ConnectionError(msg)

        async with self._pool.acquire() as conn:
            if topic:
                result = await conn.execute("DELETE FROM memories WHERE topic = $1", topic)
            else:
                result = await conn.execute("DELETE FROM memories")
            return int(result.split()[-1])

    def get_status(self) -> dict[str, Any]:
        """バックエンドの状態を取得."""
        return {
            "backend": "postgresql",
            "host": self._host,
            "port": self._port,
            "database": self._database,
            "user": self._user,
            "connected": self._connected,
        }
