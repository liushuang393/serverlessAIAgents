# -*- coding: utf-8 -*-
"""PostgresCheckpointer - PostgreSQL ベースの状態永続化.

本番環境での永続的なワークフロー状態管理に使用。
agentflow/memory/distributed/postgres_backend.py の設計パターンを参考。

使用例:
    >>> from agentflow.hitl import PostgresCheckpointer
    >>> checkpointer = PostgresCheckpointer(url="postgresql://user:pass@localhost/db")
    >>> await checkpointer.connect()
    >>> await checkpointer.save(checkpoint_data)
    >>> data = await checkpointer.load(checkpoint_id)

環境変数:
    DATABASE_URL: PostgreSQL 接続 URL
"""

from __future__ import annotations

import json
import logging
import os
from datetime import datetime
from typing import Any

from agentflow.hitl.checkpointer import Checkpointer, CheckpointData

logger = logging.getLogger(__name__)


class PostgresCheckpointer(Checkpointer):
    """PostgreSQL ベースの Checkpointer.

    特徴:
        - 永続的な状態保存
        - ACID トランザクション
        - インデックスによる高速検索
        - 親子関係のトラッキング

    テーブル構造:
        checkpoints:
            - checkpoint_id (TEXT PRIMARY KEY)
            - thread_id (TEXT NOT NULL)
            - flow_id (TEXT)
            - node_id (TEXT)
            - state (JSONB)
            - inputs (JSONB)
            - results (JSONB)
            - interrupt_payload (JSONB)
            - parent_checkpoint_id (TEXT)
            - metadata (JSONB)
            - created_at (TIMESTAMP)
            - updated_at (TIMESTAMP)
    """

    def __init__(
        self,
        url: str | None = None,
        host: str = "localhost",
        port: int = 5432,
        database: str = "agentflow",
        user: str = "postgres",
        password: str | None = None,
        table_name: str = "checkpoints",
    ) -> None:
        """初期化.

        Args:
            url: PostgreSQL URL (優先)
            host: ホスト
            port: ポート
            database: データベース名
            user: ユーザー名
            password: パスワード
            table_name: テーブル名
        """
        self._url = url or os.getenv("DATABASE_URL")
        self._host = host
        self._port = port
        self._database = database
        self._user = user
        self._password = password
        self._table_name = table_name
        self._pool: Any = None
        self._connected = False

    async def connect(self) -> None:
        """PostgreSQL に接続."""
        try:
            import asyncpg

            if self._url:
                self._pool = await asyncpg.create_pool(self._url)
            else:
                self._pool = await asyncpg.create_pool(
                    host=self._host,
                    port=self._port,
                    database=self._database,
                    user=self._user,
                    password=self._password,
                )

            await self._create_table()
            self._connected = True
            logger.info(f"PostgresCheckpointer connected: {self._url or self._database}")
        except ImportError:
            raise ImportError("asyncpg package required: pip install asyncpg")
        except Exception as e:
            raise ConnectionError(f"Failed to connect to PostgreSQL: {e}")

    async def disconnect(self) -> None:
        """PostgreSQL から切断."""
        if self._pool:
            await self._pool.close()
            self._connected = False
            logger.info("PostgresCheckpointer disconnected")

    async def _create_table(self) -> None:
        """テーブルを作成."""
        async with self._pool.acquire() as conn:
            await conn.execute(f"""
                CREATE TABLE IF NOT EXISTS {self._table_name} (
                    checkpoint_id TEXT PRIMARY KEY,
                    thread_id TEXT NOT NULL,
                    flow_id TEXT,
                    node_id TEXT,
                    state JSONB DEFAULT '{{}}',
                    inputs JSONB DEFAULT '{{}}',
                    results JSONB DEFAULT '{{}}',
                    interrupt_payload JSONB,
                    parent_checkpoint_id TEXT,
                    metadata JSONB DEFAULT '{{}}',
                    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
                    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
                )
            """)
            # インデックス作成
            await conn.execute(
                f"CREATE INDEX IF NOT EXISTS idx_{self._table_name}_thread "
                f"ON {self._table_name}(thread_id, created_at DESC)"
            )
            await conn.execute(
                f"CREATE INDEX IF NOT EXISTS idx_{self._table_name}_parent "
                f"ON {self._table_name}(parent_checkpoint_id)"
            )

    async def save(self, data: CheckpointData) -> str:
        """チェックポイントを保存."""
        if not self._connected:
            raise ConnectionError("Not connected to PostgreSQL")

        data.updated_at = datetime.utcnow()

        async with self._pool.acquire() as conn:
            await conn.execute(
                f"""
                INSERT INTO {self._table_name}
                    (checkpoint_id, thread_id, flow_id, node_id, state, inputs,
                     results, interrupt_payload, parent_checkpoint_id, metadata,
                     created_at, updated_at)
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
                ON CONFLICT (checkpoint_id) DO UPDATE SET
                    state = EXCLUDED.state,
                    inputs = EXCLUDED.inputs,
                    results = EXCLUDED.results,
                    interrupt_payload = EXCLUDED.interrupt_payload,
                    metadata = EXCLUDED.metadata,
                    updated_at = EXCLUDED.updated_at
                """,
                data.checkpoint_id,
                data.thread_id,
                data.flow_id,
                data.node_id,
                json.dumps(data.state, ensure_ascii=False),
                json.dumps(data.inputs, ensure_ascii=False),
                json.dumps(data.results, ensure_ascii=False),
                json.dumps(data.interrupt_payload, ensure_ascii=False) if data.interrupt_payload else None,
                data.parent_checkpoint_id,
                json.dumps(data.metadata, ensure_ascii=False),
                data.created_at,
                data.updated_at,
            )

        logger.debug(f"Checkpoint saved: {data.checkpoint_id}")
        return data.checkpoint_id

    def _row_to_checkpoint(self, row: Any) -> CheckpointData:
        """行データを CheckpointData に変換."""
        return CheckpointData(
            checkpoint_id=row["checkpoint_id"],
            thread_id=row["thread_id"],
            flow_id=row["flow_id"],
            node_id=row["node_id"],
            state=json.loads(row["state"]) if row["state"] else {},
            inputs=json.loads(row["inputs"]) if row["inputs"] else {},
            results=json.loads(row["results"]) if row["results"] else {},
            interrupt_payload=json.loads(row["interrupt_payload"]) if row["interrupt_payload"] else None,
            parent_checkpoint_id=row["parent_checkpoint_id"],
            metadata=json.loads(row["metadata"]) if row["metadata"] else {},
            created_at=row["created_at"],
            updated_at=row["updated_at"],
        )

    async def load(self, checkpoint_id: str) -> CheckpointData | None:
        """チェックポイントを読み込み."""
        if not self._connected:
            raise ConnectionError("Not connected to PostgreSQL")

        async with self._pool.acquire() as conn:
            row = await conn.fetchrow(
                f"SELECT * FROM {self._table_name} WHERE checkpoint_id = $1",
                checkpoint_id,
            )
            if not row:
                return None
            return self._row_to_checkpoint(row)

    async def load_latest(self, thread_id: str) -> CheckpointData | None:
        """指定スレッドの最新チェックポイントを読み込み."""
        if not self._connected:
            raise ConnectionError("Not connected to PostgreSQL")

        async with self._pool.acquire() as conn:
            row = await conn.fetchrow(
                f"""
                SELECT * FROM {self._table_name}
                WHERE thread_id = $1
                ORDER BY created_at DESC
                LIMIT 1
                """,
                thread_id,
            )
            if not row:
                return None
            return self._row_to_checkpoint(row)

    async def delete(self, checkpoint_id: str) -> bool:
        """チェックポイントを削除."""
        if not self._connected:
            raise ConnectionError("Not connected to PostgreSQL")

        async with self._pool.acquire() as conn:
            result = await conn.execute(
                f"DELETE FROM {self._table_name} WHERE checkpoint_id = $1",
                checkpoint_id,
            )
            return result.split()[-1] != "0"

    async def list_by_thread(self, thread_id: str) -> list[CheckpointData]:
        """指定スレッドの全チェックポイントを取得."""
        if not self._connected:
            raise ConnectionError("Not connected to PostgreSQL")

        async with self._pool.acquire() as conn:
            rows = await conn.fetch(
                f"""
                SELECT * FROM {self._table_name}
                WHERE thread_id = $1
                ORDER BY created_at DESC
                """,
                thread_id,
            )
            return [self._row_to_checkpoint(row) for row in rows]

    def is_connected(self) -> bool:
        """接続状態を確認."""
        return self._connected

    def get_status(self) -> dict[str, Any]:
        """ステータスを取得."""
        return {
            "backend": "postgresql",
            "url": self._url,
            "host": self._host,
            "port": self._port,
            "database": self._database,
            "table_name": self._table_name,
            "connected": self._connected,
        }

