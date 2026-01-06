# -*- coding: utf-8 -*-
"""RedisCheckpointer - Redis ベースの状態永続化.

本番環境での分散ワークフロー状態管理に使用。
agentflow/memory/distributed/redis_backend.py の設計パターンを参考。

使用例:
    >>> from agentflow.hitl import RedisCheckpointer
    >>> checkpointer = RedisCheckpointer(url="redis://localhost:6379/0")
    >>> await checkpointer.connect()
    >>> await checkpointer.save(checkpoint_data)
    >>> data = await checkpointer.load(checkpoint_id)

環境変数:
    REDIS_URL: Redis 接続 URL (例: redis://localhost:6379/0)
"""

from __future__ import annotations

import json
import logging
import os
from datetime import datetime
from typing import Any

from agentflow.hitl.checkpointer import Checkpointer, CheckpointData

logger = logging.getLogger(__name__)


class RedisCheckpointer(Checkpointer):
    """Redis ベースの Checkpointer.

    特徴:
        - 高速な読み書き
        - TTL による自動削除
        - 分散環境での共有
        - 親子関係のトラッキング

    データ構造:
        - Key: checkpoint:{checkpoint_id}
        - Value: JSON 形式の CheckpointData
        - Index: thread:{thread_id} -> Sorted Set (score=timestamp)
    """

    def __init__(
        self,
        url: str | None = None,
        host: str = "localhost",
        port: int = 6379,
        db: int = 0,
        password: str | None = None,
        ttl: int = 86400,  # 24 時間
        key_prefix: str = "agentflow",
    ) -> None:
        """初期化.

        Args:
            url: Redis URL (優先)
            host: Redis ホスト
            port: Redis ポート
            db: データベース番号
            password: パスワード
            ttl: TTL 秒数（デフォルト: 24時間）
            key_prefix: キープレフィックス
        """
        self._url = url or os.getenv("REDIS_URL")
        self._host = host
        self._port = port
        self._db = db
        self._password = password
        self._ttl = ttl
        self._key_prefix = key_prefix
        self._client: Any = None
        self._connected = False

    async def connect(self) -> None:
        """Redis に接続."""
        try:
            import redis.asyncio as redis

            if self._url:
                self._client = redis.from_url(self._url, decode_responses=True)
            else:
                self._client = redis.Redis(
                    host=self._host,
                    port=self._port,
                    db=self._db,
                    password=self._password,
                    decode_responses=True,
                )
            await self._client.ping()
            self._connected = True
            logger.info(f"RedisCheckpointer connected: {self._url or f'{self._host}:{self._port}'}")
        except ImportError:
            raise ImportError("redis package required: pip install redis")
        except Exception as e:
            raise ConnectionError(f"Failed to connect to Redis: {e}")

    async def disconnect(self) -> None:
        """Redis から切断."""
        if self._client:
            await self._client.close()
            self._connected = False
            logger.info("RedisCheckpointer disconnected")

    def _checkpoint_key(self, checkpoint_id: str) -> str:
        """チェックポイントキーを生成."""
        return f"{self._key_prefix}:checkpoint:{checkpoint_id}"

    def _thread_key(self, thread_id: str) -> str:
        """スレッドインデックスキーを生成."""
        return f"{self._key_prefix}:thread:{thread_id}"

    async def save(self, data: CheckpointData) -> str:
        """チェックポイントを保存."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        data.updated_at = datetime.utcnow()

        # JSON シリアライズ
        json_data = json.dumps(data.model_dump(), default=str, ensure_ascii=False)

        # チェックポイントを保存
        key = self._checkpoint_key(data.checkpoint_id)
        await self._client.setex(key, self._ttl, json_data)

        # スレッドインデックスに追加（Sorted Set: score=timestamp）
        thread_key = self._thread_key(data.thread_id)
        score = data.created_at.timestamp()
        await self._client.zadd(thread_key, {data.checkpoint_id: score})
        await self._client.expire(thread_key, self._ttl)

        logger.debug(f"Checkpoint saved: {data.checkpoint_id}")
        return data.checkpoint_id

    async def load(self, checkpoint_id: str) -> CheckpointData | None:
        """チェックポイントを読み込み."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        key = self._checkpoint_key(checkpoint_id)
        json_data = await self._client.get(key)

        if not json_data:
            return None

        data = json.loads(json_data)
        # datetime 復元
        data["created_at"] = datetime.fromisoformat(data["created_at"])
        data["updated_at"] = datetime.fromisoformat(data["updated_at"])
        return CheckpointData(**data)

    async def load_latest(self, thread_id: str) -> CheckpointData | None:
        """指定スレッドの最新チェックポイントを読み込み."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        thread_key = self._thread_key(thread_id)
        # Sorted Set から最新（最大スコア）を取得
        result = await self._client.zrevrange(thread_key, 0, 0)

        if not result:
            return None

        return await self.load(result[0])

    async def delete(self, checkpoint_id: str) -> bool:
        """チェックポイントを削除."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        # まずデータを取得してスレッドIDを得る
        data = await self.load(checkpoint_id)
        if not data:
            return False

        # チェックポイントを削除
        key = self._checkpoint_key(checkpoint_id)
        await self._client.delete(key)

        # スレッドインデックスからも削除
        thread_key = self._thread_key(data.thread_id)
        await self._client.zrem(thread_key, checkpoint_id)

        return True

    async def list_by_thread(self, thread_id: str) -> list[CheckpointData]:
        """指定スレッドの全チェックポイントを取得."""
        if not self._connected:
            raise ConnectionError("Not connected to Redis")

        thread_key = self._thread_key(thread_id)
        # 降順（新しい順）で取得
        checkpoint_ids = await self._client.zrevrange(thread_key, 0, -1)

        checkpoints = []
        for cid in checkpoint_ids:
            data = await self.load(cid)
            if data:
                checkpoints.append(data)

        return checkpoints

    def is_connected(self) -> bool:
        """接続状態を確認."""
        return self._connected

    def get_status(self) -> dict[str, Any]:
        """ステータスを取得."""
        return {
            "backend": "redis",
            "url": self._url,
            "host": self._host,
            "port": self._port,
            "db": self._db,
            "ttl": self._ttl,
            "connected": self._connected,
        }

