"""分散記憶マネージャー.

Redis（キャッシュ）+ PostgreSQL（永続化）の2層アーキテクチャを提供します。
"""

import logging
from typing import Any

from agentflow.memory.distributed.backend_interface import MemoryBackend
from agentflow.memory.types import MemoryEntry


class DistributedMemoryManager:
    """分散記憶マネージャー.

    アーキテクチャ:
    - Redis: 高速キャッシュ層（短期記憶）
    - PostgreSQL: 永続化層（長期記憶）

    動作:
    1. save(): Redis + PostgreSQL に保存
    2. load(): Redis → PostgreSQL の順で検索
    3. search(): Redis → PostgreSQL の順で検索
    4. delete(): Redis + PostgreSQL から削除

    利点:
    - 高速アクセス（Redisキャッシュ）
    - データ永続化（PostgreSQL）
    - 分散環境対応（複数インスタンス間で共有）
    """

    def __init__(
        self,
        cache_backend: MemoryBackend,
        storage_backend: MemoryBackend,
    ) -> None:
        """初期化.

        Args:
            cache_backend: キャッシュバックエンド（Redis）
            storage_backend: ストレージバックエンド（PostgreSQL）
        """
        self._cache = cache_backend
        self._storage = storage_backend
        self._logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """分散記憶システムを開始."""
        await self._cache.connect()
        await self._storage.connect()
        self._logger.info("Distributed memory system started")

    async def stop(self) -> None:
        """分散記憶システムを停止."""
        await self._cache.disconnect()
        await self._storage.disconnect()
        self._logger.info("Distributed memory system stopped")

    async def save(self, entry: MemoryEntry) -> None:
        """記憶を保存.

        Args:
            entry: 記憶エントリ
        """
        # キャッシュと永続化の両方に保存
        await self._cache.save(entry)
        await self._storage.save(entry)
        self._logger.debug(f"Saved memory {entry.id} to distributed storage")

    async def load(self, entry_id: str) -> MemoryEntry | None:
        """記憶を読み込み.

        Args:
            entry_id: 記憶エントリID

        Returns:
            記憶エントリ（存在しない場合はNone）
        """
        # キャッシュから検索
        entry = await self._cache.load(entry_id)
        if entry:
            self._logger.debug(f"Loaded memory {entry_id} from cache")
            return entry

        # 永続化ストレージから検索
        entry = await self._storage.load(entry_id)
        if entry:
            # キャッシュに保存
            await self._cache.save(entry)
            self._logger.debug(f"Loaded memory {entry_id} from storage and cached")
            return entry

        return None

    async def delete(self, entry_id: str) -> bool:
        """記憶を削除.

        Args:
            entry_id: 記憶エントリID

        Returns:
            削除成功の場合True
        """
        # キャッシュと永続化の両方から削除
        cache_deleted = await self._cache.delete(entry_id)
        storage_deleted = await self._storage.delete(entry_id)
        return cache_deleted or storage_deleted

    async def search(
        self,
        topic: str | None = None,
        limit: int = 10,
        min_importance: float = 0.0,
    ) -> list[MemoryEntry]:
        """記憶を検索.

        Args:
            topic: トピック名（Noneの場合は全て）
            limit: 最大取得数
            min_importance: 最小重要度

        Returns:
            記憶エントリのリスト
        """
        # キャッシュから検索
        cache_memories = await self._cache.search(topic, limit, min_importance)

        # キャッシュで十分な結果が得られた場合
        if len(cache_memories) >= limit:
            return cache_memories[:limit]

        # 永続化ストレージから追加検索
        storage_memories = await self._storage.search(topic, limit, min_importance)

        # 重複を除去してマージ
        memory_dict = {m.id: m for m in cache_memories}
        for m in storage_memories:
            if m.id not in memory_dict:
                memory_dict[m.id] = m

        # 重要度でソート
        all_memories = list(memory_dict.values())
        all_memories.sort(key=lambda e: e.importance_score, reverse=True)

        return all_memories[:limit]

    async def exists(self, entry_id: str) -> bool:
        """記憶の存在を確認.

        Args:
            entry_id: 記憶エントリID

        Returns:
            存在する場合True
        """
        return await self._cache.exists(entry_id) or await self._storage.exists(entry_id)

    async def count(self, topic: str | None = None) -> int:
        """記憶の数を取得.

        Args:
            topic: トピック名（Noneの場合は全て）

        Returns:
            記憶の数
        """
        # 永続化ストレージから取得（正確な数）
        return await self._storage.count(topic)

    async def clear(self, topic: str | None = None) -> int:
        """記憶をクリア.

        Args:
            topic: トピック名（Noneの場合は全て）

        Returns:
            削除した記憶の数
        """
        # キャッシュと永続化の両方をクリア
        cache_count = await self._cache.clear(topic)
        storage_count = await self._storage.clear(topic)
        return max(cache_count, storage_count)

    def get_status(self) -> dict[str, Any]:
        """分散記憶システムの状態を取得.

        Returns:
            状態情報
        """
        return {
            "cache": self._cache.get_status(),
            "storage": self._storage.get_status(),
        }
