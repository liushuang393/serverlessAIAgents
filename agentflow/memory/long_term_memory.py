"""AgentFlow Memory System - Long-Term Memory (Light3).

長期記憶: オンライン追加 + オフライン並行統合。

参考論文: LightMem Light3
- オンライン追加書き込み（ゼロ遅延）
- オフライン並行マージ
- 更新キュー管理
- タイムスタンプベースの統合
"""

from __future__ import annotations

import asyncio
import contextlib
import logging
from datetime import datetime
from typing import Any

from agentflow.memory.types import MemoryEntry, MemoryType, UpdateQueue


class LongTermMemory:
    """長期記憶（Light3）.

    職責:
    - オンライン追加書き込み（ゼロ遅延）
    - オフライン並行マージ
    - 更新キュー管理
    - 実行時間削減（1.7-12倍）

    Example:
        >>> ltm = LongTermMemory()
        >>> await ltm.store(entry)  # オンライン追加（即座に返る）
        >>> await ltm.consolidate()  # オフライン統合（バックグラウンド）
    """

    def __init__(
        self,
        consolidation_interval: int = 300,  # 5分
        enable_auto_consolidation: bool = True,
    ) -> None:
        """初期化.

        Args:
            consolidation_interval: 統合間隔（秒）
            enable_auto_consolidation: 自動統合を有効化
        """
        self._consolidation_interval = consolidation_interval
        self._enable_auto_consolidation = enable_auto_consolidation
        self._memories: dict[str, MemoryEntry] = {}
        self._update_queues: dict[str, UpdateQueue] = {}
        self._consolidation_task: asyncio.Task[None] | None = None
        self._logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """長期記憶システムを開始.

        自動統合が有効な場合、バックグラウンドタスクを開始。
        """
        if self._enable_auto_consolidation and self._consolidation_task is None:
            self._consolidation_task = asyncio.create_task(self._auto_consolidation_loop())
            self._logger.info("Auto-consolidation started")

    async def stop(self) -> None:
        """長期記憶システムを停止.

        バックグラウンドタスクを停止し、最終統合を実行。
        """
        if self._consolidation_task:
            self._consolidation_task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._consolidation_task
            self._consolidation_task = None

        # 最終統合
        await self.consolidate()
        self._logger.info("Long-term memory stopped")

    async def store(self, entry: MemoryEntry) -> None:
        """記憶エントリを保存（オンライン追加）.

        Args:
            entry: 記憶エントリ
        """
        # オンライン追加: 即座に書き込み（ゼロ遅延）
        entry.memory_type = MemoryType.LONG_TERM
        self._memories[entry.id] = entry

        self._logger.debug(f"Stored entry: {entry.id} (topic: {entry.topic})")

    async def update(self, entry_id: str, update_data: dict[str, Any]) -> None:
        """記憶エントリを更新（更新キューに追加）.

        Args:
            entry_id: 記憶エントリID
            update_data: 更新データ
        """
        # 更新キューに追加（オンライン）
        if entry_id not in self._update_queues:
            self._update_queues[entry_id] = UpdateQueue(entry_id=entry_id)

        self._update_queues[entry_id].add_update(update_data)

        self._logger.debug(f"Queued update for entry: {entry_id}")

    async def consolidate(self) -> int:
        """オフライン統合を実行.

        Returns:
            統合された記憶エントリ数
        """
        if not self._update_queues:
            return 0

        self._logger.info(f"Starting consolidation: {len(self._update_queues)} queues")

        # 並行マージ: 各エントリの更新キューを並行処理
        tasks = [self._merge_updates(entry_id, queue) for entry_id, queue in self._update_queues.items()]

        results = await asyncio.gather(*tasks, return_exceptions=True)

        # 成功した統合数をカウント
        consolidated_count = sum(1 for r in results if r is True)

        # 統合済みキューをクリア
        self._update_queues.clear()

        self._logger.info(f"Consolidation completed: {consolidated_count} entries")

        return consolidated_count

    async def _merge_updates(self, entry_id: str, queue: UpdateQueue) -> bool:
        """更新キューをマージ.

        Args:
            entry_id: 記憶エントリID
            queue: 更新キュー

        Returns:
            成功した場合True
        """
        if entry_id not in self._memories:
            self._logger.warning(f"Entry not found: {entry_id}")
            return False

        entry = self._memories[entry_id]

        # タイムスタンプの新しい更新のみを適用
        for update in queue.updates:
            update_timestamp = datetime.fromisoformat(update["timestamp"])
            if update_timestamp > entry.timestamp:
                # 更新を適用
                if "content" in update:
                    entry.content = update["content"]
                if "importance_score" in update:
                    entry.importance_score = update["importance_score"]
                if "metadata" in update:
                    entry.metadata.update(update["metadata"])

                entry.timestamp = update_timestamp

        return True

    async def _auto_consolidation_loop(self) -> None:
        """自動統合ループ（バックグラウンド）."""
        while True:
            try:
                await asyncio.sleep(self._consolidation_interval)
                await self.consolidate()
            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.exception(f"Consolidation error: {e}")

    def retrieve(
        self,
        topic: str | None = None,
        limit: int = 10,
        min_importance: float = 0.0,
    ) -> list[MemoryEntry]:
        """記憶エントリを検索.

        Args:
            topic: トピック名（Noneの場合は全て）
            limit: 最大取得数
            min_importance: 最小重要度

        Returns:
            記憶エントリのリスト
        """
        # フィルタリング
        filtered = [
            entry
            for entry in self._memories.values()
            if (topic is None or entry.topic == topic) and entry.importance_score >= min_importance
        ]

        # 重要度とタイムスタンプでソート
        sorted_entries = sorted(
            filtered,
            key=lambda e: (e.importance_score, e.timestamp),
            reverse=True,
        )

        return sorted_entries[:limit]
