"""重要度自動調整モジュール.

アクセスパターンに基づいて記憶の重要度スコアを自動調整します。
"""

import asyncio
import logging
import math
from datetime import datetime, timedelta
from typing import Any

from agentflow.memory.types import MemoryEntry


class ImportanceAdjuster:
    """重要度自動調整エンジン.

    機能:
    - アクセス頻度に基づく重要度ブースト
    - 時間経過による重要度減衰
    - 定期的な重要度再計算

    アルゴリズム:
    - 重要度 = 基本スコア × アクセス係数 × 時間減衰係数
    - アクセス係数 = 1 + log(1 + アクセス回数)
    - 時間減衰係数 = exp(-経過日数 / 減衰定数)
    """

    def __init__(
        self,
        decay_constant: float = 30.0,
        access_boost_factor: float = 0.1,
        recalculation_interval: int = 3600,
    ) -> None:
        """初期化.

        Args:
            decay_constant: 時間減衰定数（日数）
            access_boost_factor: アクセスブースト係数
            recalculation_interval: 再計算間隔（秒）
        """
        self._logger = logging.getLogger(__name__)
        self._decay_constant = decay_constant
        self._access_boost_factor = access_boost_factor
        self._recalculation_interval = recalculation_interval

        # アクセス履歴
        self._access_counts: dict[str, int] = {}
        self._last_access: dict[str, datetime] = {}

        # 再計算タスク
        self._recalculation_task: asyncio.Task[None] | None = None
        self._running = False

    async def start(self) -> None:
        """重要度調整システムを開始."""
        self._running = True
        self._recalculation_task = asyncio.create_task(self._recalculation_loop())
        self._logger.info("Importance adjuster started")

    async def stop(self) -> None:
        """重要度調整システムを停止."""
        self._running = False
        if self._recalculation_task:
            self._recalculation_task.cancel()
            try:
                await self._recalculation_task
            except asyncio.CancelledError:
                pass
        self._logger.info("Importance adjuster stopped")

    def record_access(self, entry_id: str) -> None:
        """記憶へのアクセスを記録.

        Args:
            entry_id: 記憶エントリID
        """
        self._access_counts[entry_id] = self._access_counts.get(entry_id, 0) + 1
        self._last_access[entry_id] = datetime.now()

    def calculate_importance(self, entry: MemoryEntry) -> float:
        """重要度スコアを計算.

        Args:
            entry: 記憶エントリ

        Returns:
            調整後の重要度スコア（0.0～1.0）
        """
        # 基本スコア
        base_score = entry.importance_score

        # アクセス係数
        access_count = self._access_counts.get(entry.id, 0)
        access_factor = 1.0 + self._access_boost_factor * math.log(1 + access_count)

        # 時間減衰係数
        days_elapsed = (datetime.now() - entry.timestamp).days
        decay_factor = math.exp(-days_elapsed / self._decay_constant)

        # 最終スコア
        importance = base_score * access_factor * decay_factor

        return max(0.0, min(1.0, importance))

    async def adjust_importance(self, entry: MemoryEntry) -> float:
        """記憶の重要度を調整.

        Args:
            entry: 記憶エントリ

        Returns:
            調整後の重要度スコア
        """
        new_importance = self.calculate_importance(entry)
        entry.importance_score = new_importance
        return new_importance

    async def adjust_batch(self, entries: list[MemoryEntry]) -> list[MemoryEntry]:
        """複数の記憶の重要度を一括調整.

        Args:
            entries: 記憶エントリのリスト

        Returns:
            調整後の記憶エントリのリスト
        """
        for entry in entries:
            await self.adjust_importance(entry)
        return entries

    async def _recalculation_loop(self) -> None:
        """定期的な重要度再計算ループ."""
        while self._running:
            try:
                await asyncio.sleep(self._recalculation_interval)
                self._logger.debug("Recalculating importance scores...")
                # 実際の再計算はMemoryManagerから呼び出される
            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.error(f"Error in recalculation loop: {e}")

    def get_statistics(self) -> dict[str, Any]:
        """アクセス統計を取得.

        Returns:
            統計情報
        """
        total_accesses = sum(self._access_counts.values())
        unique_entries = len(self._access_counts)

        return {
            "total_accesses": total_accesses,
            "unique_entries": unique_entries,
            "average_accesses": total_accesses / unique_entries if unique_entries > 0 else 0,
            "decay_constant": self._decay_constant,
            "access_boost_factor": self._access_boost_factor,
        }

