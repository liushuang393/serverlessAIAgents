"""重要度自動調整モジュール.

アクセスパターンに基づいて記憶の重要度スコアを自動調整します。

拡張機能（Evo-Memory思想）:
- 主動的忘却ポリシー: 低価値記憶を自動削除
- 強化学習スコア: タスク成功/失敗に基づく調整
- 安定性レベル考慮: 結晶化記憶は減衰しにくい
"""

import asyncio
import logging
import math
from datetime import datetime, timedelta
from typing import Any

from agentflow.memory.types import MemoryEntry, MemoryStability


class ImportanceAdjuster:
    """重要度自動調整エンジン.

    機能:
    - アクセス頻度に基づく重要度ブースト
    - 時間経過による重要度減衰
    - 定期的な重要度再計算
    - 主動的忘却ポリシー（Evo-Memory）
    - 強化学習スコア反映

    アルゴリズム:
    - 重要度 = 基本スコア × アクセス係数 × 時間減衰係数 × 安定性係数
    - アクセス係数 = 1 + log(1 + アクセス回数)
    - 時間減衰係数 = exp(-経過日数 / 減衰定数)
    - 安定性係数 = 1.0（揮発性）/ 1.5（固定）/ 2.0（結晶化）
    """

    # 忘却判定の閾値
    FORGET_IMPORTANCE_THRESHOLD = 0.1  # 重要度がこれ以下
    FORGET_DAYS_THRESHOLD = 60  # 最終アクセスからの経過日数
    FORGET_REINFORCEMENT_THRESHOLD = -0.3  # 強化スコアがこれ以下

    # 安定性係数
    STABILITY_FACTORS = {
        MemoryStability.VOLATILE: 1.0,
        MemoryStability.CONSOLIDATED: 1.5,
        MemoryStability.CRYSTALLIZED: 2.0,
    }

    def __init__(
        self,
        decay_constant: float = 30.0,
        access_boost_factor: float = 0.1,
        recalculation_interval: int = 3600,
        forget_check_interval: int = 86400,  # 1日ごとに忘却チェック
    ) -> None:
        """初期化.

        Args:
            decay_constant: 時間減衰定数（日数）
            access_boost_factor: アクセスブースト係数
            recalculation_interval: 再計算間隔（秒）
            forget_check_interval: 忘却チェック間隔（秒）
        """
        self._logger = logging.getLogger(__name__)
        self._decay_constant = decay_constant
        self._access_boost_factor = access_boost_factor
        self._recalculation_interval = recalculation_interval
        self._forget_check_interval = forget_check_interval

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
        # 基本スコア（強化スコアを考慮）
        base_score = entry.get_effective_importance()

        # アクセス係数（記憶自体のアクセス回数も考慮）
        access_count = self._access_counts.get(entry.id, 0) + entry.access_count
        access_factor = 1.0 + self._access_boost_factor * math.log(1 + access_count)

        # 時間減衰係数
        days_elapsed = (datetime.now() - entry.timestamp).days
        decay_factor = math.exp(-days_elapsed / self._decay_constant)

        # 安定性係数（結晶化記憶は減衰しにくい）
        stability_factor = self.STABILITY_FACTORS.get(entry.stability, 1.0)

        # 最終スコア
        importance = base_score * access_factor * decay_factor * stability_factor

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

    # =========================================================================
    # 主動的忘却ポリシー（Evo-Memory思想）
    # =========================================================================

    def identify_forgettable(self, memories: list[MemoryEntry]) -> list[str]:
        """忘却対象の記憶を特定.

        以下の条件を全て満たす記憶を忘却対象とする:
        - 重要度が閾値以下
        - 最終アクセスから一定期間経過
        - 強化スコアが負（タスクに悪影響を与えた）
        - 結晶化されていない

        Args:
            memories: 記憶リスト

        Returns:
            忘却対象の記憶IDリスト
        """
        forgettable_ids: list[str] = []
        now = datetime.now()

        for entry in memories:
            # 結晶化記憶は忘却しない
            if entry.stability == MemoryStability.CRYSTALLIZED:
                continue

            # 固定済み記憶は慎重に判断
            if entry.stability == MemoryStability.CONSOLIDATED:
                threshold_multiplier = 0.5  # より厳しい条件
            else:
                threshold_multiplier = 1.0

            # 重要度チェック
            current_importance = self.calculate_importance(entry)
            if current_importance > self.FORGET_IMPORTANCE_THRESHOLD * threshold_multiplier:
                continue

            # アクセス期間チェック
            last_access = entry.last_accessed or entry.timestamp
            days_since_access = (now - last_access).days
            if days_since_access < self.FORGET_DAYS_THRESHOLD * threshold_multiplier:
                continue

            # 強化スコアチェック（負のフィードバックが多い）
            if entry.reinforcement_score > self.FORGET_REINFORCEMENT_THRESHOLD:
                continue

            forgettable_ids.append(entry.id)

        self._logger.info(f"忘却対象を特定: {len(forgettable_ids)}件")
        return forgettable_ids

    async def apply_reinforcement(
        self,
        entry: MemoryEntry,
        reward: float,
        context: str = "",
    ) -> None:
        """強化学習フィードバックを適用.

        タスクの成功/失敗に基づいて記憶の価値を調整する。

        Args:
            entry: 記憶エントリ
            reward: 報酬値（正=有用、負=有害）
            context: フィードバックコンテキスト
        """
        # 強化スコアを更新（累積、ただし範囲制限）
        entry.reinforcement_score = max(
            -1.0,
            min(1.0, entry.reinforcement_score + reward * 0.1)
        )

        # 履歴を記録
        if "reinforcement_history" not in entry.metadata:
            entry.metadata["reinforcement_history"] = []

        entry.metadata["reinforcement_history"].append({
            "timestamp": datetime.now().isoformat(),
            "reward": reward,
            "context": context,
        })

        # 継続的な正のフィードバックで安定性を昇格
        if entry.reinforcement_score > 0.5:
            if entry.stability == MemoryStability.VOLATILE:
                entry.stability = MemoryStability.CONSOLIDATED
                self._logger.debug(f"記憶 {entry.id} を固定済みに昇格")
            elif (
                entry.stability == MemoryStability.CONSOLIDATED
                and entry.reinforcement_score > 0.8
            ):
                entry.stability = MemoryStability.CRYSTALLIZED
                self._logger.debug(f"記憶 {entry.id} を結晶化に昇格")

