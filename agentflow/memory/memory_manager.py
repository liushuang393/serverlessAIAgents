"""AgentFlow Memory System - Memory Manager.

記憶システムの統合マネージャー。

LightMemの3段階記憶システムを統合管理:
- Light1: 感覚記憶（予圧縮）
- Light2: 短期記憶（トピックバッファ）
- Light3: 長期記憶（オフライン統合）

拡張機能（自動最適化、ユーザー透過的）:
- 記憶蒸留: 類似記憶を抽象知識に自動変換
- 主動忘却: 低価値記憶を自動削除
- 強化学習: タスク結果に基づく記憶価値調整
"""

from __future__ import annotations

import asyncio
import contextlib
import logging
from typing import TYPE_CHECKING, Any

from agentflow.memory.importance_adjuster import ImportanceAdjuster
from agentflow.memory.long_term_memory import LongTermMemory
from agentflow.memory.memory_distiller import MemoryDistiller
from agentflow.memory.sensory_memory import SensoryMemory
from agentflow.memory.short_term_memory import ShortTermMemory
from agentflow.memory.vector_search import VectorSearch


if TYPE_CHECKING:
    from agentflow.memory.types import CompressionConfig, MemoryEntry


class MemoryManager:
    """記憶システム統合マネージャー.

    職責:
    - 3つのメモリレイヤーの統合管理
    - 記憶の自動フロー（Sensory → Short-Term → Long-Term）
    - ライフサイクル管理
    - 検索インターフェース

    Example:
        >>> manager = MemoryManager()
        >>> await manager.start()
        >>> await manager.remember("重要な情報", topic="AI")
        >>> memories = await manager.recall(topic="AI")
        >>> await manager.stop()
    """

    def __init__(
        self,
        compression_config: CompressionConfig | None = None,
        token_threshold: int = 1000,
        consolidation_interval: int = 300,
        llm_client: Any | None = None,
        enable_vector_search: bool = False,
        embedding_dim: int = 384,
        enable_importance_adjustment: bool = False,
        decay_constant: float = 30.0,
        access_boost_factor: float = 0.1,
        # 自動最適化オプション（デフォルト有効）
        enable_auto_distill: bool = True,
        enable_auto_forget: bool = True,
        distill_interval: int = 3600,  # 1時間ごと
        forget_interval: int = 86400,  # 1日ごと
    ) -> None:
        """初期化.

        Args:
            compression_config: 圧縮設定
            token_threshold: 短期記憶のToken閾値
            consolidation_interval: 長期記憶の統合間隔（秒）
            llm_client: LLMクライアント（要約生成用）
            enable_vector_search: ベクトル検索を有効化
            embedding_dim: 埋め込みベクトルの次元数
            enable_importance_adjustment: 重要度自動調整を有効化
            decay_constant: 時間減衰定数（日数）
            access_boost_factor: アクセスブースト係数
            enable_auto_distill: 自動蒸留を有効化
            enable_auto_forget: 自動忘却を有効化
            distill_interval: 蒸留間隔（秒）
            forget_interval: 忘却チェック間隔（秒）
        """
        self._sensory = SensoryMemory(compression_config)
        self._short_term = ShortTermMemory(token_threshold, llm_client)
        self._long_term = LongTermMemory(consolidation_interval)
        self._logger = logging.getLogger(__name__)
        self._llm_client = llm_client

        # ベクトル検索エンジン（オプション）
        self._enable_vector_search = enable_vector_search
        self._vector_search = VectorSearch(embedding_dim) if enable_vector_search else None

        # 重要度自動調整エンジン（オプション）
        self._enable_importance_adjustment = enable_importance_adjustment
        self._importance_adjuster = (
            ImportanceAdjuster(decay_constant, access_boost_factor) if enable_importance_adjustment else None
        )

        # 自動蒸留エンジン（Evo-Memory）
        self._enable_auto_distill = enable_auto_distill
        self._distill_interval = distill_interval
        self._distiller = MemoryDistiller(llm_client=llm_client) if enable_auto_distill else None
        self._distill_task: asyncio.Task[None] | None = None

        # 自動忘却フラグ
        self._enable_auto_forget = enable_auto_forget
        self._forget_interval = forget_interval
        self._forget_task: asyncio.Task[None] | None = None

    async def start(self) -> None:
        """記憶システムを開始."""
        await self._long_term.start()
        if self._importance_adjuster:
            await self._importance_adjuster.start()

        # 自動蒸留タスクを開始
        if self._enable_auto_distill and self._distiller:
            self._distill_task = asyncio.create_task(self._auto_distill_loop())
            self._logger.debug("自動蒸留タスクを開始")

        # 自動忘却タスクを開始
        if self._enable_auto_forget and self._importance_adjuster:
            self._forget_task = asyncio.create_task(self._auto_forget_loop())
            self._logger.debug("自動忘却タスクを開始")

        self._logger.info("Memory system started")

    async def stop(self) -> None:
        """記憶システムを停止."""
        # 自動タスクを停止
        if self._distill_task:
            self._distill_task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._distill_task

        if self._forget_task:
            self._forget_task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._forget_task

        await self._long_term.stop()
        if self._importance_adjuster:
            await self._importance_adjuster.stop()
        self._logger.info("Memory system stopped")

    async def remember(
        self,
        text: str,
        topic: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> MemoryEntry:
        """情報を記憶.

        自動的に3段階の記憶フローを実行:
        1. 感覚記憶: 予圧縮
        2. 短期記憶: トピックバッファに追加
        3. 長期記憶: 必要に応じて要約して保存

        Args:
            text: 記憶する情報
            topic: トピック名
            metadata: 追加メタデータ

        Returns:
            感覚記憶エントリ
        """
        # Light1: 感覚記憶（予圧縮）
        sensory_entry = await self._sensory.process(text, topic, metadata)

        # Light2: 短期記憶（トピックバッファに追加）
        await self._short_term.add_entry(sensory_entry)

        # 要約が必要かチェック
        if self._short_term.should_summarize(sensory_entry.topic):
            summary_entry = await self._short_term.summarize_topic(sensory_entry.topic)
            if summary_entry:
                # Light3: 長期記憶（要約を保存）
                await self._long_term.store(summary_entry)

        return sensory_entry

    async def recall(
        self,
        topic: str | None = None,
        limit: int = 10,
        min_importance: float = 0.0,
        query: str | None = None,
        min_similarity: float = 0.0,
    ) -> list[MemoryEntry]:
        """記憶を検索.

        Args:
            topic: トピック名（Noneの場合は全て）
            limit: 最大取得数
            min_importance: 最小重要度
            query: ベクトル検索クエリ（enable_vector_search=Trueの場合のみ）
            min_similarity: 最小類似度（ベクトル検索用）

        Returns:
            記憶エントリのリスト
        """
        # 基本検索
        memories = self._long_term.retrieve(topic, limit * 2, min_importance)

        # 重要度自動調整が有効な場合
        if self._enable_importance_adjustment and self._importance_adjuster:
            # アクセスを記録
            for memory in memories:
                self._importance_adjuster.record_access(memory.id)
            # 重要度を再計算
            memories = await self._importance_adjuster.adjust_batch(memories)

        # ベクトル検索が有効で、クエリが指定されている場合
        if self._enable_vector_search and query and self._vector_search:
            # 意味的類似度で検索
            similar_memories = await self._vector_search.search_similar(
                query, memories, top_k=limit, min_similarity=min_similarity
            )
            # 類似度順にソート済みの記憶を返す
            return [memory for memory, _ in similar_memories]

        # 通常検索
        return memories[:limit]

    async def update_memory(
        self,
        entry_id: str,
        update_data: dict[str, Any],
    ) -> None:
        """記憶を更新.

        Args:
            entry_id: 記憶エントリID
            update_data: 更新データ
        """
        await self._long_term.update(entry_id, update_data)

    async def consolidate(self) -> int:
        """手動で統合を実行.

        Returns:
            統合された記憶エントリ数
        """
        return await self._long_term.consolidate()

    def get_status(self) -> dict[str, Any]:
        """記憶システムの状態を取得.

        Returns:
            システム状態
        """
        status = {
            "short_term_buffers": self._short_term.get_buffer_status(),
            "long_term_count": len(self._long_term._memories),
            "pending_updates": len(self._long_term._update_queues),
            "vector_search_enabled": self._enable_vector_search,
            "importance_adjustment_enabled": self._enable_importance_adjustment,
            "auto_distill_enabled": self._enable_auto_distill,
            "auto_forget_enabled": self._enable_auto_forget,
        }

        if self._vector_search:
            status["vector_cache_size"] = len(self._vector_search._embedding_cache)
            status["vocabulary_size"] = len(self._vector_search._vocabulary)

        if self._importance_adjuster:
            status["importance_statistics"] = self._importance_adjuster.get_statistics()

        return status

    # =========================================================================
    # 強化学習インターフェース（P1: タスクフィードバック）
    # =========================================================================

    async def reinforce(
        self,
        memory_ids: list[str] | None = None,
        topic: str | None = None,
        reward: float = 0.0,
        context: str = "",
    ) -> int:
        """タスク結果に基づいて記憶を強化/弱化.

        タスクが成功した場合は正の報酬、失敗した場合は負の報酬を与える。
        これにより、有用な記憶の重要度が上がり、有害な記憶は忘却対象になりやすくなる。

        ユーザーは「タスクが成功した」とだけ伝えれば良い。内部で自動処理。

        Args:
            memory_ids: 対象記憶IDリスト（Noneの場合は最近アクセスした記憶）
            topic: 対象トピック（memory_idsがNoneの場合に使用）
            reward: 報酬値（-1.0～1.0、正=有用、負=有害）
            context: フィードバックコンテキスト

        Returns:
            強化した記憶の数
        """
        if not self._importance_adjuster:
            self._logger.warning("重要度調整が無効のため、強化をスキップ")
            return 0

        # 対象記憶を取得
        if memory_ids:
            memories = [
                m for m in self._long_term._memories.values()
                if m.id in memory_ids
            ]
        elif topic:
            memories = self._long_term.retrieve(topic, limit=20)
        else:
            # 最近アクセスした記憶を対象
            recent_ids = sorted(
                self._importance_adjuster._last_access.items(),
                key=lambda x: x[1],
                reverse=True
            )[:10]
            memories = [
                m for m in self._long_term._memories.values()
                if m.id in [id for id, _ in recent_ids]
            ]

        # 強化を適用
        for memory in memories:
            await self._importance_adjuster.apply_reinforcement(memory, reward, context)

        self._logger.info(f"強化完了: {len(memories)}件, reward={reward}")
        return len(memories)

    # =========================================================================
    # 自動最適化ループ（バックグラウンド、ユーザー透過的）
    # =========================================================================

    async def _auto_distill_loop(self) -> None:
        """自動蒸留ループ（バックグラウンド）."""
        while True:
            try:
                await asyncio.sleep(self._distill_interval)
                await self._perform_distillation()
            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.exception(f"自動蒸留エラー: {e}")

    async def _perform_distillation(self) -> int:
        """蒸留を実行.

        Returns:
            生成した抽象知識の数
        """
        if not self._distiller:
            return 0

        # 長期記憶から蒸留対象を取得
        memories = list(self._long_term._memories.values())

        if not self._distiller.should_distill(memories):
            return 0

        # 蒸留実行
        distilled = await self._distiller.distill(memories)

        # 蒸留結果を長期記憶に保存
        for abstract in distilled:
            await self._long_term.store(abstract)

        self._logger.info(f"蒸留完了: {len(distilled)}件の抽象知識を生成")
        return len(distilled)

    async def _auto_forget_loop(self) -> None:
        """自動忘却ループ（バックグラウンド）."""
        while True:
            try:
                await asyncio.sleep(self._forget_interval)
                await self._perform_forgetting()
            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.exception(f"自動忘却エラー: {e}")

    async def _perform_forgetting(self) -> int:
        """忘却を実行.

        Returns:
            削除した記憶の数
        """
        if not self._importance_adjuster:
            return 0

        # 長期記憶から忘却対象を特定
        memories = list(self._long_term._memories.values())
        forgettable_ids = self._importance_adjuster.identify_forgettable(memories)

        # 忘却実行
        for entry_id in forgettable_ids:
            if entry_id in self._long_term._memories:
                del self._long_term._memories[entry_id]

        if forgettable_ids:
            self._logger.info(f"忘却完了: {len(forgettable_ids)}件の記憶を削除")

        return len(forgettable_ids)

