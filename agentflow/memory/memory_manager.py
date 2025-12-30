"""AgentFlow Memory System - Memory Manager.

記憶システムの統合マネージャー。

LightMemの3段階記憶システムを統合管理:
- Light1: 感覚記憶（予圧縮）
- Light2: 短期記憶（トピックバッファ）
- Light3: 長期記憶（オフライン統合）
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow.memory.importance_adjuster import ImportanceAdjuster
from agentflow.memory.long_term_memory import LongTermMemory
from agentflow.memory.sensory_memory import SensoryMemory
from agentflow.memory.short_term_memory import ShortTermMemory
from agentflow.memory.types import CompressionConfig, MemoryEntry
from agentflow.memory.vector_search import VectorSearch


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
        """
        self._sensory = SensoryMemory(compression_config)
        self._short_term = ShortTermMemory(token_threshold, llm_client)
        self._long_term = LongTermMemory(consolidation_interval)
        self._logger = logging.getLogger(__name__)

        # ベクトル検索エンジン（オプション）
        self._enable_vector_search = enable_vector_search
        self._vector_search = VectorSearch(embedding_dim) if enable_vector_search else None

        # 重要度自動調整エンジン（オプション）
        self._enable_importance_adjustment = enable_importance_adjustment
        self._importance_adjuster = (
            ImportanceAdjuster(decay_constant, access_boost_factor) if enable_importance_adjustment else None
        )

    async def start(self) -> None:
        """記憶システムを開始."""
        await self._long_term.start()
        if self._importance_adjuster:
            await self._importance_adjuster.start()
        self._logger.info("Memory system started")

    async def stop(self) -> None:
        """記憶システムを停止."""
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
        }

        if self._vector_search:
            status["vector_cache_size"] = len(self._vector_search._embedding_cache)
            status["vocabulary_size"] = len(self._vector_search._vocabulary)

        if self._importance_adjuster:
            status["importance_statistics"] = self._importance_adjuster.get_statistics()

        return status

