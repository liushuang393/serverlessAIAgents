"""記憶蒸留モジュール.

Evo-Memory思想に基づき、類似記憶を抽象的な知識に自動蒸留する。
ユーザーは意識する必要なし、バックグラウンドで自動実行。

機能:
- 意味的類似度に基づくクラスタリング
- 複数記憶から抽象知識を生成
- 安定性レベルの自動昇格
"""

from __future__ import annotations

import logging
import uuid
from datetime import datetime
from typing import Any

from agentflow.memory.types import (
    MemoryEntry,
    MemorySemanticLevel,
    MemoryStability,
    MemoryType,
)


class MemoryDistiller:
    """記憶蒸留エンジン.

    類似した複数の記憶を1つの抽象的な知識に蒸留する。
    これにより記憶効率を向上させ、パターンを抽出する。

    動作原理:
    1. 同一トピック内の記憶をセマンティック類似度でクラスタリング
    2. 3件以上の類似記憶があれば蒸留対象
    3. LLMを使用して抽象知識を生成
    4. 蒸留された知識はSEMANTICレベルに昇格
    """

    def __init__(
        self,
        min_cluster_size: int = 3,
        similarity_threshold: float = 0.7,
        llm_client: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            min_cluster_size: 蒸留に必要な最小クラスタサイズ
            similarity_threshold: 類似度閾値（0.0-1.0）
            llm_client: LLMクライアント（蒸留用）
        """
        self._min_cluster_size = min_cluster_size
        self._similarity_threshold = similarity_threshold
        self._llm_client = llm_client
        self._logger = logging.getLogger(__name__)

    async def distill(
        self,
        memories: list[MemoryEntry],
    ) -> list[MemoryEntry]:
        """記憶群を蒸留して抽象知識を生成.

        Args:
            memories: 蒸留対象の記憶リスト

        Returns:
            蒸留された抽象知識のリスト
        """
        if len(memories) < self._min_cluster_size:
            return []

        # トピック別にグループ化
        topic_groups: dict[str, list[MemoryEntry]] = {}
        for m in memories:
            if m.topic not in topic_groups:
                topic_groups[m.topic] = []
            topic_groups[m.topic].append(m)

        distilled_memories: list[MemoryEntry] = []

        for topic, group in topic_groups.items():
            if len(group) >= self._min_cluster_size:
                # クラスタリング（簡易版：全て同一クラスタとして扱う）
                clusters = await self._cluster_memories(group)

                for cluster in clusters:
                    if len(cluster) >= self._min_cluster_size:
                        abstract = await self._generate_abstract(cluster, topic)
                        if abstract:
                            distilled_memories.append(abstract)

        self._logger.info(f"蒸留完了: {len(distilled_memories)}件の抽象知識を生成")
        return distilled_memories

    async def _cluster_memories(
        self,
        memories: list[MemoryEntry],
    ) -> list[list[MemoryEntry]]:
        """記憶を意味的類似度でクラスタリング.

        Args:
            memories: クラスタリング対象

        Returns:
            クラスタのリスト
        """
        # 簡易実装：コンテンツ長の近い記憶をグループ化
        # 本番環境ではベクトル埋め込みを使用すべき
        if len(memories) < self._min_cluster_size:
            return []

        # 現時点では全記憶を1クラスタとして扱う（ベクトル検索との連携は別途）
        return [memories]

    async def _generate_abstract(
        self,
        cluster: list[MemoryEntry],
        topic: str,
    ) -> MemoryEntry | None:
        """クラスタから抽象知識を生成.

        Args:
            cluster: 記憶クラスタ
            topic: トピック名

        Returns:
            抽象化された記憶エントリ
        """
        if not cluster:
            return None

        # コンテンツを結合
        combined = "\n".join([m.content for m in cluster])

        # 抽象知識を生成
        abstract_content = await self._summarize_to_abstract(combined, topic)

        # 親記憶のIDリスト
        parent_ids = [m.id for m in cluster]

        # 平均重要度を計算
        avg_importance = sum(m.importance_score for m in cluster) / len(cluster)

        # 蒸留された記憶を作成
        return MemoryEntry(
            id=str(uuid.uuid4()),
            content=abstract_content,
            topic=topic,
            timestamp=datetime.now(),
            memory_type=MemoryType.LONG_TERM,
            importance_score=min(1.0, avg_importance * 1.2),  # 蒸留で重要度UP
            semantic_level=MemorySemanticLevel.SEMANTIC,  # 抽象知識に昇格
            stability=MemoryStability.CONSOLIDATED,  # 固定済み
            metadata={
                "distilled_from": parent_ids,
                "distilled_at": datetime.now().isoformat(),
                "source_count": len(cluster),
            },
        )

    async def _summarize_to_abstract(self, combined: str, topic: str) -> str:
        """複数記憶を抽象知識に要約.

        Args:
            combined: 結合されたコンテンツ
            topic: トピック名

        Returns:
            抽象化されたコンテンツ
        """
        if self._llm_client:
            try:
                prompt = f"""以下の複数の記憶から、共通するパターンや抽象的な知識を抽出してください。
トピック: {topic}

記憶内容:
{combined}

抽象的な知識として簡潔にまとめてください（箇条書き可）:"""
                response = await self._llm_client.generate(prompt)
                return response.strip()
            except Exception as e:
                self._logger.warning(f"LLM蒸留失敗、フォールバック使用: {e}")

        # フォールバック：最初の記憶の要約版を使用
        lines = combined.split("\n")
        if len(lines) > 5:
            return f"[{topic}に関する{len(lines)}件の記憶を統合] " + " ".join(lines[:3])
        return f"[{topic}] " + combined[:200]

    def should_distill(self, memories: list[MemoryEntry]) -> bool:
        """蒸留が必要かどうかを判定.

        Args:
            memories: 記憶リスト

        Returns:
            蒸留が必要な場合True
        """
        # 同一トピック内に十分な数のエピソード記憶がある場合
        episodic_count: dict[str, int] = {}
        for m in memories:
            if m.semantic_level == MemorySemanticLevel.EPISODIC:
                episodic_count[m.topic] = episodic_count.get(m.topic, 0) + 1

        return any(count >= self._min_cluster_size for count in episodic_count.values())
