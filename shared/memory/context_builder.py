"""MemOSアダプター: コンテキストビルダー.

LLMに注入する記憶コンテキストの生成・制御を担う。
MemOS風のGating/Budget/Scoring/フォーマット変換を提供する。

設計原則:
- デフォルトで記憶を注入しない（先少後多）
- Gating: need_levelが NONE の場合は常に空を返す
- Budget: レベル別の上限（件数/トークン）を厳守する
- Scoring: relevance(0.55) + importance(0.30) + recency(0.15)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from infrastructure.memory.types import MemoryEntry
    from shared.memory.memory_manager import MemoryManager


# Gatingキーワード（日本語・中国語・英語）
_HIGH_NEED_KEYWORDS: list[str] = [
    "前回",
    "上次",
    "之前",
    "previous",
    "last time",
    "継続",
    "继续",
    "continue",
    "multi-step",
    "多段",
    "migration",
    "移行",
    "architecture",
    "アーキテクチャ",
]
_MEDIUM_NEED_KEYWORDS: list[str] = [
    "based on",
    "に基づいて",
    "既存の",
    "existing",
    "prior",
    "以前の結論",
    "interface",
    "インターフェース",
]
_LOW_NEED_KEYWORDS: list[str] = [
    "prefer",
    "いつも",
    "format",
    "style",
    "スタイル",
    "惯例",
    "habit",
    "フォーマット",
]


class MemoryNeedLevel(str, Enum):
    """記憶必要度レベル."""

    NONE = "none"  # 0件: 単純な一問一答
    LOW = "low"  # 最大2件: ユーザー好み/設定
    MEDIUM = "medium"  # 最大5件: プロジェクトコンテキスト
    HIGH = "high"  # 最大8件: 長期タスク/継続作業


@dataclass
class MemoryBudget:
    """記憶予算（注入上限）.

    Attributes:
        max_items: 最大注入件数
        max_tokens_per_item: 1件あたり最大トークン数（目安）
    """

    max_items: int = 0
    max_tokens_per_item: int = 80


@dataclass
class ContextBlock:
    """LLMに注入するコンテキストブロック.

    Attributes:
        memory_id: 記憶エントリID（追跡用）
        content: 注入内容（圧縮済み）
        score: ランキングスコア
        source_label: 出処ラベル（topic + timestamp）
        metadata: 追加メタデータ
    """

    memory_id: str
    content: str
    score: float
    source_label: str
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_prompt_text(self) -> str:
        """プロンプト挿入用テキストに変換."""
        return f"[{self.source_label}] {self.content}"


# レベル別予算テーブル
_BUDGET_TABLE: dict[MemoryNeedLevel, MemoryBudget] = {
    MemoryNeedLevel.NONE: MemoryBudget(max_items=0, max_tokens_per_item=0),
    MemoryNeedLevel.LOW: MemoryBudget(max_items=2, max_tokens_per_item=60),
    MemoryNeedLevel.MEDIUM: MemoryBudget(max_items=5, max_tokens_per_item=80),
    MemoryNeedLevel.HIGH: MemoryBudget(max_items=8, max_tokens_per_item=100),
}


class ContextBuilder:
    """コンテキストビルダー（MemOSアダプター）.

    利用例:
        >>> cb = ContextBuilder()
        >>> blocks = await cb.build(
        ...     user_request="前回の設計を継続して",
        ...     memory_manager=manager,
        ...     topic="architecture",
        ... )
        >>> prompt_ctx = "\\n".join(b.to_prompt_text() for b in blocks)
    """

    def __init__(self, filter_episodic: bool = True) -> None:
        """初期化.

        Args:
            filter_episodic: Trueの場合、通常タスクでEPISODIC記憶を除外する
        """
        self._filter_episodic = filter_episodic
        self._logger = logging.getLogger(__name__)

    def assess_need(self, user_request: str) -> MemoryNeedLevel:
        """ユーザーリクエストから記憶必要度を評価（Gating）.

        Args:
            user_request: ユーザーのリクエスト文字列

        Returns:
            記憶必要度レベル
        """
        lower = user_request.lower()

        for kw in _HIGH_NEED_KEYWORDS:
            if kw.lower() in lower:
                return MemoryNeedLevel.HIGH

        for kw in _MEDIUM_NEED_KEYWORDS:
            if kw.lower() in lower:
                return MemoryNeedLevel.MEDIUM

        for kw in _LOW_NEED_KEYWORDS:
            if kw.lower() in lower:
                return MemoryNeedLevel.LOW

        return MemoryNeedLevel.NONE

    def get_budget(self, level: MemoryNeedLevel) -> MemoryBudget:
        """レベルに対応する予算を取得.

        Args:
            level: 記憶必要度レベル

        Returns:
            予算設定
        """
        return _BUDGET_TABLE[level]

    def _score(self, entry: MemoryEntry, query: str) -> float:
        """記憶のランキングスコアを計算.

        スコア = 0.55*relevance + 0.30*importance + 0.15*recency

        Args:
            entry: 記憶エントリ
            query: 検索クエリ（将来的にベクトル類似度計算で使用）

        Returns:
            スコア（0.0〜1.0）
        """
        # relevance: ベクトル検索未使用時は重要度で代替
        relevance = entry.get_effective_importance()

        # importance
        importance = entry.importance_score

        # recency: タイムスタンプから計算（1時間以内=1.0、24時間後=0.0）
        elapsed_hours = (datetime.now() - entry.timestamp).total_seconds() / 3600
        recency = max(0.0, 1.0 - elapsed_hours / 24.0)

        total_score = 0.55 * relevance + 0.30 * importance + 0.15 * recency
        return float(total_score)

    def _should_include(self, entry: MemoryEntry) -> bool:
        """記憶を注入対象に含めるかフィルタリング.

        Args:
            entry: 記憶エントリ

        Returns:
            含める場合True
        """
        from infrastructure.memory.types import MemorySemanticLevel

        return not (self._filter_episodic and entry.semantic_level == MemorySemanticLevel.EPISODIC)

    def _truncate_content(self, content: str, max_tokens: int) -> str:
        """コンテンツをmax_tokens相当の文字数に切り詰める（4文字≒1token概算）.

        Args:
            content: 元のコンテンツ
            max_tokens: 最大トークン数

        Returns:
            切り詰められたコンテンツ
        """
        max_chars = max_tokens * 4
        if len(content) <= max_chars:
            return content
        return content[:max_chars] + "..."

    def _make_source_label(self, entry: MemoryEntry) -> str:
        """source_labelを生成.

        Args:
            entry: 記憶エントリ

        Returns:
            追跡用ラベル文字列（memory_idとtopic+timestampを含む）
        """
        ts = entry.timestamp.strftime("%Y-%m-%d")
        return f"topic:{entry.topic}/{ts}"

    async def _multi_view_recall(
        self,
        query: str,
        memory_manager: MemoryManager,
        topic: str | None,
        limit: int,
        min_importance: float,
    ) -> list[MemoryEntry]:
        """多視点検索: 象徴・意味・語彙の3視点を RRF で統合.

        1. 象徴検索: topic/importance フィルタ（既存 recall() 活用）
        2. 意味検索: VectorSearch.search_similar()
        3. 語彙検索: VectorSearch.search_lexical()
        4. RRF融合: reciprocal_rank_score = Σ 1/(k + rank_i), k=60

        Args:
            query: 検索クエリ
            memory_manager: 記憶マネージャー
            topic: トピックフィルタ
            limit: 最大取得数
            min_importance: 最小重要度

        Returns:
            RRF融合後の記憶エントリリスト
        """
        # 視点1: 象徴検索（topic/importance ベース）
        symbolic_memories = await memory_manager.recall(topic=topic, limit=limit * 2, min_importance=min_importance)

        # ベクトル検索が利用不可の場合は象徴検索のみ返す
        if not memory_manager._enable_vector_search or not memory_manager._vector_search:
            return symbolic_memories[:limit]

        # 視点2: 意味検索
        semantic_results = await memory_manager._vector_search.search_similar(query, symbolic_memories, top_k=limit)
        semantic_ranked = [m for m, _ in semantic_results]

        # 視点3: 語彙検索
        lexical_results = await memory_manager._vector_search.search_lexical(query, symbolic_memories, top_k=limit)
        lexical_ranked = [m for m, _ in lexical_results]

        # RRF融合: Reciprocal Rank Fusion (k=60)
        rrf_k = 60
        scores: dict[str, float] = {}
        entry_map: dict[str, MemoryEntry] = {}

        def _add_ranked_list(ranked: list[MemoryEntry]) -> None:
            for rank, mem in enumerate(ranked):
                scores[mem.id] = scores.get(mem.id, 0.0) + 1.0 / (rrf_k + rank + 1)
                entry_map[mem.id] = mem

        # 3つのランクリストを統合
        _add_ranked_list(symbolic_memories)
        _add_ranked_list(semantic_ranked)
        _add_ranked_list(lexical_ranked)

        # RRFスコア降順でソート
        sorted_ids = sorted(scores, key=lambda eid: scores[eid], reverse=True)
        return [entry_map[eid] for eid in sorted_ids[:limit]]

    async def build(
        self,
        user_request: str,
        memory_manager: MemoryManager,
        need_level: MemoryNeedLevel | None = None,
        topic: str | None = None,
        min_importance: float = 0.3,
    ) -> list[ContextBlock]:
        """コンテキストブロックを構築.

        Args:
            user_request: ユーザーのリクエスト
            memory_manager: 長期記憶マネージャー
            need_level: 明示的なレベル指定（Noneの場合はassess_needで自動判定）
            topic: トピックフィルタ
            min_importance: recall時の最小重要度

        Returns:
            LLMに注入するContextBlockのリスト（スコア降順・Budget制限済み）
        """
        # Gating: need_levelが明示されていない場合は自動評価
        level = need_level if need_level is not None else self.assess_need(user_request)
        budget = self.get_budget(level)

        # NONE: 記憶注入不要
        if budget.max_items == 0:
            return []

        # 多視点検索（意味+語彙+象徴）で recall
        memories = await self._multi_view_recall(
            query=user_request,
            memory_manager=memory_manager,
            topic=topic,
            limit=budget.max_items * 3,
            min_importance=min_importance,
        )

        # フィルタリング
        filtered = [m for m in memories if self._should_include(m)]

        # スコアリング & ソート
        scored = sorted(
            filtered,
            key=lambda m: self._score(m, user_request),
            reverse=True,
        )

        # Budget制限 + コンテンツ切り詰め
        blocks: list[ContextBlock] = []
        for entry in scored[: budget.max_items]:
            content = self._truncate_content(entry.content, budget.max_tokens_per_item)
            blocks.append(
                ContextBlock(
                    memory_id=entry.id,
                    content=content,
                    score=self._score(entry, user_request),
                    source_label=self._make_source_label(entry),
                    metadata={
                        "topic": entry.topic,
                        "importance": entry.importance_score,
                        "semantic_level": entry.semantic_level.value,
                    },
                )
            )

        self._logger.debug(
            "ContextBuilder: level=%s, recalled=%d, filtered=%d, injected=%d",
            level.value,
            len(memories),
            len(filtered),
            len(blocks),
        )
        return blocks


__all__ = [
    "ContextBlock",
    "ContextBuilder",
    "MemoryBudget",
    "MemoryNeedLevel",
]
