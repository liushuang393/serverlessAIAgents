# -*- coding: utf-8 -*-
"""Tool Relevance Selector - 関連性ベースのツール選択.

クエリに基づいて最も関連性の高いツールを動的に選択する。
毎回全ツールを公開するのではなく、Top5-7の関連ツールのみを公開。

設計原則:
- セマンティック類似度: クエリとツール説明の意味的類似度
- キーワードマッチング: 高速フィルタリング用
- 安全性考慮: リスクレベルも選択基準に含める
- 予算制約: BudgetManagerと連携

使用例:
    >>> selector = ToolRelevanceSelector()
    >>> tools = await selector.select_relevant_tools(
    ...     query="データベースから顧客を検索",
    ...     all_tools=tool_provider.list_tools(),
    ...     max_tools=7,
    ... )
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any, Protocol

if TYPE_CHECKING:
    from agentflow.providers.tool_provider import RegisteredTool, RiskLevel

_logger = logging.getLogger(__name__)


class ScoringStrategy(str, Enum):
    """スコアリング戦略."""

    KEYWORD = "keyword"       # キーワードマッチングのみ（高速）
    SEMANTIC = "semantic"     # セマンティック類似度（高精度）
    HYBRID = "hybrid"         # キーワード + セマンティック


@dataclass
class ToolScore:
    """ツールスコア結果.

    Attributes:
        tool: ツール情報
        relevance_score: 関連性スコア（0-1）
        keyword_score: キーワードスコア（0-1）
        semantic_score: セマンティックスコア（0-1）
        safety_score: 安全性スコア（0-1）
        final_score: 最終スコア（重み付け合計）
    """

    tool: Any  # RegisteredTool
    relevance_score: float = 0.0
    keyword_score: float = 0.0
    semantic_score: float = 0.0
    safety_score: float = 1.0
    final_score: float = 0.0


@dataclass
class SelectionConfig:
    """選択設定.

    Attributes:
        max_tools: 最大ツール数
        min_score: 最小スコア閾値
        keyword_weight: キーワードスコアの重み
        semantic_weight: セマンティックスコアの重み
        safety_weight: 安全性スコアの重み
        strategy: スコアリング戦略
    """

    max_tools: int = 7
    min_score: float = 0.1
    keyword_weight: float = 0.3
    semantic_weight: float = 0.5
    safety_weight: float = 0.2
    strategy: ScoringStrategy = ScoringStrategy.HYBRID


class EmbeddingProvider(Protocol):
    """埋め込みプロバイダインターフェース（DI用）."""

    async def embed_text(self, text: str) -> list[float]:
        """テキストを埋め込みベクトルに変換."""
        ...

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """バッチで埋め込み."""
        ...


class ToolRelevanceSelector:
    """関連性ベースのツール選択器.

    クエリに対して最も関連性の高いツールを選択する。
    毎ターンの注意力予算を節約するため、全ツールではなく
    Top-K の関連ツールのみを公開。

    Example:
        >>> selector = ToolRelevanceSelector()
        >>> scores = await selector.score_tools(
        ...     query="顧客データを検索",
        ...     tools=all_tools,
        ... )
        >>> selected = selector.select_top_k(scores, k=7)
    """

    # 日本語・中国語ストップワード
    _STOP_WORDS = frozenset([
        "の", "は", "が", "を", "に", "で", "と", "から", "まで", "より",
        "的", "是", "在", "和", "了", "有", "把", "被", "给", "从",
        "the", "a", "an", "is", "are", "was", "were", "be", "been",
        "to", "of", "in", "for", "on", "with", "at", "by", "from",
    ])

    def __init__(
        self,
        config: SelectionConfig | None = None,
        embedding_provider: EmbeddingProvider | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 選択設定
            embedding_provider: 埋め込みプロバイダ（セマンティック用）
        """
        self._config = config or SelectionConfig()
        self._embedding = embedding_provider
        self._tool_embeddings: dict[str, list[float]] = {}
        self._logger = logging.getLogger(__name__)

    async def select_relevant_tools(
        self,
        query: str,
        all_tools: list["RegisteredTool"],
        max_tools: int | None = None,
    ) -> list["RegisteredTool"]:
        """関連ツールを選択.

        Args:
            query: ユーザークエリ
            all_tools: 全ツールリスト
            max_tools: 最大ツール数（デフォルトは設定値）

        Returns:
            選択されたツールリスト（スコア降順）
        """
        max_k = max_tools or self._config.max_tools
        scores = await self.score_tools(query, all_tools)
        return self.select_top_k(scores, k=max_k)

    async def score_tools(
        self,
        query: str,
        tools: list["RegisteredTool"],
    ) -> list[ToolScore]:
        """全ツールをスコアリング.

        Args:
            query: ユーザークエリ
            tools: ツールリスト

        Returns:
            スコア付きツールリスト
        """
        scores: list[ToolScore] = []

        # クエリからキーワードを抽出
        query_keywords = self._extract_keywords(query)

        # セマンティック埋め込み（HYBRID/SEMANTIC戦略の場合）
        query_embedding = None
        if self._config.strategy in (ScoringStrategy.SEMANTIC, ScoringStrategy.HYBRID):
            query_embedding = await self._get_query_embedding(query)

        for tool in tools:
            score = ToolScore(tool=tool)

            # キーワードスコア
            score.keyword_score = self._calculate_keyword_score(
                query_keywords, tool.name, tool.description
            )

            # セマンティックスコア
            if query_embedding is not None:
                score.semantic_score = await self._calculate_semantic_score(
                    query_embedding, tool
                )

            # 安全性スコア
            score.safety_score = self._calculate_safety_score(tool)

            # 最終スコア計算
            score.final_score = self._calculate_final_score(score)
            score.relevance_score = score.final_score

            scores.append(score)

        # スコア降順でソート
        scores.sort(key=lambda s: s.final_score, reverse=True)
        return scores

    def select_top_k(
        self,
        scores: list[ToolScore],
        k: int | None = None,
    ) -> list["RegisteredTool"]:
        """Top-Kツールを選択.

        Args:
            scores: スコアリスト
            k: 選択数

        Returns:
            選択されたツールリスト
        """
        k = k or self._config.max_tools

        # 最小スコアでフィルタリング
        filtered = [s for s in scores if s.final_score >= self._config.min_score]

        # Top-K を返す
        return [s.tool for s in filtered[:k]]

    def _extract_keywords(self, text: str) -> set[str]:
        """テキストからキーワードを抽出.

        Args:
            text: 入力テキスト

        Returns:
            キーワードセット
        """
        # 正規化
        text = text.lower()

        # 単語分割（日本語・中国語・英語対応）
        # 簡易トークナイズ: 空白 + CJK文字境界
        words: list[str] = []

        # 英語単語
        words.extend(re.findall(r"[a-z]+", text))

        # CJK文字（単文字）
        words.extend(re.findall(r"[\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff]", text))

        # ストップワード除去
        keywords = {w for w in words if w not in self._STOP_WORDS and len(w) > 1}

        return keywords

    def _calculate_keyword_score(
        self,
        query_keywords: set[str],
        tool_name: str,
        tool_description: str,
    ) -> float:
        """キーワードスコアを計算.

        Args:
            query_keywords: クエリキーワード
            tool_name: ツール名
            tool_description: ツール説明

        Returns:
            キーワードスコア（0-1）
        """
        if not query_keywords:
            return 0.0

        # ツールのキーワード
        tool_text = f"{tool_name} {tool_description}".lower()
        tool_keywords = self._extract_keywords(tool_text)

        if not tool_keywords:
            return 0.0

        # Jaccard類似度
        intersection = query_keywords & tool_keywords
        union = query_keywords | tool_keywords

        jaccard = len(intersection) / len(union) if union else 0.0

        # 名前完全一致ボーナス
        name_bonus = 0.0
        tool_name_lower = tool_name.lower()
        for kw in query_keywords:
            if kw in tool_name_lower:
                name_bonus = 0.3
                break

        return min(1.0, jaccard + name_bonus)

    async def _get_query_embedding(self, query: str) -> list[float] | None:
        """クエリの埋め込みを取得.

        Args:
            query: クエリテキスト

        Returns:
            埋め込みベクトル、またはNone
        """
        if self._embedding is None:
            return None

        try:
            return await self._embedding.embed_text(query)
        except Exception as e:
            self._logger.warning("埋め込み取得に失敗: %s", e)
            return None

    async def _calculate_semantic_score(
        self,
        query_embedding: list[float],
        tool: "RegisteredTool",
    ) -> float:
        """セマンティックスコアを計算.

        Args:
            query_embedding: クエリ埋め込み
            tool: ツール

        Returns:
            セマンティックスコア（0-1）
        """
        if self._embedding is None:
            return 0.0

        # ツール埋め込みをキャッシュから取得、または計算
        tool_text = f"{tool.name}: {tool.description}"
        if tool.name not in self._tool_embeddings:
            try:
                embedding = await self._embedding.embed_text(tool_text)
                self._tool_embeddings[tool.name] = embedding
            except Exception as e:
                self._logger.warning("ツール埋め込み取得に失敗: %s", e)
                return 0.0

        tool_embedding = self._tool_embeddings[tool.name]

        # コサイン類似度
        return self._cosine_similarity(query_embedding, tool_embedding)

    def _calculate_safety_score(self, tool: "RegisteredTool") -> float:
        """安全性スコアを計算.

        Args:
            tool: ツール

        Returns:
            安全性スコア（0-1）
        """
        # リスクレベルに基づくスコア
        risk_scores = {
            "low": 1.0,
            "medium": 0.7,
            "high": 0.4,
            "critical": 0.1,
        }

        risk_level = getattr(tool, "risk_level", None)
        if risk_level is None:
            return 1.0

        risk_value = risk_level.value if hasattr(risk_level, "value") else str(risk_level)
        return risk_scores.get(risk_value.lower(), 0.5)

    def _calculate_final_score(self, score: ToolScore) -> float:
        """最終スコアを計算.

        Args:
            score: ツールスコア

        Returns:
            最終スコア（0-1）
        """
        # 戦略に応じた重み付け
        if self._config.strategy == ScoringStrategy.KEYWORD:
            return score.keyword_score * score.safety_score

        if self._config.strategy == ScoringStrategy.SEMANTIC:
            return score.semantic_score * score.safety_score

        # HYBRID
        weighted = (
            self._config.keyword_weight * score.keyword_score
            + self._config.semantic_weight * score.semantic_score
            + self._config.safety_weight * score.safety_score
        )

        # 正規化
        total_weight = (
            self._config.keyword_weight
            + self._config.semantic_weight
            + self._config.safety_weight
        )

        return weighted / total_weight if total_weight > 0 else 0.0

    @staticmethod
    def _cosine_similarity(vec1: list[float], vec2: list[float]) -> float:
        """コサイン類似度を計算.

        Args:
            vec1: ベクトル1
            vec2: ベクトル2

        Returns:
            コサイン類似度（0-1）
        """
        if len(vec1) != len(vec2):
            return 0.0

        dot_product = sum(a * b for a, b in zip(vec1, vec2))
        norm1 = sum(a * a for a in vec1) ** 0.5
        norm2 = sum(b * b for b in vec2) ** 0.5

        if norm1 == 0 or norm2 == 0:
            return 0.0

        similarity = dot_product / (norm1 * norm2)
        # 0-1 に正規化（負の類似度を0に）
        return max(0.0, similarity)

    def clear_cache(self) -> None:
        """埋め込みキャッシュをクリア."""
        self._tool_embeddings.clear()
