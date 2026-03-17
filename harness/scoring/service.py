"""Layer 4 Scoring - 実行品質スコアリングサービス.

フロー/Agent の実行結果を評価し、品質スコアを算出する。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

_logger = logging.getLogger(__name__)


class ScoreDimension(str, Enum):
    """スコア次元."""

    ACCURACY = "accuracy"
    COMPLETENESS = "completeness"
    LATENCY = "latency"
    COST = "cost"
    SAFETY = "safety"


@dataclass
class DimensionScore:
    """次元別スコア.

    Attributes:
        dimension: スコア次元
        score: スコア値（0.0〜1.0）
        weight: 重み
        details: 詳細情報
    """

    dimension: ScoreDimension
    score: float
    weight: float = 1.0
    details: str = ""


@dataclass
class ScoringResult:
    """スコアリング結果.

    Attributes:
        overall_score: 総合スコア（0.0〜1.0）
        dimension_scores: 次元別スコア
        metadata: メタデータ
    """

    overall_score: float = 0.0
    dimension_scores: list[DimensionScore] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


class ExecutionScorer:
    """実行品質スコアラー."""

    def __init__(self, weights: dict[ScoreDimension, float] | None = None) -> None:
        """初期化.

        Args:
            weights: 次元別重み
        """
        self._weights = weights or {dim: 1.0 for dim in ScoreDimension}

    def score(self, dimension_scores: list[DimensionScore]) -> ScoringResult:
        """総合スコアを算出.

        Args:
            dimension_scores: 次元別スコアリスト

        Returns:
            スコアリング結果
        """
        if not dimension_scores:
            return ScoringResult()

        total_weight = sum(
            self._weights.get(ds.dimension, ds.weight) for ds in dimension_scores
        )
        if total_weight == 0:
            return ScoringResult(dimension_scores=dimension_scores)

        weighted_sum = sum(
            ds.score * self._weights.get(ds.dimension, ds.weight)
            for ds in dimension_scores
        )
        overall = weighted_sum / total_weight

        return ScoringResult(
            overall_score=round(overall, 4),
            dimension_scores=dimension_scores,
        )


__all__ = [
    "DimensionScore",
    "ExecutionScorer",
    "ScoreDimension",
    "ScoringResult",
]

