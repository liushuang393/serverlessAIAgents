"""Layer 4 Risk - リスク評価サービス.

ツール実行やフロー操作のリスクレベルを評価する。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


_logger = logging.getLogger(__name__)


class RiskLevel(str, Enum):
    """リスクレベル."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class RiskFactor:
    """リスク要因.

    Attributes:
        name: 要因名
        level: リスクレベル
        description: 説明
        score: スコア（0.0〜1.0）
    """

    name: str
    level: RiskLevel
    description: str = ""
    score: float = 0.0


@dataclass
class RiskAssessment:
    """リスク評価結果.

    Attributes:
        overall_level: 総合リスクレベル
        factors: リスク要因リスト
        mitigations: 緩和策
        metadata: メタデータ
    """

    overall_level: RiskLevel = RiskLevel.LOW
    factors: list[RiskFactor] = field(default_factory=list)
    mitigations: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def is_acceptable(self) -> bool:
        """許容可能なリスクかどうか."""
        return self.overall_level in (RiskLevel.LOW, RiskLevel.MEDIUM)


class RiskAssessor:
    """リスク評価器."""

    def __init__(self, threshold: RiskLevel = RiskLevel.HIGH) -> None:
        """初期化.

        Args:
            threshold: 許容リスク閾値
        """
        self._threshold = threshold
        self._level_order = [RiskLevel.LOW, RiskLevel.MEDIUM, RiskLevel.HIGH, RiskLevel.CRITICAL]

    def assess(self, factors: list[RiskFactor]) -> RiskAssessment:
        """リスクを評価.

        Args:
            factors: リスク要因リスト

        Returns:
            リスク評価結果
        """
        if not factors:
            return RiskAssessment()

        max_level = max(factors, key=lambda f: self._level_order.index(f.level)).level

        mitigations: list[str] = []
        if self._level_order.index(max_level) >= self._level_order.index(self._threshold):
            mitigations.append("承認フローの実行を推奨")

        return RiskAssessment(
            overall_level=max_level,
            factors=factors,
            mitigations=mitigations,
        )


__all__ = [
    "RiskAssessment",
    "RiskAssessor",
    "RiskFactor",
    "RiskLevel",
]
