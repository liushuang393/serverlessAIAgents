"""適応的スコアリングサービス.

予測的中率に基づく5軸重みの自動調整を提供します。
ポリシーグラディエント的アプローチで重みを更新します。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime

from apps.market_trend_monitor.backend.models import PredictionOutcome, PredictionReview


@dataclass
class ScoringWeights:
    """スコアリング重みデータモデル."""

    reliability: float = 0.2
    leading: float = 0.2
    relevance: float = 0.2
    actionability: float = 0.2
    convergence: float = 0.2
    updated_at: datetime = field(default_factory=datetime.now)
    update_count: int = 0

    def to_dict(self) -> dict[str, float]:
        """辞書形式に変換."""
        return {
            "reliability": self.reliability,
            "leading": self.leading,
            "relevance": self.relevance,
            "actionability": self.actionability,
            "convergence": self.convergence,
            "updated_at": self.updated_at.isoformat(),
            "update_count": self.update_count,
        }

    def as_weight_dict(self) -> dict[str, float]:
        """重みのみの辞書を返す."""
        return {
            "reliability": self.reliability,
            "leading": self.leading,
            "relevance": self.relevance,
            "actionability": self.actionability,
            "convergence": self.convergence,
        }


class AdaptiveScoringService:
    """適応的スコアリングサービス.

    - 予測的中率に基づく5軸重みの自動調整
    - ポリシーグラディエント的アプローチ（勾配ベース更新）
    - 重みの上下限制約（0.05-0.95）
    """

    LEARNING_RATE: float = 0.01
    MIN_WEIGHT: float = 0.05
    MAX_WEIGHT: float = 0.95

    OUTCOME_SCORES: dict[PredictionOutcome, float] = {
        PredictionOutcome.CORRECT: 1.0,
        PredictionOutcome.PARTIAL: 0.5,
        PredictionOutcome.INCORRECT: 0.0,
        PredictionOutcome.UNKNOWN: 0.25,
    }

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._current_weights = ScoringWeights()
        self._history: list[ScoringWeights] = []

    async def update_weights(
        self,
        prediction_reviews: list[PredictionReview],
    ) -> ScoringWeights:
        """予測復盤結果に基づいて重みを更新.

        Args:
            prediction_reviews: 予測復盤結果リスト

        Returns:
            更新後の重み
        """
        if not prediction_reviews:
            return self._current_weights

        gradients = self._calculate_gradient(prediction_reviews)

        weights = self._current_weights.as_weight_dict()
        for axis, gradient in gradients.items():
            weights[axis] = weights.get(axis, 0.2) + self.LEARNING_RATE * gradient

        weights = self._clip_weights(weights)
        weights = self._normalize_weights(weights)

        self._history.append(ScoringWeights(**self._current_weights.as_weight_dict()))

        self._current_weights = ScoringWeights(
            **weights,
            updated_at=datetime.now(),
            update_count=self._current_weights.update_count + 1,
        )

        self._logger.info(
            "重み更新: count=%d, weights=%s",
            self._current_weights.update_count,
            {k: f"{v:.4f}" for k, v in weights.items()},
        )
        return self._current_weights

    async def get_current_weights(self) -> ScoringWeights:
        """現在の重みを取得."""
        return self._current_weights

    async def get_weight_history(self, limit: int = 50) -> list[ScoringWeights]:
        """重み更新履歴を取得.

        Args:
            limit: 返却する最大件数

        Returns:
            重み履歴リスト（新しい順）
        """
        return list(reversed(self._history[-limit:]))

    def _calculate_gradient(
        self,
        reviews: list[PredictionReview],
    ) -> dict[str, float]:
        """REINFORCE型ポリシーグラディエントで勾配を計算.

        予測作成時にmetadataに保存されたsignal_scoresを使い、
        各軸の寄与度に基づいて帰属分析を行う。

        gradient[axis] = reward * (axis_score - 0.5)
        これにより、高スコア軸が成功に、低スコア軸が失敗に帰属される。

        Args:
            reviews: 予測復盤結果リスト

        Returns:
            各軸の勾配
        """
        axes = ["reliability", "leading", "relevance", "actionability", "convergence"]
        gradients = dict.fromkeys(axes, 0.0)

        for review in reviews:
            outcome_score = self.OUTCOME_SCORES.get(review.outcome, 0.25)
            reward = outcome_score - 0.5  # 0.5を基準に正負の報酬

            # Phase 10: metadataからsignal_scoresを取得して軸別帰属
            signal_scores = getattr(review, "metadata", {}).get("signal_scores", {})
            if signal_scores:
                for axis in axes:
                    axis_val = signal_scores.get(axis, 0.5)
                    gradients[axis] += reward * (axis_val - 0.5)
            else:
                # signal_scoresがない場合のフォールバック: 均等分配
                for axis in axes:
                    gradients[axis] += reward / len(axes)

        # レビュー数で平均化
        n = len(reviews)
        return {axis: grad / n for axis, grad in gradients.items()}

    def _clip_weights(self, weights: dict[str, float]) -> dict[str, float]:
        """重みを上下限制約でクリップ."""
        return {axis: max(self.MIN_WEIGHT, min(self.MAX_WEIGHT, w)) for axis, w in weights.items()}

    def _normalize_weights(self, weights: dict[str, float]) -> dict[str, float]:
        """重みを正規化して合計1.0にする."""
        total = sum(weights.values())
        if total == 0:
            n = len(weights)
            return dict.fromkeys(weights, 1.0 / n)
        return {axis: w / total for axis, w in weights.items()}
