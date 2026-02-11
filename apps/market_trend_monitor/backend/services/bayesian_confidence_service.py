"""ベイズ信頼度サービス.

Beta分布による信頼度のベイズ更新を提供します。
新証拠到着時に逐次更新を行います。
"""

from __future__ import annotations

import logging
import math
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


@dataclass
class BayesianState:
    """ベイズ状態データモデル.

    Beta(alpha, beta) 分布のパラメータ。
    """

    alpha: float = 2.0  # 成功パラメータ（事前: 中立的）
    beta: float = 2.0   # 失敗パラメータ（事前: 中立的）
    updated_at: datetime = field(default_factory=datetime.now)
    update_count: int = 0

    @property
    def mean(self) -> float:
        """Beta分布の平均値（期待値）."""
        total = self.alpha + self.beta
        if total == 0:
            return 0.5
        return self.alpha / total

    @property
    def variance(self) -> float:
        """Beta分布の分散."""
        total = self.alpha + self.beta
        if total == 0:
            return 0.0
        return (self.alpha * self.beta) / (total ** 2 * (total + 1))

    @property
    def std(self) -> float:
        """Beta分布の標準偏差."""
        return math.sqrt(self.variance)

    @property
    def confidence_interval(self) -> tuple[float, float]:
        """近似95%信頼区間."""
        mean = self.mean
        margin = 1.96 * self.std
        return (max(0.0, mean - margin), min(1.0, mean + margin))

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "alpha": self.alpha,
            "beta": self.beta,
            "mean": self.mean,
            "variance": self.variance,
            "std": self.std,
            "confidence_interval": list(self.confidence_interval),
            "updated_at": self.updated_at.isoformat(),
            "update_count": self.update_count,
        }


class BayesianConfidenceService:
    """ベイズ信頼度サービス.

    - Beta分布による信頼度のベイズ更新
    - 事前分布: Beta(2, 2)（中立的）
    - 新証拠到着時の逐次更新
    """

    DEFAULT_PRIOR_ALPHA: float = 2.0
    DEFAULT_PRIOR_BETA: float = 2.0

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._states: dict[str, BayesianState] = {}

    async def update_confidence(
        self,
        claim_id: str,
        is_supporting: bool,
        weight: float = 1.0,
    ) -> float:
        """証拠に基づいて信頼度を更新.

        Args:
            claim_id: 主張ID
            is_supporting: 支持証拠かどうか
            weight: 証拠の重み（信頼度スコアを反映）

        Returns:
            更新後の信頼度（Beta分布の平均）
        """
        state = self._get_or_create_state(claim_id)

        if is_supporting:
            state.alpha += weight
        else:
            state.beta += weight

        state.updated_at = datetime.now()
        state.update_count += 1

        self._logger.debug(
            "ベイズ更新: claim=%s, supporting=%s, weight=%.2f, "
            "alpha=%.2f, beta=%.2f, mean=%.4f",
            claim_id, is_supporting, weight,
            state.alpha, state.beta, state.mean,
        )

        return state.mean

    async def get_confidence_with_uncertainty(
        self,
        claim_id: str,
    ) -> tuple[float, float]:
        """信頼度と不確実性を取得.

        Args:
            claim_id: 主張ID

        Returns:
            (平均信頼度, 標準偏差) のタプル
        """
        state = self._get_or_create_state(claim_id)
        return state.mean, state.std

    async def get_bayesian_state(self, claim_id: str) -> BayesianState:
        """ベイズ状態を取得.

        Args:
            claim_id: 主張ID

        Returns:
            ベイズ状態
        """
        return self._get_or_create_state(claim_id)

    async def reset_state(self, claim_id: str) -> BayesianState:
        """ベイズ状態をリセット.

        Args:
            claim_id: 主張ID

        Returns:
            リセット後の状態
        """
        state = BayesianState(
            alpha=self.DEFAULT_PRIOR_ALPHA,
            beta=self.DEFAULT_PRIOR_BETA,
        )
        self._states[claim_id] = state
        return state

    async def batch_update(
        self,
        claim_id: str,
        supporting_count: int,
        opposing_count: int,
        weight: float = 1.0,
    ) -> float:
        """複数証拠の一括更新.

        Args:
            claim_id: 主張ID
            supporting_count: 支持証拠数
            opposing_count: 反対証拠数
            weight: 各証拠の重み

        Returns:
            更新後の信頼度
        """
        state = self._get_or_create_state(claim_id)
        state.alpha += supporting_count * weight
        state.beta += opposing_count * weight
        state.updated_at = datetime.now()
        state.update_count += supporting_count + opposing_count
        return state.mean

    def list_states(self) -> dict[str, BayesianState]:
        """全状態を取得."""
        return dict(self._states)

    def _get_or_create_state(self, claim_id: str) -> BayesianState:
        """状態を取得または新規作成."""
        if claim_id not in self._states:
            self._states[claim_id] = BayesianState(
                alpha=self.DEFAULT_PRIOR_ALPHA,
                beta=self.DEFAULT_PRIOR_BETA,
            )
        return self._states[claim_id]
