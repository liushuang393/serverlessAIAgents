"""多次元評価スコア計算エンジン.

目的:
    - 8次元評価のスコア計算（重み付け平均）
    - inverse 次元の正規化処理
    - 閾値判定（GO/PILOT/DELAY/NO_GO）
    - Hard veto チェック

作成日: 2026-02-10
"""

from __future__ import annotations

import logging
from enum import Enum

from pydantic import BaseModel, Field

from apps.decision_governance_engine.config import DecisionWeightsConfig


logger = logging.getLogger(__name__)


class DecisionVerdict(str, Enum):
    """決定判定結果."""

    GO = "GO"
    PILOT = "PILOT"
    DELAY = "DELAY"
    NO_GO = "NO_GO"


class DimensionScore(BaseModel):
    """次元別スコア."""

    dimension_id: str = Field(..., description="次元ID")
    raw_score: float = Field(..., ge=1.0, le=5.0, description="生スコア（1-5）")
    normalized_score: float = Field(..., ge=1.0, le=5.0, description="正規化スコア")
    weight: int = Field(..., ge=0, le=100, description="重み（%）")
    weighted_contribution: float = Field(..., description="重み付け寄与度")
    is_inverse: bool = Field(default=False, description="逆転評価か")


class ScoringResult(BaseModel):
    """スコアリング結果."""

    weighted_score: float = Field(..., ge=1.0, le=5.0, description="重み付け平均スコア")
    dimension_scores: list[DimensionScore] = Field(..., description="次元別スコア詳細")
    verdict: DecisionVerdict = Field(..., description="判定結果")
    confidence: float = Field(..., ge=0.0, le=1.0, description="信頼度")
    evidence_coverage: float = Field(..., ge=0.0, le=1.0, description="証拠カバレッジ")
    hard_veto_triggered: bool = Field(default=False, description="Hard veto が発動したか")
    hard_veto_reason: str | None = Field(default=None, description="Hard veto の理由")
    threshold_details: dict[str, float] = Field(
        default_factory=dict, description="閾値詳細"
    )


class ScoringEngine:
    """多次元評価スコア計算エンジン.

    8次元の評価スコアを重み付け平均し、閾値判定を行う。
    """

    def __init__(self, config: DecisionWeightsConfig) -> None:
        """初期化.

        Args:
            config: 決策評価パラメータ設定
        """
        self.config = config
        self._validate_config()

    def _validate_config(self) -> None:
        """設定の妥当性検証."""
        weights = self.config.get_weights()
        total_weight = sum(weights.values())

        if total_weight != 100:
            logger.warning(f"重みの合計が 100 ではありません: {total_weight}")

        # 全次元が定義されているか確認
        for dim_id in weights:
            if dim_id not in self.config.dimensions:
                msg = f"次元 {dim_id} が dimensions に定義されていません"
                raise ValueError(msg)

    def calculate_score(
        self,
        dimension_scores: dict[str, float],
        confidence: float = 1.0,
        evidence_coverage: float = 1.0,
    ) -> ScoringResult:
        """多次元スコアを計算.

        Args:
            dimension_scores: 次元ID -> 生スコア（1-5）のマッピング
            confidence: 信頼度（0.0-1.0）
            evidence_coverage: 証拠カバレッジ（0.0-1.0）

        Returns:
            ScoringResult: スコアリング結果

        Raises:
            ValueError: スコアが範囲外の場合
        """
        weights = self.config.get_weights()
        dimensions = self.config.dimensions

        # 1. 次元別スコア計算（正規化 + 重み付け）
        dim_score_details: list[DimensionScore] = []
        total_weighted_score = 0.0
        total_weight = 0

        for dim_id, raw_score in dimension_scores.items():
            if dim_id not in dimensions:
                logger.warning(f"未定義の次元をスキップ: {dim_id}")
                continue

            dim_config = dimensions[dim_id]
            weight = weights.get(dim_id, 0)

            # スコア範囲チェック
            if not (dim_config.min_score <= raw_score <= dim_config.max_score):
                msg = f"次元 {dim_id} のスコアが範囲外: {raw_score}"
                raise ValueError(msg)

            # inverse 次元の正規化（スコアを反転）
            normalized = raw_score
            if dim_config.inverse:
                # 例: cost=5（高コスト）→ normalized=1（悪い）
                # 例: cost=1（低コスト）→ normalized=5（良い）
                normalized = dim_config.max_score - raw_score + dim_config.min_score

            # 重み付け寄与度
            weighted_contribution = normalized * (weight / 100.0)

            dim_score_details.append(
                DimensionScore(
                    dimension_id=dim_id,
                    raw_score=raw_score,
                    normalized_score=normalized,
                    weight=weight,
                    weighted_contribution=weighted_contribution,
                    is_inverse=dim_config.inverse,
                )
            )

            total_weighted_score += weighted_contribution
            total_weight += weight

        # 2. 重み付け平均スコア
        if total_weight == 0:
            msg = "有効な次元スコアがありません"
            raise ValueError(msg)

        weighted_score = total_weighted_score / (total_weight / 100.0)

        # 3. Hard veto チェック
        hard_veto_triggered, hard_veto_reason = self._check_hard_veto(dimension_scores)

        # 4. 閾値判定
        verdict = self._determine_verdict(
            weighted_score, confidence, evidence_coverage, hard_veto_triggered
        )

        # 5. 閾値詳細
        thresholds = self.config.get_thresholds()
        threshold_details = {
            "go_min_score": thresholds.go.min_score,
            "pilot_min_score": thresholds.pilot.min_score,
            "delay_min_score": thresholds.delay.min_score,
            "go_min_confidence": thresholds.go.min_confidence,
            "pilot_min_confidence": thresholds.pilot.min_confidence,
        }

        return ScoringResult(
            weighted_score=round(weighted_score, 2),
            dimension_scores=dim_score_details,
            verdict=verdict,
            confidence=confidence,
            evidence_coverage=evidence_coverage,
            hard_veto_triggered=hard_veto_triggered,
            hard_veto_reason=hard_veto_reason,
            threshold_details=threshold_details,
        )

    def _check_hard_veto(
        self, dimension_scores: dict[str, float]
    ) -> tuple[bool, str | None]:
        """Hard veto チェック.

        特定の次元が閾値を超えた場合、無条件で却下。

        Args:
            dimension_scores: 次元ID -> 生スコア

        Returns:
            (veto発動, 理由) のタプル
        """
        thresholds = self.config.get_thresholds()
        hard_veto_dims = thresholds.go.hard_veto_dimensions
        hard_veto_score = thresholds.go.hard_veto_score

        for dim_id in hard_veto_dims:
            if dim_id not in dimension_scores:
                continue

            score = dimension_scores[dim_id]
            dim_config = self.config.dimensions.get(dim_id)

            if not dim_config:
                continue

            # raw_score は 1-5 の共通評価尺度を想定し、
            # hard_veto は「高リスク/高コストなど悪化方向」を閾値で判定する。
            # inverse 次元でも raw_score の意味は同じため、閾値以上を veto とする。
            is_veto = score >= hard_veto_score

            if is_veto:
                reason = (
                    f"Hard veto: {dim_id}={score} "
                    f"(閾値: {hard_veto_score}, inverse={dim_config.inverse})"
                )
                logger.warning(reason)
                return True, reason

        return False, None

    def _determine_verdict(
        self,
        weighted_score: float,
        confidence: float,
        evidence_coverage: float,
        hard_veto: bool,
    ) -> DecisionVerdict:
        """閾値判定.

        Args:
            weighted_score: 重み付けスコア
            confidence: 信頼度
            evidence_coverage: 証拠カバレッジ
            hard_veto: Hard veto が発動したか

        Returns:
            DecisionVerdict: 判定結果
        """
        if hard_veto:
            return DecisionVerdict.NO_GO

        thresholds = self.config.get_thresholds()

        # GO 判定
        if (
            weighted_score >= thresholds.go.min_score
            and confidence >= thresholds.go.min_confidence
            and evidence_coverage >= thresholds.go.min_evidence_coverage
        ):
            return DecisionVerdict.GO

        # PILOT 判定
        if (
            weighted_score >= thresholds.pilot.min_score
            and confidence >= thresholds.pilot.min_confidence
            and evidence_coverage >= thresholds.pilot.min_evidence_coverage
        ):
            return DecisionVerdict.PILOT

        # DELAY 判定
        if (
            weighted_score >= thresholds.delay.min_score
            and confidence >= thresholds.delay.min_confidence
            and evidence_coverage >= thresholds.delay.min_evidence_coverage
        ):
            return DecisionVerdict.DELAY

        # NO_GO
        return DecisionVerdict.NO_GO


__all__ = [
    "ScoringEngine",
    "ScoringResult",
    "DimensionScore",
    "DecisionVerdict",
]
