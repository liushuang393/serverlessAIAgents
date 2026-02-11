"""Prediction Review サービス.

予測復盤の管理を行うビジネスロジック層。
過去の予測と実際の結果を比較し、精度を評価します。
"""

import logging
import uuid
from datetime import date, datetime
from typing import Any

from apps.market_trend_monitor.backend.models import (
    Prediction,
    PredictionOutcome,
    PredictionReview,
    PredictionStatus,
)


class PredictionService:
    """予測復盤サービス.

    過去の予測と実際の結果を比較し、精度を評価します。
    Phase 9: 復盤完了後にAdaptiveScoringServiceへフィードバック。
    """

    def __init__(
        self,
        *,
        adaptive_scoring_service: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            adaptive_scoring_service: 適応的スコアリングサービス（Phase 9）
        """
        self._logger = logging.getLogger(self.__class__.__name__)
        # インメモリストレージ（将来的にDB化）
        self._predictions: dict[str, Prediction] = {}
        self._reviews: dict[str, PredictionReview] = {}
        # Phase 9: 適応的スコアリング
        self._adaptive_scoring = adaptive_scoring_service

    def create_prediction(
        self,
        claim_id: str,
        statement: str,
        confidence: float,
        target_date: date,
        metadata: dict[str, Any] | None = None,
    ) -> Prediction:
        """予測を作成.

        Args:
            claim_id: 関連する主張ID
            statement: 予測内容
            confidence: 信頼度
            target_date: 予測対象日
            metadata: メタデータ

        Returns:
            作成された予測
        """
        prediction = Prediction(
            id=str(uuid.uuid4()),
            statement=statement,
            target_date=target_date,
            confidence=confidence,
            claim_id=claim_id,
            created_at=datetime.now(),
            status=PredictionStatus.PENDING,
            metadata=metadata or {},
        )

        self._predictions[prediction.id] = prediction
        self._logger.info(f"Prediction created: {prediction.id}")
        return prediction

    def review_prediction(
        self,
        prediction_id: str,
        actual_outcome: str,
        outcome: PredictionOutcome,
        notes: str = "",
    ) -> PredictionReview | None:
        """予測を復盤.

        Args:
            prediction_id: 予測ID
            actual_outcome: 実際の結果
            outcome: 結果判定
            notes: 復盤メモ

        Returns:
            復盤結果（予測が存在しない場合はNone）
        """
        prediction = self._predictions.get(prediction_id)
        if not prediction:
            return None

        # 精度スコアを計算
        accuracy_score = self._calculate_accuracy(
            prediction=prediction,
            actual_outcome=actual_outcome,
            outcome=outcome,
        )

        # Phase 10: signal_scoresをメタデータに引き継いで軸別帰属分析に利用
        review_metadata = {}
        if prediction.metadata.get("signal_scores"):
            review_metadata["signal_scores"] = prediction.metadata["signal_scores"]

        review = PredictionReview(
            id=str(uuid.uuid4()),
            prediction_id=prediction_id,
            actual_outcome=actual_outcome,
            outcome=outcome,
            accuracy_score=accuracy_score,
            reviewed_at=datetime.now(),
            notes=notes,
            metadata=review_metadata,
        )

        self._reviews[review.id] = review
        self._logger.info(
            f"Prediction reviewed: {review.id}, "
            f"outcome={outcome.value}, accuracy={accuracy_score:.2f}"
        )

        # Phase 9: フィードバックループ - 重みの自動調整
        if self._adaptive_scoring:
            try:
                import asyncio
                recent_reviews = list(self._reviews.values())[-10:]
                asyncio.ensure_future(
                    self._adaptive_scoring.update_weights(recent_reviews)
                )
            except Exception as e:
                self._logger.warning("適応的スコアリング更新失敗: %s", e)

        return review

    def get_prediction(self, prediction_id: str) -> Prediction | None:
        """予測を取得."""
        return self._predictions.get(prediction_id)

    def get_review(self, review_id: str) -> PredictionReview | None:
        """復盤結果を取得."""
        return self._reviews.get(review_id)

    def list_predictions(
        self,
        reviewed: bool | None = None,
    ) -> list[Prediction]:
        """予測一覧を取得.

        Args:
            reviewed: 復盤済みフィルタ

        Returns:
            予測リスト
        """
        predictions = list(self._predictions.values())

        if reviewed is not None:
            reviewed_ids = {
                r.prediction_id for r in self._reviews.values()
            }
            if reviewed:
                predictions = [
                    p for p in predictions if p.id in reviewed_ids
                ]
            else:
                predictions = [
                    p for p in predictions if p.id not in reviewed_ids
                ]

        return sorted(
            predictions, key=lambda p: p.created_at, reverse=True
        )

    def get_accuracy_stats(self) -> dict[str, Any]:
        """精度統計を取得."""
        reviews = list(self._reviews.values())

        if not reviews:
            return {
                "total_predictions": len(self._predictions),
                "total_reviews": 0,
                "average_accuracy": 0.0,
                "outcome_distribution": {},
            }

        outcome_counts = {}
        for r in reviews:
            o = r.outcome.value
            outcome_counts[o] = outcome_counts.get(o, 0) + 1

        avg_accuracy = sum(r.accuracy_score for r in reviews) / len(reviews)

        return {
            "total_predictions": len(self._predictions),
            "total_reviews": len(reviews),
            "average_accuracy": avg_accuracy,
            "outcome_distribution": outcome_counts,
        }

    def _calculate_accuracy(
        self,
        prediction: Prediction,
        actual_outcome: str,
        outcome: PredictionOutcome,
    ) -> float:
        """精度スコアを計算.

        計算方法:
        - CORRECT: 1.0 * confidence
        - PARTIAL: 0.5 * confidence
        - INCORRECT: 0.0
        - UNKNOWN: 0.25
        """
        base_scores = {
            PredictionOutcome.CORRECT: 1.0,
            PredictionOutcome.PARTIAL: 0.5,
            PredictionOutcome.INCORRECT: 0.0,
            PredictionOutcome.UNKNOWN: 0.25,
        }

        base = base_scores.get(outcome, 0.0)
        return base * prediction.confidence

    def get_pending_reviews(self) -> list[Prediction]:
        """復盤待ちの予測を取得."""
        reviewed_ids = {r.prediction_id for r in self._reviews.values()}
        today = date.today()

        return [
            p for p in self._predictions.values()
            if p.id not in reviewed_ids and p.target_date <= today
        ]

    def get_review_for_prediction(
        self, prediction_id: str
    ) -> PredictionReview | None:
        """予測に対する復盤結果を取得."""
        for review in self._reviews.values():
            if review.prediction_id == prediction_id:
                return review
        return None

    def get_calibration_metrics(self) -> dict[str, Any]:
        """Phase 12: 予測キャリブレーションメトリクス（Brier Score）を取得.

        Brier score = mean((confidence - outcome)²)
        低いほど良い。完璧な校正で0.0。
        """
        reviews = list(self._reviews.values())
        if not reviews:
            return {
                "brier_score": None,
                "total_reviewed": 0,
                "calibration_bins": [],
            }

        # Brier Score計算
        brier_sum = 0.0
        bin_data: dict[int, dict[str, float]] = {}  # bin -> {sum_conf, sum_outcome, count}

        for review in reviews:
            prediction = self._predictions.get(review.prediction_id)
            if not prediction:
                continue

            confidence = prediction.confidence
            outcome_val = {
                PredictionOutcome.CORRECT: 1.0,
                PredictionOutcome.PARTIAL: 0.5,
                PredictionOutcome.INCORRECT: 0.0,
                PredictionOutcome.UNKNOWN: 0.5,
            }.get(review.outcome, 0.5)

            brier_sum += (confidence - outcome_val) ** 2

            # 10分割ビンでキャリブレーション
            bin_idx = min(int(confidence * 10), 9)
            if bin_idx not in bin_data:
                bin_data[bin_idx] = {"sum_conf": 0.0, "sum_outcome": 0.0, "count": 0.0}
            bin_data[bin_idx]["sum_conf"] += confidence
            bin_data[bin_idx]["sum_outcome"] += outcome_val
            bin_data[bin_idx]["count"] += 1

        brier_score = brier_sum / len(reviews)

        calibration_bins = []
        for bin_idx in sorted(bin_data.keys()):
            d = bin_data[bin_idx]
            calibration_bins.append({
                "bin_range": f"{bin_idx * 10}-{(bin_idx + 1) * 10}%",
                "avg_confidence": d["sum_conf"] / d["count"],
                "avg_outcome": d["sum_outcome"] / d["count"],
                "count": int(d["count"]),
            })

        return {
            "brier_score": round(brier_score, 4),
            "total_reviewed": len(reviews),
            "calibration_bins": calibration_bins,
        }
