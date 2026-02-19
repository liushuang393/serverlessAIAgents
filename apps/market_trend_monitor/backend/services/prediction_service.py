"""Prediction Review サービス.

予測復盤の管理を行うビジネスロジック層。
過去の予測と実際の結果を比較し、精度を評価します。
"""

import asyncio
import logging
import uuid
from collections.abc import Awaitable, Callable
from datetime import date, datetime, timedelta
from typing import Any

from apps.market_trend_monitor.backend.db import init_db
from apps.market_trend_monitor.backend.db.models import (
    PredictionModel,
    PredictionReviewModel,
)
from apps.market_trend_monitor.backend.db.session import async_session
from apps.market_trend_monitor.backend.models import (
    Prediction,
    PredictionOutcome,
    PredictionReview,
    PredictionStatus,
)
from sqlalchemy import select
from sqlalchemy.exc import IntegrityError


class PredictionService:
    """予測復盤サービス.

    過去の予測と実際の結果を比較し、精度を評価します。
    Phase 9: 復盤完了後にAdaptiveScoringServiceへフィードバック。
    """

    def __init__(
        self,
        *,
        adaptive_scoring_service: Any | None = None,
        session_factory: Any | None = None,
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
        self._session_factory = session_factory or async_session
        self._loaded_from_db = False
        self._prediction_persist_lock = asyncio.Lock()
        self._review_persist_lock = asyncio.Lock()

    async def initialize(self) -> None:
        """永続層から予測/復盤データをロード."""
        if self._loaded_from_db:
            return
        await init_db()

        async with self._session_factory() as session:
            prediction_rows = (await session.execute(select(PredictionModel))).scalars().all()
            review_rows = (await session.execute(select(PredictionReviewModel))).scalars().all()

        predictions: dict[str, Prediction] = {}
        for row in prediction_rows:
            predictions[row.id] = Prediction(
                id=row.id,
                statement=row.statement,
                target_date=self._parse_iso_date(row.target_date),
                confidence=float(row.confidence),
                claim_id=row.claim_id,
                created_at=row.created_at,
                status=self._parse_prediction_status(row.status),
                metadata=dict(row.metadata_json or {}),
            )

        reviews: dict[str, PredictionReview] = {}
        for row in review_rows:
            review = PredictionReview(
                id=row.id,
                prediction_id=row.prediction_id,
                actual_outcome=row.actual_outcome,
                outcome=self._parse_prediction_outcome(row.outcome),
                accuracy_score=float(row.accuracy_score),
                reviewed_at=row.reviewed_at,
                notes=row.notes,
                metadata=dict(row.metadata_json or {}),
            )
            reviews[review.id] = review

            prediction = predictions.get(review.prediction_id)
            if prediction:
                prediction.status = self._status_from_outcome(review.outcome)
                prediction.actual_outcome = review.actual_outcome
                prediction.review_note = review.notes
                prediction.reviewed_at = review.reviewed_at

        self._predictions = predictions
        self._reviews = reviews
        self._loaded_from_db = True
        self._logger.info(
            "PredictionService 初期化: predictions=%s reviews=%s",
            len(predictions),
            len(reviews),
        )

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
        self._schedule_task(lambda: self.persist_prediction(prediction))
        return prediction

    def bootstrap_from_trends(
        self,
        trends: list[dict[str, Any]],
        *,
        horizon_days: int = 30,
        limit: int = 8,
    ) -> dict[str, Any]:
        """トレンド一覧から予測を自動生成.

        重複生成を避けるため、trend_id（なければtopic）単位で既存予測を判定する。
        """
        if limit <= 0:
            return {
                "created": [],
                "created_count": 0,
                "skipped_count": 0,
            }

        existing_keys: set[str] = set()
        for prediction in self._predictions.values():
            key = self._trend_prediction_key(prediction.metadata)
            if key:
                existing_keys.add(key)

        created: list[Prediction] = []
        skipped_count = 0
        target_date = date.today() + timedelta(days=max(horizon_days, 1))

        sorted_trends = sorted(
            trends,
            key=lambda trend: float(trend.get("score", 0.0)),
            reverse=True,
        )

        for trend in sorted_trends:
            if len(created) >= limit:
                break

            key = self._trend_prediction_key(trend)
            if key and key in existing_keys:
                skipped_count += 1
                continue

            topic = str(trend.get("topic", "")).strip()
            if not topic:
                skipped_count += 1
                continue

            confidence = self._build_bootstrap_confidence(trend)
            statement = (
                f"今後{max(horizon_days, 1)}日以内に「{topic}」関連シグナルは現在水準以上を維持する"
            )
            metadata = {
                "source": "trend_bootstrap",
                "trend_id": str(trend.get("id", "")),
                "topic": topic,
                "trend_score": float(trend.get("score", 0.0)),
                "growth_rate": float(trend.get("growth_rate", 0.0)),
                "articles_count": int(trend.get("articles_count", trend.get("article_count", 0))),
            }

            prediction = self.create_prediction(
                claim_id=str(trend.get("id", "")),
                statement=statement,
                confidence=confidence,
                target_date=target_date,
                metadata=metadata,
            )
            created.append(prediction)

            if key:
                existing_keys.add(key)

        return {
            "created": [prediction.to_dict() for prediction in created],
            "created_count": len(created),
            "skipped_count": skipped_count,
            "created_ids": [prediction.id for prediction in created],
        }

    def _trend_prediction_key(self, payload: dict[str, Any]) -> str:
        """重複回避用キーを生成."""
        # Analyzer の trend_id は実行ごとに変わるため、topic を優先キーにする
        topic = str(payload.get("topic", payload.get("statement", ""))).strip().casefold()
        if topic:
            return f"topic:{topic}"

        trend_id = str(payload.get("trend_id", payload.get("id", ""))).strip()
        if trend_id:
            return f"trend:{trend_id}"
        return ""

    def _build_bootstrap_confidence(self, trend: dict[str, Any]) -> float:
        """トレンド情報から初期信頼度を算出."""
        score = float(trend.get("score", 0.5))
        growth_rate = float(trend.get("growth_rate", 0.0))
        articles_count = int(trend.get("articles_count", trend.get("article_count", 0)))

        confidence = score
        if growth_rate > 0:
            confidence += 0.08
        elif growth_rate < 0:
            confidence -= 0.08

        if articles_count >= 8:
            confidence += 0.05
        elif articles_count <= 1:
            confidence -= 0.05

        # 極端値を避ける
        return max(0.2, min(confidence, 0.95))

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

        prediction.review(
            status=self._status_from_outcome(outcome),
            actual_outcome=actual_outcome,
            review_note=notes,
        )
        self._reviews[review.id] = review
        self._logger.info(
            f"Prediction reviewed: {review.id}, "
            f"outcome={outcome.value}, accuracy={accuracy_score:.2f}"
        )

        # Phase 9: フィードバックループ - 重みの自動調整
        if self._adaptive_scoring:
            try:
                recent_reviews = list(self._reviews.values())[-10:]
                self._schedule_task(lambda: self._adaptive_scoring.update_weights(recent_reviews))
            except Exception as e:
                self._logger.warning("適応的スコアリング更新失敗: %s", e)

        self._schedule_task(lambda: self.persist_prediction(prediction))
        self._schedule_task(lambda: self.persist_review(review))
        return review

    async def persist_prediction(self, prediction: Prediction) -> None:
        """予測を永続化（upsert）."""
        async with self._prediction_persist_lock:
            await init_db()
            async with self._session_factory() as session:
                try:
                    row = await session.get(PredictionModel, prediction.id)
                    if row is None:
                        row = PredictionModel(id=prediction.id)
                        session.add(row)

                    self._apply_prediction_row(row, prediction)
                    await session.commit()
                    return
                except IntegrityError as exc:
                    await session.rollback()
                    self._logger.warning(
                        "予測の同時保存競合を解消: prediction_id=%s",
                        prediction.id,
                    )
                    row = await session.get(PredictionModel, prediction.id)
                    if row is None:
                        raise exc

                    self._apply_prediction_row(row, prediction)
                    await session.commit()

    async def persist_review(self, review: PredictionReview) -> None:
        """復盤結果を永続化（upsert）."""
        async with self._review_persist_lock:
            await init_db()
            async with self._session_factory() as session:
                try:
                    row = await session.get(PredictionReviewModel, review.id)
                    if row is None:
                        row = PredictionReviewModel(id=review.id)
                        session.add(row)

                    self._apply_review_row(row, review)
                    prediction_row = await session.get(PredictionModel, review.prediction_id)
                    if prediction_row is not None:
                        prediction_row.status = self._status_from_outcome(review.outcome).value
                    await session.commit()
                    return
                except IntegrityError as exc:
                    await session.rollback()
                    self._logger.warning(
                        "復盤結果の同時保存競合を解消: review_id=%s",
                        review.id,
                    )
                    row = await session.get(PredictionReviewModel, review.id)
                    if row is None:
                        raise exc

                    self._apply_review_row(row, review)
                    prediction_row = await session.get(PredictionModel, review.prediction_id)
                    if prediction_row is not None:
                        prediction_row.status = self._status_from_outcome(review.outcome).value
                    await session.commit()

    async def persist_predictions_by_ids(self, prediction_ids: list[str]) -> int:
        """ID一覧で予測を永続化."""
        persisted = 0
        for prediction_id in prediction_ids:
            prediction = self._predictions.get(prediction_id)
            if not prediction:
                continue
            await self.persist_prediction(prediction)
            persisted += 1
        return persisted

    def _schedule_task(self, task_factory: Callable[[], Awaitable[Any]]) -> None:
        """実行中イベントループに永続化タスクを登録."""
        try:
            loop = asyncio.get_running_loop()
        except RuntimeError:
            return
        loop.create_task(task_factory())

    @staticmethod
    def _apply_prediction_row(
        row: PredictionModel,
        prediction: Prediction,
    ) -> None:
        """PredictionModel へ Prediction の値を反映."""
        row.statement = prediction.statement
        row.target_date = prediction.target_date.isoformat()
        row.confidence = float(prediction.confidence)
        row.claim_id = prediction.claim_id or None
        row.status = prediction.status.value
        row.created_at = prediction.created_at
        row.metadata_json = dict(prediction.metadata or {})

    @staticmethod
    def _apply_review_row(
        row: PredictionReviewModel,
        review: PredictionReview,
    ) -> None:
        """PredictionReviewModel へ PredictionReview の値を反映."""
        row.prediction_id = review.prediction_id
        row.actual_outcome = review.actual_outcome
        row.outcome = review.outcome.value
        row.accuracy_score = float(review.accuracy_score)
        row.reviewed_at = review.reviewed_at
        row.notes = review.notes
        row.metadata_json = dict(review.metadata or {})

    @staticmethod
    def _status_from_outcome(outcome: PredictionOutcome) -> PredictionStatus:
        """復盤結果を PredictionStatus に変換."""
        mapping = {
            PredictionOutcome.CORRECT: PredictionStatus.CORRECT,
            PredictionOutcome.PARTIAL: PredictionStatus.PARTIAL,
            PredictionOutcome.INCORRECT: PredictionStatus.INCORRECT,
            PredictionOutcome.UNKNOWN: PredictionStatus.PARTIAL,
        }
        return mapping.get(outcome, PredictionStatus.PENDING)

    @staticmethod
    def _parse_iso_date(raw: str) -> date:
        """日付文字列を安全に date へ変換."""
        try:
            return date.fromisoformat(raw)
        except ValueError:
            return date.today()

    @staticmethod
    def _parse_prediction_status(raw: str) -> PredictionStatus:
        """保存値から PredictionStatus を復元."""
        try:
            return PredictionStatus(raw)
        except ValueError:
            return PredictionStatus.PENDING

    @staticmethod
    def _parse_prediction_outcome(raw: str) -> PredictionOutcome:
        """保存値から PredictionOutcome を復元."""
        try:
            return PredictionOutcome(raw)
        except ValueError:
            return PredictionOutcome.UNKNOWN

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
            reviewed_ids = {r.prediction_id for r in self._reviews.values()}
            if reviewed:
                predictions = [p for p in predictions if p.id in reviewed_ids]
            else:
                predictions = [p for p in predictions if p.id not in reviewed_ids]

        return sorted(predictions, key=lambda p: p.created_at, reverse=True)

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
            p
            for p in self._predictions.values()
            if p.id not in reviewed_ids and p.target_date <= today
        ]

    def get_review_for_prediction(self, prediction_id: str) -> PredictionReview | None:
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
            calibration_bins.append(
                {
                    "bin_range": f"{bin_idx * 10}-{(bin_idx + 1) * 10}%",
                    "avg_confidence": d["sum_conf"] / d["count"],
                    "avg_outcome": d["sum_outcome"] / d["count"],
                    "count": int(d["count"]),
                }
            )

        return {
            "brier_score": round(brier_score, 4),
            "total_reviewed": len(reviews),
            "calibration_bins": calibration_bins,
        }
