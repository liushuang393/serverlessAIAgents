"""Prediction Review ユニットテスト.

Prediction/PredictionReview/PredictionAccuracy モデルおよび PredictionService のテスト。
"""

from __future__ import annotations

from datetime import date, timedelta

import pytest
from apps.market_trend_monitor.backend.models import (
    Prediction,
    PredictionAccuracy,
    PredictionOutcome,
    PredictionReview,
    PredictionStatus,
)
from apps.market_trend_monitor.backend.services.prediction_service import (
    PredictionService,
)


# ============================================================
# Model Tests
# ============================================================


class TestPredictionModel:
    """Prediction データモデルのテスト."""

    def test_prediction_creation(self) -> None:
        """予測生成テスト."""
        pred = Prediction(
            id="p-1",
            statement="AI市場は成長する",
            target_date=date(2025, 6, 1),
            confidence=0.8,
        )
        assert pred.id == "p-1"
        assert pred.status == PredictionStatus.PENDING
        assert pred.actual_outcome is None

    def test_prediction_to_dict(self) -> None:
        """to_dict 変換テスト."""
        pred = Prediction(
            id="p-1",
            statement="テスト予測",
            target_date=date(2025, 3, 1),
            confidence=0.7,
            claim_id="c-1",
        )
        d = pred.to_dict()
        assert d["id"] == "p-1"
        assert d["status"] == "pending"
        assert d["claim_id"] == "c-1"
        assert d["reviewed_at"] is None

    def test_prediction_is_due_past(self) -> None:
        """過去日付の期限チェック."""
        pred = Prediction(
            id="p-1",
            statement="test",
            target_date=date(2020, 1, 1),
            confidence=0.5,
        )
        assert pred.is_due() is True

    def test_prediction_is_due_future(self) -> None:
        """未来日付の期限チェック."""
        pred = Prediction(
            id="p-1",
            statement="test",
            target_date=date.today() + timedelta(days=365),
            confidence=0.5,
        )
        assert pred.is_due() is False

    def test_prediction_review_method(self) -> None:
        """予測レビューメソッドテスト."""
        pred = Prediction(
            id="p-1",
            statement="test",
            target_date=date(2025, 1, 1),
            confidence=0.8,
        )
        pred.review(
            status=PredictionStatus.CORRECT,
            actual_outcome="予測通り成長した",
            review_note="的中",
        )
        assert pred.status == PredictionStatus.CORRECT
        assert pred.actual_outcome == "予測通り成長した"
        assert pred.review_note == "的中"
        assert pred.reviewed_at is not None


class TestPredictionReviewModel:
    """PredictionReview データモデルのテスト."""

    def test_review_creation(self) -> None:
        """復盤結果生成テスト."""
        review = PredictionReview(
            id="r-1",
            prediction_id="p-1",
            actual_outcome="実際の結果",
            outcome=PredictionOutcome.CORRECT,
            accuracy_score=0.8,
        )
        assert review.outcome == PredictionOutcome.CORRECT
        assert review.accuracy_score == 0.8

    def test_review_to_dict(self) -> None:
        """to_dict 変換テスト."""
        review = PredictionReview(
            id="r-1",
            prediction_id="p-1",
            actual_outcome="テスト結果",
            outcome=PredictionOutcome.PARTIAL,
            accuracy_score=0.4,
            notes="部分的に的中",
        )
        d = review.to_dict()
        assert d["outcome"] == "partial"
        assert d["notes"] == "部分的に的中"


class TestPredictionAccuracyModel:
    """PredictionAccuracy データモデルのテスト."""

    def test_accuracy_rate_all_correct(self) -> None:
        """全的中時の正解率テスト."""
        acc = PredictionAccuracy(
            total_count=10,
            correct_count=10,
            partial_count=0,
            incorrect_count=0,
            pending_count=0,
        )
        assert acc.accuracy_rate == pytest.approx(1.0)

    def test_accuracy_rate_with_partial(self) -> None:
        """部分的中を含む正解率テスト."""
        acc = PredictionAccuracy(
            total_count=4,
            correct_count=2,
            partial_count=2,
            incorrect_count=0,
            pending_count=0,
        )
        # (2 + 2*0.5) / 4 = 3.0 / 4 = 0.75
        assert acc.accuracy_rate == pytest.approx(0.75)

    def test_accuracy_rate_excludes_pending(self) -> None:
        """PENDING除外の正解率テスト."""
        acc = PredictionAccuracy(
            total_count=10,
            correct_count=3,
            partial_count=0,
            incorrect_count=2,
            pending_count=5,
        )
        # reviewed = 10 - 5 = 5, correct_weighted = 3 / 5 = 0.6
        assert acc.accuracy_rate == pytest.approx(0.6)

    def test_accuracy_rate_all_pending(self) -> None:
        """全てPENDINGの正解率テスト."""
        acc = PredictionAccuracy(
            total_count=5,
            correct_count=0,
            partial_count=0,
            incorrect_count=0,
            pending_count=5,
        )
        assert acc.accuracy_rate == 0.0

    def test_accuracy_rate_zero_total(self) -> None:
        """件数0の正解率テスト."""
        acc = PredictionAccuracy()
        assert acc.accuracy_rate == 0.0

    def test_to_dict(self) -> None:
        """to_dict 変換テスト."""
        acc = PredictionAccuracy(
            total_count=3,
            correct_count=2,
            partial_count=1,
            incorrect_count=0,
            pending_count=0,
            period_start=date(2025, 1, 1),
            period_end=date(2025, 1, 31),
        )
        d = acc.to_dict()
        assert d["total_count"] == 3
        assert d["accuracy_rate"] == pytest.approx(0.833, abs=0.01)
        assert d["period_start"] == "2025-01-01"


class TestPredictionStatus:
    """PredictionStatus Enum のテスト."""

    def test_status_values(self) -> None:
        """全ステータスの値テスト."""
        assert PredictionStatus.PENDING.value == "pending"
        assert PredictionStatus.CORRECT.value == "correct"
        assert PredictionStatus.PARTIAL.value == "partial"
        assert PredictionStatus.INCORRECT.value == "incorrect"


class TestPredictionOutcome:
    """PredictionOutcome Enum のテスト."""

    def test_outcome_values(self) -> None:
        """全結果判定の値テスト."""
        assert PredictionOutcome.CORRECT.value == "correct"
        assert PredictionOutcome.PARTIAL.value == "partial"
        assert PredictionOutcome.INCORRECT.value == "incorrect"
        assert PredictionOutcome.UNKNOWN.value == "unknown"


# ============================================================
# Service Tests
# ============================================================


class TestPredictionService:
    """PredictionService のテスト."""

    def test_create_prediction(self) -> None:
        """予測作成テスト."""
        service = PredictionService()
        pred = service.create_prediction(
            claim_id="c-1",
            statement="AI市場は成長する",
            confidence=0.8,
            target_date=date(2025, 6, 1),
        )

        assert pred.id is not None
        assert pred.statement == "AI市場は成長する"
        assert pred.confidence == 0.8
        assert pred.status == PredictionStatus.PENDING

    def test_create_prediction_with_metadata(self) -> None:
        """メタデータ付き予測作成テスト."""
        service = PredictionService()
        pred = service.create_prediction(
            claim_id="c-1",
            statement="テスト",
            confidence=0.5,
            target_date=date(2025, 3, 1),
            metadata={"source": "analysis"},
        )
        assert pred.metadata == {"source": "analysis"}

    def test_review_prediction_correct(self) -> None:
        """的中レビューテスト."""
        service = PredictionService()
        pred = service.create_prediction(
            claim_id="c-1",
            statement="AI市場は成長する",
            confidence=0.8,
            target_date=date(2025, 1, 1),
        )

        review = service.review_prediction(
            prediction_id=pred.id,
            actual_outcome="AI市場は30%成長した",
            outcome=PredictionOutcome.CORRECT,
            notes="的中",
        )

        assert review is not None
        assert review.outcome == PredictionOutcome.CORRECT
        # accuracy = 1.0 * 0.8 = 0.8
        assert review.accuracy_score == pytest.approx(0.8)

    def test_review_prediction_partial(self) -> None:
        """部分的中レビューテスト."""
        service = PredictionService()
        pred = service.create_prediction(
            claim_id="c-1",
            statement="テスト",
            confidence=0.6,
            target_date=date(2025, 1, 1),
        )

        review = service.review_prediction(
            prediction_id=pred.id,
            actual_outcome="部分的に的中",
            outcome=PredictionOutcome.PARTIAL,
        )

        assert review is not None
        # accuracy = 0.5 * 0.6 = 0.3
        assert review.accuracy_score == pytest.approx(0.3)

    def test_review_prediction_incorrect(self) -> None:
        """外れレビューテスト."""
        service = PredictionService()
        pred = service.create_prediction(
            claim_id="c-1",
            statement="テスト",
            confidence=0.9,
            target_date=date(2025, 1, 1),
        )

        review = service.review_prediction(
            prediction_id=pred.id,
            actual_outcome="予測と異なる結果",
            outcome=PredictionOutcome.INCORRECT,
        )

        assert review is not None
        assert review.accuracy_score == pytest.approx(0.0)

    def test_review_prediction_unknown(self) -> None:
        """不明レビューテスト."""
        service = PredictionService()
        pred = service.create_prediction(
            claim_id="c-1",
            statement="テスト",
            confidence=0.8,
            target_date=date(2025, 1, 1),
        )

        review = service.review_prediction(
            prediction_id=pred.id,
            actual_outcome="判定不能",
            outcome=PredictionOutcome.UNKNOWN,
        )

        assert review is not None
        # accuracy = 0.25 * 0.8 = 0.2
        assert review.accuracy_score == pytest.approx(0.2)

    def test_review_nonexistent_prediction(self) -> None:
        """存在しない予測のレビューテスト."""
        service = PredictionService()
        review = service.review_prediction(
            prediction_id="nonexistent",
            actual_outcome="test",
            outcome=PredictionOutcome.CORRECT,
        )
        assert review is None

    def test_get_prediction(self) -> None:
        """予測取得テスト."""
        service = PredictionService()
        pred = service.create_prediction(
            claim_id="c-1",
            statement="テスト",
            confidence=0.5,
            target_date=date(2025, 6, 1),
        )

        retrieved = service.get_prediction(pred.id)
        assert retrieved is not None
        assert retrieved.id == pred.id

    def test_get_prediction_not_found(self) -> None:
        """存在しない予測取得テスト."""
        service = PredictionService()
        assert service.get_prediction("nonexistent") is None

    def test_list_predictions(self) -> None:
        """予測一覧テスト."""
        service = PredictionService()
        service.create_prediction("c-1", "予測1", 0.5, date(2025, 3, 1))
        service.create_prediction("c-2", "予測2", 0.7, date(2025, 6, 1))

        predictions = service.list_predictions()
        assert len(predictions) == 2

    def test_list_predictions_reviewed_filter(self) -> None:
        """復盤済みフィルタテスト."""
        service = PredictionService()
        p1 = service.create_prediction("c-1", "予測1", 0.5, date(2025, 1, 1))
        service.create_prediction("c-2", "予測2", 0.7, date(2025, 6, 1))

        service.review_prediction(p1.id, "結果", PredictionOutcome.CORRECT)

        reviewed = service.list_predictions(reviewed=True)
        assert len(reviewed) == 1
        assert reviewed[0].id == p1.id

        unreviewed = service.list_predictions(reviewed=False)
        assert len(unreviewed) == 1

    def test_get_accuracy_stats_empty(self) -> None:
        """空の精度統計テスト."""
        service = PredictionService()
        stats = service.get_accuracy_stats()

        assert stats["total_predictions"] == 0
        assert stats["total_reviews"] == 0
        assert stats["average_accuracy"] == 0.0

    def test_get_accuracy_stats(self) -> None:
        """精度統計テスト."""
        service = PredictionService()
        p1 = service.create_prediction("c-1", "予測1", 0.8, date(2025, 1, 1))
        p2 = service.create_prediction("c-2", "予測2", 0.6, date(2025, 1, 1))

        service.review_prediction(p1.id, "的中", PredictionOutcome.CORRECT)
        service.review_prediction(p2.id, "外れ", PredictionOutcome.INCORRECT)

        stats = service.get_accuracy_stats()
        assert stats["total_predictions"] == 2
        assert stats["total_reviews"] == 2
        # avg = (0.8 * 1.0 + 0.6 * 0.0) / 2 = 0.4
        assert stats["average_accuracy"] == pytest.approx(0.4)
        assert stats["outcome_distribution"]["correct"] == 1
        assert stats["outcome_distribution"]["incorrect"] == 1

    def test_get_pending_reviews(self) -> None:
        """復盤待ち予測取得テスト."""
        service = PredictionService()
        # 過去の期限（復盤待ち）
        p1 = service.create_prediction("c-1", "過去予測", 0.7, date(2020, 1, 1))
        # 未来の期限（まだ復盤不要）
        service.create_prediction("c-2", "未来予測", 0.5, date.today() + timedelta(days=365))

        pending = service.get_pending_reviews()
        assert len(pending) == 1
        assert pending[0].id == p1.id

    def test_get_pending_reviews_excludes_reviewed(self) -> None:
        """復盤済みは復盤待ちから除外テスト."""
        service = PredictionService()
        p = service.create_prediction("c-1", "過去予測", 0.7, date(2020, 1, 1))
        service.review_prediction(p.id, "結果", PredictionOutcome.CORRECT)

        pending = service.get_pending_reviews()
        assert len(pending) == 0

    def test_get_review_for_prediction(self) -> None:
        """予測の復盤結果取得テスト."""
        service = PredictionService()
        p = service.create_prediction("c-1", "テスト", 0.8, date(2025, 1, 1))
        service.review_prediction(p.id, "結果", PredictionOutcome.CORRECT)

        review = service.get_review_for_prediction(p.id)
        assert review is not None
        assert review.prediction_id == p.id

    def test_get_review_for_prediction_not_found(self) -> None:
        """復盤結果なしのテスト."""
        service = PredictionService()
        p = service.create_prediction("c-1", "テスト", 0.5, date(2025, 6, 1))

        review = service.get_review_for_prediction(p.id)
        assert review is None

    def test_bootstrap_from_trends_creates_predictions(self) -> None:
        """トレンドから予測を自動生成できること."""
        service = PredictionService()
        trends = [
            {
                "id": "t-1",
                "topic": "COBOL migration",
                "score": 0.72,
                "growth_rate": 0.2,
                "articles_count": 10,
            },
            {
                "id": "t-2",
                "topic": "Legacy modernization",
                "score": 0.61,
                "growth_rate": 0.1,
                "articles_count": 6,
            },
        ]
        result = service.bootstrap_from_trends(trends, horizon_days=30, limit=8)
        assert result["created_count"] == 2
        assert len(service.list_predictions()) == 2

    def test_bootstrap_from_trends_skips_duplicates(self) -> None:
        """同一トレンドの重複生成を回避できること."""
        service = PredictionService()
        first_trends = [
            {
                "id": "t-dup-1",
                "topic": "Mainframe to cloud",
                "score": 0.7,
                "growth_rate": 0.15,
                "articles_count": 8,
            }
        ]
        second_trends = [
            {
                "id": "t-dup-2",
                "topic": "Mainframe to cloud",
                "score": 0.68,
                "growth_rate": 0.1,
                "articles_count": 7,
            }
        ]
        first = service.bootstrap_from_trends(first_trends, horizon_days=30, limit=8)
        second = service.bootstrap_from_trends(second_trends, horizon_days=30, limit=8)
        assert first["created_count"] == 1
        assert second["created_count"] == 0
        assert second["skipped_count"] == 1
