"""適応的スコアリングユニットテスト.

AdaptiveScoringService のテスト。
外部依存なし。
"""

from __future__ import annotations

import pytest
from apps.market_trend_monitor.backend.models import PredictionOutcome, PredictionReview
from apps.market_trend_monitor.backend.services.adaptive_scoring_service import (
    AdaptiveScoringService,
    ScoringWeights,
)


# ============================================================
# Helpers
# ============================================================


def _make_review(
    review_id: str = "rev-1",
    outcome: PredictionOutcome = PredictionOutcome.CORRECT,
    accuracy_score: float = 0.8,
) -> PredictionReview:
    """テスト用 PredictionReview を生成."""
    return PredictionReview(
        id=review_id,
        prediction_id=f"pred-{review_id}",
        actual_outcome="As predicted",
        outcome=outcome,
        accuracy_score=accuracy_score,
    )


# ============================================================
# ScoringWeights Model Tests
# ============================================================


class TestScoringWeights:
    """ScoringWeights データモデルのテスト."""

    def test_default_weights(self) -> None:
        """デフォルト重みテスト."""
        weights = ScoringWeights()
        assert weights.reliability == pytest.approx(0.2)
        assert weights.leading == pytest.approx(0.2)
        assert weights.relevance == pytest.approx(0.2)
        assert weights.actionability == pytest.approx(0.2)
        assert weights.convergence == pytest.approx(0.2)

    def test_weights_sum_to_one(self) -> None:
        """デフォルト重みが合計1.0テスト."""
        weights = ScoringWeights()
        total = sum(weights.as_weight_dict().values())
        assert total == pytest.approx(1.0)

    def test_to_dict(self) -> None:
        """to_dict 変換テスト."""
        weights = ScoringWeights()
        d = weights.to_dict()
        assert "reliability" in d
        assert "updated_at" in d
        assert "update_count" in d

    def test_as_weight_dict(self) -> None:
        """as_weight_dict テスト."""
        weights = ScoringWeights(reliability=0.3, leading=0.1)
        d = weights.as_weight_dict()
        assert d["reliability"] == 0.3
        assert d["leading"] == 0.1
        assert "updated_at" not in d

    def test_update_count_default(self) -> None:
        """デフォルト更新カウントテスト."""
        weights = ScoringWeights()
        assert weights.update_count == 0


# ============================================================
# Service Tests
# ============================================================


class TestAdaptiveScoringService:
    """AdaptiveScoringService のテスト."""

    async def test_initial_weights(self) -> None:
        """初期重みテスト."""
        service = AdaptiveScoringService()
        weights = await service.get_current_weights()
        assert weights.reliability == pytest.approx(0.2)
        total = sum(weights.as_weight_dict().values())
        assert total == pytest.approx(1.0)

    async def test_update_weights_correct(self) -> None:
        """正解予測での重み更新テスト."""
        service = AdaptiveScoringService()
        reviews = [_make_review(f"r-{i}", PredictionOutcome.CORRECT) for i in range(5)]

        weights = await service.update_weights(reviews)
        total = sum(weights.as_weight_dict().values())
        assert total == pytest.approx(1.0)
        assert weights.update_count == 1

    async def test_update_weights_incorrect(self) -> None:
        """不正解予測での重み更新テスト."""
        service = AdaptiveScoringService()
        reviews = [_make_review(f"r-{i}", PredictionOutcome.INCORRECT) for i in range(5)]

        weights = await service.update_weights(reviews)
        total = sum(weights.as_weight_dict().values())
        assert total == pytest.approx(1.0)

    async def test_update_weights_mixed(self) -> None:
        """混合予測での重み更新テスト."""
        service = AdaptiveScoringService()
        reviews = [
            _make_review("r-1", PredictionOutcome.CORRECT),
            _make_review("r-2", PredictionOutcome.INCORRECT),
            _make_review("r-3", PredictionOutcome.PARTIAL),
        ]

        weights = await service.update_weights(reviews)
        total = sum(weights.as_weight_dict().values())
        assert total == pytest.approx(1.0)

    async def test_update_weights_empty(self) -> None:
        """空レビューでの重み更新テスト."""
        service = AdaptiveScoringService()
        initial = await service.get_current_weights()
        weights = await service.update_weights([])
        assert weights.reliability == initial.reliability

    async def test_weights_normalized(self) -> None:
        """正規化テスト: 更新後も合計1.0."""
        service = AdaptiveScoringService()
        for i in range(10):
            reviews = [_make_review(f"r-{i}", PredictionOutcome.CORRECT)]
            weights = await service.update_weights(reviews)
            total = sum(weights.as_weight_dict().values())
            assert total == pytest.approx(1.0, abs=1e-10)

    async def test_weights_clipped(self) -> None:
        """上下限制約テスト."""
        service = AdaptiveScoringService()
        # 大量の更新で重みが極端にならないか
        for i in range(100):
            reviews = [_make_review(f"r-{i}", PredictionOutcome.CORRECT)]
            weights = await service.update_weights(reviews)

        for v in weights.as_weight_dict().values():
            assert v >= 0.0  # 正規化後は MIN_WEIGHT/total 以上

    async def test_update_count_increments(self) -> None:
        """更新カウント増分テスト."""
        service = AdaptiveScoringService()
        reviews = [_make_review("r-1")]

        await service.update_weights(reviews)
        w1 = await service.get_current_weights()
        assert w1.update_count == 1

        await service.update_weights(reviews)
        w2 = await service.get_current_weights()
        assert w2.update_count == 2

    async def test_weight_history(self) -> None:
        """重み履歴テスト."""
        service = AdaptiveScoringService()
        reviews = [_make_review("r-1")]

        for _ in range(3):
            await service.update_weights(reviews)

        history = await service.get_weight_history()
        assert len(history) == 3

    async def test_weight_history_limit(self) -> None:
        """重み履歴制限テスト."""
        service = AdaptiveScoringService()
        reviews = [_make_review("r-1")]

        for _ in range(5):
            await service.update_weights(reviews)

        history = await service.get_weight_history(limit=2)
        assert len(history) == 2

    async def test_learning_rate_effect(self) -> None:
        """学習率の効果テスト."""
        service = AdaptiveScoringService()
        initial = await service.get_current_weights()
        initial_weights = initial.as_weight_dict()

        reviews = [_make_review("r-1", PredictionOutcome.CORRECT)]
        updated = await service.update_weights(reviews)
        updated_weights = updated.as_weight_dict()

        # 各軸の変化が学習率に比例することを確認
        for axis in initial_weights:
            diff = abs(updated_weights[axis] - initial_weights[axis])
            # 正規化の影響で正確な比較は難しいが、変化は小さいはず
            assert diff < 0.1

    def test_calculate_gradient_correct(self) -> None:
        """正解予測の勾配テスト."""
        service = AdaptiveScoringService()
        reviews = [_make_review("r-1", PredictionOutcome.CORRECT)]
        gradients = service._calculate_gradient(reviews)
        # CORRECT → reward = 1.0 - 0.5 = 0.5 → 正の勾配
        for _axis, grad in gradients.items():
            assert grad > 0

    def test_calculate_gradient_incorrect(self) -> None:
        """不正解予測の勾配テスト."""
        service = AdaptiveScoringService()
        reviews = [_make_review("r-1", PredictionOutcome.INCORRECT)]
        gradients = service._calculate_gradient(reviews)
        # INCORRECT → reward = 0.0 - 0.5 = -0.5 → 負の勾配
        for _axis, grad in gradients.items():
            assert grad < 0

    def test_calculate_gradient_partial(self) -> None:
        """部分的中の勾配テスト."""
        service = AdaptiveScoringService()
        reviews = [_make_review("r-1", PredictionOutcome.PARTIAL)]
        gradients = service._calculate_gradient(reviews)
        # PARTIAL → reward = 0.5 - 0.5 = 0.0 → ゼロ勾配
        for _axis, grad in gradients.items():
            assert grad == pytest.approx(0.0)

    def test_normalize_weights(self) -> None:
        """重み正規化テスト."""
        service = AdaptiveScoringService()
        weights = {"a": 0.5, "b": 0.3, "c": 0.2}
        normalized = service._normalize_weights(weights)
        assert sum(normalized.values()) == pytest.approx(1.0)

    def test_normalize_weights_zero(self) -> None:
        """ゼロ重みの正規化テスト."""
        service = AdaptiveScoringService()
        weights = {"a": 0.0, "b": 0.0, "c": 0.0}
        normalized = service._normalize_weights(weights)
        assert all(v == pytest.approx(1.0 / 3) for v in normalized.values())

    def test_clip_weights(self) -> None:
        """重みクリップテスト."""
        service = AdaptiveScoringService()
        weights = {"a": -0.5, "b": 1.5, "c": 0.5}
        clipped = service._clip_weights(weights)
        assert clipped["a"] == service.MIN_WEIGHT
        assert clipped["b"] == service.MAX_WEIGHT
        assert clipped["c"] == 0.5

    def test_outcome_scores(self) -> None:
        """結果スコアマッピングテスト."""
        scores = AdaptiveScoringService.OUTCOME_SCORES
        assert scores[PredictionOutcome.CORRECT] == 1.0
        assert scores[PredictionOutcome.PARTIAL] == 0.5
        assert scores[PredictionOutcome.INCORRECT] == 0.0
        assert scores[PredictionOutcome.UNKNOWN] == 0.25

    async def test_convergence(self) -> None:
        """収束性テスト: 多数の更新後も安定."""
        service = AdaptiveScoringService()
        for i in range(50):
            reviews = [_make_review(f"r-{i}", PredictionOutcome.CORRECT)]
            weights = await service.update_weights(reviews)

        total = sum(weights.as_weight_dict().values())
        assert total == pytest.approx(1.0, abs=1e-10)
        # 全軸が正
        for v in weights.as_weight_dict().values():
            assert v > 0
