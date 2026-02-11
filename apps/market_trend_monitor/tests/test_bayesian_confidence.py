"""ベイズ信頼度ユニットテスト.

BayesianConfidenceService のテスト。
外部依存なし。
"""

from __future__ import annotations

import math

import pytest

from apps.market_trend_monitor.backend.services.bayesian_confidence_service import (
    BayesianConfidenceService,
    BayesianState,
)


# ============================================================
# BayesianState Model Tests
# ============================================================


class TestBayesianState:
    """BayesianState データモデルのテスト."""

    def test_default_prior(self) -> None:
        """デフォルト事前分布テスト."""
        state = BayesianState()
        assert state.alpha == 2.0
        assert state.beta == 2.0

    def test_mean_calculation(self) -> None:
        """平均値計算テスト."""
        state = BayesianState(alpha=2.0, beta=2.0)
        assert state.mean == pytest.approx(0.5)

        state = BayesianState(alpha=8.0, beta=2.0)
        assert state.mean == pytest.approx(0.8)

        state = BayesianState(alpha=1.0, beta=9.0)
        assert state.mean == pytest.approx(0.1)

    def test_mean_zero_total(self) -> None:
        """合計ゼロ時の平均値テスト."""
        state = BayesianState(alpha=0.0, beta=0.0)
        assert state.mean == 0.5

    def test_variance_calculation(self) -> None:
        """分散計算テスト."""
        state = BayesianState(alpha=2.0, beta=2.0)
        # Beta(2,2) の分散 = 2*2 / (4^2 * 5) = 4/80 = 0.05
        assert state.variance == pytest.approx(0.05)

    def test_variance_decreases_with_evidence(self) -> None:
        """証拠が増えると分散が減少するテスト."""
        state_few = BayesianState(alpha=3.0, beta=3.0)
        state_many = BayesianState(alpha=30.0, beta=30.0)
        assert state_many.variance < state_few.variance

    def test_std_calculation(self) -> None:
        """標準偏差計算テスト."""
        state = BayesianState(alpha=2.0, beta=2.0)
        assert state.std == pytest.approx(math.sqrt(0.05))

    def test_confidence_interval(self) -> None:
        """信頼区間テスト."""
        state = BayesianState(alpha=2.0, beta=2.0)
        lower, upper = state.confidence_interval
        assert lower < state.mean
        assert upper > state.mean
        assert lower >= 0.0
        assert upper <= 1.0

    def test_confidence_interval_tight(self) -> None:
        """多数データ後の狭い信頼区間テスト."""
        state = BayesianState(alpha=100.0, beta=100.0)
        lower, upper = state.confidence_interval
        # 大量データで信頼区間が狭くなる
        # Beta(100,100): std≈0.035, 95% CI width≈0.138
        assert upper - lower < 0.15

    def test_to_dict(self) -> None:
        """to_dict 変換テスト."""
        state = BayesianState(alpha=3.0, beta=2.0)
        d = state.to_dict()
        assert d["alpha"] == 3.0
        assert d["beta"] == 2.0
        assert "mean" in d
        assert "variance" in d
        assert "confidence_interval" in d


# ============================================================
# Service Tests
# ============================================================


class TestBayesianConfidenceService:
    """BayesianConfidenceService のテスト."""

    async def test_initial_state(self) -> None:
        """初期状態テスト."""
        service = BayesianConfidenceService()
        state = await service.get_bayesian_state("claim-1")
        assert state.alpha == 2.0
        assert state.beta == 2.0
        assert state.mean == pytest.approx(0.5)

    async def test_update_supporting(self) -> None:
        """支持証拠での更新テスト."""
        service = BayesianConfidenceService()
        confidence = await service.update_confidence("claim-1", is_supporting=True)
        # alpha: 2+1=3, beta: 2 → mean = 3/5 = 0.6
        assert confidence == pytest.approx(0.6)

    async def test_update_opposing(self) -> None:
        """反対証拠での更新テスト."""
        service = BayesianConfidenceService()
        confidence = await service.update_confidence("claim-1", is_supporting=False)
        # alpha: 2, beta: 2+1=3 → mean = 2/5 = 0.4
        assert confidence == pytest.approx(0.4)

    async def test_sequential_updates(self) -> None:
        """逐次更新テスト."""
        service = BayesianConfidenceService()

        # 3回支持
        for _ in range(3):
            await service.update_confidence("claim-1", is_supporting=True)

        # alpha: 2+3=5, beta: 2 → mean = 5/7
        state = await service.get_bayesian_state("claim-1")
        assert state.alpha == pytest.approx(5.0)
        assert state.beta == pytest.approx(2.0)
        assert state.mean == pytest.approx(5.0 / 7.0)

    async def test_weighted_update(self) -> None:
        """重み付き更新テスト."""
        service = BayesianConfidenceService()
        await service.update_confidence("claim-1", is_supporting=True, weight=0.9)
        state = await service.get_bayesian_state("claim-1")
        assert state.alpha == pytest.approx(2.9)

    async def test_get_confidence_with_uncertainty(self) -> None:
        """信頼度・不確実性取得テスト."""
        service = BayesianConfidenceService()
        mean, std = await service.get_confidence_with_uncertainty("claim-1")
        assert mean == pytest.approx(0.5)
        assert std > 0

    async def test_uncertainty_decreases(self) -> None:
        """証拠追加で不確実性が減少するテスト."""
        service = BayesianConfidenceService()
        _, initial_std = await service.get_confidence_with_uncertainty("claim-1")

        for _ in range(10):
            await service.update_confidence("claim-1", is_supporting=True)

        _, updated_std = await service.get_confidence_with_uncertainty("claim-1")
        assert updated_std < initial_std

    async def test_reset_state(self) -> None:
        """状態リセットテスト."""
        service = BayesianConfidenceService()
        await service.update_confidence("claim-1", is_supporting=True)
        state = await service.reset_state("claim-1")
        assert state.alpha == 2.0
        assert state.beta == 2.0

    async def test_batch_update(self) -> None:
        """一括更新テスト."""
        service = BayesianConfidenceService()
        confidence = await service.batch_update("claim-1", supporting_count=3, opposing_count=1)
        state = await service.get_bayesian_state("claim-1")
        # alpha: 2+3=5, beta: 2+1=3 → mean = 5/8 = 0.625
        assert state.alpha == pytest.approx(5.0)
        assert state.beta == pytest.approx(3.0)
        assert confidence == pytest.approx(0.625)

    async def test_batch_update_weighted(self) -> None:
        """重み付き一括更新テスト."""
        service = BayesianConfidenceService()
        await service.batch_update("claim-1", supporting_count=2, opposing_count=0, weight=0.5)
        state = await service.get_bayesian_state("claim-1")
        assert state.alpha == pytest.approx(3.0)  # 2 + 2*0.5
        assert state.beta == pytest.approx(2.0)

    async def test_list_states(self) -> None:
        """全状態一覧テスト."""
        service = BayesianConfidenceService()
        await service.update_confidence("claim-1", is_supporting=True)
        await service.update_confidence("claim-2", is_supporting=False)
        states = service.list_states()
        assert len(states) == 2
        assert "claim-1" in states
        assert "claim-2" in states

    async def test_independent_claims(self) -> None:
        """独立した主張の独立性テスト."""
        service = BayesianConfidenceService()
        await service.update_confidence("claim-1", is_supporting=True)
        state1 = await service.get_bayesian_state("claim-1")
        state2 = await service.get_bayesian_state("claim-2")
        assert state1.alpha != state2.alpha

    async def test_update_count_tracking(self) -> None:
        """更新カウント追跡テスト."""
        service = BayesianConfidenceService()
        await service.update_confidence("claim-1", is_supporting=True)
        await service.update_confidence("claim-1", is_supporting=True)
        state = await service.get_bayesian_state("claim-1")
        assert state.update_count == 2

    async def test_convergence_to_true_rate(self) -> None:
        """多数更新後の真の確率への収束テスト."""
        service = BayesianConfidenceService()
        # 80%支持、20%反対
        for _ in range(80):
            await service.update_confidence("claim-1", is_supporting=True)
        for _ in range(20):
            await service.update_confidence("claim-1", is_supporting=False)

        state = await service.get_bayesian_state("claim-1")
        # Beta(82, 22) → mean ≈ 0.788
        assert abs(state.mean - 0.8) < 0.05  # 0.8に近い

    async def test_prior_influence_diminishes(self) -> None:
        """事前分布の影響が減少するテスト."""
        service = BayesianConfidenceService()

        # 少数データ: 事前分布 (2,2) の影響が大きい
        await service.update_confidence("few", is_supporting=True)
        state_few = await service.get_bayesian_state("few")
        # alpha=3, beta=2 → 0.6 (事前により 1.0 から離れる)

        # 多数データ: 事前分布の影響が小さい
        for _ in range(100):
            await service.update_confidence("many", is_supporting=True)
        state_many = await service.get_bayesian_state("many")
        # alpha=102, beta=2 → ~0.98 (1.0 に近い)

        assert state_many.mean > state_few.mean
