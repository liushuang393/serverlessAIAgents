"""ScoringEngine のユニットテスト.

スコア計算、inverse 次元の正規化、閾値判定をテスト。
"""
import pytest

from apps.decision_governance_engine.config import (
    DecisionWeightsConfig,
    DimensionConfig,
    PresetConfig,
    PresetThresholds,
    ThresholdConfig,
)
from apps.decision_governance_engine.services.scoring_engine import (
    DecisionVerdict,
    ScoringEngine,
)


@pytest.fixture
def mock_config() -> DecisionWeightsConfig:
    """テスト用の設定を作成."""
    dimensions = {
        "user_impact": DimensionConfig(
            id="user_impact",
            name_zh="用户价值",
            name_ja="ユーザー価値",
            name_en="User Impact",
            description="ユーザー価値",
            min_score=1,
            max_score=5,
            inverse=False,
        ),
        "cost": DimensionConfig(
            id="cost",
            name_zh="成本",
            name_ja="コスト",
            name_en="Cost",
            description="コスト",
            min_score=1,
            max_score=5,
            inverse=True,  # コストは低い方が良い
        ),
        "risk": DimensionConfig(
            id="risk",
            name_zh="风险",
            name_ja="リスク",
            name_en="Risk",
            description="リスク",
            min_score=1,
            max_score=5,
            inverse=True,  # リスクは低い方が良い
        ),
    }

    thresholds = PresetThresholds(
        go=ThresholdConfig(
            min_score=4.0,
            min_confidence=0.7,
            min_evidence_coverage=0.6,
            hard_veto_dimensions=["risk"],
            hard_veto_score=4,
        ),
        pilot=ThresholdConfig(
            min_score=3.0,
            min_confidence=0.5,
            min_evidence_coverage=0.4,
        ),
        delay=ThresholdConfig(
            min_score=2.5,
            min_confidence=0.3,
            min_evidence_coverage=0.2,
        ),
    )

    preset = PresetConfig(
        id="test_preset",
        name_zh="测试",
        name_ja="テスト",
        name_en="Test",
        description="テスト用プリセット",
        weights={
            "user_impact": 50,
            "cost": 30,
            "risk": 20,
        },
        thresholds=thresholds,
    )

    return DecisionWeightsConfig(
        version="1.0.0",
        active_preset="test_preset",
        dimensions=dimensions,
        presets={"test_preset": preset},
        mode_settings={},
        evidence_reliability={"domain_scores": {}, "freshness_decay": {}},
    )


def test_scoring_engine_basic_calculation(mock_config: DecisionWeightsConfig) -> None:
    """基本的なスコア計算をテスト."""
    engine = ScoringEngine(mock_config)

    dimension_scores = {
        "user_impact": 5.0,  # 高い = 良い
        "cost": 2.0,  # 低い = 良い（inverse）
        "risk": 2.0,  # 低い = 良い（inverse）
    }

    result = engine.calculate_score(
        dimension_scores=dimension_scores,
        confidence=0.8,
        evidence_coverage=0.7,
    )

    # user_impact: 5.0 * 0.5 = 2.5
    # cost: (5 - 2 + 1) = 4.0 * 0.3 = 1.2
    # risk: (5 - 2 + 1) = 4.0 * 0.2 = 0.8
    # 合計: 2.5 + 1.2 + 0.8 = 4.5
    assert result.weighted_score == pytest.approx(4.5, abs=0.01)
    assert result.verdict == DecisionVerdict.GO
    assert not result.hard_veto_triggered


def test_inverse_dimension_normalization(mock_config: DecisionWeightsConfig) -> None:
    """inverse 次元の正規化をテスト."""
    engine = ScoringEngine(mock_config)

    # コスト=5（高コスト）は正規化後 1（悪い）になるべき
    dimension_scores = {
        "user_impact": 3.0,
        "cost": 5.0,  # 高コスト = 悪い
        "risk": 1.0,  # 低リスク = 良い
    }

    result = engine.calculate_score(
        dimension_scores=dimension_scores,
        confidence=0.8,
        evidence_coverage=0.7,
    )

    # cost の正規化: 5 - 5 + 1 = 1
    cost_detail = next(d for d in result.dimension_scores if d.dimension_id == "cost")
    assert cost_detail.raw_score == 5.0
    assert cost_detail.normalized_score == 1.0
    assert cost_detail.is_inverse is True


def test_hard_veto_triggered(mock_config: DecisionWeightsConfig) -> None:
    """Hard veto が発動することをテスト."""
    engine = ScoringEngine(mock_config)

    # risk=5（高リスク）で hard veto 発動
    dimension_scores = {
        "user_impact": 5.0,
        "cost": 1.0,
        "risk": 5.0,  # hard_veto_score=4 以上
    }

    result = engine.calculate_score(
        dimension_scores=dimension_scores,
        confidence=0.9,
        evidence_coverage=0.9,
    )

    assert result.hard_veto_triggered is True
    assert result.verdict == DecisionVerdict.NO_GO
    assert "Hard veto" in (result.hard_veto_reason or "")


def test_verdict_pilot(mock_config: DecisionWeightsConfig) -> None:
    """PILOT 判定をテスト."""
    engine = ScoringEngine(mock_config)

    dimension_scores = {
        "user_impact": 3.0,
        "cost": 3.0,
        "risk": 3.0,
    }

    result = engine.calculate_score(
        dimension_scores=dimension_scores,
        confidence=0.6,  # GO には足りない
        evidence_coverage=0.5,
    )

    # スコアは 3.0 付近（PILOT 範囲）
    assert result.verdict == DecisionVerdict.PILOT


def test_verdict_delay(mock_config: DecisionWeightsConfig) -> None:
    """DELAY 判定をテスト."""
    engine = ScoringEngine(mock_config)

    dimension_scores = {
        "user_impact": 2.5,
        "cost": 3.0,
        "risk": 3.0,
    }

    result = engine.calculate_score(
        dimension_scores=dimension_scores,
        confidence=0.4,
        evidence_coverage=0.3,
    )

    assert result.verdict == DecisionVerdict.DELAY


def test_verdict_no_go(mock_config: DecisionWeightsConfig) -> None:
    """NO_GO 判定をテスト."""
    engine = ScoringEngine(mock_config)

    dimension_scores = {
        "user_impact": 1.0,
        "cost": 5.0,
        "risk": 5.0,
    }

    result = engine.calculate_score(
        dimension_scores=dimension_scores,
        confidence=0.2,
        evidence_coverage=0.1,
    )

    assert result.verdict == DecisionVerdict.NO_GO


def test_invalid_score_range(mock_config: DecisionWeightsConfig) -> None:
    """スコア範囲外のエラーをテスト."""
    engine = ScoringEngine(mock_config)

    dimension_scores = {
        "user_impact": 6.0,  # 範囲外（max=5）
        "cost": 3.0,
        "risk": 3.0,
    }

    with pytest.raises(ValueError, match="範囲外"):
        engine.calculate_score(
            dimension_scores=dimension_scores,
            confidence=0.8,
            evidence_coverage=0.7,
        )


def test_dimension_score_details(mock_config: DecisionWeightsConfig) -> None:
    """次元別スコア詳細をテスト."""
    engine = ScoringEngine(mock_config)

    dimension_scores = {
        "user_impact": 4.0,
        "cost": 2.0,
        "risk": 3.0,
    }

    result = engine.calculate_score(
        dimension_scores=dimension_scores,
        confidence=0.8,
        evidence_coverage=0.7,
    )

    assert len(result.dimension_scores) == 3

    # user_impact の詳細
    user_impact = next(d for d in result.dimension_scores if d.dimension_id == "user_impact")
    assert user_impact.raw_score == 4.0
    assert user_impact.normalized_score == 4.0  # inverse=False
    assert user_impact.weight == 50
    assert user_impact.weighted_contribution == pytest.approx(2.0, abs=0.01)

    # cost の詳細（inverse）
    cost = next(d for d in result.dimension_scores if d.dimension_id == "cost")
    assert cost.raw_score == 2.0
    assert cost.normalized_score == 4.0  # 5 - 2 + 1 = 4
    assert cost.is_inverse is True
