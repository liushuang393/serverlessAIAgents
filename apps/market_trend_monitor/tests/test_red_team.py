"""Red Team ユニットテスト.

Challenge/ChallengeResult/RedTeamAnalysis モデルおよび RedTeamService のテスト。
"""

from __future__ import annotations

import pytest
from apps.market_trend_monitor.backend.models import (
    Challenge,
    ChallengeResult,
    ChallengeType,
    Claim,
    ClaimLevel,
    CounterArgument,
    InvalidationCondition,
    RedTeamAnalysis,
)
from apps.market_trend_monitor.backend.services.redteam_service import RedTeamService


# ============================================================
# Model Tests
# ============================================================


class TestChallengeModel:
    """Challenge データモデルのテスト."""

    def test_challenge_creation(self) -> None:
        """チャレンジ生成テスト."""
        challenge = Challenge(
            id="ch-1",
            claim_id="c-1",
            challenge_type=ChallengeType.COUNTER_EVIDENCE,
            argument="反証がある",
        )
        assert challenge.id == "ch-1"
        assert challenge.challenge_type == ChallengeType.COUNTER_EVIDENCE

    def test_challenge_to_dict(self) -> None:
        """to_dict 変換テスト."""
        challenge = Challenge(
            id="ch-1",
            claim_id="c-1",
            challenge_type=ChallengeType.ALTERNATIVE_EXPLANATION,
            argument="代替説明",
            counter_evidence_ids=["ev-1"],
        )
        d = challenge.to_dict()
        assert d["challenge_type"] == "alternative_explanation"
        assert d["counter_evidence_ids"] == ["ev-1"]


class TestChallengeResultModel:
    """ChallengeResult データモデルのテスト."""

    def test_result_creation(self) -> None:
        """評価結果生成テスト."""
        result = ChallengeResult(
            id="cr-1",
            challenge_id="ch-1",
            is_valid=True,
            impact_score=0.7,
            confidence_adjustment=-0.21,
        )
        assert result.is_valid is True
        assert result.impact_score == 0.7

    def test_result_to_dict(self) -> None:
        """to_dict 変換テスト."""
        result = ChallengeResult(
            id="cr-1",
            challenge_id="ch-1",
            is_valid=False,
            impact_score=0.3,
            confidence_adjustment=0.0,
            evaluator_notes="根拠不十分",
        )
        d = result.to_dict()
        assert d["is_valid"] is False
        assert d["evaluator_notes"] == "根拠不十分"


class TestCounterArgumentModel:
    """CounterArgument データモデルのテスト."""

    def test_creation(self) -> None:
        """反論生成テスト."""
        ca = CounterArgument(argument="反論内容", strength=0.8)
        assert ca.strength == 0.8

    def test_to_dict(self) -> None:
        """to_dict 変換テスト."""
        ca = CounterArgument(
            argument="テスト反論",
            strength=0.6,
            evidence_ids=["ev-1"],
        )
        d = ca.to_dict()
        assert d["argument"] == "テスト反論"
        assert d["evidence_ids"] == ["ev-1"]

    def test_default_strength(self) -> None:
        """デフォルト強度テスト."""
        ca = CounterArgument(argument="test")
        assert ca.strength == 0.5


class TestInvalidationConditionModel:
    """InvalidationCondition データモデルのテスト."""

    def test_creation(self) -> None:
        """失効条件生成テスト."""
        ic = InvalidationCondition(
            condition="市場クラッシュが発生",
            probability=0.2,
            trigger_indicators=["株価暴落"],
        )
        assert ic.probability == 0.2

    def test_to_dict(self) -> None:
        """to_dict 変換テスト."""
        ic = InvalidationCondition(
            condition="規制変更",
            probability=0.4,
        )
        d = ic.to_dict()
        assert d["condition"] == "規制変更"
        assert d["probability"] == 0.4


class TestRedTeamAnalysisModel:
    """RedTeamAnalysis データモデルのテスト."""

    def test_creation(self) -> None:
        """分析結果生成テスト."""
        analysis = RedTeamAnalysis(
            id="rta-1",
            claim_id="c-1",
            recommendation="継続監視",
        )
        assert analysis.overall_uncertainty == 0.0

    def test_calculate_uncertainty_with_both(self) -> None:
        """反論+失効条件の不確実性計算テスト."""
        analysis = RedTeamAnalysis(
            id="rta-1",
            claim_id="c-1",
            counter_arguments=[
                CounterArgument(argument="反論1", strength=0.6),
                CounterArgument(argument="反論2", strength=0.8),
            ],
            invalidation_conditions=[
                InvalidationCondition(condition="条件1", probability=0.3),
                InvalidationCondition(condition="条件2", probability=0.5),
            ],
        )
        uncertainty = analysis.calculate_uncertainty()
        # counter_weight = (0.6 + 0.8) / 2 = 0.7
        # invalidation_weight = (0.3 + 0.5) / 2 = 0.4
        # uncertainty = (0.7 + 0.4) / 2 = 0.55
        assert uncertainty == pytest.approx(0.55)
        assert analysis.overall_uncertainty == pytest.approx(0.55)

    def test_calculate_uncertainty_counter_only(self) -> None:
        """反論のみの不確実性計算テスト."""
        analysis = RedTeamAnalysis(
            id="rta-1",
            claim_id="c-1",
            counter_arguments=[
                CounterArgument(argument="反論", strength=0.6),
            ],
        )
        uncertainty = analysis.calculate_uncertainty()
        # counter_weight = 0.6, invalidation_weight = 0.0
        # uncertainty = (0.6 + 0.0) / 2 = 0.3
        assert uncertainty == pytest.approx(0.3)

    def test_calculate_uncertainty_empty(self) -> None:
        """反論・失効条件なしの不確実性計算テスト."""
        analysis = RedTeamAnalysis(id="rta-1", claim_id="c-1")
        assert analysis.calculate_uncertainty() == 0.0

    def test_to_dict(self) -> None:
        """to_dict 変換テスト."""
        analysis = RedTeamAnalysis(
            id="rta-1",
            claim_id="c-1",
            counter_arguments=[CounterArgument(argument="反論", strength=0.5)],
            recommendation="監視継続",
        )
        d = analysis.to_dict()
        assert d["id"] == "rta-1"
        assert len(d["counter_arguments"]) == 1
        assert d["recommendation"] == "監視継続"


class TestChallengeType:
    """ChallengeType Enum のテスト."""

    def test_type_values(self) -> None:
        """全タイプの値テスト."""
        assert ChallengeType.COUNTER_EVIDENCE.value == "counter_evidence"
        assert ChallengeType.ALTERNATIVE_EXPLANATION.value == "alternative_explanation"
        assert ChallengeType.BIAS_CHECK.value == "bias_check"
        assert ChallengeType.ASSUMPTION_CHALLENGE.value == "assumption_challenge"


# ============================================================
# Service Tests
# ============================================================


def _make_claim(
    claim_id: str = "c-1",
    statement: str = "AI市場は2025年に成長する",
    confidence: float = 0.7,
) -> Claim:
    """テスト用 Claim を生成."""
    return Claim(
        id=claim_id,
        statement=statement,
        level=ClaimLevel.HYPOTHESIS,
        confidence=confidence,
        evidence_ids=["ev-1"],
    )


class TestRedTeamService:
    """RedTeamService のテスト."""

    def test_create_challenge(self) -> None:
        """チャレンジ作成テスト."""
        service = RedTeamService()
        claim = _make_claim()

        challenge = service.create_challenge(
            claim=claim,
            challenge_type=ChallengeType.COUNTER_EVIDENCE,
            argument="反証が存在する",
        )

        assert challenge.id is not None
        assert challenge.claim_id == "c-1"
        assert challenge.challenge_type == ChallengeType.COUNTER_EVIDENCE

    def test_create_challenge_with_evidence(self) -> None:
        """反証証拠付きチャレンジ作成テスト."""
        service = RedTeamService()
        claim = _make_claim()

        challenge = service.create_challenge(
            claim=claim,
            challenge_type=ChallengeType.COUNTER_EVIDENCE,
            argument="反証",
            counter_evidence_ids=["ev-counter-1"],
        )

        assert challenge.counter_evidence_ids == ["ev-counter-1"]

    def test_evaluate_challenge_valid(self) -> None:
        """有効なチャレンジ評価テスト."""
        service = RedTeamService()
        claim = _make_claim()
        challenge = service.create_challenge(
            claim=claim,
            challenge_type=ChallengeType.COUNTER_EVIDENCE,
            argument="反証",
        )

        result = service.evaluate_challenge(
            challenge=challenge,
            is_valid=True,
            impact_score=0.7,
            evaluator_notes="有効な反証",
        )

        assert result.is_valid is True
        assert result.impact_score == 0.7
        # confidence_adjustment = -0.7 * 0.3 = -0.21
        assert result.confidence_adjustment == pytest.approx(-0.21)

    def test_evaluate_challenge_invalid(self) -> None:
        """無効なチャレンジ評価テスト."""
        service = RedTeamService()
        claim = _make_claim()
        challenge = service.create_challenge(
            claim=claim,
            challenge_type=ChallengeType.BIAS_CHECK,
            argument="バイアスなし",
        )

        result = service.evaluate_challenge(
            challenge=challenge,
            is_valid=False,
            impact_score=0.3,
        )

        assert result.is_valid is False
        # 無効なチャレンジは信頼度調整なし
        assert result.confidence_adjustment == 0.0

    def test_list_challenges_for_claim(self) -> None:
        """主張別チャレンジ一覧テスト."""
        service = RedTeamService()
        claim1 = _make_claim(claim_id="c-1")
        claim2 = _make_claim(claim_id="c-2")

        service.create_challenge(claim1, ChallengeType.COUNTER_EVIDENCE, "反論1")
        service.create_challenge(claim1, ChallengeType.ALTERNATIVE_EXPLANATION, "代替説明")
        service.create_challenge(claim2, ChallengeType.BIAS_CHECK, "バイアス")

        c1_challenges = service.list_challenges_for_claim("c-1")
        assert len(c1_challenges) == 2

        c2_challenges = service.list_challenges_for_claim("c-2")
        assert len(c2_challenges) == 1

    def test_get_adjusted_confidence(self) -> None:
        """調整済み信頼度テスト."""
        service = RedTeamService()
        claim = _make_claim(confidence=0.8)

        ch1 = service.create_challenge(claim, ChallengeType.COUNTER_EVIDENCE, "反証1")
        service.evaluate_challenge(ch1, is_valid=True, impact_score=0.5)

        adjusted = service.get_adjusted_confidence(claim)
        # original: 0.8, adjustment: -0.5 * 0.3 = -0.15
        assert adjusted == pytest.approx(0.65)

    def test_get_adjusted_confidence_no_challenges(self) -> None:
        """チャレンジなしの調整済み信頼度テスト."""
        service = RedTeamService()
        claim = _make_claim(confidence=0.7)

        adjusted = service.get_adjusted_confidence(claim)
        assert adjusted == pytest.approx(0.7)

    def test_get_adjusted_confidence_clamped(self) -> None:
        """調整済み信頼度がクランプされるテスト."""
        service = RedTeamService()
        claim = _make_claim(confidence=0.1)

        ch = service.create_challenge(claim, ChallengeType.COUNTER_EVIDENCE, "強い反証")
        service.evaluate_challenge(ch, is_valid=True, impact_score=1.0)

        adjusted = service.get_adjusted_confidence(claim)
        # 0.1 + (-1.0 * 0.3) = -0.2 -> clamped to 0.0
        assert adjusted == pytest.approx(0.0)

    def test_generate_challenge_prompts(self) -> None:
        """チャレンジプロンプト生成テスト."""
        service = RedTeamService()
        claim = _make_claim()

        prompts = service.generate_challenge_prompts(claim)

        assert len(prompts) == 4
        types = {p["type"] for p in prompts}
        assert ChallengeType.COUNTER_EVIDENCE.value in types
        assert ChallengeType.ALTERNATIVE_EXPLANATION.value in types
        assert ChallengeType.BIAS_CHECK.value in types
        assert ChallengeType.ASSUMPTION_CHALLENGE.value in types

    def test_generate_challenge_prompts_contain_statement(self) -> None:
        """プロンプトに主張内容が含まれるテスト."""
        service = RedTeamService()
        claim = _make_claim(statement="テスト主張内容")

        prompts = service.generate_challenge_prompts(claim)

        for prompt in prompts:
            assert "テスト主張内容" in prompt["prompt"]

    def test_get_redteam_stats_empty(self) -> None:
        """空の統計テスト."""
        service = RedTeamService()
        stats = service.get_redteam_stats()

        assert stats["total_challenges"] == 0
        assert stats["total_results"] == 0
        assert stats["valid_challenges"] == 0

    def test_get_redteam_stats(self) -> None:
        """統計テスト."""
        service = RedTeamService()
        claim = _make_claim()

        ch1 = service.create_challenge(claim, ChallengeType.COUNTER_EVIDENCE, "反論1")
        ch2 = service.create_challenge(claim, ChallengeType.BIAS_CHECK, "バイアス")
        service.evaluate_challenge(ch1, is_valid=True, impact_score=0.6)
        service.evaluate_challenge(ch2, is_valid=False, impact_score=0.2)

        stats = service.get_redteam_stats()
        assert stats["total_challenges"] == 2
        assert stats["total_results"] == 2
        assert stats["valid_challenges"] == 1
        assert stats["average_impact_score"] == pytest.approx(0.4)

    def test_get_challenge(self) -> None:
        """チャレンジ取得テスト."""
        service = RedTeamService()
        claim = _make_claim()
        ch = service.create_challenge(claim, ChallengeType.COUNTER_EVIDENCE, "反論")

        retrieved = service.get_challenge(ch.id)
        assert retrieved is not None
        assert retrieved.id == ch.id

    def test_get_challenge_not_found(self) -> None:
        """存在しないチャレンジ取得テスト."""
        service = RedTeamService()
        assert service.get_challenge("nonexistent") is None

    def test_get_result(self) -> None:
        """評価結果取得テスト."""
        service = RedTeamService()
        claim = _make_claim()
        ch = service.create_challenge(claim, ChallengeType.COUNTER_EVIDENCE, "反論")
        result = service.evaluate_challenge(ch, is_valid=True, impact_score=0.5)

        retrieved = service.get_result(result.id)
        assert retrieved is not None
        assert retrieved.id == result.id

    def test_get_result_not_found(self) -> None:
        """存在しない評価結果取得テスト."""
        service = RedTeamService()
        assert service.get_result("nonexistent") is None
