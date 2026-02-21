"""技術成熟度評価ユニットテスト.

MaturityAssessmentService のテスト。
Mock LLM を使用（外部依存なし）。
"""

from __future__ import annotations

import json
from datetime import datetime
from unittest.mock import AsyncMock

from apps.market_trend_monitor.backend.models import Evidence, SourceType
from apps.market_trend_monitor.backend.services.maturity_assessment_service import (
    DEFAULT_TECHNOLOGIES,
    PHASE_RISK_MAP,
    PHASE_TIME_MAP,
    MaturityAssessmentService,
    MaturityPhase,
    TechnologyMaturity,
)


# ============================================================
# Helpers
# ============================================================


def _make_evidence(
    evidence_id: str = "e-1",
    title: str = "AI Migration Study",
) -> Evidence:
    """テスト用 Evidence を生成."""
    return Evidence(
        id=evidence_id,
        source_id="src-1",
        source_type=SourceType.ARXIV,
        url=f"https://example.com/{evidence_id}",
        title=title,
        content_hash=f"hash-{evidence_id}",
        collected_at=datetime(2026, 1, 15),
        reliability_score=0.9,
        extracted_data={"content": "AI-powered code migration tools"},
    )


def _make_mock_llm(phase: str = "slope_of_enlightenment", confidence: float = 0.8):
    """テスト用 Mock LLM を生成."""
    mock = AsyncMock()
    mock.chat = AsyncMock(
        return_value=json.dumps(
            {
                "phase": phase,
                "confidence": confidence,
            }
        )
    )
    return mock


# ============================================================
# Model Tests
# ============================================================


class TestTechnologyMaturity:
    """TechnologyMaturity データモデルのテスト."""

    def test_maturity_creation(self) -> None:
        """成熟度データ生成テスト."""
        maturity = TechnologyMaturity(
            technology="AI Code Conversion",
            phase=MaturityPhase.SLOPE_OF_ENLIGHTENMENT,
            confidence=0.8,
        )
        assert maturity.technology == "AI Code Conversion"
        assert maturity.phase == MaturityPhase.SLOPE_OF_ENLIGHTENMENT

    def test_maturity_to_dict(self) -> None:
        """to_dict 変換テスト."""
        maturity = TechnologyMaturity(
            technology="Test",
            phase=MaturityPhase.INNOVATION_TRIGGER,
            adoption_risk=0.9,
        )
        d = maturity.to_dict()
        assert d["technology"] == "Test"
        assert d["phase"] == "innovation_trigger"
        assert d["adoption_risk"] == 0.9

    def test_maturity_defaults(self) -> None:
        """デフォルト値テスト."""
        maturity = TechnologyMaturity(
            technology="Test",
            phase=MaturityPhase.INNOVATION_TRIGGER,
        )
        assert maturity.confidence == 0.5
        assert maturity.evidence_count == 0
        assert maturity.time_to_plateau == "2-5 years"


class TestMaturityPhase:
    """MaturityPhase Enum のテスト."""

    def test_all_phases(self) -> None:
        """全フェーズの値テスト."""
        assert MaturityPhase.INNOVATION_TRIGGER.value == "innovation_trigger"
        assert MaturityPhase.PEAK_OF_EXPECTATIONS.value == "peak_of_expectations"
        assert MaturityPhase.TROUGH_OF_DISILLUSIONMENT.value == "trough_of_disillusionment"
        assert MaturityPhase.SLOPE_OF_ENLIGHTENMENT.value == "slope_of_enlightenment"
        assert MaturityPhase.PLATEAU_OF_PRODUCTIVITY.value == "plateau_of_productivity"

    def test_phase_risk_map_complete(self) -> None:
        """フェーズリスクマップの完全性テスト."""
        for phase in MaturityPhase:
            assert phase in PHASE_RISK_MAP

    def test_phase_time_map_complete(self) -> None:
        """フェーズ時間マップの完全性テスト."""
        for phase in MaturityPhase:
            assert phase in PHASE_TIME_MAP

    def test_risk_decreases_with_maturity(self) -> None:
        """成熟度が上がるとリスクが下がるテスト."""
        phases = list(MaturityPhase)
        risks = [PHASE_RISK_MAP[p] for p in phases]
        for i in range(len(risks) - 1):
            assert risks[i] >= risks[i + 1]


# ============================================================
# Service Tests
# ============================================================


class TestMaturityAssessmentService:
    """MaturityAssessmentService のテスト."""

    async def test_assess_technology_basic(self) -> None:
        """基本的な技術評価テスト."""
        service = MaturityAssessmentService(llm=_make_mock_llm())
        evidences = [_make_evidence(f"e-{i}") for i in range(5)]

        maturity = await service.assess_technology("AI Code Conversion", evidences)
        assert maturity.technology == "AI Code Conversion"
        assert maturity.phase == MaturityPhase.SLOPE_OF_ENLIGHTENMENT
        assert maturity.evidence_count == 5
        assert maturity.confidence == 0.8

    async def test_assess_technology_risk_mapping(self) -> None:
        """リスクマッピングテスト."""
        service = MaturityAssessmentService(
            llm=_make_mock_llm(phase="innovation_trigger"),
        )
        maturity = await service.assess_technology("New Tech", [_make_evidence()])
        assert maturity.adoption_risk == 0.9  # innovation_trigger = 高リスク

    async def test_assess_technology_time_mapping(self) -> None:
        """プラトー到達時間マッピングテスト."""
        service = MaturityAssessmentService(
            llm=_make_mock_llm(phase="plateau_of_productivity"),
        )
        maturity = await service.assess_technology("Mature Tech", [_make_evidence()])
        assert maturity.time_to_plateau == "Already there"

    async def test_assess_technology_llm_failure(self) -> None:
        """LLM失敗時のフォールバックテスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(side_effect=Exception("LLM error"))
        service = MaturityAssessmentService(llm=mock_llm)

        evidences = [_make_evidence(f"e-{i}") for i in range(10)]
        maturity = await service.assess_technology("Test", evidences)
        assert maturity.confidence == 0.3  # フォールバック信頼度
        # Phase 10: multi-factor heuristic
        # All same source (arxiv), same date → low diversity, zero span
        # score = 0.25*0.3 + 0*0.4 + 0.9*0.3 = 0.345 → PEAK_OF_EXPECTATIONS
        assert maturity.phase == MaturityPhase.PEAK_OF_EXPECTATIONS

    async def test_assess_technology_stored(self) -> None:
        """評価結果が保存されるテスト."""
        service = MaturityAssessmentService(llm=_make_mock_llm())
        await service.assess_technology("Test", [_make_evidence()])
        assert service.get_assessment("Test") is not None

    async def test_get_maturity_landscape(self) -> None:
        """成熟度ランドスケープ取得テスト."""
        service = MaturityAssessmentService(llm=_make_mock_llm())
        service._assessments["Tech1"] = TechnologyMaturity(
            technology="Tech1",
            phase=MaturityPhase.PLATEAU_OF_PRODUCTIVITY,
        )
        service._assessments["Tech2"] = TechnologyMaturity(
            technology="Tech2",
            phase=MaturityPhase.INNOVATION_TRIGGER,
        )

        landscape = await service.get_maturity_landscape()
        assert len(landscape) == 2
        # フェーズ順にソート
        assert landscape[0].phase == MaturityPhase.INNOVATION_TRIGGER
        assert landscape[1].phase == MaturityPhase.PLATEAU_OF_PRODUCTIVITY

    def test_get_assessment(self) -> None:
        """評価結果取得テスト."""
        service = MaturityAssessmentService(llm=_make_mock_llm())
        service._assessments["Test"] = TechnologyMaturity(
            technology="Test",
            phase=MaturityPhase.INNOVATION_TRIGGER,
        )
        assert service.get_assessment("Test") is not None
        assert service.get_assessment("Unknown") is None

    def test_list_assessments(self) -> None:
        """評価結果一覧テスト."""
        service = MaturityAssessmentService(llm=_make_mock_llm())
        service._assessments["A"] = TechnologyMaturity(
            technology="A",
            phase=MaturityPhase.INNOVATION_TRIGGER,
        )
        service._assessments["B"] = TechnologyMaturity(
            technology="B",
            phase=MaturityPhase.PLATEAU_OF_PRODUCTIVITY,
        )
        assert len(service.list_assessments()) == 2

    def test_estimate_phase_from_evidence_count(self) -> None:
        """証拠数によるフェーズ推定テスト.

        Phase 10: この関数は最小フォールバック用。
        多因子ヒューリスティック (_estimate_phase_from_evidence) が優先使用される。
        """
        assert MaturityAssessmentService._estimate_phase_from_evidence_count(0) == (MaturityPhase.INNOVATION_TRIGGER)
        assert MaturityAssessmentService._estimate_phase_from_evidence_count(1) == (MaturityPhase.INNOVATION_TRIGGER)
        assert MaturityAssessmentService._estimate_phase_from_evidence_count(2) == (MaturityPhase.PEAK_OF_EXPECTATIONS)
        # 5件: PEAK_OF_EXPECTATIONS（count-only fallbackは保守的）
        assert MaturityAssessmentService._estimate_phase_from_evidence_count(5) == (MaturityPhase.PEAK_OF_EXPECTATIONS)
        # 10件: TROUGH_OF_DISILLUSIONMENT
        assert MaturityAssessmentService._estimate_phase_from_evidence_count(10) == (
            MaturityPhase.TROUGH_OF_DISILLUSIONMENT
        )
        # 20件: SLOPE_OF_ENLIGHTENMENT
        assert MaturityAssessmentService._estimate_phase_from_evidence_count(20) == (
            MaturityPhase.SLOPE_OF_ENLIGHTENMENT
        )

    def test_parse_assessment_valid(self) -> None:
        """正常なJSONパーステスト."""
        raw = '{"phase": "slope_of_enlightenment", "confidence": 0.8}'
        result = MaturityAssessmentService._parse_assessment(raw)
        assert result["phase"] == "slope_of_enlightenment"

    def test_parse_assessment_invalid(self) -> None:
        """不正JSONパーステスト."""
        result = MaturityAssessmentService._parse_assessment("not json")
        assert result == {}

    def test_default_technologies(self) -> None:
        """デフォルト技術リストテスト."""
        assert len(DEFAULT_TECHNOLOGIES) > 0
        assert "AI Code Conversion" in DEFAULT_TECHNOLOGIES

    async def test_assess_invalid_phase_fallback(self) -> None:
        """不正フェーズのフォールバックテスト."""
        service = MaturityAssessmentService(
            llm=_make_mock_llm(phase="invalid_phase"),
        )
        maturity = await service.assess_technology("Test", [_make_evidence()])
        assert maturity.phase == MaturityPhase.INNOVATION_TRIGGER
