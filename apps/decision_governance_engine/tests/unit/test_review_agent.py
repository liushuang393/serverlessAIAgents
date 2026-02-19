"""Unit tests for ReviewAgent."""

import json
from unittest.mock import AsyncMock, MagicMock

import pytest
from apps.decision_governance_engine.agents.review_agent import ReviewAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    ActionPhase,
    DaoOutput,
    FaOutput,
    FindingCategory,
    FindingSeverity,
    Implementation,
    PathOption,
    ProblemType,
    QiOutput,
    ReviewFinding,
    ReviewInput,
    ReviewOutput,
    ReviewVerdict,
    ShuOutput,
    StrategyType,
)


@pytest.fixture
def review_agent() -> ReviewAgent:
    """Create ReviewAgent instance for testing."""
    return ReviewAgent(llm_client=None)


@pytest.fixture
def sample_dao_output() -> DaoOutput:
    """Create sample DaoOutput."""
    return DaoOutput(
        problem_type=ProblemType.TRADE_OFF,
        essence="市場拡大と内部効率化のトレードオフ",
        immutable_constraints=["予算1億円"],
        hidden_assumptions=["市場は成長する"],
        causal_gears=[],
        death_traps=[],
    )


@pytest.fixture
def sample_fa_output() -> FaOutput:
    """Create sample FaOutput."""
    return FaOutput(
        recommended_paths=[
            PathOption(
                path_id="path_a",
                name="市場拡大",
                description="国内市場を拡大",
                pros=["成長性高い"],
                cons=["リスク高い"],
                success_probability=0.7,
                strategy_type=StrategyType.AGGRESSIVE,
            )
        ],
        decision_criteria=["ROI", "リスク"],
    )


@pytest.fixture
def sample_shu_output() -> ShuOutput:
    """Create sample ShuOutput."""
    return ShuOutput(
        phases=[
            ActionPhase(phase_number=1, name="準備", duration="2週間", actions=["計画"]),
            ActionPhase(phase_number=2, name="実行", duration="1ヶ月", actions=["開発"]),
            ActionPhase(phase_number=3, name="検証", duration="2週間", actions=["テスト"]),
        ],
        first_action="キックオフ会議を開催",
    )


@pytest.fixture
def sample_qi_output() -> QiOutput:
    """Create sample QiOutput."""
    return QiOutput(
        implementations=[
            Implementation(
                component="API Server",
                technology="Python/FastAPI",
                estimated_effort="2週間",
            )
        ],
        tool_recommendations=["Docker"],
    )


@pytest.fixture
def sample_input(
    sample_dao_output: DaoOutput,
    sample_fa_output: FaOutput,
    sample_shu_output: ShuOutput,
    sample_qi_output: QiOutput,
) -> ReviewInput:
    """Create sample ReviewInput."""
    return ReviewInput(
        dao_result=sample_dao_output,
        fa_result=sample_fa_output,
        shu_result=sample_shu_output,
        qi_result=sample_qi_output,
    )


class TestReviewAgentInit:
    """Test cases for ReviewAgent initialization."""

    def test_agent_inherits_resilient_agent(self, review_agent: ReviewAgent) -> None:
        """Test that ReviewAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent

        assert isinstance(review_agent, ResilientAgent)

    def test_agent_has_correct_name(self, review_agent: ReviewAgent) -> None:
        """Test that agent has correct name."""
        assert review_agent.name == "ReviewAgent"


class TestReviewAgentParseInput:
    """Test cases for ReviewAgent._parse_input."""

    def test_parse_input_with_valid_dict(
        self,
        review_agent: ReviewAgent,
        sample_dao_output: DaoOutput,
        sample_fa_output: FaOutput,
        sample_shu_output: ShuOutput,
        sample_qi_output: QiOutput,
    ) -> None:
        """Test that _parse_input correctly parses a valid dict."""
        raw_input = {
            "dao_result": sample_dao_output.model_dump(),
            "fa_result": sample_fa_output.model_dump(),
            "shu_result": sample_shu_output.model_dump(),
            "qi_result": sample_qi_output.model_dump(),
        }
        parsed = review_agent._parse_input(raw_input)
        assert isinstance(parsed, ReviewInput)


class TestReviewAgentOutputStructure:
    """Test cases for ReviewAgent output structure via process()."""

    @pytest.mark.asyncio
    async def test_output_returns_review_output(
        self, review_agent: ReviewAgent, sample_input: ReviewInput
    ) -> None:
        """Test that output is valid ReviewOutput."""
        result = await review_agent.process(sample_input)

        assert isinstance(result, ReviewOutput)
        assert result.overall_verdict in [
            ReviewVerdict.PASS,
            ReviewVerdict.REVISE,
            ReviewVerdict.COACH,
        ]
        assert 0.0 <= result.confidence_score <= 1.0

    @pytest.mark.asyncio
    async def test_output_has_findings_structure(
        self, review_agent: ReviewAgent, sample_input: ReviewInput
    ) -> None:
        """Test that output findings have correct structure."""
        result = await review_agent.process(sample_input)

        for finding in result.findings:
            assert isinstance(finding, ReviewFinding)
            assert finding.severity in [
                FindingSeverity.CRITICAL,
                FindingSeverity.WARNING,
                FindingSeverity.INFO,
            ]
            assert finding.category in [
                FindingCategory.LOGIC_FLAW,
                FindingCategory.OVER_OPTIMISM,
                FindingCategory.RESPONSIBILITY_GAP,
                FindingCategory.RESOURCE_MISMATCH,
                FindingCategory.TIMELINE_UNREALISTIC,
            ]

    @pytest.mark.asyncio
    async def test_output_with_problematic_input(self, review_agent: ReviewAgent) -> None:
        """Test output with incomplete input data."""
        # Create input with potential issues
        dao_output = DaoOutput(
            problem_type=ProblemType.TRADE_OFF,
            essence="問題の本質",  # Non-empty to avoid validation failure
            immutable_constraints=[],
            hidden_assumptions=[],
            causal_gears=[],
            death_traps=[],
        )
        fa_output = FaOutput(
            recommended_paths=[
                PathOption(
                    path_id="A",
                    name="パスA",
                    description="テスト",
                    pros=["良い点"],
                    cons=["悪い点"],
                    success_probability=0.5,
                    strategy_type=StrategyType.BALANCED,
                )
            ],
            decision_criteria=["基準1"],
        )
        shu_output = ShuOutput(
            phases=[
                ActionPhase(phase_number=1, name="準備", duration="1週間", actions=["計画"]),
                ActionPhase(phase_number=2, name="実行", duration="2週間", actions=["開発"]),
                ActionPhase(phase_number=3, name="検証", duration="1週間", actions=["テスト"]),
            ],
            first_action="計画開始",
        )
        qi_output = QiOutput(
            implementations=[
                Implementation(component="API", technology="Python", estimated_effort="1週間")
            ],
        )

        input_data = ReviewInput(
            dao_result=dao_output,
            fa_result=fa_output,
            shu_result=shu_output,
            qi_result=qi_output,
        )

        result = await review_agent.process(input_data)

        # Should return a valid ReviewOutput with some verdict
        assert isinstance(result, ReviewOutput)
        assert result.overall_verdict in [
            ReviewVerdict.PASS,
            ReviewVerdict.REVISE,
            ReviewVerdict.COACH,
        ]


class TestReviewAgentProcess:
    """Test cases for ReviewAgent.process."""

    @pytest.mark.asyncio
    async def test_process_without_llm(
        self, review_agent: ReviewAgent, sample_input: ReviewInput
    ) -> None:
        """Test process falls back to rule-based when no LLM."""
        result = await review_agent.process(sample_input)

        assert isinstance(result, ReviewOutput)
        assert result.overall_verdict is not None

    @pytest.mark.asyncio
    async def test_process_with_good_input_passes(
        self, review_agent: ReviewAgent, sample_input: ReviewInput
    ) -> None:
        """Test that process passes well-formed input."""
        result = await review_agent.process(sample_input)

        # Well-formed input should PASS or have only INFO/WARNING findings
        if result.overall_verdict == ReviewVerdict.PASS:
            critical_findings = [
                f for f in result.findings if f.severity == FindingSeverity.CRITICAL
            ]
            assert len(critical_findings) == 0


class TestReviewAgentValidation:
    """Test cases for output validation."""

    def test_validate_output_with_pass_verdict(self, review_agent: ReviewAgent) -> None:
        """Test validation passes with PASS verdict."""
        output = ReviewOutput(
            overall_verdict=ReviewVerdict.PASS,
            findings=[],
            confidence_score=0.9,
            final_warnings=[],
        )
        result = review_agent.validate_output(output)
        assert result is True


class TestReviewAgentScoringConsistency:
    """判定と信頼度の整合性テスト."""

    def test_warning_forces_revise_with_capped_confidence(self, review_agent: ReviewAgent) -> None:
        """WARNING がある場合は REVISE かつ 80% 未満."""
        findings = [
            ReviewFinding(
                severity=FindingSeverity.WARNING,
                category=FindingCategory.OVER_OPTIMISM,
                description="過度に楽観的",
                affected_agent="FaAgent",
                suggested_revision="見積もりを保守化",
            )
        ]
        verdict, confidence = review_agent.derive_verdict_and_confidence(findings=findings)
        assert verdict == ReviewVerdict.REVISE
        assert confidence <= 0.79

    @pytest.mark.asyncio
    async def test_llm_output_is_normalized_by_findings(self, sample_input: ReviewInput) -> None:
        """LLM の高信頼 REVISE/PASS 不整合を正規化できること."""
        review_agent = ReviewAgent(llm_client=MagicMock())
        review_agent._call_llm = AsyncMock(
            return_value="""
            {
              "overall_verdict": "PASS",
              "findings": [
                {
                  "severity": "WARNING",
                  "category": "RESPONSIBILITY_GAP",
                  "description": "責任分担が曖昧",
                  "affected_agent": "DaoAgent",
                  "suggested_revision": "RACI を明確化"
                }
              ],
              "confidence_score": 0.96,
              "final_warnings": []
            }
            """
        )

        result = await review_agent.process(sample_input)
        assert result.overall_verdict == ReviewVerdict.REVISE
        assert result.confidence_score <= 0.79

    @pytest.mark.asyncio
    async def test_llm_minimal_patch_is_truncated_to_schema_limits(
        self, sample_input: ReviewInput
    ) -> None:
        """minimal_patch の超過文字列がスキーマ上限に収まること."""
        review_agent = ReviewAgent(llm_client=MagicMock())
        review_agent._call_llm = AsyncMock(
            return_value=json.dumps(
                {
                    "overall_verdict": "REVISE",
                    "findings": [
                        {
                            "severity": "WARNING",
                            "category": "LOGIC_FLAW",
                            "description": "境界条件の定義が不足",
                            "affected_agent": "ReviewAgent",
                            "suggested_revision": "入力条件の境界を明示する",
                            "failure_point": "F" * 260,
                            "impact_scope": "I" * 260,
                            "minimal_patch": {
                                "checkbox_label": "L" * 120,
                                "annotation_hint": "H" * 60,
                                "default_value": "D" * 120,
                            },
                            "score_improvements": [
                                {
                                    "target_score": "T" * 80,
                                    "current_estimate": 45,
                                    "improved_estimate": 65,
                                    "delta": 20,
                                }
                            ],
                            "action_type": "RECALC",
                        }
                    ],
                },
                ensure_ascii=False,
            )
        )

        result = await review_agent.process(sample_input)
        finding = result.findings[0]
        assert finding.minimal_patch is not None
        assert len(finding.minimal_patch.checkbox_label) <= 80
        assert len(finding.minimal_patch.annotation_hint) <= 30
        assert len(finding.minimal_patch.default_value) <= 50
        assert len(finding.failure_point) <= 200
        assert len(finding.impact_scope) <= 200

    def test_validate_output_with_revise_verdict(self, review_agent: ReviewAgent) -> None:
        """Test validation passes with REVISE verdict and findings."""
        output = ReviewOutput(
            overall_verdict=ReviewVerdict.REVISE,
            findings=[
                ReviewFinding(
                    severity=FindingSeverity.WARNING,
                    category=FindingCategory.OVER_OPTIMISM,
                    description="成功確率が楽観的すぎる",
                    affected_agent="FaAgent",
                    suggested_revision="成功確率を再評価",
                )
            ],
            confidence_score=0.75,
            final_warnings=["楽観バイアスに注意"],
        )
        result = review_agent.validate_output(output)
        assert result is True
