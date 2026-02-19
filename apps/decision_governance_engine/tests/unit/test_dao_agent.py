"""Unit tests for DaoAgent."""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from apps.decision_governance_engine.agents.dao_agent import DaoAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    DaoInput,
    DaoOutput,
    ProblemNatureType,
    ProblemType,
)


@pytest.fixture
def dao_agent() -> DaoAgent:
    """Create DaoAgent instance for testing."""
    return DaoAgent(llm_client=None)


@pytest.fixture
def sample_input() -> DaoInput:
    """Create sample DaoInput."""
    return DaoInput(
        question="新規事業への投資判断をしたい。市場拡大とコスト削減のどちらを優先すべきか？",
        constraints=["予算は1億円以内", "6ヶ月以内に成果を出す必要がある"],
        stakeholders=["経営陣", "開発チーム"],
    )


class TestDaoAgentInit:
    """Test cases for DaoAgent initialization."""

    def test_agent_inherits_resilient_agent(self, dao_agent: DaoAgent) -> None:
        """Test that DaoAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent

        assert isinstance(dao_agent, ResilientAgent)

    def test_agent_has_correct_name(self, dao_agent: DaoAgent) -> None:
        """Test that agent has correct name."""
        assert dao_agent.name == "DaoAgent"

    def test_agent_has_use_rag_disabled(self, dao_agent: DaoAgent) -> None:
        """Test that RAG is disabled for DaoAgent."""
        assert dao_agent.USE_RAG is False


class TestDaoAgentParseInput:
    """Test cases for DaoAgent._parse_input."""

    def test_parse_input_with_valid_dict(self, dao_agent: DaoAgent) -> None:
        """Test that _parse_input correctly parses a valid dict."""
        raw_input = {
            "question": "投資判断をしたい",
            "constraints": ["予算制約あり"],
            "stakeholders": ["経営陣"],
        }

        parsed = dao_agent._parse_input(raw_input)

        assert isinstance(parsed, DaoInput)
        assert parsed.question == "投資判断をしたい"
        assert parsed.constraints == ["予算制約あり"]
        assert parsed.stakeholders == ["経営陣"]

    def test_parse_input_with_minimal_input(self, dao_agent: DaoAgent) -> None:
        """Test that _parse_input works with minimal required fields."""
        raw_input = {"question": "テスト質問"}

        parsed = dao_agent._parse_input(raw_input)

        assert isinstance(parsed, DaoInput)
        assert parsed.question == "テスト質問"
        assert parsed.constraints == []
        assert parsed.stakeholders == []


class TestDaoAgentInferProblemType:
    """Test cases for DaoAgent._infer_problem_type."""

    def test_infer_resource_allocation(self, dao_agent: DaoAgent) -> None:
        """Test inference of RESOURCE_ALLOCATION type."""
        question = "予算配分をどうすべきか？"
        result = dao_agent._infer_problem_type(question)
        assert result == ProblemType.RESOURCE_ALLOCATION

    def test_infer_timing_decision(self, dao_agent: DaoAgent) -> None:
        """Test inference of TIMING_DECISION type."""
        question = "いつ着手すべきか？"
        result = dao_agent._infer_problem_type(question)
        assert result == ProblemType.TIMING_DECISION

    def test_infer_trade_off(self, dao_agent: DaoAgent) -> None:
        """Test inference of TRADE_OFF type."""
        question = "AとBのどちらを選択すべきか？"
        result = dao_agent._infer_problem_type(question)
        assert result == ProblemType.TRADE_OFF

    def test_infer_risk_assessment(self, dao_agent: DaoAgent) -> None:
        """Test inference of RISK_ASSESSMENT type."""
        question = "このプロジェクトのリスクは？"
        result = dao_agent._infer_problem_type(question)
        assert result == ProblemType.RISK_ASSESSMENT

    def test_infer_default_to_strategy(self, dao_agent: DaoAgent) -> None:
        """Test that unknown questions default to STRATEGY_DIRECTION."""
        question = "どうすればいいですか？"
        result = dao_agent._infer_problem_type(question)
        assert result == ProblemType.STRATEGY_DIRECTION


class TestDaoAgentRuleBasedAnalysis:
    """Test cases for DaoAgent._analyze_rule_based."""

    def test_rule_based_returns_dao_output(
        self, dao_agent: DaoAgent, sample_input: DaoInput
    ) -> None:
        """Test that rule-based analysis returns valid DaoOutput."""
        result = dao_agent._analyze_rule_based(
            question=sample_input.question,
            constraints=sample_input.constraints,
            problem_type=ProblemType.TRADE_OFF,
        )

        assert isinstance(result, DaoOutput)
        assert result.problem_type == ProblemType.TRADE_OFF
        assert result.essence is not None
        assert len(result.essence) <= 50
        assert len(result.immutable_constraints) <= 5
        assert len(result.hidden_assumptions) <= 3
        assert 3 <= len(result.causal_gears) <= 5
        assert len(result.death_traps) <= 3

    def test_rule_based_with_constraint_driven(self, dao_agent: DaoAgent) -> None:
        """Test rule-based analysis detects constraint-driven problems."""
        question = "国際会議システムを構築したい。規制対応が必要。"
        result = dao_agent._analyze_rule_based(
            question=question,
            constraints=["データ主権の規制あり"],
            problem_type=ProblemType.STRATEGY_DIRECTION,
        )

        assert result.problem_nature == ProblemNatureType.CONSTRAINT_DRIVEN


class TestDaoAgentProcess:
    """Test cases for DaoAgent.process."""

    @pytest.mark.asyncio
    async def test_process_without_llm(self, dao_agent: DaoAgent, sample_input: DaoInput) -> None:
        """Test process falls back to rule-based when no LLM."""
        result = await dao_agent.process(sample_input)

        assert isinstance(result, DaoOutput)
        assert result.essence is not None
        assert len(result.causal_gears) >= 3

    @pytest.mark.asyncio
    async def test_process_with_llm(self, sample_input: DaoInput) -> None:
        """Test process uses LLM when available."""
        mock_llm = MagicMock()

        agent = DaoAgent(llm_client=mock_llm)

        with patch.object(agent, "_analyze_with_llm", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = DaoOutput(
                problem_type=ProblemType.TRADE_OFF,
                essence="テスト本質",
                immutable_constraints=["制約1"],
                hidden_assumptions=["前提1"],
                causal_gears=[],
                death_traps=[],
            )

            result = await agent.process(sample_input)

            mock_analyze.assert_called_once()
            assert result.essence == "テスト本質"


class TestDaoAgentValidateOutput:
    """Test cases for DaoAgent.validate_output."""

    def test_validate_output_with_valid_output(self, dao_agent: DaoAgent) -> None:
        """Test validation passes with valid output."""
        output = DaoOutput(
            problem_type=ProblemType.TRADE_OFF,
            essence="問題の本質",
            immutable_constraints=["制約1"],
            hidden_assumptions=["前提1"],
            causal_gears=[],
            death_traps=[],
        )

        result = dao_agent.validate_output(output)
        assert result is True

    def test_validate_output_with_empty_essence(self, dao_agent: DaoAgent) -> None:
        """Test validation fails with empty essence."""
        output = DaoOutput(
            problem_type=ProblemType.TRADE_OFF,
            essence="",
            immutable_constraints=[],
            hidden_assumptions=[],
            causal_gears=[],
            death_traps=[],
        )

        result = dao_agent.validate_output(output)
        assert result is False


class TestDaoAgentHelperMethods:
    """Test cases for DaoAgent helper methods."""

    def test_detect_constraint_driven_high(self, dao_agent: DaoAgent) -> None:
        """Test constraint-driven detection returns high for multiple keywords."""
        question = "国際会議システムを独自開発したい"
        result = dao_agent._detect_constraint_driven(question)
        assert "高い" in result

    def test_detect_constraint_driven_low(self, dao_agent: DaoAgent) -> None:
        """Test constraint-driven detection returns low for no keywords."""
        question = "売上を伸ばしたい"
        result = dao_agent._detect_constraint_driven(question)
        assert "低い" in result

    def test_is_template_essence_true(self, dao_agent: DaoAgent) -> None:
        """Test template essence detection returns True for templates."""
        assert dao_agent._is_template_essence("中長期的な方向性の決定") is True
        assert dao_agent._is_template_essence("最適な投資判断") is True

    def test_is_template_essence_false(self, dao_agent: DaoAgent) -> None:
        """Test template essence detection returns False for non-templates."""
        assert dao_agent._is_template_essence("既存の標準解が自社の固有制約を満たせない") is False
