"""Unit tests for CognitiveGateAgent."""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from apps.decision_governance_engine.agents.cognitive_gate_agent import CognitiveGateAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    CognitiveGateInput,
    CognitiveGateOutput,
    IrreversibilityLevel,
)


@pytest.fixture
def cognitive_gate_agent() -> CognitiveGateAgent:
    """Create CognitiveGateAgent instance for testing."""
    return CognitiveGateAgent(llm_client=None)


@pytest.fixture
def sample_input() -> CognitiveGateInput:
    """Create sample CognitiveGateInput."""
    return CognitiveGateInput(
        raw_question="新規事業への投資判断をしたい。市場拡大か内部強化か。",
        constraints=["予算1億円", "6ヶ月以内"],
    )


class TestCognitiveGateAgentInit:
    """Test cases for CognitiveGateAgent initialization."""

    def test_agent_inherits_resilient_agent(self, cognitive_gate_agent: CognitiveGateAgent) -> None:
        """Test that CognitiveGateAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent

        assert isinstance(cognitive_gate_agent, ResilientAgent)

    def test_agent_has_correct_name(self, cognitive_gate_agent: CognitiveGateAgent) -> None:
        """Test that agent has correct name."""
        assert cognitive_gate_agent.name == "CognitiveGateAgent"

    def test_agent_has_appropriate_temperature(
        self, cognitive_gate_agent: CognitiveGateAgent
    ) -> None:
        """Test that temperature is set low for strict judgment."""
        assert cognitive_gate_agent.temperature == 0.3


class TestCognitiveGateAgentParseInput:
    """Test cases for CognitiveGateAgent._parse_input."""

    def test_parse_input_with_valid_dict(self, cognitive_gate_agent: CognitiveGateAgent) -> None:
        """Test that _parse_input correctly parses a valid dict."""
        raw_input = {
            "raw_question": "新規投資の判断",
            "constraints": ["予算制約"],
        }
        parsed = cognitive_gate_agent._parse_input(raw_input)
        assert isinstance(parsed, CognitiveGateInput)
        assert parsed.raw_question == "新規投資の判断"

    def test_parse_input_with_minimal_input(self, cognitive_gate_agent: CognitiveGateAgent) -> None:
        """Test that _parse_input works with minimal required fields."""
        raw_input = {"raw_question": "テスト質問"}
        parsed = cognitive_gate_agent._parse_input(raw_input)
        assert isinstance(parsed, CognitiveGateInput)
        assert parsed.raw_question == "テスト質問"
        assert parsed.constraints == []


class TestCognitiveGateAgentRuleBasedAnalysis:
    """Test cases for CognitiveGateAgent._analyze_rule_based."""

    def test_rule_based_returns_cognitive_gate_output(
        self, cognitive_gate_agent: CognitiveGateAgent, sample_input: CognitiveGateInput
    ) -> None:
        """Test that rule-based analysis returns valid CognitiveGateOutput."""
        result = cognitive_gate_agent._analyze_rule_based(sample_input)

        assert isinstance(result, CognitiveGateOutput)
        assert result.evaluation_object is not None
        assert result.intent is not None
        assert len(result.criteria) >= 1
        assert result.irreversibility is not None
        assert isinstance(result.proceed, bool)

    def test_rule_based_identifies_evaluation_object(
        self, cognitive_gate_agent: CognitiveGateAgent
    ) -> None:
        """Test that rule-based analysis identifies evaluation object."""
        input_data = CognitiveGateInput(
            raw_question="新製品Xへの投資を検討したい",
            constraints=[],
        )
        result = cognitive_gate_agent._analyze_rule_based(input_data)

        assert result.evaluation_object is not None
        assert len(result.evaluation_object) <= 50

    def test_rule_based_extracts_criteria(self, cognitive_gate_agent: CognitiveGateAgent) -> None:
        """Test that rule-based analysis extracts evaluation criteria."""
        input_data = CognitiveGateInput(
            raw_question="市場参入のリスクとROIを検討したい",
            constraints=["予算制約あり"],
        )
        result = cognitive_gate_agent._analyze_rule_based(input_data)

        assert len(result.criteria) >= 1
        assert len(result.criteria) <= 5


class TestCognitiveGateAgentProcess:
    """Test cases for CognitiveGateAgent.process."""

    @pytest.mark.asyncio
    async def test_process_without_llm(
        self, cognitive_gate_agent: CognitiveGateAgent, sample_input: CognitiveGateInput
    ) -> None:
        """Test process falls back to rule-based when no LLM."""
        result = await cognitive_gate_agent.process(sample_input)

        assert isinstance(result, CognitiveGateOutput)
        assert result.evaluation_object is not None
        assert result.irreversibility is not None

    @pytest.mark.asyncio
    async def test_process_with_clear_question_proceeds(
        self, cognitive_gate_agent: CognitiveGateAgent
    ) -> None:
        """Test process allows proceeding with clear question."""
        input_data = CognitiveGateInput(
            raw_question="新規事業Aへの投資判断。予算配分と市場参入タイミングを評価したい。",
            constraints=["予算5000万円", "Q2までに判断必要"],
        )
        result = await cognitive_gate_agent.process(input_data)

        assert isinstance(result, CognitiveGateOutput)
        # Rule-based analysis returns output with proceed flag

    @pytest.mark.asyncio
    async def test_process_with_llm(self, sample_input: CognitiveGateInput) -> None:
        """Test process uses LLM when available."""
        from apps.decision_governance_engine.schemas.agent_schemas import Irreversibility

        mock_llm = MagicMock()
        agent = CognitiveGateAgent(llm_client=mock_llm)

        with patch.object(agent, "_analyze_with_llm", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = CognitiveGateOutput(
                evaluation_object="新規事業投資",
                intent="ROI最大化",
                criteria=["収益性", "リスク"],
                irreversibility=Irreversibility(
                    level=IrreversibilityLevel.HIGH, description="大規模投資"
                ),
                proceed=True,
            )

            result = await agent.process(sample_input)

            mock_analyze.assert_called_once()
            assert result.proceed is True


class TestCognitiveGateAgentRuleBasedOutput:
    """Test cases for rule-based output structure."""

    def test_output_has_irreversibility(
        self, cognitive_gate_agent: CognitiveGateAgent, sample_input: CognitiveGateInput
    ) -> None:
        """Test that output includes irreversibility assessment."""
        result = cognitive_gate_agent._analyze_rule_based(sample_input)
        assert result.irreversibility is not None
        assert result.irreversibility.level in [
            IrreversibilityLevel.HIGH,
            IrreversibilityLevel.MEDIUM,
            IrreversibilityLevel.LOW,
        ]

    def test_output_has_evaluation_object(
        self, cognitive_gate_agent: CognitiveGateAgent, sample_input: CognitiveGateInput
    ) -> None:
        """Test that output includes evaluation object."""
        result = cognitive_gate_agent._analyze_rule_based(sample_input)
        assert result.evaluation_object is not None
        assert len(result.evaluation_object) <= 50
