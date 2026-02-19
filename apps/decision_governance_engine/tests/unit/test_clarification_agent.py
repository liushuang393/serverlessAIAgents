"""Unit tests for ClarificationAgent."""

import pytest
from apps.decision_governance_engine.agents.clarification_agent import ClarificationAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    ClarificationInput,
    ClarificationOutput,
)


@pytest.fixture
def clarification_agent() -> ClarificationAgent:
    """Create ClarificationAgent instance for testing."""
    return ClarificationAgent(llm_client=None)


@pytest.fixture
def sample_input() -> ClarificationInput:
    """Create sample ClarificationInput."""
    return ClarificationInput(
        raw_question="新規事業を始めたいが、どの市場に参入すべきか？",
        constraints=["予算1億円", "1年以内に収益化"],
    )


class TestClarificationAgentInit:
    """Test cases for ClarificationAgent initialization."""

    def test_agent_inherits_resilient_agent(self, clarification_agent: ClarificationAgent) -> None:
        """Test that ClarificationAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent

        assert isinstance(clarification_agent, ResilientAgent)

    def test_agent_has_correct_name(self, clarification_agent: ClarificationAgent) -> None:
        """Test that agent has correct name."""
        assert clarification_agent.name == "ClarificationAgent"


class TestClarificationAgentParseInput:
    """Test cases for ClarificationAgent._parse_input."""

    def test_parse_input_with_valid_dict(self, clarification_agent: ClarificationAgent) -> None:
        """Test that _parse_input correctly parses a valid dict."""
        raw_input = {
            "raw_question": "市場参入を検討したい",
            "constraints": ["予算制約"],
        }
        parsed = clarification_agent._parse_input(raw_input)
        assert isinstance(parsed, ClarificationInput)
        assert parsed.raw_question == "市場参入を検討したい"

    def test_parse_input_with_minimal_input(self, clarification_agent: ClarificationAgent) -> None:
        """Test that _parse_input works with minimal required fields."""
        raw_input = {"raw_question": "テスト質問"}
        parsed = clarification_agent._parse_input(raw_input)
        assert isinstance(parsed, ClarificationInput)
        assert parsed.constraints == []


class TestClarificationAgentOutputStructure:
    """Test cases for ClarificationAgent output structure via process()."""

    @pytest.mark.asyncio
    async def test_output_has_required_fields(
        self, clarification_agent: ClarificationAgent, sample_input: ClarificationInput
    ) -> None:
        """Test that output has all required fields."""
        result = await clarification_agent.process(sample_input)

        assert isinstance(result, ClarificationOutput)
        assert result.restated_question is not None
        assert result.refined_question is not None
        assert result.diagnosis_confidence >= 0.0
        assert result.diagnosis_confidence <= 1.0

    @pytest.mark.asyncio
    async def test_output_with_ambiguous_input(
        self, clarification_agent: ClarificationAgent
    ) -> None:
        """Test output structure with ambiguous input."""
        input_data = ClarificationInput(
            raw_question="いいシステムを作りたい",
            constraints=[],
        )
        result = await clarification_agent.process(input_data)

        # Should have output with ambiguities list
        assert result.ambiguities is not None
        assert len(result.ambiguities) <= 3

    @pytest.mark.asyncio
    async def test_output_has_hidden_assumptions(
        self, clarification_agent: ClarificationAgent, sample_input: ClarificationInput
    ) -> None:
        """Test that output includes hidden assumptions."""
        result = await clarification_agent.process(sample_input)

        assert result.hidden_assumptions is not None
        assert len(result.hidden_assumptions) <= 3


class TestClarificationAgentProcess:
    """Test cases for ClarificationAgent.process."""

    @pytest.mark.asyncio
    async def test_process_without_llm(
        self, clarification_agent: ClarificationAgent, sample_input: ClarificationInput
    ) -> None:
        """Test process falls back to rule-based when no LLM."""
        result = await clarification_agent.process(sample_input)

        assert isinstance(result, ClarificationOutput)
        assert result.restated_question is not None
        assert result.refined_question is not None

    @pytest.mark.asyncio
    async def test_process_refines_question(self, clarification_agent: ClarificationAgent) -> None:
        """Test that process refines ambiguous question."""
        input_data = ClarificationInput(
            raw_question="どうすればいいですか？",
            constraints=["予算がない"],
        )
        result = await clarification_agent.process(input_data)

        assert isinstance(result, ClarificationOutput)
        # Refined question should be more specific
        assert len(result.refined_question) > 0


class TestClarificationAgentValidation:
    """Test cases for output validation."""

    def test_validate_output_with_valid_output(
        self, clarification_agent: ClarificationAgent
    ) -> None:
        """Test validation passes with valid output."""
        output = ClarificationOutput(
            restated_question="新規事業参入の市場選定",
            refined_question="新規事業として参入すべき市場はどこか？",
            diagnosis_confidence=0.85,
            ambiguities=[],
            hidden_assumptions=[],
            cognitive_biases=[],
        )
        result = clarification_agent.validate_output(output)
        assert result is True
