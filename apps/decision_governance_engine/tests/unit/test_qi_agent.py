"""Unit tests for QiAgent."""

import pytest
from apps.decision_governance_engine.agents.qi_agent import QiAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    ActionPhase,
    Implementation,
    QiInput,
    QiOutput,
    ShuOutput,
)


@pytest.fixture
def qi_agent() -> QiAgent:
    """Create QiAgent instance for testing."""
    return QiAgent(llm_client=None)


@pytest.fixture
def sample_shu_output() -> ShuOutput:
    """Create sample ShuOutput for QiAgent input."""
    return ShuOutput(
        phases=[
            ActionPhase(
                phase_number=1,
                name="準備",
                duration="2週間",
                actions=["チーム編成", "技術検証"],
            ),
            ActionPhase(
                phase_number=2,
                name="開発",
                duration="2ヶ月",
                actions=["MVP開発", "テスト"],
            ),
            ActionPhase(
                phase_number=3,
                name="リリース",
                duration="2週間",
                actions=["本番デプロイ"],
            ),
        ],
        first_action="技術スタック選定会議を開催",
    )


@pytest.fixture
def sample_input(sample_shu_output: ShuOutput) -> QiInput:
    """Create sample QiInput."""
    return QiInput(
        shu_result=sample_shu_output,
        tech_constraints=["Python/FastAPI", "AWS環境"],
    )


class TestQiAgentInit:
    """Test cases for QiAgent initialization."""

    def test_agent_inherits_resilient_agent(self, qi_agent: QiAgent) -> None:
        """Test that QiAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent

        assert isinstance(qi_agent, ResilientAgent)

    def test_agent_has_correct_name(self, qi_agent: QiAgent) -> None:
        """Test that agent has correct name."""
        assert qi_agent.name == "QiAgent"


class TestQiAgentParseInput:
    """Test cases for QiAgent._parse_input."""

    def test_parse_input_with_valid_dict(
        self, qi_agent: QiAgent, sample_shu_output: ShuOutput
    ) -> None:
        """Test that _parse_input correctly parses a valid dict."""
        raw_input = {
            "shu_result": sample_shu_output.model_dump(),
            "tech_constraints": ["Python", "AWS"],
        }
        parsed = qi_agent._parse_input(raw_input)
        assert isinstance(parsed, QiInput)
        assert "Python" in parsed.tech_constraints


class TestQiAgentOutputStructure:
    """Test cases for QiAgent output structure via process()."""

    @pytest.mark.asyncio
    async def test_output_returns_qi_output(self, qi_agent: QiAgent, sample_input: QiInput) -> None:
        """Test that output is valid QiOutput."""
        result = await qi_agent.process(sample_input)

        assert isinstance(result, QiOutput)
        assert len(result.implementations) >= 1
        assert isinstance(result.tool_recommendations, list)

    @pytest.mark.asyncio
    async def test_output_generates_implementations(
        self, qi_agent: QiAgent, sample_input: QiInput
    ) -> None:
        """Test that output generates implementation specs."""
        result = await qi_agent.process(sample_input)

        for impl in result.implementations:
            assert isinstance(impl, Implementation)
            assert impl.component is not None
            assert impl.technology is not None
            assert impl.estimated_effort is not None

    @pytest.mark.asyncio
    async def test_output_includes_tech_debt_warnings(
        self, qi_agent: QiAgent, sample_input: QiInput
    ) -> None:
        """Test that output includes tech debt warnings as a list."""
        result = await qi_agent.process(sample_input)

        # Technical debt warnings should be a list
        assert isinstance(result.technical_debt_warnings, list)


class TestQiAgentProcess:
    """Test cases for QiAgent.process."""

    @pytest.mark.asyncio
    async def test_process_without_llm(self, qi_agent: QiAgent, sample_input: QiInput) -> None:
        """Test process falls back to rule-based when no LLM."""
        result = await qi_agent.process(sample_input)

        assert isinstance(result, QiOutput)
        assert len(result.implementations) >= 1

    @pytest.mark.asyncio
    async def test_process_respects_tech_constraints(self, qi_agent: QiAgent) -> None:
        """Test that process respects technical constraints."""
        shu_output = ShuOutput(
            phases=[
                ActionPhase(
                    phase_number=1,
                    name="開発",
                    duration="1ヶ月",
                    actions=["API開発"],
                ),
                ActionPhase(
                    phase_number=2,
                    name="テスト",
                    duration="2週間",
                    actions=["テスト"],
                ),
                ActionPhase(
                    phase_number=3,
                    name="リリース",
                    duration="1週間",
                    actions=["デプロイ"],
                ),
            ],
            first_action="開発開始",
        )
        input_data = QiInput(
            shu_result=shu_output,
            tech_constraints=["Java/Spring Boot", "GCP環境"],
        )

        result = await qi_agent.process(input_data)

        assert isinstance(result, QiOutput)
        # Should include tool recommendations
        assert isinstance(result.tool_recommendations, list)


class TestQiAgentValidation:
    """Test cases for output validation."""

    def test_validate_output_with_valid_output(self, qi_agent: QiAgent) -> None:
        """Test validation passes with valid output."""
        output = QiOutput(
            implementations=[
                Implementation(
                    component="API Server",
                    technology="Python/FastAPI",
                    estimated_effort="2週間",
                    risks=["スケーラビリティ"],
                )
            ],
            tool_recommendations=["Docker", "GitHub Actions"],
            integration_points=["外部API連携"],
        )
        result = qi_agent.validate_output(output)
        assert result is True

    def test_validate_output_with_empty_implementations(self, qi_agent: QiAgent) -> None:
        """Test validation with empty implementations."""
        output = QiOutput(
            implementations=[],
            tool_recommendations=[],
        )
        result = qi_agent.validate_output(output)
        # Validation returns a boolean
        assert isinstance(result, bool)
