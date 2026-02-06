# -*- coding: utf-8 -*-
"""Unit tests for ShuAgent."""
import pytest
from unittest.mock import AsyncMock, patch, MagicMock

from apps.decision_governance_engine.agents.shu_agent import ShuAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    ShuInput,
    ShuOutput,
    FaOutput,
    PathOption,
    StrategyType,
    ActionPhase,
)


@pytest.fixture
def shu_agent() -> ShuAgent:
    """Create ShuAgent instance for testing."""
    return ShuAgent(llm_client=None)


@pytest.fixture
def sample_fa_output() -> FaOutput:
    """Create sample FaOutput for ShuAgent input."""
    return FaOutput(
        recommended_paths=[
            PathOption(
                path_id="path_a",
                name="市場拡大",
                description="国内市場を拡大する戦略",
                pros=["成長性高い", "既存ノウハウ活用"],
                cons=["競争激化リスク"],
                success_probability=0.7,
                strategy_type=StrategyType.AGGRESSIVE,
            )
        ],
        decision_criteria=["ROI", "実行リスク"],
    )


@pytest.fixture
def sample_input(sample_fa_output: FaOutput) -> ShuInput:
    """Create sample ShuInput."""
    return ShuInput(
        fa_result=sample_fa_output,
        selected_path_id="path_a",
    )


class TestShuAgentInit:
    """Test cases for ShuAgent initialization."""

    def test_agent_inherits_resilient_agent(self, shu_agent: ShuAgent) -> None:
        """Test that ShuAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent
        assert isinstance(shu_agent, ResilientAgent)

    def test_agent_has_correct_name(self, shu_agent: ShuAgent) -> None:
        """Test that agent has correct name."""
        assert shu_agent.name == "ShuAgent"


class TestShuAgentParseInput:
    """Test cases for ShuAgent._parse_input."""

    def test_parse_input_with_valid_dict(
        self, shu_agent: ShuAgent, sample_fa_output: FaOutput
    ) -> None:
        """Test that _parse_input correctly parses a valid dict."""
        raw_input = {
            "fa_result": sample_fa_output.model_dump(),
            "selected_path_id": "path_a",
        }
        parsed = shu_agent._parse_input(raw_input)
        assert isinstance(parsed, ShuInput)
        assert parsed.selected_path_id == "path_a"


class TestShuAgentOutputStructure:
    """Test cases for ShuAgent output structure via process()."""

    @pytest.mark.asyncio
    async def test_output_returns_shu_output(
        self, shu_agent: ShuAgent, sample_input: ShuInput
    ) -> None:
        """Test that output is valid ShuOutput."""
        result = await shu_agent.process(sample_input)

        assert isinstance(result, ShuOutput)
        assert len(result.phases) >= 1
        assert result.first_action is not None
        assert len(result.first_action) > 0

    @pytest.mark.asyncio
    async def test_output_generates_action_phases(
        self, shu_agent: ShuAgent, sample_input: ShuInput
    ) -> None:
        """Test that output generates valid action phases."""
        result = await shu_agent.process(sample_input)

        for phase in result.phases:
            assert isinstance(phase, ActionPhase)
            assert phase.phase_number >= 1
            assert phase.name is not None
            assert phase.duration is not None
            assert len(phase.actions) >= 1

    @pytest.mark.asyncio
    async def test_output_may_include_rhythm_control(
        self, shu_agent: ShuAgent, sample_input: ShuInput
    ) -> None:
        """Test that output may include 30-day rhythm control."""
        result = await shu_agent.process(sample_input)

        if result.rhythm_control:
            assert result.rhythm_control.focus is not None
            assert result.rhythm_control.focus.name is not None
            assert len(result.rhythm_control.focus.name) <= 20


class TestShuAgentProcess:
    """Test cases for ShuAgent.process."""

    @pytest.mark.asyncio
    async def test_process_without_llm(
        self, shu_agent: ShuAgent, sample_input: ShuInput
    ) -> None:
        """Test process falls back to rule-based when no LLM."""
        result = await shu_agent.process(sample_input)

        assert isinstance(result, ShuOutput)
        assert len(result.phases) >= 3
        assert result.first_action is not None

    @pytest.mark.asyncio
    async def test_process_with_selected_path(
        self, shu_agent: ShuAgent, sample_input: ShuInput
    ) -> None:
        """Test that process uses selected path for planning."""
        result = await shu_agent.process(sample_input)

        # First action should be concrete
        assert len(result.first_action) > 0
        # Should have clear phases
        assert all(p.duration for p in result.phases)


class TestShuAgentValidation:
    """Test cases for output validation."""

    def test_validate_output_with_valid_output(self, shu_agent: ShuAgent) -> None:
        """Test validation passes with valid output."""
        output = ShuOutput(
            phases=[
                ActionPhase(
                    phase_number=1,
                    name="準備",
                    duration="2週間",
                    actions=["チーム編成", "計画策定"],
                ),
                ActionPhase(
                    phase_number=2,
                    name="実行",
                    duration="1ヶ月",
                    actions=["開発開始"],
                ),
                ActionPhase(
                    phase_number=3,
                    name="検証",
                    duration="2週間",
                    actions=["テスト実施"],
                ),
            ],
            first_action="プロジェクトキックオフミーティングを設定する",
        )
        result = shu_agent.validate_output(output)
        assert result is True

    def test_validate_output_with_minimum_phases(self, shu_agent: ShuAgent) -> None:
        """Test validation with minimum required phases (3)."""
        output = ShuOutput(
            phases=[
                ActionPhase(
                    phase_number=1,
                    name="準備",
                    duration="2週間",
                    actions=["計画"],
                ),
                ActionPhase(
                    phase_number=2,
                    name="実行",
                    duration="1ヶ月",
                    actions=["開発"],
                ),
                ActionPhase(
                    phase_number=3,
                    name="検証",
                    duration="2週間",
                    actions=["テスト"],
                ),
            ],
            first_action="開発開始",
        )
        # Validation should return True for valid output
        result = shu_agent.validate_output(output)
        assert result is True
