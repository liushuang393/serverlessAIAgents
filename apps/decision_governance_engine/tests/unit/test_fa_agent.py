# -*- coding: utf-8 -*-
"""Unit tests for FaAgent."""
import json
import pytest
from unittest.mock import AsyncMock, patch, MagicMock

from apps.decision_governance_engine.agents.fa_agent import FaAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    FaInput,
    FaOutput,
    DaoOutput,
    ProblemType,
    PathOption,
    SelfCheckStatus,
    StrategyType,
)


@pytest.fixture
def fa_agent() -> FaAgent:
    """Create FaAgent instance for testing."""
    return FaAgent(llm_client=None)


@pytest.fixture
def sample_dao_output() -> DaoOutput:
    """Create sample DaoOutput for FaAgent input."""
    return DaoOutput(
        problem_type=ProblemType.TRADE_OFF,
        essence="市場拡大と内部効率化のトレードオフ",
        immutable_constraints=["予算1億円以内"],
        hidden_assumptions=["市場は成長する"],
        causal_gears=[],
        death_traps=[],
    )


@pytest.fixture
def sample_input(sample_dao_output: DaoOutput) -> FaInput:
    """Create sample FaInput."""
    return FaInput(
        dao_result=sample_dao_output,
        available_resources={"budget": "1億円", "team": "5人"},
        time_horizon="6ヶ月",
    )


class TestFaAgentInit:
    """Test cases for FaAgent initialization."""

    def test_agent_inherits_resilient_agent(self, fa_agent: FaAgent) -> None:
        """Test that FaAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent
        assert isinstance(fa_agent, ResilientAgent)

    def test_agent_has_correct_name(self, fa_agent: FaAgent) -> None:
        """Test that agent has correct name."""
        assert fa_agent.name == "FaAgent"

    def test_agent_has_rag_disabled(self, fa_agent: FaAgent) -> None:
        """Test that RAG is disabled for FaAgent."""
        assert fa_agent.USE_RAG is False


class TestFaAgentParseInput:
    """Test cases for FaAgent._parse_input."""

    def test_parse_input_with_valid_dict(
        self, fa_agent: FaAgent, sample_dao_output: DaoOutput
    ) -> None:
        """Test that _parse_input correctly parses a valid dict."""
        raw_input = {
            "dao_result": sample_dao_output.model_dump(),
            "available_resources": {"budget": "1億円"},
            "time_horizon": "6ヶ月",
        }
        parsed = fa_agent._parse_input(raw_input)
        assert isinstance(parsed, FaInput)
        assert parsed.time_horizon == "6ヶ月"


class TestFaAgentOutputStructure:
    """Test cases for FaAgent output structure via process()."""

    @pytest.mark.asyncio
    async def test_output_returns_fa_output(
        self, fa_agent: FaAgent, sample_input: FaInput
    ) -> None:
        """Test that output is valid FaOutput."""
        result = await fa_agent.process(sample_input)

        assert isinstance(result, FaOutput)
        assert len(result.recommended_paths) >= 1
        assert len(result.recommended_paths) <= 2
        assert len(result.decision_criteria) >= 1

    @pytest.mark.asyncio
    async def test_output_generates_path_options(
        self, fa_agent: FaAgent, sample_input: FaInput
    ) -> None:
        """Test that output generates valid path options."""
        result = await fa_agent.process(sample_input)

        for path in result.recommended_paths:
            assert isinstance(path, PathOption)
            assert path.path_id is not None
            assert path.name is not None
            assert len(path.name) <= 10
            assert len(path.pros) <= 3
            assert len(path.cons) <= 3
            assert 0.0 <= path.success_probability <= 1.0

    @pytest.mark.asyncio
    async def test_output_includes_strategy_types(
        self, fa_agent: FaAgent, sample_input: FaInput
    ) -> None:
        """Test that paths include strategy type classification."""
        result = await fa_agent.process(sample_input)

        for path in result.recommended_paths:
            assert path.strategy_type in [
                StrategyType.CONSERVATIVE,
                StrategyType.AGGRESSIVE,
                StrategyType.BALANCED,
            ]


class TestFaAgentProcess:
    """Test cases for FaAgent.process."""

    @pytest.mark.asyncio
    async def test_process_without_llm(
        self, fa_agent: FaAgent, sample_input: FaInput
    ) -> None:
        """Test process falls back to rule-based when no LLM."""
        result = await fa_agent.process(sample_input)

        assert isinstance(result, FaOutput)
        assert len(result.recommended_paths) >= 1

    @pytest.mark.asyncio
    async def test_process_generates_comparison_matrix(
        self, fa_agent: FaAgent, sample_input: FaInput
    ) -> None:
        """Test that process generates path comparison matrix."""
        result = await fa_agent.process(sample_input)

        if result.path_comparison:
            assert len(result.path_comparison.dimensions) >= 1
            # Scores should have entries for each path
            for path in result.recommended_paths:
                if path.path_id in result.path_comparison.scores:
                    scores = result.path_comparison.scores[path.path_id]
                    assert all(1 <= s <= 5 for s in scores)

    @pytest.mark.asyncio
    async def test_process_with_invalid_enum_values_is_normalized(
        self, sample_input: FaInput
    ) -> None:
        """LLMの不正な列挙値を安全に正規化できること."""
        agent = FaAgent(llm_client=MagicMock())
        agent._call_llm = AsyncMock(
            return_value=json.dumps(
                {
                    "recommended_paths": [
                        {
                            "path_id": "A",
                            "name": "戦略A",
                            "description": "説明",
                            "strategy_type": "INVALID_TYPE",
                            "reversibility": "BROKEN",
                            "pros": ["利点"],
                            "cons": ["欠点"],
                        }
                    ],
                    "decision_criteria": ["ROI"],
                    "fa_self_check": {
                        "baseless_numbers": [],
                        "missing_intermediate": [],
                        "missing_gates": [],
                        "appearance_precision": [],
                        "overall_status": "NOT_A_STATUS",
                    },
                },
                ensure_ascii=False,
            )
        )

        result = await agent.process(sample_input)
        assert result.recommended_paths[0].strategy_type == StrategyType.BALANCED
        assert result.recommended_paths[0].reversibility.value == "MEDIUM"
        assert result.fa_self_check is not None
        assert result.fa_self_check.overall_status == SelfCheckStatus.WARNING


class TestFaAgentValidation:
    """Test cases for output validation."""

    def test_validate_output_with_valid_output(self, fa_agent: FaAgent) -> None:
        """Test validation passes with valid output."""
        output = FaOutput(
            recommended_paths=[
                PathOption(
                    path_id="A",
                    name="市場拡大",
                    description="国内市場を拡大する",
                    pros=["成長性高い"],
                    cons=["リスク高い"],
                    success_probability=0.7,
                    strategy_type=StrategyType.AGGRESSIVE,
                )
            ],
            decision_criteria=["ROI", "リスク"],
        )
        result = fa_agent.validate_output(output)
        assert result is True

    def test_validate_output_with_no_paths(self, fa_agent: FaAgent) -> None:
        """Test validation fails with no recommended paths."""
        output = FaOutput(
            recommended_paths=[],
            decision_criteria=[],
        )
        result = fa_agent.validate_output(output)
        assert result is False
