"""Unit tests for GatekeeperAgent."""

import pytest
from apps.decision_governance_engine.agents.gatekeeper_agent import GatekeeperAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    GatekeeperInput,
    GatekeeperOutput,
    QuestionCategory,
)


@pytest.fixture
def gatekeeper_agent() -> GatekeeperAgent:
    """Create GatekeeperAgent instance for testing."""
    return GatekeeperAgent(llm_client=None)


class TestGatekeeperAgentInit:
    """Test cases for GatekeeperAgent initialization."""

    def test_agent_inherits_resilient_agent(self, gatekeeper_agent: GatekeeperAgent) -> None:
        """Test that GatekeeperAgent inherits from ResilientAgent."""
        from agentflow import ResilientAgent

        assert isinstance(gatekeeper_agent, ResilientAgent)

    def test_agent_has_correct_name(self, gatekeeper_agent: GatekeeperAgent) -> None:
        """Test that agent has correct name."""
        assert gatekeeper_agent.name == "GatekeeperAgent"

    def test_agent_has_low_temperature(self, gatekeeper_agent: GatekeeperAgent) -> None:
        """Test that temperature is set very low for consistent judgment."""
        assert gatekeeper_agent.temperature == 0.1


class TestGatekeeperAgentParseInput:
    """Test cases for GatekeeperAgent._parse_input."""

    def test_parse_input_with_valid_dict(self, gatekeeper_agent: GatekeeperAgent) -> None:
        """Test that _parse_input correctly parses a valid dict."""
        raw_input = {"raw_question": "新規事業に投資すべきか？"}
        parsed = gatekeeper_agent._parse_input(raw_input)
        assert isinstance(parsed, GatekeeperInput)
        assert parsed.raw_question == "新規事業に投資すべきか？"


class TestGatekeeperAgentPatterns:
    """Test cases for pattern matching via process()."""

    @pytest.mark.asyncio
    async def test_reject_weather_question_via_process(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """Test that weather questions are rejected."""
        input_data = GatekeeperInput(raw_question="今日の天気はどうですか？")
        result = await gatekeeper_agent.process(input_data)
        assert result.is_acceptable is False
        assert result.category == QuestionCategory.FACTUAL_LOOKUP

    @pytest.mark.asyncio
    async def test_reject_system_question_via_process(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """Test that system inquiry questions are rejected."""
        input_data = GatekeeperInput(raw_question="このシステムはどうやって作られましたか？")
        result = await gatekeeper_agent.process(input_data)
        assert result.is_acceptable is False
        assert result.category == QuestionCategory.SYSTEM_INQUIRY

    @pytest.mark.asyncio
    async def test_reject_short_greeting_via_process(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """Test that short greetings are rejected."""
        input_data = GatekeeperInput(raw_question="こんにちは！")
        result = await gatekeeper_agent.process(input_data)
        assert result.is_acceptable is False
        # Short questions are rejected as CASUAL_CHAT

    @pytest.mark.asyncio
    async def test_accept_trade_off_question_via_process(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """Test detection of trade-off questions."""
        input_data = GatekeeperInput(
            raw_question="新規事業でAとBのどちらを選ぶべきか？予算制約あり"
        )
        result = await gatekeeper_agent.process(input_data)
        assert result.is_acceptable is True
        assert result.category == QuestionCategory.TRADE_OFF_CHOICE

    @pytest.mark.asyncio
    async def test_accept_resource_allocation_via_process(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """Test detection of resource allocation questions."""
        input_data = GatekeeperInput(raw_question="新製品開発の予算配分をどうすべきか？")
        result = await gatekeeper_agent.process(input_data)
        assert result.is_acceptable is True
        assert result.category == QuestionCategory.RESOURCE_ALLOCATION

    @pytest.mark.asyncio
    async def test_accept_timing_question_via_process(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """Test detection of timing questions."""
        input_data = GatekeeperInput(
            raw_question="新規事業にいつ着手すべきか？競合の動向を考慮したい"
        )
        result = await gatekeeper_agent.process(input_data)
        assert result.is_acceptable is True
        assert result.category == QuestionCategory.TIMING_JUDGMENT

    @pytest.mark.asyncio
    async def test_now_word_in_decision_question_is_not_auto_rejected(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """「現在/现在」を含んでも意思決定文脈なら即時拒否しない."""
        input_data = GatekeeperInput(raw_question="现在这个季度我们是否应该追加投资到新产品线？")
        result = await gatekeeper_agent.process(input_data)
        assert result.is_acceptable is True


class TestGatekeeperAgentProcess:
    """Test cases for GatekeeperAgent.process."""

    @pytest.mark.asyncio
    async def test_process_rejects_instant_reject_pattern(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """Test that process rejects instant-reject patterns."""
        input_data = GatekeeperInput(raw_question="今日の天気は？")
        result = await gatekeeper_agent.process(input_data)

        assert isinstance(result, GatekeeperOutput)
        assert result.is_acceptable is False
        assert result.category == QuestionCategory.FACTUAL_LOOKUP

    @pytest.mark.asyncio
    async def test_process_accepts_decision_question(
        self, gatekeeper_agent: GatekeeperAgent
    ) -> None:
        """Test that process accepts valid decision questions."""
        # Use a longer question with decision keywords to ensure acceptance
        input_data = GatekeeperInput(
            raw_question="新製品への投資判断をしたい。予算配分とリスク評価が必要。"
        )
        result = await gatekeeper_agent.process(input_data)

        assert isinstance(result, GatekeeperOutput)
        assert result.is_acceptable is True

    @pytest.mark.asyncio
    async def test_process_with_ambiguous_question(self, gatekeeper_agent: GatekeeperAgent) -> None:
        """Test process with ambiguous question falls back to rule-based."""
        input_data = GatekeeperInput(raw_question="これについて考えたい")
        result = await gatekeeper_agent.process(input_data)

        assert isinstance(result, GatekeeperOutput)
        # Without LLM, ambiguous questions should be accepted (err on side of caution)


class TestGatekeeperAgentValidation:
    """Test cases for output validation."""

    def test_validate_output_with_valid_output(self, gatekeeper_agent: GatekeeperAgent) -> None:
        """Test validation passes with valid output."""
        output = GatekeeperOutput(
            is_acceptable=True,
            category=QuestionCategory.STRATEGIC_DECISION,
            confidence=0.9,
        )
        result = gatekeeper_agent.validate_output(output)
        assert result is True

    def test_validate_output_with_rejection(self, gatekeeper_agent: GatekeeperAgent) -> None:
        """Test validation passes with rejection output."""
        output = GatekeeperOutput(
            is_acceptable=False,
            category=QuestionCategory.CASUAL_CHAT,
            confidence=0.95,
            rejection_reason="雑談は対象外",
            rejection_message="意思決定に関する質問をお願いします",
        )
        result = gatekeeper_agent.validate_output(output)
        assert result is True
