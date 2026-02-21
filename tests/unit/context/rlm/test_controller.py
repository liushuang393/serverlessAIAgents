"""Unit tests for RLM controller module."""

from typing import Any
from unittest.mock import MagicMock

import pytest

from agentflow.context.rlm.config import RLMConfig, StopReason
from agentflow.context.rlm.controller import RLMController, RLMResult


class MockLLMClient:
    """Mock LLM client for testing."""

    def __init__(self, responses: list[dict[str, Any]] | None = None) -> None:
        """Initialize with optional response queue."""
        self._responses = responses or []
        self._call_count = 0

    async def chat(self, messages: list[dict[str, str]], **kwargs: Any) -> dict[str, Any]:
        """Return mock response."""
        if self._call_count < len(self._responses):
            response = self._responses[self._call_count]
        else:
            response = {
                "content": '{"action": "synthesize", "parameters": {}}',
                "usage": {"total_tokens": 100},
            }
        self._call_count += 1
        return response


class TestRLMResult:
    """Tests for RLMResult."""

    def test_creation(self) -> None:
        """Test result creation."""
        result = RLMResult(
            success=True,
            answer="The answer is 42",
            confidence=0.95,
            stop_reason=StopReason.ANSWER_FOUND,
            iterations=5,
            subcalls=3,
            tokens_used=5000,
            evidence=["Evidence 1", "Evidence 2"],
        )

        assert result.success is True
        assert result.answer == "The answer is 42"
        assert result.confidence == 0.95
        assert result.stop_reason == StopReason.ANSWER_FOUND
        assert result.iterations == 5
        assert len(result.evidence) == 2

    def test_default_values(self) -> None:
        """Test default values."""
        result = RLMResult(
            success=False,
            answer="",
            confidence=0.0,
            stop_reason=StopReason.ERROR,
            iterations=0,
            subcalls=0,
            tokens_used=0,
        )

        assert result.evidence == []
        assert result.metadata == {}


class TestRLMController:
    """Tests for RLMController."""

    @pytest.fixture
    def config(self) -> RLMConfig:
        """Create test config."""
        return RLMConfig(
            activation_threshold=1000,
            max_iterations=5,
            max_subcalls=10,
            max_subcall_budget=10000,
            convergence_threshold=0.9,
        )

    @pytest.fixture
    def controller(self, config: RLMConfig) -> RLMController:
        """Create controller without LLM client."""
        return RLMController(config=config, llm_client=None)

    def test_initialization(self, config: RLMConfig) -> None:
        """Test controller initialization."""
        controller = RLMController(config=config)

        assert controller._config == config
        assert controller._llm_client is None

    def test_set_llm_client(self, controller: RLMController) -> None:
        """Test setting LLM client."""
        mock_client = MockLLMClient()
        controller.set_llm_client(mock_client)

        assert controller._llm_client == mock_client

    def test_set_event_callback(self, controller: RLMController) -> None:
        """Test setting event callback."""
        callback = MagicMock()
        # Need to initialize event emitter first by running
        controller._flow_id = "test"
        from agentflow.context.rlm.events import RLMEventEmitter

        controller._event_emitter = RLMEventEmitter("test")

        controller.set_event_callback(callback)

    @pytest.mark.asyncio
    async def test_run_small_input_no_llm(self, controller: RLMController) -> None:
        """Test run with small input and no LLM (uses workspace fallback)."""
        result = await controller.run(
            query="What is in this document?",
            long_inputs=["This is a short test document about testing."],
        )

        # Should complete even without LLM (with low confidence)
        assert isinstance(result, RLMResult)
        assert result.stop_reason in [
            StopReason.MAX_ITERATIONS,
            StopReason.CONVERGENCE,
            StopReason.ANSWER_FOUND,
        ]

    @pytest.mark.asyncio
    async def test_run_with_event_callback(self, config: RLMConfig) -> None:
        """Test run emits events."""
        events: list[dict] = []

        controller = RLMController(config=config)
        await controller.run(
            query="Test query",
            long_inputs=["Test content"],
            event_callback=lambda e: events.append(e),
        )

        # Should have at least start and complete events
        event_types = [e["event_type"] for e in events]
        assert "rlm.start" in event_types
        assert "rlm.complete" in event_types or "rlm.error" in event_types

    @pytest.mark.asyncio
    async def test_run_stores_contexts(self, controller: RLMController) -> None:
        """Test run stores contexts in store."""
        await controller.run(
            query="Test",
            long_inputs=["Document 1", "Document 2"],
        )

        # Check that contexts were stored
        handles = controller._store.list_handles()
        assert len(handles) >= 2

    @pytest.mark.asyncio
    async def test_run_with_metadata(self, controller: RLMController) -> None:
        """Test run with metadata."""
        result = await controller.run(
            query="Test",
            long_inputs=["Content"],
            metadata={"source": "test"},
        )

        assert isinstance(result, RLMResult)

    def test_get_stats(self, controller: RLMController) -> None:
        """Test getting statistics."""
        stats = controller.get_stats()

        assert "store" in stats
        assert "workspace" in stats
        assert "budget" in stats
        assert "confidence" in stats

    @pytest.mark.asyncio
    async def test_run_max_iterations(self, config: RLMConfig) -> None:
        """Test run stops at max iterations."""
        # Set very low max iterations
        config.max_iterations = 2
        config.convergence_threshold = 1.0  # Never converge

        controller = RLMController(config=config)
        result = await controller.run(
            query="Complex question",
            long_inputs=["Long content " * 100],
        )

        assert result.iterations <= config.max_iterations

    @pytest.mark.asyncio
    async def test_reset_state(self, controller: RLMController) -> None:
        """Test state reset between runs."""
        await controller.run(query="First query", long_inputs=["Content 1"])

        # Run again - state should be reset
        await controller.run(query="Second query", long_inputs=["Content 2"])

        # Should have fresh store with only second content
        handles = controller._store.list_handles()
        assert len(handles) == 1

    @pytest.mark.asyncio
    async def test_run_with_mock_llm(self, config: RLMConfig) -> None:
        """Test run with mock LLM client."""
        mock_responses = [
            {
                "content": '{"action": "keyword_find", "parameters": {"handle_id": "ctx_test", "keywords": ["test"]}}',
                "usage": {"total_tokens": 50},
            },
            {
                "content": '{"confidence": 0.95, "reasoning": "Found answer"}',
                "usage": {"total_tokens": 30},
            },
            {
                "content": "The answer based on the document is: test content.",
                "usage": {"total_tokens": 100},
            },
        ]

        mock_client = MockLLMClient(mock_responses)
        controller = RLMController(config=config, llm_client=mock_client)

        result = await controller.run(
            query="What is in the document?",
            long_inputs=["This is test content about testing."],
        )

        assert isinstance(result, RLMResult)

    @pytest.mark.asyncio
    async def test_error_handling(self, config: RLMConfig) -> None:
        """Test error handling in run."""

        class FailingLLMClient:
            async def chat(self, *args: Any, **kwargs: Any) -> dict:
                msg = "LLM failed"
                raise RuntimeError(msg)

        controller = RLMController(config=config, llm_client=FailingLLMClient())

        result = await controller.run(
            query="Test",
            long_inputs=["Content"],
        )

        # Should not crash, but may have error result
        assert isinstance(result, RLMResult)


class TestRLMControllerInternal:
    """Tests for internal controller methods."""

    @pytest.fixture
    def controller(self) -> RLMController:
        """Create controller for testing."""
        config = RLMConfig(max_iterations=5)
        return RLMController(config=config)

    def test_extract_keywords(self, controller: RLMController) -> None:
        """Test keyword extraction from query."""
        keywords = controller._extract_keywords("What are the authentication requirements?")

        assert "authentication" in keywords
        assert "requirements" in keywords
        # Stop words should be excluded
        assert "what" not in keywords
        assert "are" not in keywords
        assert "the" not in keywords

    def test_extract_keywords_japanese(self, controller: RLMController) -> None:
        """Test keyword extraction with Japanese."""
        keywords = controller._extract_keywords("認証について教えてください")

        # Should extract meaningful parts
        assert len(keywords) > 0
        # Stop words should be excluded
        assert "について" not in keywords
        assert "教えて" not in keywords

    def test_check_stop_conditions_confidence(self, controller: RLMController) -> None:
        """Test stop condition: confidence threshold."""
        controller._current_confidence = 0.95
        controller._config.convergence_threshold = 0.9

        reason = controller._check_stop_conditions(1)
        assert reason == StopReason.ANSWER_FOUND

    def test_check_stop_conditions_budget(self, controller: RLMController) -> None:
        """Test stop condition: budget exhausted."""
        controller._budget.tokens_used = controller._budget.max_tokens

        reason = controller._check_stop_conditions(1)
        assert reason == StopReason.BUDGET_EXHAUSTED

    def test_check_stop_conditions_convergence(self, controller: RLMController) -> None:
        """Test stop condition: convergence (stagnation)."""
        controller._config.stagnation_rounds = 3
        controller._config.min_confidence_delta = 0.01
        controller._confidence_history = [0.5, 0.505, 0.508, 0.51]

        reason = controller._check_stop_conditions(5)
        assert reason == StopReason.CONVERGENCE

    def test_check_stop_conditions_continue(self, controller: RLMController) -> None:
        """Test stop condition: should continue."""
        controller._current_confidence = 0.5
        controller._confidence_history = []

        reason = controller._check_stop_conditions(1)
        assert reason is None
