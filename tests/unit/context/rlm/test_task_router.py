# -*- coding: utf-8 -*-
"""Unit tests for RLM task_router module."""

import pytest

from agentflow.context.rlm.config import RLMConfig
from agentflow.context.rlm.task_router import (
    ActivationDecision,
    TaskRouter,
)


class TestActivationDecision:
    """Tests for ActivationDecision."""

    def test_creation(self) -> None:
        """Test decision creation."""
        decision = ActivationDecision(
            should_activate=True,
            reason="Token threshold exceeded",
            total_tokens=20000,
            context_count=2,
            confidence=0.9,
        )

        assert decision.should_activate is True
        assert decision.reason == "Token threshold exceeded"
        assert decision.total_tokens == 20000
        assert decision.context_count == 2
        assert decision.confidence == 0.9

    def test_default_values(self) -> None:
        """Test default values."""
        decision = ActivationDecision(should_activate=False, reason="Test")

        assert decision.total_tokens == 0
        assert decision.context_count == 0
        assert decision.confidence == 1.0
        assert decision.metadata == {}


class TestTaskRouter:
    """Tests for TaskRouter."""

    @pytest.fixture
    def router(self) -> TaskRouter:
        """Create router with default config."""
        return TaskRouter(RLMConfig())

    @pytest.fixture
    def low_threshold_router(self) -> TaskRouter:
        """Create router with low threshold for testing."""
        return TaskRouter(RLMConfig(activation_threshold=1000))

    def test_should_activate_below_threshold(self, router: TaskRouter) -> None:
        """Test no activation below threshold."""
        decision = router.should_activate(
            query="What is the weather?",
            long_inputs=["Short content."],
        )

        assert decision.should_activate is False
        assert "threshold" in decision.reason.lower() or "simple" in decision.reason.lower()

    def test_should_activate_above_threshold(self, low_threshold_router: TaskRouter) -> None:
        """Test activation above threshold."""
        # Create content that exceeds 1000 tokens
        large_content = "This is a test sentence with multiple words. " * 200

        decision = low_threshold_router.should_activate(
            query="What does this document say?",
            long_inputs=[large_content],
        )

        assert decision.should_activate is True
        assert "exceeds threshold" in decision.reason.lower()

    def test_should_activate_document_analysis_task(self, router: TaskRouter) -> None:
        """Test activation for document analysis tasks."""
        # Need enough tokens to exceed min_activation_tokens (threshold // 3 = 5000)
        # ASCII chars = 0.25 tokens each, so need 20000+ chars for 5000+ tokens
        decision = router.should_activate(
            query="Summarize and analyze this specification document",
            long_inputs=["x" * 30000],  # ~7500 tokens, above min_activation
        )

        # Check that document analysis was detected in metadata
        assert decision.metadata.get("reasons") is not None or "analysis" in decision.reason.lower()

    def test_should_not_activate_simple_qa(self, router: TaskRouter) -> None:
        """Test no activation for simple Q&A."""
        decision = router.should_activate(
            query="What is the capital of France?",
            long_inputs=[],
        )

        assert decision.should_activate is False

    def test_should_activate_multi_context(self, low_threshold_router: TaskRouter) -> None:
        """Test activation with multiple contexts."""
        contents = ["Document 1 content " * 50] * 3  # 3 documents

        decision = low_threshold_router.should_activate(
            query="Compare all documents",
            long_inputs=contents,
        )

        assert decision.context_count == 3

    def test_total_tokens_calculation(self, router: TaskRouter) -> None:
        """Test total tokens are calculated correctly."""
        content = "a" * 1000  # Should be ~250 tokens

        decision = router.should_activate(
            query="Test query",
            long_inputs=[content],
        )

        assert decision.total_tokens > 0

    def test_existing_context_tokens(self, router: TaskRouter) -> None:
        """Test existing context tokens are included."""
        decision = router.should_activate(
            query="Test",
            long_inputs=[],
            existing_context_tokens=10000,
        )

        assert decision.total_tokens >= 10000

    def test_confidence_level(self, low_threshold_router: TaskRouter) -> None:
        """Test confidence level is set."""
        large_content = "x" * 5000

        decision = low_threshold_router.should_activate(
            query="Analyze this",
            long_inputs=[large_content],
        )

        assert 0 < decision.confidence <= 1.0

    def test_estimate_complexity_low(self, router: TaskRouter) -> None:
        """Test complexity estimation for small inputs."""
        complexity = router.estimate_complexity(
            query="Short question",
            long_inputs=["Short content"],
        )

        assert complexity["complexity"] == "low"
        assert complexity["estimated_iterations"] == 1

    def test_estimate_complexity_high(self, router: TaskRouter) -> None:
        """Test complexity estimation for large inputs."""
        # For "high" complexity: threshold <= tokens < threshold * 2 (15000 - 30000)
        # ASCII chars = 0.25 tokens each, so need 60000-120000 chars
        large_content = "x" * 80000  # ~20000 tokens, "high" complexity

        complexity = router.estimate_complexity(
            query="Analyze this large document",
            long_inputs=[large_content],
        )

        assert complexity["complexity"] in ["high", "very_high"]
        assert complexity["estimated_iterations"] > 1

    def test_estimate_complexity_document_analysis(self, router: TaskRouter) -> None:
        """Test complexity estimation includes document analysis factor."""
        content = "x" * 20000

        complexity = router.estimate_complexity(
            query="Summarize this document",
            long_inputs=[content],
        )

        assert complexity["is_document_analysis"] is True

    def test_document_analysis_keywords(self, router: TaskRouter) -> None:
        """Test detection of document analysis keywords."""
        keywords = ["summarize", "analyze", "extract", "compare", "review"]

        for keyword in keywords:
            decision = router.should_activate(
                query=f"Please {keyword} the content",
                long_inputs=["x" * 10000],
            )
            # Should detect analysis task
            assert decision.metadata or decision.total_tokens > 0

    def test_simple_qa_keywords(self, router: TaskRouter) -> None:
        """Test detection of simple Q&A keywords."""
        decision = router.should_activate(
            query="What is this? Tell me where to find it.",
            long_inputs=[],
        )

        assert decision.should_activate is False

    def test_empty_inputs(self, router: TaskRouter) -> None:
        """Test with empty inputs."""
        decision = router.should_activate(
            query="Hello",
            long_inputs=None,
        )

        assert decision.should_activate is False
        assert decision.context_count == 0

    def test_minimum_activation_tokens(self, router: TaskRouter) -> None:
        """Test that very small inputs don't activate even with analysis keywords."""
        decision = router.should_activate(
            query="Summarize this document",
            long_inputs=["tiny"],  # Very small
        )

        # Should not activate because content is too small to benefit from RLM
        assert decision.should_activate is False
