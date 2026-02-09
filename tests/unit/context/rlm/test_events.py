# -*- coding: utf-8 -*-
"""Unit tests for RLM events module."""

import json
import time

import pytest

from agentflow.context.rlm.events import (
    RLMActionEvent,
    RLMCompleteEvent,
    RLMErrorEvent,
    RLMEvent,
    RLMEventEmitter,
    RLMEventType,
    RLMIterationEvent,
    RLMStartEvent,
    RLMSubcallEvent,
    RLMWorkspaceUpdateEvent,
)


class TestRLMEventType:
    """Tests for RLMEventType enum."""

    def test_lifecycle_events(self) -> None:
        """Test lifecycle event types."""
        assert RLMEventType.RLM_START.value == "rlm.start"
        assert RLMEventType.RLM_COMPLETE.value == "rlm.complete"
        assert RLMEventType.RLM_ERROR.value == "rlm.error"

    def test_progress_events(self) -> None:
        """Test progress event types."""
        assert RLMEventType.RLM_ITERATION.value == "rlm.iteration"
        assert RLMEventType.RLM_SUBCALL.value == "rlm.subcall"
        assert RLMEventType.RLM_ACTION.value == "rlm.action"

    def test_workspace_events(self) -> None:
        """Test workspace event types."""
        assert RLMEventType.RLM_WORKSPACE_UPDATE.value == "rlm.workspace_update"


class TestRLMEvent:
    """Tests for RLMEvent base class."""

    def test_to_dict(self) -> None:
        """Test dictionary conversion."""
        event = RLMStartEvent(
            flow_id="flow-123",
            total_contexts=2,
            total_tokens=10000,
        )

        d = event.to_dict()
        assert d["event_type"] == "rlm.start"
        assert d["flow_id"] == "flow-123"
        assert "timestamp" in d

    def test_to_sse(self) -> None:
        """Test SSE format conversion."""
        event = RLMStartEvent(
            flow_id="flow-123",
            total_contexts=2,
            total_tokens=10000,
        )

        sse = event.to_sse()
        assert sse.startswith("data: ")
        assert sse.endswith("\n\n")

        # Parse JSON part
        json_str = sse[6:-2]  # Remove "data: " and "\n\n"
        data = json.loads(json_str)
        assert data["event_type"] == "rlm.start"


class TestRLMStartEvent:
    """Tests for RLMStartEvent."""

    def test_creation(self) -> None:
        """Test event creation."""
        event = RLMStartEvent(
            flow_id="flow-abc",
            total_contexts=3,
            total_tokens=25000,
            activation_reason="Token threshold exceeded",
            config_summary={"max_iterations": 20},
        )

        assert event.event_type == RLMEventType.RLM_START
        assert event.total_contexts == 3
        assert event.total_tokens == 25000
        assert event.activation_reason == "Token threshold exceeded"

    def test_default_values(self) -> None:
        """Test default values."""
        event = RLMStartEvent(
            flow_id="flow-123",
            total_contexts=1,
            total_tokens=5000,
        )

        assert event.activation_reason == ""
        assert event.config_summary == {}


class TestRLMIterationEvent:
    """Tests for RLMIterationEvent."""

    def test_creation(self) -> None:
        """Test event creation."""
        event = RLMIterationEvent(
            flow_id="flow-123",
            iteration=5,
            max_iterations=20,
            confidence=0.75,
            action_planned="peek",
            budget_remaining={"tokens": 40000, "calls": 25},
        )

        assert event.event_type == RLMEventType.RLM_ITERATION
        assert event.iteration == 5
        assert event.confidence == 0.75


class TestRLMSubcallEvent:
    """Tests for RLMSubcallEvent."""

    def test_creation(self) -> None:
        """Test event creation."""
        event = RLMSubcallEvent(
            flow_id="flow-123",
            subcall_type="summarize",
            tokens_used=500,
            total_tokens_used=2500,
            result_summary="Summarized 3 sections",
        )

        assert event.event_type == RLMEventType.RLM_SUBCALL
        assert event.subcall_type == "summarize"
        assert event.tokens_used == 500


class TestRLMActionEvent:
    """Tests for RLMActionEvent."""

    def test_creation(self) -> None:
        """Test event creation."""
        event = RLMActionEvent(
            flow_id="flow-123",
            action_type="regex_find",
            target_handle="ctx_abc",
            parameters={"pattern": r"def \w+"},
            result_count=5,
            execution_time_ms=15.5,
        )

        assert event.event_type == RLMEventType.RLM_ACTION
        assert event.action_type == "regex_find"
        assert event.result_count == 5


class TestRLMWorkspaceUpdateEvent:
    """Tests for RLMWorkspaceUpdateEvent."""

    def test_creation(self) -> None:
        """Test event creation."""
        event = RLMWorkspaceUpdateEvent(
            flow_id="flow-123",
            variable_name="summary_1",
            variable_type="summary",
            operation="set",
            workspace_size=5,
            workspace_tokens=2500,
        )

        assert event.event_type == RLMEventType.RLM_WORKSPACE_UPDATE
        assert event.variable_name == "summary_1"
        assert event.operation == "set"


class TestRLMCompleteEvent:
    """Tests for RLMCompleteEvent."""

    def test_creation(self) -> None:
        """Test event creation."""
        event = RLMCompleteEvent(
            flow_id="flow-123",
            stop_reason="answer_found",
            total_iterations=12,
            total_subcalls=8,
            total_tokens_used=15000,
            final_confidence=0.95,
            has_answer=True,
            answer_preview="The authentication requires...",
        )

        assert event.event_type == RLMEventType.RLM_COMPLETE
        assert event.stop_reason == "answer_found"
        assert event.has_answer is True


class TestRLMErrorEvent:
    """Tests for RLMErrorEvent."""

    def test_creation(self) -> None:
        """Test event creation."""
        event = RLMErrorEvent(
            flow_id="flow-123",
            error_message="Budget exhausted",
            error_type="BudgetError",
            iteration=10,
            recoverable=False,
        )

        assert event.event_type == RLMEventType.RLM_ERROR
        assert event.error_message == "Budget exhausted"
        assert event.recoverable is False


class TestRLMEventEmitter:
    """Tests for RLMEventEmitter."""

    def test_emit_start(self) -> None:
        """Test emitting start event."""
        events: list[dict] = []
        emitter = RLMEventEmitter(
            flow_id="flow-123",
            callback=lambda e: events.append(e),
            enabled=True,
        )

        emitter.emit_start(
            total_contexts=2,
            total_tokens=10000,
            activation_reason="Test",
        )

        assert len(events) == 1
        assert events[0]["event_type"] == "rlm.start"

    def test_emit_iteration(self) -> None:
        """Test emitting iteration event."""
        events: list[dict] = []
        emitter = RLMEventEmitter(
            flow_id="flow-123",
            callback=lambda e: events.append(e),
        )

        emitter.emit_iteration(
            iteration=1,
            max_iterations=20,
            confidence=0.5,
        )

        assert len(events) == 1
        assert events[0]["event_type"] == "rlm.iteration"

    def test_emit_subcall(self) -> None:
        """Test emitting subcall event."""
        events: list[dict] = []
        emitter = RLMEventEmitter(
            flow_id="flow-123",
            callback=lambda e: events.append(e),
        )

        emitter.emit_subcall(
            subcall_type="summarize",
            tokens_used=500,
            total_tokens_used=1500,
        )

        assert len(events) == 1
        assert events[0]["event_type"] == "rlm.subcall"

    def test_emit_action(self) -> None:
        """Test emitting action event."""
        events: list[dict] = []
        emitter = RLMEventEmitter(
            flow_id="flow-123",
            callback=lambda e: events.append(e),
        )

        emitter.emit_action(
            action_type="peek",
            target_handle="ctx_abc",
            result_count=10,
        )

        assert len(events) == 1
        assert events[0]["event_type"] == "rlm.action"

    def test_emit_complete(self) -> None:
        """Test emitting complete event."""
        events: list[dict] = []
        emitter = RLMEventEmitter(
            flow_id="flow-123",
            callback=lambda e: events.append(e),
        )

        emitter.emit_complete(
            stop_reason="answer_found",
            total_iterations=10,
            total_subcalls=5,
            total_tokens_used=8000,
            final_confidence=0.92,
            has_answer=True,
        )

        assert len(events) == 1
        assert events[0]["event_type"] == "rlm.complete"

    def test_emit_error(self) -> None:
        """Test emitting error event."""
        events: list[dict] = []
        emitter = RLMEventEmitter(
            flow_id="flow-123",
            callback=lambda e: events.append(e),
        )

        emitter.emit_error(
            error_message="Test error",
            error_type="TestError",
        )

        assert len(events) == 1
        assert events[0]["event_type"] == "rlm.error"

    def test_disabled_emitter(self) -> None:
        """Test disabled emitter doesn't emit."""
        events: list[dict] = []
        emitter = RLMEventEmitter(
            flow_id="flow-123",
            callback=lambda e: events.append(e),
            enabled=False,
        )

        emitter.emit_start(total_contexts=1, total_tokens=1000)

        assert len(events) == 0

    def test_no_callback(self) -> None:
        """Test emitter with no callback doesn't raise."""
        emitter = RLMEventEmitter(flow_id="flow-123", callback=None)

        # Should not raise
        emitter.emit_start(total_contexts=1, total_tokens=1000)

    def test_set_callback(self) -> None:
        """Test setting callback after creation."""
        events: list[dict] = []
        emitter = RLMEventEmitter(flow_id="flow-123")

        emitter.emit_start(total_contexts=1, total_tokens=1000)  # No events yet

        emitter.set_callback(lambda e: events.append(e))
        emitter.emit_start(total_contexts=2, total_tokens=2000)

        assert len(events) == 1

    def test_answer_preview_truncation(self) -> None:
        """Test answer preview is truncated."""
        events: list[dict] = []
        emitter = RLMEventEmitter(
            flow_id="flow-123",
            callback=lambda e: events.append(e),
        )

        long_answer = "x" * 200

        emitter.emit_complete(
            stop_reason="answer_found",
            total_iterations=1,
            total_subcalls=1,
            total_tokens_used=1000,
            final_confidence=0.9,
            has_answer=True,
            answer_preview=long_answer,
        )

        assert len(events[0]["answer_preview"]) <= 100
