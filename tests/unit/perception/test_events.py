import pytest
from agentflow.perception.events import PerceptionEvent, PerceptionEventType


def test_perception_event_creation():
    event = PerceptionEvent(
        event_type=PerceptionEventType.USER_INSTRUCTION,
        payload={"content": "Hello"},
        source_type="user",
        source_id="user-123",
    )
    assert event.event_type == PerceptionEventType.USER_INSTRUCTION
    assert event.payload["content"] == "Hello"
    assert event.source_type == "user"
    assert event.source_id == "user-123"


def test_perception_event_default_values():
    event = PerceptionEvent(
        event_type=PerceptionEventType.TOOL_OUTPUT,
        source_type="tool",
        source_id="tool-search",
    )
    assert event.event_id.startswith("perc-")
    assert event.timestamp > 0
    assert event.payload == {}
    assert event.confidence == 1.0
    assert event.requires_response is False


def test_perception_event_types():
    # Verify all event types are defined
    assert PerceptionEventType.USER_INSTRUCTION
    assert PerceptionEventType.TOOL_OUTPUT
    assert PerceptionEventType.SYSTEM_SIGNAL
    assert PerceptionEventType.ENVIRONMENT_CHANGE
    assert PerceptionEventType.FEEDBACK
    assert PerceptionEventType.TIMEOUT


def test_perception_event_with_custom_confidence():
    event = PerceptionEvent(
        event_type=PerceptionEventType.TOOL_OUTPUT,
        payload={"data": [1, 2, 3]},
        source_type="tool",
        source_id="api-call",
        confidence=0.85,
    )
    assert event.confidence == 0.85


def test_perception_event_requires_response():
    event = PerceptionEvent(
        event_type=PerceptionEventType.USER_INSTRUCTION,
        payload={"query": "What is the weather?"},
        source_type="user",
        source_id="user-456",
        requires_response=True,
    )
    assert event.requires_response is True


def test_perception_event_system_signal():
    event = PerceptionEvent(
        event_type=PerceptionEventType.SYSTEM_SIGNAL,
        payload={"signal": "shutdown", "reason": "timeout"},
        source_type="system",
        source_id="scheduler",
    )
    assert event.event_type == PerceptionEventType.SYSTEM_SIGNAL
    assert event.payload["signal"] == "shutdown"
