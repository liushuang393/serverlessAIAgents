"""Unified perception events for Agent OS.

All inputs to the agent are normalized as PerceptionEvents,
providing a consistent interface regardless of the source.
"""

import time
import uuid
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class PerceptionEventType(str, Enum):
    """Types of perception events.

    These represent different categories of inputs that an agent
    can perceive from its environment.
    """

    USER_INSTRUCTION = "user_instruction"
    """Direct instruction or query from a user."""

    TOOL_OUTPUT = "tool_output"
    """Output from a tool execution."""

    SYSTEM_SIGNAL = "system_signal"
    """System-level signals (shutdown, restart, etc.)."""

    ENVIRONMENT_CHANGE = "environment_change"
    """Changes in the execution environment."""

    FEEDBACK = "feedback"
    """Feedback on previous actions or outputs."""

    TIMEOUT = "timeout"
    """Timeout signal indicating time limit exceeded."""


class PerceptionEvent(BaseModel):
    """Unified perception event - all inputs normalized.

    A PerceptionEvent represents any input to the agent system,
    normalized to a consistent format. This allows the agent to
    handle all inputs uniformly regardless of their source.

    Attributes:
        event_id: Unique identifier for this event
        event_type: Type of perception event
        timestamp: When the event was created (Unix timestamp)
        payload: Event-specific data
        source_type: Type of source (user, tool, agent, system)
        source_id: Identifier of the source
        confidence: Confidence level of the perception (0.0-1.0)
        requires_response: Whether this event requires a response
    """

    event_id: str = Field(default_factory=lambda: f"perc-{uuid.uuid4().hex[:12]}")
    event_type: PerceptionEventType
    timestamp: float = Field(default_factory=time.time)
    payload: dict[str, Any] = Field(default_factory=dict)
    source_type: str  # "user", "tool", "agent", "system"
    source_id: str
    confidence: float = Field(default=1.0, ge=0.0, le=1.0)
    requires_response: bool = False

    model_config = {"frozen": False}

    def is_from_user(self) -> bool:
        """Check if this event originated from a user."""
        return self.source_type == "user"

    def is_from_tool(self) -> bool:
        """Check if this event originated from a tool."""
        return self.source_type == "tool"

    def is_from_system(self) -> bool:
        """Check if this event originated from the system."""
        return self.source_type == "system"

    def get_content(self, key: str, default: Any = None) -> Any:
        """Get a value from the payload.

        Args:
            key: The key to retrieve
            default: Default value if key not found

        Returns:
            The value or default
        """
        return self.payload.get(key, default)
