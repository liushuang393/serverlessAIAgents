"""Structured reasoner output for Agent OS.

ActionDecision represents what the agent decides to do next.
This is NOT text - it's a structured decision that can be
validated and processed by the system.
"""

import time
import uuid
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class ActionType(str, Enum):
    """Types of actions the reasoner can decide on.

    These represent the possible next steps an agent can take.
    """

    TOOL_CALL = "tool_call"
    """Call a tool with specific parameters."""

    MESSAGE = "message"
    """Send a message (to user or another agent)."""

    DELEGATE = "delegate"
    """Delegate the task to another agent."""

    WAIT = "wait"
    """Wait for more information or evidence."""

    ESCALATE = "escalate"
    """Escalate to human or higher authority."""

    CLARIFY = "clarify"
    """Ask for clarification."""

    COMPLETE = "complete"
    """Mark the task as complete."""

    ABORT = "abort"
    """Abort the current task."""


class ActionDecision(BaseModel):
    """Structured reasoner output - what to do next (NOT text).

    An ActionDecision represents the agent's decision about what
    action to take next. It includes:
    - The action type and parameters
    - The reasoning behind the decision
    - Required evidence for the decision
    - Confidence level

    This structured format ensures that agent decisions are
    machine-processable and verifiable.

    Attributes:
        decision_id: Unique identifier for this decision
        next_action: The type of action to take
        action_params: Parameters for the action
        reason: Human-readable explanation of the decision
        chain_of_thought: Step-by-step reasoning process
        required_evidence: Evidence needed before proceeding
        confidence: Confidence level (0.0-1.0)
        timestamp: When the decision was made
    """

    decision_id: str = Field(
        default_factory=lambda: f"dec-{uuid.uuid4().hex[:8]}"
    )
    next_action: ActionType
    action_params: dict[str, Any] = Field(default_factory=dict)
    reason: str
    chain_of_thought: list[str] = Field(default_factory=list)
    required_evidence: list[str] = Field(default_factory=list)
    confidence: float = Field(default=0.8, ge=0.0, le=1.0)
    timestamp: float = Field(default_factory=time.time)

    model_config = {"frozen": False}

    def is_terminal(self) -> bool:
        """Check if this decision ends the task.

        Returns:
            True if the action is COMPLETE or ABORT
        """
        return self.next_action in {ActionType.COMPLETE, ActionType.ABORT}

    def requires_tool(self) -> bool:
        """Check if this decision requires a tool call.

        Returns:
            True if the action is TOOL_CALL
        """
        return self.next_action == ActionType.TOOL_CALL

    def requires_human(self) -> bool:
        """Check if this decision requires human input.

        Returns:
            True if the action is ESCALATE or CLARIFY
        """
        return self.next_action in {ActionType.ESCALATE, ActionType.CLARIFY}

    def get_tool_name(self) -> str | None:
        """Get the tool name if this is a tool call.

        Returns:
            The tool name or None if not a tool call
        """
        if self.next_action == ActionType.TOOL_CALL:
            return self.action_params.get("tool")
        return None
