"""Perception layer for Agent OS.

This module provides unified perception events for normalizing
all inputs to the agent system. All external stimuli (user input,
tool output, system signals, etc.) are converted to PerceptionEvents.
"""

from agentflow.perception.events import PerceptionEvent, PerceptionEventType

__all__ = [
    "PerceptionEvent",
    "PerceptionEventType",
]
