"""AgentFlow - Lightweight AI Agent Development Framework.

AgentFlow is a lightweight framework for building AI agents with native support
for MCP, A2A, and AG-UI protocols. Built on top of PocketFlow, it provides a
simple yet powerful way to create, compose, and deploy AI agents.

Example:
    >>> from agentflow import AgentFlowEngine
    >>> engine = AgentFlowEngine()
    >>> result = await engine.execute("my-workflow", {"input": "test"})
"""

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.exceptions import (
    AgentFlowError,
    ProtocolError,
    WorkflowError,
    WorkflowNotFoundError,
)
from agentflow.core.types import AgentMetadata, WorkflowConfig


__version__ = "0.1.0"
__all__ = [
    "AgentFlowEngine",
    "AgentFlowError",
    "AgentMetadata",
    "ProtocolError",
    "WorkflowConfig",
    "WorkflowError",
    "WorkflowNotFoundError",
]
