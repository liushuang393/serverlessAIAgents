"""Pytest configuration and shared fixtures."""

import pytest

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.types import WorkflowConfig


@pytest.fixture
def engine() -> AgentFlowEngine:
    """Create a test AgentFlow engine instance.

    Returns:
        AgentFlowEngine instance for testing.
    """
    return AgentFlowEngine()


@pytest.fixture
def sample_workflow() -> WorkflowConfig:
    """Create a sample workflow configuration.

    Returns:
        WorkflowConfig instance for testing.
    """
    return WorkflowConfig(
        workflow_id="test-workflow",
        name="Test Workflow",
        description="A test workflow for unit tests",
        nodes=[
            {"id": "node1", "type": "start"},
            {"id": "node2", "type": "process"},
            {"id": "node3", "type": "end"},
        ],
        edges=[
            {"from": "node1", "to": "node2"},
            {"from": "node2", "to": "node3"},
        ],
    )

