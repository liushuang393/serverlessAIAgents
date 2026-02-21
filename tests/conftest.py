"""Pytest configuration and shared fixtures."""

import os

import pytest

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.types import WorkflowConfig
from agentflow.providers.llm_provider import reset_llm


@pytest.fixture(autouse=True)
def force_mock_llm(request: pytest.FixtureRequest, monkeypatch: pytest.MonkeyPatch) -> None:
    """Force mock LLM provider in tests unless explicitly marked real_llm."""
    if request.node.get_closest_marker("real_llm"):
        return

    monkeypatch.setenv("LLM_PROVIDER", "mock")
    for key in (
        "OPENAI_API_KEY",
        "ANTHROPIC_API_KEY",
        "GOOGLE_API_KEY",
        "DEEPSEEK_API_KEY",
    ):
        monkeypatch.delenv(key, raising=False)
        os.environ.pop(key, None)
    reset_llm()


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
