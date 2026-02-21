"""Pytest configuration and shared fixtures."""

import os

import pytest

# Phase 系は PowerShell 依存のスタンドアロンスクリプトのため収集から除外（Linux/WSL では powershell が無い）
# Windows では python tests/test_phase1_coverage.py のように直接実行可
collect_ignore_glob = [
    "test_phase1_coverage.py",
    "test_phase2_task*.py",
]

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


@pytest.fixture(autouse=True)
def _redirect_home_to_tmp(tmp_path: Path) -> None:  # type: ignore[misc]
    """全テストで Path.home() を一時ディレクトリにリダイレクトする。

    これにより ~/.agentflow など、ホームディレクトリへの書き込みを
    サンドボックスや CI 環境でも安全に実行できる。
    """
    fake_home = tmp_path / "home"
    fake_home.mkdir(parents=True, exist_ok=True)
    with patch.object(pathlib.Path, "home", staticmethod(lambda: fake_home)):
        yield


@pytest.fixture
def engine() -> AgentFlowEngine:
    """テスト用 AgentFlowEngine インスタンスを生成する。"""
    return AgentFlowEngine()


@pytest.fixture
def sample_workflow() -> WorkflowConfig:
    """テスト用サンプルワークフロー設定を生成する。"""
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
