"""@agent デコレータと AgentRegistry 統合テスト."""

import pytest

from agentflow.core.agent_registry import get_global_agent_registry, reset_global_agent_registry


@pytest.fixture(autouse=True)
def reset_registry():
    """テスト前後にレジストリをリセット."""
    reset_global_agent_registry()
    yield
    reset_global_agent_registry()


def test_agent_decorator_registers_in_agent_registry():
    """@agent デコレータが AgentRegistry に登録することを確認."""
    from agentflow import agent

    @agent
    class TestAgent:
        """テスト用Agent."""

        system_prompt = "テストAgent"

    registry = get_global_agent_registry()

    # AgentRegistry に登録されていることを確認
    agent_ids = [entry.agent_id for entry in registry.list_all()]
    assert "TestAgent" in agent_ids

    # ファクトリが取得できることを確認
    factory = registry.get_factory("TestAgent")
    assert factory is not None


def test_agent_decorator_creates_capability_spec():
    """@agent デコレータが AgentCapabilitySpec を作成することを確認."""
    from agentflow import agent

    @agent(skills=["rag", "chatbot"])
    class SkillfulAgent:
        """スキル付きAgent."""

        system_prompt = "スキル付きAgent"

    registry = get_global_agent_registry()
    capability = registry.get_capability("SkillfulAgent")

    assert capability is not None
    # スキルがタグまたは required_tools に含まれていることを確認
    has_rag = "rag" in capability.tags or any("rag" in t for t in capability.required_tools)
    assert has_rag
    assert "スキル付きAgent" in capability.description


def test_agent_registry_find_matching():
    """AgentRegistry でタスク要件マッチングができることを確認."""
    from agentflow import agent
    from agentflow.core.capability_spec import CapabilityRequirement

    @agent(skills=["rag"])
    class RAGAgent:
        """RAG Agent."""

        system_prompt = "RAG検索Agent"

    @agent(skills=["chatbot"])
    class ChatAgent:
        """Chat Agent."""

        system_prompt = "チャットAgent"

    registry = get_global_agent_registry()

    # RAG タスク要件でマッチング
    requirement = CapabilityRequirement(
        description="ドキュメントを検索",
        required_tags=["rag"],
    )

    matches = registry.find_matching(requirement)
    assert len(matches) > 0
    assert matches[0][0] == "RAGAgent"
