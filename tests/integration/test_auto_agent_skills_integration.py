# -*- coding: utf-8 -*-
"""Auto-Agent + Skills 統合テスト.

ToolDiscoveryService、AgentRegistry、SimpleEngine、ToolBinder の
完全統合ワークフローをテスト。
"""

import pytest
from agentflow.core.tool_registry import reset_global_tool_registry
from agentflow.core.agent_registry import reset_global_agent_registry


@pytest.fixture(autouse=True)
def reset_registries():
    """テスト前後にレジストリをリセット."""
    reset_global_tool_registry()
    reset_global_agent_registry()
    yield
    reset_global_tool_registry()
    reset_global_agent_registry()


@pytest.mark.integration
@pytest.mark.asyncio
async def test_full_auto_agent_skills_workflow():
    """Auto-Agent + Skills の完全統合ワークフロー."""
    from agentflow import agent
    from agentflow.engines import SimpleEngine
    from agentflow.core.tool_registry import get_global_tool_registry
    from agentflow.core.agent_registry import get_global_agent_registry
    from agentflow.core.tool_discovery import ToolDiscoveryService
    from agentflow.core.capability_spec import CapabilityRequirement

    # Step 1: Skills を発見
    tool_registry = get_global_tool_registry()
    service = ToolDiscoveryService(tool_registry)
    skill_count = await service.discover_skills_from_engine()
    assert skill_count > 0, "スキルが発見されませんでした"

    # Step 2: @agent で Agent を定義
    @agent(skills=["rag"])
    class RAGTestAgent:
        """RAG テストAgent."""

        system_prompt = "RAG検索を行うAgent"

        async def run(self, inputs):
            return {
                "query": inputs.get("query"),
                "has_tools": self._tools is not None,
            }

    # Step 3: AgentRegistry でマッチング
    agent_registry = get_global_agent_registry()
    requirement = CapabilityRequirement(
        description="ドキュメント検索",
        required_tags=["rag"],
    )
    matches = agent_registry.find_matching(requirement)
    assert len(matches) > 0, "マッチするAgentがありません"
    assert matches[0][0] == "RAGTestAgent"

    # Step 4: SimpleEngine で実行（自動ツールバインド）
    engine = SimpleEngine(
        agent=RAGTestAgent,
        skills=["rag"],
    )
    result = await engine.run({"query": "テストクエリ"})

    assert result["query"] == "テストクエリ"
    # ツールがバインドされていることを確認
    assert result.get("has_tools") is True


@pytest.mark.integration
@pytest.mark.asyncio
async def test_skill_discovery_registers_all_sources():
    """スキル発見が全ソースを登録することを確認."""
    from agentflow.core.tool_registry import get_global_tool_registry
    from agentflow.core.tool_discovery import ToolDiscoveryService
    from agentflow.core.tool_definition import ToolSource

    tool_registry = get_global_tool_registry()
    service = ToolDiscoveryService(tool_registry)

    # スキルを発見
    count = await service.discover_skills_from_engine()
    assert count > 0

    # スキルソースのツールが登録されている
    skill_tools = tool_registry.filter_by_source(ToolSource.SKILL)
    assert len(skill_tools) > 0

    # RAG スキルが存在
    rag_tool = tool_registry.get("tool://skill/rag")
    assert rag_tool is not None


@pytest.mark.integration
@pytest.mark.asyncio
async def test_agent_capability_matching():
    """Agent能力マッチングが正しく動作することを確認."""
    from agentflow import agent
    from agentflow.core.agent_registry import get_global_agent_registry
    from agentflow.core.capability_spec import CapabilityRequirement

    # 複数の Agent を定義
    @agent(skills=["rag", "search"])
    class SearchAgent:
        """検索Agent."""
        system_prompt = "検索を行う"

    @agent(skills=["chatbot"])
    class ChatAgent:
        """チャットAgent."""
        system_prompt = "チャット"

    @agent(skills=["analytics", "chart"])
    class AnalyticsAgent:
        """分析Agent."""
        system_prompt = "分析"

    registry = get_global_agent_registry()

    # 検索タスク
    search_req = CapabilityRequirement(
        description="検索",
        required_tags=["search"],
    )
    search_matches = registry.find_matching(search_req)
    assert len(search_matches) > 0
    assert search_matches[0][0] == "SearchAgent"

    # チャットタスク
    chat_req = CapabilityRequirement(
        description="チャット",
        required_tags=["chatbot"],
    )
    chat_matches = registry.find_matching(chat_req)
    assert len(chat_matches) > 0
    assert chat_matches[0][0] == "ChatAgent"

    # 分析タスク
    analytics_req = CapabilityRequirement(
        description="分析",
        required_tags=["analytics"],
    )
    analytics_matches = registry.find_matching(analytics_req)
    assert len(analytics_matches) > 0
    assert analytics_matches[0][0] == "AnalyticsAgent"
