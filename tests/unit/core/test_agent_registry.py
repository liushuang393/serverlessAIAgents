"""AgentRegistryインターフェースのテスト.

Agent登録・発見・インスタンス化のユニットテスト。
"""

import pytest


@pytest.fixture
def agent_registry():
    """新しいAgentRegistryインスタンスを作成."""
    from agentflow.core.agent_registry import AgentRegistry

    return AgentRegistry()


@pytest.fixture
def sample_capability():
    """サンプルAgentCapabilitySpecを作成."""
    from agentflow.core.capability_spec import AgentCapabilitySpec

    return AgentCapabilitySpec(
        id="test_agent_v1",
        name="Test Agent",
        description="テスト用Agent",
        tags=["test", "demo"],
        required_tools=["tool://builtin/echo"],
    )


class TestAgentRegistry:
    """AgentRegistryのテスト."""

    def test_register_agent(self, agent_registry, sample_capability):
        """Agentと能力を登録するテスト."""
        agent_registry.register(
            agent_id="test_agent",
            capability=sample_capability,
            factory=lambda: "mock_agent_instance",
        )

        assert "test_agent" in agent_registry

    def test_get_agent_factory(self, agent_registry, sample_capability):
        """Agentファクトリ取得のテスト."""

        def factory():
            return "instance"

        agent_registry.register("test", sample_capability, factory)

        retrieved_factory = agent_registry.get_factory("test")

        assert retrieved_factory() == "instance"

    def test_get_capability(self, agent_registry, sample_capability):
        """Agent能力取得のテスト."""
        agent_registry.register("test", sample_capability, lambda: None)

        cap = agent_registry.get_capability("test")

        assert cap.name == "Test Agent"

    def test_find_by_tags(self, agent_registry):
        """タグでAgent検索のテスト."""
        from agentflow.core.capability_spec import AgentCapabilitySpec

        cap1 = AgentCapabilitySpec(id="a1", name="A1", description="", tags=["pdf", "analysis"])
        cap2 = AgentCapabilitySpec(id="a2", name="A2", description="", tags=["text", "analysis"])

        agent_registry.register("a1", cap1, lambda: None)
        agent_registry.register("a2", cap2, lambda: None)

        pdf_agents = agent_registry.find_by_tags(["pdf"])
        analysis_agents = agent_registry.find_by_tags(["analysis"])

        assert len(pdf_agents) == 1
        assert len(analysis_agents) == 2

    def test_find_matching(self, agent_registry):
        """要件マッチングでAgent検索のテスト."""
        from agentflow.core.capability_spec import AgentCapabilitySpec, CapabilityRequirement

        cap1 = AgentCapabilitySpec(
            id="pdf_analyzer",
            name="PDF Analyzer",
            description="PDF文書を分析",
            tags=["pdf"],
        )
        cap2 = AgentCapabilitySpec(
            id="text_summarizer",
            name="Text Summarizer",
            description="テキストを要約",
            tags=["text"],
        )

        agent_registry.register("pdf", cap1, lambda: None)
        agent_registry.register("text", cap2, lambda: None)

        req = CapabilityRequirement(
            description="PDF文書を分析",
            required_tags=["pdf"],
        )

        matches = agent_registry.find_matching(req)

        assert len(matches) > 0
        assert matches[0][0] == "pdf"  # (agent_id, score)

    def test_list_all(self, agent_registry, sample_capability):
        """全Agent一覧のテスト."""
        agent_registry.register("a1", sample_capability, lambda: None)
        agent_registry.register("a2", sample_capability, lambda: None)

        all_agents = agent_registry.list_all()

        assert len(all_agents) == 2

    def test_unregister(self, agent_registry, sample_capability):
        """Agent登録解除のテスト."""
        agent_registry.register("test", sample_capability, lambda: None)
        assert "test" in agent_registry

        result = agent_registry.unregister("test")

        assert result is True
        assert "test" not in agent_registry

    def test_get_factory_nonexistent_returns_none(self, agent_registry):
        """存在しないAgentファクトリ取得でNoneを返すテスト."""
        result = agent_registry.get_factory("nonexistent")
        assert result is None

    def test_len(self, agent_registry, sample_capability):
        """長さ取得のテスト."""
        assert len(agent_registry) == 0

        agent_registry.register("a1", sample_capability, lambda: None)

        assert len(agent_registry) == 1


class TestGlobalAgentRegistry:
    """グローバルAgentRegistryのテスト."""

    def test_get_global_registry(self):
        """グローバルレジストリ取得のテスト."""
        from agentflow.core.agent_registry import get_global_agent_registry

        registry = get_global_agent_registry()

        assert registry is not None

    def test_global_registry_singleton(self):
        """グローバルレジストリがシングルトンかテスト."""
        from agentflow.core.agent_registry import get_global_agent_registry

        registry1 = get_global_agent_registry()
        registry2 = get_global_agent_registry()

        assert registry1 is registry2
