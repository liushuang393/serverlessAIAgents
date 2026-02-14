# -*- coding: utf-8 -*-
"""AgentAggregatorService のユニットテスト.

テスト対象: apps/platform/services/agent_aggregator.py
"""

from __future__ import annotations

from apps.platform.services.agent_aggregator import (
    AggregatedAgent,
    AgentAggregatorService,
)


class TestAggregatedAgent:
    """AggregatedAgent データクラスのテスト."""

    def test_to_dict(self) -> None:
        """to_dict() が全フィールドを含む辞書を返す."""
        agent = AggregatedAgent(
            name="TestAgent",
            app_name="test_app",
            app_display_name="テストアプリ",
            app_icon="🧪",
            module="apps.test_app.agents.test",
            capabilities=["test", "helper"],
        )
        d = agent.to_dict()
        assert d["name"] == "TestAgent"
        assert d["app_name"] == "test_app"
        assert d["app_display_name"] == "テストアプリ"
        assert d["app_icon"] == "🧪"
        assert d["module"] == "apps.test_app.agents.test"
        assert d["capabilities"] == ["test", "helper"]

    def test_to_dict_none_module(self) -> None:
        """module が None でも辞書に含まれる."""
        agent = AggregatedAgent(
            name="A", app_name="x", app_display_name="X",
            app_icon="📦", module=None, capabilities=[],
        )
        assert agent.to_dict()["module"] is None


class TestListAll:
    """list_all() メソッドのテスト."""

    def test_returns_all_agents(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """全 App の Agent を返す（test_app: 2 + library_app: 1 = 3）."""
        agents = aggregator.list_all()
        assert len(agents) == 3
        names = {a.name for a in agents}
        assert "TestAgent" in names
        assert "HelperAgent" in names
        assert "LibAgent" in names

    def test_agent_has_app_context(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """各 Agent に所属 App 情報が付与されている."""
        agents = aggregator.list_all()
        test_agent = next(a for a in agents if a.name == "TestAgent")
        assert test_agent.app_name == "test_app"
        assert test_agent.app_display_name == "テストアプリ"
        assert test_agent.app_icon == "🧪"

    def test_with_rag_app(
        self, aggregator_with_rag: AgentAggregatorService,
    ) -> None:
        """RAG App を含む場合、全 Agent を返す（test: 2 + rag: 2 = 4）."""
        agents = aggregator_with_rag.list_all()
        assert len(agents) == 4
        names = {a.name for a in agents}
        assert "RAGAgent" in names
        assert "IndexAgent" in names


class TestSearchByCapability:
    """search_by_capability() メソッドのテスト."""

    def test_exact_match(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """完全一致する能力タグで検索."""
        results = aggregator.search_by_capability("test")
        assert len(results) == 1
        assert results[0].name == "TestAgent"

    def test_partial_match(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """部分一致で検索."""
        results = aggregator.search_by_capability("help")
        assert len(results) == 1
        assert results[0].name == "HelperAgent"

    def test_case_insensitive(
        self, aggregator_with_rag: AgentAggregatorService,
    ) -> None:
        """大文字小文字を区別しない検索."""
        results = aggregator_with_rag.search_by_capability("RAG")
        assert len(results) == 1
        assert results[0].name == "RAGAgent"

    def test_no_match(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """マッチしない場合は空リスト."""
        results = aggregator.search_by_capability("nonexistent")
        assert results == []


class TestGroupByApp:
    """group_by_app() メソッドのテスト."""

    def test_groups_by_app_name(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """App 名でグルーピングされる."""
        groups = aggregator.group_by_app()
        assert "test_app" in groups
        assert "library_app" in groups
        assert len(groups["test_app"]) == 2
        assert len(groups["library_app"]) == 1

    def test_group_values_are_dicts(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """グループ内の値は辞書形式."""
        groups = aggregator.group_by_app()
        agent_dict = groups["test_app"][0]
        assert isinstance(agent_dict, dict)
        assert "name" in agent_dict
        assert "capabilities" in agent_dict


class TestAllCapabilities:
    """all_capabilities() メソッドのテスト."""

    def test_returns_tag_count_apps(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """tag, count, apps フィールドを含む."""
        caps = aggregator.all_capabilities()
        assert len(caps) > 0
        first = caps[0]
        assert "tag" in first
        assert "count" in first


    def test_unique_capabilities(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """各タグが一意に集約される."""
        caps = aggregator.all_capabilities()
        tags = [c["tag"] for c in caps]
        assert len(tags) == len(set(tags))


class TestStats:
    """stats() メソッドのテスト."""

    def test_returns_expected_keys(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """統計辞書に必要なキーが含まれる."""
        s = aggregator.stats()
        assert "total_agents" in s
        assert "total_apps_with_agents" in s
        assert "total_capabilities" in s

    def test_correct_counts(
        self, aggregator: AgentAggregatorService,
    ) -> None:
        """正しいカウントを返す."""
        s = aggregator.stats()
        # test_app(2) + library_app(1) = 3 agents
        assert s["total_agents"] == 3
        # test_app + library_app = 2 apps with agents
        assert s["total_apps_with_agents"] == 2
        # test, helper, util, lib = 4 unique capabilities
        assert s["total_capabilities"] == 4

    def test_with_rag_app(
        self, aggregator_with_rag: AgentAggregatorService,
    ) -> None:
        """RAG App を含む場合の統計."""
        s = aggregator_with_rag.stats()
        # test_app(2) + rag_app(2) = 4 agents
        assert s["total_agents"] == 4
        assert s["total_apps_with_agents"] == 2

