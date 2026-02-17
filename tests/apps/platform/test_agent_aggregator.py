# -*- coding: utf-8 -*-
"""AgentAggregatorService のユニットテスト."""

from __future__ import annotations

from apps.platform.services.agent_aggregator import (
    AggregatedAgent,
    AgentAggregatorService,
)


class TestAggregatedAgent:
    """AggregatedAgent データクラスのテスト."""

    def test_to_dict(self) -> None:
        """to_dict() が canonical / legacy を含む."""
        agent = AggregatedAgent(
            name="TestAgent",
            app_name="test_app",
            app_display_name="テストアプリ",
            app_icon="🧪",
            module="apps.test_app.agents.test",
            capabilities=[
                {
                    "id": "knowledge.retrieval.rag",
                    "domain": "knowledge",
                    "task": "retrieval",
                    "qualifier": "rag",
                    "label": "Retrieval Rag",
                    "aliases": ["rag"],
                },
            ],
            capabilities_legacy=["rag"],
            business_base="knowledge",
            agent_pattern="specialist",
            app_business_base="knowledge",
            app_engine_pattern="simple",
        )
        payload = agent.to_dict()
        assert payload["name"] == "TestAgent"
        assert payload["capabilities"][0]["id"] == "knowledge.retrieval.rag"
        assert payload["capabilities_legacy"] == ["rag"]
        assert payload["business_base"] == "knowledge"
        assert payload["agent_pattern"] == "specialist"


class TestListAll:
    """list_all() のテスト."""

    def test_returns_all_agents(self, aggregator: AgentAggregatorService) -> None:
        agents = aggregator.list_all()
        assert len(agents) == 3
        assert {a.name for a in agents} == {"TestAgent", "HelperAgent", "LibAgent"}

    def test_capabilities_are_canonical_objects(
        self,
        aggregator_with_rag: AgentAggregatorService,
    ) -> None:
        agents = aggregator_with_rag.list_all()
        rag_agent = next(a for a in agents if a.name == "RAGAgent")
        assert len(rag_agent.capabilities) >= 1
        assert "id" in rag_agent.capabilities[0]
        assert rag_agent.capabilities_legacy == ["rag", "search"]


class TestSearchByCapability:
    """search_by_capability() のテスト."""

    def test_search_by_legacy(self, aggregator: AgentAggregatorService) -> None:
        results = aggregator.search_by_capability("helper")
        assert len(results) == 1
        assert results[0].name == "HelperAgent"

    def test_search_by_canonical_id(self, aggregator_with_rag: AgentAggregatorService) -> None:
        results = aggregator_with_rag.search_by_capability("knowledge.retrieval.rag")
        assert len(results) == 1
        assert results[0].name == "RAGAgent"

    def test_search_by_label(self, aggregator_with_rag: AgentAggregatorService) -> None:
        results = aggregator_with_rag.search_by_capability("Retrieval Rag")
        assert len(results) == 1
        assert results[0].name == "RAGAgent"

    def test_no_match(self, aggregator: AgentAggregatorService) -> None:
        assert aggregator.search_by_capability("nonexistent") == []


class TestGroupByApp:
    """group_by_app() のテスト."""

    def test_returns_array_groups(self, aggregator: AgentAggregatorService) -> None:
        groups = aggregator.group_by_app()
        assert isinstance(groups, list)
        assert len(groups) == 2
        app_names = {item["app_name"] for item in groups}
        assert app_names == {"library_app", "test_app"}

    def test_group_shape(self, aggregator: AgentAggregatorService) -> None:
        groups = aggregator.group_by_app()
        first = groups[0]
        assert {"app_name", "display_name", "icon", "agents"} <= set(first.keys())
        assert isinstance(first["agents"], list)
        if first["agents"]:
            assert "business_base" in first["agents"][0]
            assert "agent_pattern" in first["agents"][0]


class TestAllCapabilities:
    """all_capabilities() のテスト."""

    def test_returns_canonical_aggregate(
        self,
        aggregator_with_rag: AgentAggregatorService,
    ) -> None:
        caps = aggregator_with_rag.all_capabilities()
        assert len(caps) > 0
        first = caps[0]
        required = {"id", "domain", "label", "count", "apps", "aliases"}
        assert required.issubset(first.keys())

    def test_contains_rag_capability(self, aggregator_with_rag: AgentAggregatorService) -> None:
        caps = aggregator_with_rag.all_capabilities()
        ids = {item["id"] for item in caps}
        assert "knowledge.retrieval.rag" in ids


class TestStats:
    """stats() のテスト."""

    def test_returns_expected_keys(self, aggregator: AgentAggregatorService) -> None:
        stats = aggregator.stats()
        assert {"total_agents", "total_apps_with_agents", "total_capabilities"} <= set(stats.keys())
        assert "by_business_base" in stats
        assert "by_agent_pattern" in stats

    def test_with_rag(self, aggregator_with_rag: AgentAggregatorService) -> None:
        stats = aggregator_with_rag.stats()
        assert stats["total_agents"] == 4
        assert stats["total_apps_with_agents"] == 2
        assert stats["total_capabilities"] >= 3
