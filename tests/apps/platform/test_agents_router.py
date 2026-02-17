# -*- coding: utf-8 -*-
"""Agents Router エンドポイントのユニットテスト.

テスト対象: apps/platform/routers/agents.py
phase3_test_client を使用してエンドポイントを検証する。
"""

from __future__ import annotations

from fastapi.testclient import TestClient


class TestListAgents:
    """GET /api/studios/framework/agents テスト."""

    def test_returns_agent_list(self, phase3_test_client: TestClient) -> None:
        """Agent 一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/agents")
        assert resp.status_code == 200
        data = resp.json()
        assert "agents" in data
        assert "total" in data
        # test_app(2) + rag_app(2) = 4
        assert data["total"] == 4

    def test_agent_item_structure(self, phase3_test_client: TestClient) -> None:
        """各 Agent アイテムに必要なフィールドがある."""
        resp = phase3_test_client.get("/api/studios/framework/agents")
        agents = resp.json()["agents"]
        assert len(agents) > 0
        agent = agents[0]
        required_keys = {
            "name",
            "app_name",
            "app_display_name",
            "app_icon",
            "module",
            "capabilities",
            "capabilities_legacy",
            "business_base",
            "agent_pattern",
            "app_business_base",
            "app_engine_pattern",
        }
        assert required_keys.issubset(agent.keys())
        if agent["capabilities"]:
            assert "id" in agent["capabilities"][0]


class TestGetAgentStats:
    """GET /api/studios/framework/agents/stats テスト."""

    def test_returns_stats(self, phase3_test_client: TestClient) -> None:
        """Agent 統計を返す."""
        resp = phase3_test_client.get("/api/studios/framework/agents/stats")
        assert resp.status_code == 200
        data = resp.json()
        assert "total_agents" in data
        assert "total_apps_with_agents" in data
        assert "total_capabilities" in data
        assert data["total_agents"] == 4


class TestListCapabilities:
    """GET /api/studios/framework/agents/capabilities テスト."""

    def test_returns_capabilities(self, phase3_test_client: TestClient) -> None:
        """能力タグ一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/agents/capabilities")
        assert resp.status_code == 200
        data = resp.json()
        assert "capabilities" in data
        assert "total" in data
        assert data["total"] > 0

    def test_capability_structure(self, phase3_test_client: TestClient) -> None:
        """各能力タグに canonical フィールドがある."""
        resp = phase3_test_client.get("/api/studios/framework/agents/capabilities")
        caps = resp.json()["capabilities"]
        assert len(caps) > 0
        cap = caps[0]
        assert "id" in cap
        assert "domain" in cap
        assert "label" in cap
        assert "count" in cap
        assert "apps" in cap
        assert "aliases" in cap


class TestAgentsByApp:
    """GET /api/studios/framework/agents/by-app テスト."""

    def test_returns_groups(self, phase3_test_client: TestClient) -> None:
        """App 別グループを返す."""
        resp = phase3_test_client.get("/api/studios/framework/agents/by-app")
        assert resp.status_code == 200
        data = resp.json()
        assert "groups" in data
        assert "total_apps" in data
        assert data["total_apps"] == 2

    def test_group_contains_agents(self, phase3_test_client: TestClient) -> None:
        """各グループに Agent リストが含まれる."""
        resp = phase3_test_client.get("/api/studios/framework/agents/by-app")
        groups = resp.json()["groups"]
        assert isinstance(groups, list)
        assert len(groups) == 2
        by_app = {g["app_name"]: g for g in groups}
        assert "test_app" in by_app
        assert "rag_app" in by_app
        assert len(by_app["test_app"]["agents"]) == 2
        assert len(by_app["rag_app"]["agents"]) == 2


class TestAgentsByBusinessBase:
    """GET /api/studios/framework/agents/by-business-base テスト."""

    def test_returns_groups(self, phase3_test_client: TestClient) -> None:
        resp = phase3_test_client.get("/api/studios/framework/agents/by-business-base")
        assert resp.status_code == 200
        data = resp.json()
        assert "groups" in data
        assert "total_groups" in data
        assert data["total_groups"] >= 1


class TestAgentsByPattern:
    """GET /api/studios/framework/agents/by-pattern テスト."""

    def test_returns_groups(self, phase3_test_client: TestClient) -> None:
        resp = phase3_test_client.get("/api/studios/framework/agents/by-pattern")
        assert resp.status_code == 200
        data = resp.json()
        assert "groups" in data
        assert "total_groups" in data
        assert data["total_groups"] >= 1


class TestSearchAgents:
    """GET /api/studios/framework/agents/search テスト."""

    def test_search_found(self, phase3_test_client: TestClient) -> None:
        """能力タグ検索でマッチする Agent を返す."""
        resp = phase3_test_client.get("/api/studios/framework/agents/search", params={"capability": "rag"})
        assert resp.status_code == 200
        data = resp.json()
        assert data["total"] >= 1
        assert data["query"] == "rag"
        names = {a["name"] for a in data["agents"]}
        assert "RAGAgent" in names

    def test_search_no_match(self, phase3_test_client: TestClient) -> None:
        """マッチしない検索は空リストを返す."""
        resp = phase3_test_client.get(
            "/api/studios/framework/agents/search", params={"capability": "nonexistent_xyz"}
        )
        assert resp.status_code == 200
        assert resp.json()["total"] == 0
        assert resp.json()["agents"] == []

    def test_search_missing_param_returns_422(self, phase3_test_client: TestClient) -> None:
        """必須パラメータなしは 422 を返す."""
        resp = phase3_test_client.get("/api/studios/framework/agents/search")
        assert resp.status_code == 422
