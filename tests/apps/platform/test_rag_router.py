"""RAG Router エンドポイントのユニットテスト.

テスト対象: apps/platform/routers/rag.py
phase3_test_client を使用してエンドポイントを検証する。
"""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from fastapi.testclient import TestClient


class TestGetRagOverview:
    """GET /api/studios/framework/rag/overview テスト."""

    def test_returns_overview(self, phase3_test_client: TestClient) -> None:
        """RAG 概要を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/overview")
        assert resp.status_code == 200
        data = resp.json()
        assert "description" in data
        assert "chunk_strategies" in data
        assert "rerankers" in data
        assert "database_types" in data
        assert "vector_providers" in data
        assert "apps_using_rag" in data
        assert "stats" in data

    def test_overview_stats_structure(self, phase3_test_client: TestClient) -> None:
        """概要内の stats に必要なキーが含まれる."""
        resp = phase3_test_client.get("/api/studios/framework/rag/overview")
        stats = resp.json()["stats"]
        assert stats["total_strategies"] == 5
        assert stats["total_rerankers"] == 4
        assert stats["total_apps_using_rag"] >= 1


class TestListStrategies:
    """GET /api/studios/framework/rag/strategies テスト."""

    def test_returns_strategies(self, phase3_test_client: TestClient) -> None:
        """チャンキング戦略一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/strategies")
        assert resp.status_code == 200
        data = resp.json()
        assert "strategies" in data
        assert "total" in data
        assert data["total"] == 5

    def test_strategy_structure(self, phase3_test_client: TestClient) -> None:
        """各戦略に name, label, description がある."""
        resp = phase3_test_client.get("/api/studios/framework/rag/strategies")
        strategies = resp.json()["strategies"]
        for s in strategies:
            assert "name" in s
            assert "label" in s
            assert "description" in s


class TestListRerankers:
    """GET /api/studios/framework/rag/rerankers テスト."""

    def test_returns_rerankers(self, phase3_test_client: TestClient) -> None:
        """リランカー一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/rerankers")
        assert resp.status_code == 200
        data = resp.json()
        assert "rerankers" in data
        assert "total" in data
        assert data["total"] == 4

    def test_reranker_structure(self, phase3_test_client: TestClient) -> None:
        """各リランカーに name, label, description がある."""
        resp = phase3_test_client.get("/api/studios/framework/rag/rerankers")
        rerankers = resp.json()["rerankers"]
        for r in rerankers:
            assert "name" in r
            assert "label" in r
            assert "description" in r


class TestRetrievalMethods:
    """GET /api/studios/framework/rag/retrieval-methods テスト."""

    def test_returns_methods(self, phase3_test_client: TestClient) -> None:
        """検索方式一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/retrieval-methods")
        assert resp.status_code == 200
        data = resp.json()
        assert "methods" in data
        assert "total" in data
        assert data["total"] >= 3


class TestPatterns:
    """GET /api/studios/framework/rag/patterns テスト."""

    def test_returns_patterns(self, phase3_test_client: TestClient) -> None:
        """RAG パターン一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/patterns")
        assert resp.status_code == 200
        data = resp.json()
        assert "patterns" in data
        assert "total" in data
        assert data["total"] >= 3


class TestListRagApps:
    """GET /api/studios/framework/rag/apps テスト."""

    def test_returns_rag_apps(self, phase3_test_client: TestClient) -> None:
        """RAG 使用 App 一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/apps")
        assert resp.status_code == 200
        data = resp.json()
        assert "apps" in data
        assert "total" in data
        assert data["total"] >= 1

    def test_rag_app_structure(self, phase3_test_client: TestClient) -> None:
        """各 App に必要なフィールドがある."""
        resp = phase3_test_client.get("/api/studios/framework/rag/apps")
        apps = resp.json()["apps"]
        for app in apps:
            assert "app_name" in app
            assert "display_name" in app
            assert "icon" in app
            assert "rag_details" in app
            assert isinstance(app["rag_details"], list)

    def test_rag_app_contains_expected(self, phase3_test_client: TestClient) -> None:
        """rag_app が含まれる."""
        resp = phase3_test_client.get("/api/studios/framework/rag/apps")
        app_names = {a["app_name"] for a in resp.json()["apps"]}
        assert "rag_app" in app_names


class TestRagAppConfigs:
    """GET/PATCH /api/studios/framework/rag/apps/{app}/config テスト."""

    def test_list_configs(self, phase3_test_client: TestClient) -> None:
        """全 App の RAG 設定一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/apps/configs")
        assert resp.status_code == 200
        data = resp.json()
        assert "apps" in data
        assert "total" in data
        assert data["total"] >= 1

    def test_get_single_config(self, phase3_test_client: TestClient) -> None:
        """単一 App の RAG 設定を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/apps/rag_app/config")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "rag_app"
        assert "rag" in data
        assert "db_hint" in data
        assert "chunk_strategy" in data["rag"]
        assert data["db_hint"]["available"] is True

    def test_patch_config_updates_rag_setting(self, phase3_test_client: TestClient) -> None:
        """PATCH で RAG 設定が更新されること."""
        resp = phase3_test_client.patch(
            "/api/studios/framework/rag/apps/rag_app/config",
            json={"enabled": True, "vector_provider": "qdrant"},
        )
        assert resp.status_code == 200
        data = resp.json()
        assert "rag" in data
        assert data["rag"]["vector_provider"] == "qdrant"

    def test_patch_pattern_autosets_params(self, phase3_test_client: TestClient) -> None:
        """PATCH でパターン選択時に関連パラメータが自動セットされること."""
        resp = phase3_test_client.patch(
            "/api/studios/framework/rag/apps/rag_app/config",
            json={"pattern": "faq_precision"},
        )
        assert resp.status_code == 200
        data = resp.json()
        assert data["rag"]["chunk_strategy"] == "sentence"
        assert data["rag"]["chunk_size"] == 500
        assert data["rag"]["top_k"] == 8

    def test_patch_nonexistent_app_returns_404(self, phase3_test_client: TestClient) -> None:
        """存在しない App への PATCH は 404 を返すこと."""
        resp = phase3_test_client.patch(
            "/api/studios/framework/rag/apps/does_not_exist/config",
            json={"enabled": True},
        )
        assert resp.status_code == 404
        assert resp.json()["detail"]["error_code"] == "APP_NOT_FOUND"


class TestSetupQdrant:
    """POST /api/studios/framework/rag/setup/qdrant テスト."""

    def test_setup_qdrant_returns_result_structure(self, phase3_test_client: TestClient) -> None:
        """セットアップ結果に必要なフィールドが含まれること（Docker 未起動でも 500 にならない）."""
        resp = phase3_test_client.post("/api/studios/framework/rag/setup/qdrant")
        # Docker 不在でも 200 か 404（スクリプト不在）のいずれか - 500 は不可
        assert resp.status_code in (200, 404)
        data = resp.json()
        if resp.status_code == 200:
            assert "success" in data
            assert "message" in data
            assert "output" in data


class TestGetRagStats:
    """GET /api/studios/framework/rag/stats テスト."""

    def test_returns_stats(self, phase3_test_client: TestClient) -> None:
        """RAG 統計を返す."""
        resp = phase3_test_client.get("/api/studios/framework/rag/stats")
        assert resp.status_code == 200
        data = resp.json()
        assert "total_strategies" in data
        assert "total_rerankers" in data
        assert "total_apps_using_rag" in data
        assert data["total_strategies"] == 5
        assert data["total_rerankers"] == 4
