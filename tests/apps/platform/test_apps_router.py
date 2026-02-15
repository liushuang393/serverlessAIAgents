# -*- coding: utf-8 -*-
"""Apps Router エンドポイントのユニットテスト.

テスト対象: apps/platform/routers/apps.py
FastAPI TestClient を使用してエンドポイントを検証する。
"""

from __future__ import annotations

import pytest
from fastapi.testclient import TestClient


class TestListApps:
    """GET /api/apps テスト."""

    def test_returns_app_list(self, test_client: TestClient) -> None:
        """App 一覧を返す."""
        resp = test_client.get("/api/apps")
        assert resp.status_code == 200
        data = resp.json()
        assert "apps" in data
        assert "total" in data
        assert data["total"] == 3

    def test_app_item_structure(self, test_client: TestClient) -> None:
        """各 App アイテムに必要なフィールドがある."""
        resp = test_client.get("/api/apps")
        items = resp.json()["apps"]
        assert len(items) > 0
        item = items[0]
        required_keys = {"name", "display_name", "version", "icon", "status", "ports", "agent_count", "tags"}
        assert required_keys.issubset(item.keys())
        assert "runtime" in item
        assert "urls" in item["runtime"]
        assert "database" in item["runtime"]
        assert "commands" in item["runtime"]


class TestGetSummary:
    """GET /api/apps/summary テスト."""

    def test_returns_summary(self, test_client: TestClient) -> None:
        """概要統計を返す."""
        resp = test_client.get("/api/apps/summary")
        assert resp.status_code == 200
        data = resp.json()
        assert data["total_apps"] == 3
        assert data["total_agents"] == 3


class TestGetAppDetail:
    """GET /api/apps/{app_name} テスト."""

    def test_existing_app(self, test_client: TestClient) -> None:
        """存在する App の詳細を返す."""
        resp = test_client.get("/api/apps/test_app")
        assert resp.status_code == 200
        data = resp.json()
        assert data["name"] == "test_app"
        assert data["display_name"] == "テストアプリ"
        assert len(data["agents"]) == 2
        assert "ports" in data
        assert "entry_points" in data
        assert "dependencies" in data

    def test_nonexistent_app_returns_404(self, test_client: TestClient) -> None:
        """存在しない App は 404 を返す."""
        resp = test_client.get("/api/apps/nonexistent")
        assert resp.status_code == 404
        detail = resp.json()["detail"]
        assert detail["error_code"] == "APP_NOT_FOUND"


class TestCheckAppHealth:
    """GET /api/apps/{app_name}/health テスト."""

    def test_health_check_returns_result(self, test_client: TestClient) -> None:
        """ヘルスチェック結果を返す."""
        resp = test_client.get("/api/apps/test_app/health")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert "status" in data
        assert "checked_at" in data

    def test_health_nonexistent_app_returns_404(self, test_client: TestClient) -> None:
        """存在しない App のヘルスチェックは 404."""
        resp = test_client.get("/api/apps/nonexistent/health")
        assert resp.status_code == 404

    def test_library_app_health_unknown(self, test_client: TestClient) -> None:
        """API ポートなし App のヘルスは unknown."""
        resp = test_client.get("/api/apps/library_app/health")
        assert resp.status_code == 200
        assert resp.json()["status"] == "unknown"


class TestAppActions:
    """POST /api/apps/{app_name}/{action} テスト."""

    def test_publish_returns_action_result(self, test_client: TestClient) -> None:
        """publish 実行結果を返す."""
        resp = test_client.post("/api/apps/test_app/publish")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert data["action"] == "publish"
        assert "success" in data

    def test_start_returns_action_result(self, test_client: TestClient) -> None:
        """start 実行結果を返す."""
        resp = test_client.post("/api/apps/test_app/start")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert data["action"] == "start"
        assert "success" in data

    def test_stop_returns_action_result(self, test_client: TestClient) -> None:
        """stop 実行結果を返す."""
        resp = test_client.post("/api/apps/test_app/stop")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert data["action"] == "stop"
        assert "success" in data

    def test_action_nonexistent_app_returns_404(self, test_client: TestClient) -> None:
        """存在しない App は 404."""
        resp = test_client.post("/api/apps/nonexistent/start")
        assert resp.status_code == 404


class TestRefreshApps:
    """POST /api/apps/refresh テスト."""

    def test_refresh_returns_count(self, test_client: TestClient) -> None:
        """再スキャン結果を返す."""
        resp = test_client.post("/api/apps/refresh")
        assert resp.status_code == 200
        data = resp.json()
        assert "discovered" in data
        assert data["discovered"] == 3
        assert "new" in data
        assert "removed" in data
        assert "unchanged" in data


class TestMigrateManifests:
    """POST /api/apps/migrate-manifests テスト."""

    def test_migrate_dry_run(self, test_client: TestClient) -> None:
        resp = test_client.post("/api/apps/migrate-manifests", json={"dry_run": True})
        assert resp.status_code == 200
        data = resp.json()
        assert data["dry_run"] is True
        assert "total" in data
        assert "changed" in data
        assert "apps" in data

    def test_migrate_apply(self, test_client: TestClient) -> None:
        resp = test_client.post("/api/apps/migrate-manifests", json={"dry_run": False})
        assert resp.status_code == 200
        data = resp.json()
        assert data["dry_run"] is False
        assert data["total"] == 3


class TestRootAndHealth:
    """ルート・ヘルスエンドポイントテスト."""

    def test_root_endpoint(self, test_client: TestClient) -> None:
        """ルートが Platform 情報を返す."""
        resp = test_client.get("/")
        assert resp.status_code == 200
        data = resp.json()
        assert data["name"] == "AgentFlow Platform"
        assert "apps_api" in data

    def test_health_endpoint(self, test_client: TestClient) -> None:
        """ヘルスチェックが healthy を返す."""
        resp = test_client.get("/health")
        assert resp.status_code == 200
        assert resp.json()["status"] == "healthy"
