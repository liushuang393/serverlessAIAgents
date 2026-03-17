"""Apps Router エンドポイントのユニットテスト.

テスト対象: control_plane/routers/apps.py
FastAPI TestClient を使用してエンドポイントを検証する。
"""

from __future__ import annotations

import asyncio
import time
from typing import TYPE_CHECKING

from control_plane.services.app_lifecycle import AppStatus, HealthCheckResult


if TYPE_CHECKING:
    from fastapi.testclient import TestClient


class TestListApps:
    """GET /api/studios/framework/apps テスト."""

    def test_returns_app_list(self, test_client: TestClient) -> None:
        """App 一覧を返す."""
        resp = test_client.get("/api/studios/framework/apps?wait_for_health=true")
        assert resp.status_code == 200
        data = resp.json()
        assert "apps" in data
        assert "total" in data
        assert data["total"] == 3

    def test_app_item_structure(self, test_client: TestClient) -> None:
        """各 App アイテムに必要なフィールドがある."""
        resp = test_client.get("/api/studios/framework/apps?wait_for_health=true")
        items = resp.json()["apps"]
        assert len(items) > 0
        item = items[0]
        required_keys = {
            "name",
            "display_name",
            "version",
            "icon",
            "status",
            "ports",
            "agent_count",
            "tags",
            "business_base",
        }
        assert required_keys.issubset(item.keys())
        assert "runtime" in item
        assert "urls" in item["runtime"]
        assert "database" in item["runtime"]
        assert "commands" in item["runtime"]

    def test_business_surface_hides_internal_fields(self, test_client: TestClient) -> None:
        """business surface では内部設定を返さない."""
        resp = test_client.get("/api/studios/framework/apps?surface_profile=business&wait_for_health=true")
        assert resp.status_code == 200
        item = resp.json()["apps"][0]
        assert "contracts" not in item
        assert "blueprint" not in item
        assert "visibility" not in item
        assert "config_path" not in item
        assert "runtime" in item
        assert set(item["runtime"].keys()) == {"urls"}

    def test_default_non_blocking_starts_unknown(self, test_client: TestClient) -> None:
        """デフォルトは non-blocking のため初回は unknown を返せる."""
        resp = test_client.get("/api/studios/framework/apps")
        assert resp.status_code == 200
        statuses = {item["status"] for item in resp.json()["apps"]}
        assert statuses == {"unknown"}
        # 背景タスクの取りこぼしを防ぐため、同一 client で drain する
        drain = test_client.get("/api/studios/framework/apps?wait_for_health=true")
        assert drain.status_code == 200

    def test_wait_for_health_true_blocks(self, test_client: TestClient) -> None:
        """wait_for_health=true はヘルス完了待機を維持する."""
        from control_plane.routers import apps as apps_router

        lifecycle = apps_router._get_lifecycle()
        original_check_health = lifecycle.check_health

        async def _slow_check_health(*args, **kwargs):
            await asyncio.sleep(0.2)
            config = args[0]
            return HealthCheckResult(app_name=config.name, status=AppStatus.HEALTHY)

        lifecycle.check_health = _slow_check_health  # type: ignore[method-assign]
        try:
            started = time.perf_counter()
            resp = test_client.get("/api/studios/framework/apps?wait_for_health=true")
            elapsed = time.perf_counter() - started
        finally:
            lifecycle.check_health = original_check_health  # type: ignore[method-assign]

        assert resp.status_code == 200
        assert elapsed >= 0.18


class TestGetSummary:
    """GET /api/studios/framework/apps/summary テスト."""

    def test_returns_summary(self, test_client: TestClient) -> None:
        """概要統計を返す."""
        resp = test_client.get("/api/studios/framework/apps/summary")
        assert resp.status_code == 200
        data = resp.json()
        assert data["total_apps"] == 3
        assert data["total_agents"] == 3


class TestGetAppDetail:
    """GET /api/studios/framework/apps/{app_name} テスト."""

    def test_existing_app(self, test_client: TestClient) -> None:
        """存在する App の詳細を返す."""
        resp = test_client.get("/api/studios/framework/apps/test_app?wait_for_health=true")
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
        resp = test_client.get("/api/studios/framework/apps/nonexistent")
        assert resp.status_code == 404
        detail = resp.json()["detail"]
        assert detail["error_code"] == "APP_NOT_FOUND"

    def test_business_surface_hides_internal_fields(self, test_client: TestClient) -> None:
        """business surface では底層フィールドを返さない."""
        resp = test_client.get("/api/studios/framework/apps/test_app?surface_profile=business&wait_for_health=true")
        assert resp.status_code == 200
        data = resp.json()
        assert "entry_points" not in data
        assert "contracts" not in data
        assert "blueprint" not in data
        assert "visibility" not in data
        assert "config_path" not in data
        assert data["run_flow"] == ["template", "data_permissions", "run", "artifacts"]

    def test_default_non_blocking_returns_immediately(self, test_client: TestClient) -> None:
        """詳細 API はデフォルトで non-blocking."""
        started = time.perf_counter()
        resp = test_client.get("/api/studios/framework/apps/test_app")
        elapsed = time.perf_counter() - started
        assert resp.status_code == 200
        assert elapsed < 0.15
        # 背景タスクの取りこぼしを防ぐため、同一 client で drain する
        drain = test_client.get("/api/studios/framework/apps/test_app?wait_for_health=true")
        assert drain.status_code == 200

    def test_wait_for_health_true_blocks(self, test_client: TestClient) -> None:
        """詳細 API でも wait_for_health=true は待機する."""
        from control_plane.routers import apps as apps_router

        lifecycle = apps_router._get_lifecycle()
        original_check_health = lifecycle.check_health

        async def _slow_check_health(*args, **kwargs):
            await asyncio.sleep(0.2)
            config = args[0]
            return HealthCheckResult(app_name=config.name, status=AppStatus.HEALTHY)

        lifecycle.check_health = _slow_check_health  # type: ignore[method-assign]
        try:
            started = time.perf_counter()
            resp = test_client.get("/api/studios/framework/apps/test_app?wait_for_health=true")
            elapsed = time.perf_counter() - started
        finally:
            lifecycle.check_health = original_check_health  # type: ignore[method-assign]

        assert resp.status_code == 200
        assert elapsed >= 0.18


class TestCheckAppHealth:
    """GET /api/studios/framework/apps/{app_name}/health テスト."""

    def test_health_check_returns_result(self, test_client: TestClient) -> None:
        """ヘルスチェック結果を返す."""
        resp = test_client.get("/api/studios/framework/apps/test_app/health")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert "status" in data
        assert "checked_at" in data

    def test_health_nonexistent_app_returns_404(self, test_client: TestClient) -> None:
        """存在しない App のヘルスチェックは 404."""
        resp = test_client.get("/api/studios/framework/apps/nonexistent/health")
        assert resp.status_code == 404

    def test_library_app_health_unknown(self, test_client: TestClient) -> None:
        """API ポートなし App のヘルスは unknown."""
        resp = test_client.get("/api/studios/framework/apps/library_app/health")
        assert resp.status_code == 200
        assert resp.json()["status"] == "unknown"


class TestAppActions:
    """POST /api/studios/framework/apps/{app_name}/{action} テスト."""

    def test_publish_returns_action_result(self, test_client: TestClient) -> None:
        """publish 実行結果を返す."""
        resp = test_client.post("/api/studios/framework/apps/test_app/publish")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert data["action"] == "publish"
        assert "success" in data

    def test_start_returns_action_result(self, test_client: TestClient) -> None:
        """start 実行結果を返す."""
        resp = test_client.post("/api/studios/framework/apps/test_app/start")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert data["action"] == "start"
        assert "success" in data
        assert "command_source" in data

    def test_stop_returns_action_result(self, test_client: TestClient) -> None:
        """stop 実行結果を返す."""
        resp = test_client.post("/api/studios/framework/apps/test_app/stop")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert data["action"] == "stop"
        assert "success" in data

    def test_restart_returns_action_result(self, test_client: TestClient) -> None:
        """restart 実行結果を返す."""
        resp = test_client.post("/api/studios/framework/apps/test_app/restart")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert data["action"] == "restart"
        assert data["success"] is True
        assert "stop" in data
        assert "start" in data

    def test_action_nonexistent_app_returns_404(self, test_client: TestClient) -> None:
        """存在しない App は 404."""
        resp = test_client.post("/api/studios/framework/apps/nonexistent/start")
        assert resp.status_code == 404


class TestRefreshApps:
    """POST /api/studios/framework/apps/refresh テスト."""

    def test_refresh_returns_count(self, test_client: TestClient) -> None:
        """再スキャン結果を返す."""
        resp = test_client.post("/api/studios/framework/apps/refresh")
        assert resp.status_code == 200
        data = resp.json()
        assert "discovered" in data
        assert data["discovered"] == 3
        assert "new" in data
        assert "removed" in data
        assert "unchanged" in data


class TestCLIEndpoints:
    """CLI status/setup エンドポイント."""

    def test_cli_status_endpoint(self, test_client: TestClient) -> None:
        resp = test_client.get("/api/studios/framework/apps/test_app/cli/status")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert "status" in data
        assert "tools" in data["status"]

    def test_cli_setup_endpoint(self, test_client: TestClient) -> None:
        resp = test_client.post("/api/studios/framework/apps/test_app/cli/setup")
        assert resp.status_code == 200
        data = resp.json()
        assert data["app_name"] == "test_app"
        assert "setup" in data
        assert "ready" in data["setup"]


class TestMigrateManifests:
    """POST /api/studios/framework/apps/migrate-manifests テスト."""

    def test_migrate_dry_run(self, test_client: TestClient) -> None:
        resp = test_client.post("/api/studios/framework/apps/migrate-manifests", json={"dry_run": True})
        assert resp.status_code == 200
        data = resp.json()
        assert data["dry_run"] is True
        assert "total" in data
        assert "changed" in data
        assert "apps" in data

    def test_migrate_apply(self, test_client: TestClient) -> None:
        resp = test_client.post("/api/studios/framework/apps/migrate-manifests", json={"dry_run": False})
        assert resp.status_code == 200
        data = resp.json()
        assert data["dry_run"] is False
        assert data["total"] == 3


class TestFrameworkAudit:
    """GET /api/studios/framework/apps/framework-audit テスト."""

    def test_returns_report(self, test_client: TestClient) -> None:
        resp = test_client.get("/api/studios/framework/apps/framework-audit")
        assert resp.status_code == 200
        data = resp.json()
        assert "total_apps" in data
        assert "apps" in data
        assert data["total_apps"] == 3
        assert isinstance(data["apps"], list)

    def test_accepts_audit_profile_override(self, test_client: TestClient) -> None:
        """audit_profile クエリを受け付ける."""
        resp = test_client.get("/api/studios/framework/apps/framework-audit?audit_profile=business")
        assert resp.status_code == 200
        data = resp.json()
        assert data["audit_profile"] == "business"
        assert len(data["apps"]) == 3


class TestRootAndHealth:
    """ルート・ヘルスエンドポイントテスト."""

    def test_root_endpoint(self, test_client: TestClient) -> None:
        """ルートが Platform 情報を返す."""
        resp = test_client.get("/")
        assert resp.status_code == 200
        data = resp.json()
        assert data["name"] == "BizCore Control Plane"
        assert "framework_apps_api" in data
        assert "studios_api" in data

    def test_health_endpoint(self, test_client: TestClient) -> None:
        """ヘルスチェックが healthy を返す."""
        resp = test_client.get("/health")
        assert resp.status_code == 200
        assert resp.json()["status"] == "healthy"


class TestCreateOptions:
    """GET /api/studios/framework/apps/create/options テスト."""

    def test_default_profile_returns_engine_options(self, test_client: TestClient) -> None:
        resp = test_client.get("/api/studios/framework/apps/create/options")
        assert resp.status_code == 200
        data = resp.json()
        assert "engine_patterns" in data
        assert "agent_type_options" in data
        assert "app_template_options" in data
        assert data["surface_profile"] == "developer"

    def test_business_profile_returns_simplified_options(self, test_client: TestClient) -> None:
        resp = test_client.get("/api/studios/framework/apps/create/options?surface_profile=business")
        assert resp.status_code == 200
        data = resp.json()
        assert data["surface_profile"] == "business"
        assert "templates" in data
        assert "app_template_options" in data
        assert "risk_levels" in data
        assert "engine_patterns" not in data


class TestAppTemplates:
    """GET /api/studios/framework/apps/templates テスト."""

    def test_returns_template_catalog(self, test_client: TestClient) -> None:
        resp = test_client.get("/api/studios/framework/apps/templates")
        assert resp.status_code == 200
        data = resp.json()
        assert "templates" in data
        assert "apps" in data
        assert "total_templates" in data
        assert "total_apps" in data
        assert data["total_templates"] == 6
        assert data["total_apps"] == 3

    def test_template_item_shape(self, test_client: TestClient) -> None:
        resp = test_client.get("/api/studios/framework/apps/templates")
        templates = resp.json()["templates"]
        assert len(templates) == 6
        first = templates[0]
        assert {"app_template", "label", "description", "count", "apps"} <= set(first.keys())
        assert isinstance(first["apps"], list)


class TestBusinessSurfaceAccessControl:
    """business surface のアクセス制御テスト."""

    def test_config_access_denied_for_business(self, test_client: TestClient) -> None:
        resp = test_client.get("/api/studios/framework/apps/test_app/config?surface_profile=business")
        assert resp.status_code == 403
        assert resp.json()["detail"]["error_code"] == "SURFACE_ACCESS_DENIED"

    def test_contracts_access_denied_for_business(self, test_client: TestClient) -> None:
        resp = test_client.get("/api/studios/framework/apps/test_app/contracts?surface_profile=business")
        assert resp.status_code == 403
        assert resp.json()["detail"]["error_code"] == "SURFACE_ACCESS_DENIED"
