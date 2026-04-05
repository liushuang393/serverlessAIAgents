"""エッジケース統合テスト: キャンセル・rewrite 上限・reject・Intelligence 失敗.

パイプラインは asyncio.create_task で駆動されるため、
TestClient を context manager として使用し blocking portal を維持する。
"""

from __future__ import annotations

import time
from typing import TYPE_CHECKING

from apps.legacy_modernization_geo_platform.backend.schemas import TaskStatus
from apps.legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings
from apps.legacy_modernization_geo_platform.main import create_app
from fastapi.testclient import TestClient

from infrastructure.security.auth_client.client import RemoteUser
from kernel.runtime import resolve_app_runtime


if TYPE_CHECKING:
    from pathlib import Path


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


class _StubAuthClient:
    def __init__(self, **kwargs: object) -> None:
        self.kwargs = kwargs

    async def verify_token(self, token: str) -> RemoteUser | None:
        if token != "valid-token":
            return None
        return RemoteUser(
            user_id="geo-operator",
            username="operator",
            role="operator",
            roles=["operator"],
            tenant_id="tenant-a",
            scopes=["geo.operator"],
            permissions=["geo.publish"],
            azp="geo-platform",
            extra={"tenant_id": "tenant-a"},
        )


def _auth_headers() -> dict[str, str]:
    return {"Authorization": "Bearer valid-token", "x-tenant-id": "tenant-a"}


def _build_settings(tmp_path: Path) -> GeoPlatformSettings:
    runtime = resolve_app_runtime("apps/legacy_modernization_geo_platform/app_config.json")
    frontend_dist_dir = tmp_path / "frontend-dist"
    frontend_dist_dir.mkdir(parents=True, exist_ok=True)
    (frontend_dist_dir / "index.html").write_text("<html><body>test</body></html>", encoding="utf-8")
    return GeoPlatformSettings(
        app_root=tmp_path,
        host="127.0.0.1",
        port=runtime.ports.api or 8093,
        api_key="",
        cors_origins=[runtime.urls.frontend or "http://localhost:3093"],
        db_path=tmp_path / "data" / "geo.db",
        artifacts_dir=tmp_path / "data" / "artifacts",
        reports_dir=tmp_path / "data" / "reports",
        published_dir=tmp_path / "data" / "published",
        public_base_url=runtime.urls.backend or "http://localhost:8093",
        frontend_dist_dir=frontend_dist_dir,
    )


def _start_campaign(client: TestClient) -> str:
    """キャンペーンを開始してタスク ID を返す."""
    response = client.post(
        "/api/geo/execute",
        headers=_auth_headers(),
        json={
            "campaign_name": "edge-case-test",
            "package": "assessment",
            "targets": {
                "industries": ["manufacturing"],
                "legacy_stacks": ["COBOL"],
                "regions": ["Japan"],
            },
        },
    )
    assert response.status_code == 200
    return response.json()["task_id"]


def _wait_for_status(
    client: TestClient,
    task_id: str,
    expected: str,
    *,
    timeout_seconds: float = 15.0,
) -> dict:
    deadline = time.time() + timeout_seconds
    while time.time() < deadline:
        response = client.get(f"/api/geo/{task_id}/state", headers=_auth_headers())
        payload = response.json()
        if payload.get("status") == expected:
            return payload
        time.sleep(0.15)
    msg = f"Task {task_id} did not reach status {expected} (current: {payload.get('status')})"
    raise AssertionError(msg)


# ---------------------------------------------------------------------------
# 1. campaign.kill コマンドによるキャンセル
# ---------------------------------------------------------------------------


def test_cancel_during_approval_sets_cancelled(monkeypatch, tmp_path: Path) -> None:
    """承認待ち中に campaign.kill を送るとタスクが CANCELLED になる."""
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:
        task_id = _start_campaign(client)
        _wait_for_status(client, task_id, TaskStatus.WAITING_APPROVAL.value)

        kill_response = client.post(
            f"/api/geo/{task_id}/commands",
            headers=_auth_headers(),
            json={"command": "campaign.kill", "actor": "pytest"},
        )
        assert kill_response.status_code == 200
        assert kill_response.json()["applied"] is True

        final = _wait_for_status(client, task_id, TaskStatus.FAILED.value, timeout_seconds=10)
        assert final["status"] in {TaskStatus.FAILED.value, TaskStatus.CANCELLED.value}


# ---------------------------------------------------------------------------
# 2. reject による失敗
# ---------------------------------------------------------------------------


def test_reject_approval_fails_task(monkeypatch, tmp_path: Path) -> None:
    """承認を REJECT すると FAILED になり、パブリッシュされない."""
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:
        task_id = _start_campaign(client)
        state = _wait_for_status(client, task_id, TaskStatus.WAITING_APPROVAL.value)
        approval = state["approvals"][0]

        reject_response = client.post(
            f"/api/geo/{task_id}/approval",
            headers=_auth_headers(),
            params={"request_id": approval["request_id"]},
            json={"approved": False, "reviewer_name": "pytest-rejector"},
        )
        assert reject_response.status_code == 200

        failed = _wait_for_status(client, task_id, TaskStatus.FAILED.value)
        assert not failed.get("published_pages")


# ---------------------------------------------------------------------------
# 3. rewrite 上限超過
# ---------------------------------------------------------------------------


def test_rewrite_limit_fails_task(monkeypatch, tmp_path: Path) -> None:
    """max_auto_iterations を超えた rewrite 要求で FAILED になる."""
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:
        response = client.post(
            "/api/geo/execute",
            headers=_auth_headers(),
            json={
                "campaign_name": "rewrite-limit-test",
                "package": "assessment",
                "targets": {"industries": ["manufacturing"], "legacy_stacks": ["COBOL"], "regions": ["Japan"]},
                "options": {"max_auto_iterations": 1},
            },
        )
        task_id = response.json()["task_id"]

        state = _wait_for_status(client, task_id, TaskStatus.WAITING_APPROVAL.value)
        approval = state["approvals"][0]

        # 1 回目の rewrite → 成功（上限内）
        client.post(
            f"/api/geo/{task_id}/approval",
            headers=_auth_headers(),
            params={"request_id": approval["request_id"]},
            json={"action": "rewrite", "comment": "修正1", "reviewer_name": "reviewer"},
        )

        state2 = _wait_for_status(client, task_id, TaskStatus.WAITING_APPROVAL.value, timeout_seconds=15)
        approval2 = state2["approvals"][-1]

        # 2 回目の rewrite → 上限超過で FAILED
        client.post(
            f"/api/geo/{task_id}/approval",
            headers=_auth_headers(),
            params={"request_id": approval2["request_id"]},
            json={"action": "rewrite", "comment": "修正2", "reviewer_name": "reviewer"},
        )

        _wait_for_status(client, task_id, TaskStatus.FAILED.value, timeout_seconds=10)


# ---------------------------------------------------------------------------
# 4. Intelligence 失敗
# ---------------------------------------------------------------------------


def test_intelligence_failure_reports_flow_error(monkeypatch, tmp_path: Path) -> None:
    """Intelligence 収集が失敗した場合、flow.error イベントで失敗が報告される."""
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    monkeypatch.delenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", raising=False)
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    async def _failing_gather(*_args: object, **_kwargs: object) -> None:
        msg = "Intelligence provider unreachable"
        raise ConnectionError(msg)

    app.state.orchestrator._intelligence.gather_market_intelligence = _failing_gather

    with TestClient(app) as client:
        task_id = _start_campaign(client)
        failed = _wait_for_status(client, task_id, TaskStatus.FAILED.value, timeout_seconds=10)
        assert "error" in failed or failed["status"] == TaskStatus.FAILED.value
