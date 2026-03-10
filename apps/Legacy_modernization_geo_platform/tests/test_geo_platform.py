"""Backend tests for the Legacy Modernization GEO Platform."""

from __future__ import annotations

import time
from pathlib import Path

from fastapi.testclient import TestClient

from apps.Legacy_modernization_geo_platform.backend.schemas import GeoExecuteRequest, TaskStatus
from apps.Legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings
from apps.Legacy_modernization_geo_platform.main import create_app


def _build_settings(tmp_path: Path) -> GeoPlatformSettings:
    frontend_dist_dir = tmp_path / "frontend-dist"
    frontend_dist_dir.mkdir(parents=True, exist_ok=True)
    (frontend_dist_dir / "index.html").write_text("<html><body>test</body></html>", encoding="utf-8")
    return GeoPlatformSettings(
        app_root=tmp_path,
        host="127.0.0.1",
        port=8010,
        api_key="",
        cors_origins=["http://localhost:3010"],
        db_path=tmp_path / "data" / "geo.db",
        artifacts_dir=tmp_path / "data" / "artifacts",
        reports_dir=tmp_path / "data" / "reports",
        published_dir=tmp_path / "data" / "published",
        public_base_url="http://localhost:8010",
        frontend_dist_dir=frontend_dist_dir,
    )


def _wait_for_status(client: TestClient, task_id: str, expected: str, timeout_seconds: float = 10.0) -> dict:
    deadline = time.time() + timeout_seconds
    while time.time() < deadline:
        response = client.get(f"/api/geo/{task_id}/state")
        payload = response.json()
        if payload.get("status") == expected:
            return payload
        time.sleep(0.1)
    raise AssertionError(f"Task {task_id} did not reach status {expected}")


def test_execute_request_defaults() -> None:
    request = GeoExecuteRequest(campaign_name="demo")
    assert request.package == "assessment"
    assert request.options.skill_mode == "skill_first"
    assert request.constraints.must_cite_sources is True


def test_execute_creates_task_and_blocks_for_approval(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    app = create_app(_build_settings(tmp_path))
    client = TestClient(app)

    response = client.post(
        "/api/geo/execute",
        json={
            "campaign_name": "legacy-modernization-japan-b2b",
            "package": "assessment",
            "targets": {
                "industries": ["manufacturing"],
                "legacy_stacks": ["COBOL", "Struts"],
                "regions": ["Japan"],
            },
        },
    )
    assert response.status_code == 200
    payload = response.json()
    state = _wait_for_status(client, payload["task_id"], TaskStatus.WAITING_APPROVAL.value)

    artifact_names = [item["artifact_name"] for item in state["artifacts"]]
    assert "account_signal_artifact" in artifact_names
    assert "geo_qa_report" in artifact_names
    assert state["approvals"]
    assert state["approvals"][0]["status"] == "pending"


def test_approval_publishes_page_and_report(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    app = create_app(_build_settings(tmp_path))
    client = TestClient(app)

    start = client.post(
        "/api/geo/execute",
        json={
            "campaign_name": "legacy-modernization-japan-b2b",
            "package": "assessment",
            "targets": {
                "industries": ["manufacturing"],
                "legacy_stacks": ["COBOL"],
                "regions": ["Japan"],
            },
        },
    ).json()
    waiting_state = _wait_for_status(client, start["task_id"], TaskStatus.WAITING_APPROVAL.value)
    approval = waiting_state["approvals"][0]

    approve_response = client.post(
        f"/api/geo/{start['task_id']}/approval",
        params={"request_id": approval["request_id"]},
        json={"approved": True, "reviewer_name": "pytest"},
    )
    assert approve_response.status_code == 200

    completed = _wait_for_status(client, start["task_id"], TaskStatus.COMPLETED.value)
    assert completed["published_pages"]
    assert completed["report"] is not None

    page_url = completed["published_pages"][0]["page_url"]
    public_page = client.get(page_url.replace("http://localhost:8010", ""))
    assert public_page.status_code == 200
    assert "FAQPage" in public_page.text

    sitemap = client.get("/geo/sitemap.xml")
    assert sitemap.status_code == 200
    assert "modernization-guide" in sitemap.text


def test_rewrite_command_updates_draft(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    app = create_app(_build_settings(tmp_path))
    client = TestClient(app)

    start = client.post(
        "/api/geo/execute",
        json={
            "campaign_name": "legacy-modernization-japan-b2b",
            "package": "assessment",
            "targets": {
                "industries": ["manufacturing"],
                "legacy_stacks": ["COBOL"],
                "regions": ["Japan"],
            },
        },
    ).json()
    _wait_for_status(client, start["task_id"], TaskStatus.WAITING_APPROVAL.value)

    command_response = client.post(
        f"/api/geo/{start['task_id']}/commands",
        json={"command": "content.rewrite", "actor": "pytest", "comment": "请淡化比较表述"},
    )
    assert command_response.status_code == 200

    deadline = time.time() + 10
    while time.time() < deadline:
        artifact = client.get(f"/api/geo/{start['task_id']}/artifacts/content_draft_artifact")
        if artifact.status_code == 200 and "レビュー反映メモ" in artifact.text:
            break
        time.sleep(0.1)
    else:
        raise AssertionError("Draft was not rewritten")
