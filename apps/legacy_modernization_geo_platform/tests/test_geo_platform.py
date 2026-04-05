"""Backend tests for the Legacy Modernization GEO Platform.

パイプラインは asyncio.create_task で駆動されるため、
TestClient を context manager として使用し blocking portal を維持する。
"""

from __future__ import annotations

import time
from typing import TYPE_CHECKING

import pytest
from apps.legacy_modernization_geo_platform.backend.schemas import GeoExecuteRequest, TaskEvent, TaskStatus
from apps.legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings
from apps.legacy_modernization_geo_platform.main import create_app
from fastapi.testclient import TestClient

from infrastructure.security.auth_client.client import RemoteUser
from kernel.runtime import resolve_app_runtime


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from pathlib import Path


def _build_settings(tmp_path: Path) -> GeoPlatformSettings:
    runtime = resolve_app_runtime("apps/legacy_modernization_geo_platform/app_config.json")
    if runtime.ports.api is None or runtime.urls.backend is None or runtime.urls.frontend is None:
        msg = "legacy_modernization_geo_platform の manifest に必要な runtime 値がありません。"
        raise AssertionError(msg)
    frontend_dist_dir = tmp_path / "frontend-dist"
    frontend_dist_dir.mkdir(parents=True, exist_ok=True)
    (frontend_dist_dir / "index.html").write_text("<html><body>test</body></html>", encoding="utf-8")
    return GeoPlatformSettings(
        app_root=tmp_path,
        host="127.0.0.1",
        port=runtime.ports.api,
        api_key="",
        cors_origins=[runtime.urls.frontend],
        db_path=tmp_path / "data" / "geo.db",
        artifacts_dir=tmp_path / "data" / "artifacts",
        reports_dir=tmp_path / "data" / "reports",
        published_dir=tmp_path / "data" / "published",
        public_base_url=runtime.urls.backend,
        frontend_dist_dir=frontend_dist_dir,
    )


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
    return {
        "Authorization": "Bearer valid-token",
        "x-tenant-id": "tenant-a",
    }


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
    msg = f"Task {task_id} did not reach status {expected}"
    raise AssertionError(msg)


def test_execute_request_defaults() -> None:
    request = GeoExecuteRequest(campaign_name="demo")
    assert request.package == "assessment"
    assert request.options.skill_mode == "skill_first"
    assert request.constraints.must_cite_sources is True


@pytest.mark.parametrize(
    ("raw_language", "normalized_language"),
    [
        ("ja-JP", "ja"),
        ("en-US", "en"),
        ("zh-CN", "zh"),
        ("fr-FR", "ja"),
    ],
)
def test_execute_request_content_language_normalization(
    raw_language: str,
    normalized_language: str,
) -> None:
    request = GeoExecuteRequest(
        campaign_name="demo",
        inputs={"content_languages": [raw_language]},
    )
    assert request.inputs.content_languages == [normalized_language]


def test_execute_creates_task_and_blocks_for_approval(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:
        response = client.post(
            "/api/geo/execute",
            headers=_auth_headers(),
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
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    runtime = resolve_app_runtime("apps/legacy_modernization_geo_platform/app_config.json")
    if runtime.urls.backend is None:
        msg = "legacy_modernization_geo_platform の backend URL がありません。"
        raise AssertionError(msg)
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:
        start = client.post(
            "/api/geo/execute",
            headers=_auth_headers(),
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
            headers=_auth_headers(),
            params={"request_id": approval["request_id"]},
            json={"approved": True, "reviewer_name": "pytest"},
        )
        assert approve_response.status_code == 200

        completed = _wait_for_status(client, start["task_id"], TaskStatus.COMPLETED.value)
        assert completed["published_pages"]
        assert completed["report"] is not None

        page_url = completed["published_pages"][0]["page_url"]
        public_page = client.get(page_url.replace(runtime.urls.backend, ""))
        assert public_page.status_code == 200
        assert "FAQPage" in public_page.text

        sitemap = client.get("/geo/sitemap.xml")
        assert sitemap.status_code == 200
        assert "modernization-guide" in sitemap.text


@pytest.mark.parametrize(
    (
        "requested_language",
        "expected_language",
        "expected_schema_language",
        "expected_html_lang",
        "expected_faq_label",
        "expected_report_title",
    ),
    [
        (
            "en-US",
            "en",
            "en-US",
            'lang="en"',
            "Frequently Asked Questions",
            "Legacy Modernization GEO Platform Report",
        ),
        (
            "zh-CN",
            "zh",
            "zh-CN",
            'lang="zh-CN"',
            "常见问题",
            "Legacy Modernization GEO 平台报告",
        ),
    ],
)
def test_content_language_propagates_to_draft_publish_and_report(
    monkeypatch,
    tmp_path: Path,
    requested_language: str,
    expected_language: str,
    expected_schema_language: str,
    expected_html_lang: str,
    expected_faq_label: str,
    expected_report_title: str,
) -> None:
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    runtime = resolve_app_runtime("apps/legacy_modernization_geo_platform/app_config.json")
    if runtime.urls.backend is None:
        msg = "legacy_modernization_geo_platform の backend URL がありません。"
        raise AssertionError(msg)
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:
        start = client.post(
            "/api/geo/execute",
            headers=_auth_headers(),
            json={
                "campaign_name": "legacy-modernization-i18n",
                "package": "assessment",
                "targets": {
                    "industries": ["manufacturing"],
                    "legacy_stacks": ["COBOL"],
                    "regions": ["Japan"],
                },
                "inputs": {
                    "content_languages": [requested_language],
                },
            },
        ).json()

        waiting_state = _wait_for_status(client, start["task_id"], TaskStatus.WAITING_APPROVAL.value)
        draft_artifact_response = client.get(
            f"/api/geo/{start['task_id']}/artifacts/content_draft_artifact",
            headers=_auth_headers(),
        )
        assert draft_artifact_response.status_code == 200
        draft_artifact = draft_artifact_response.json()
        assert draft_artifact["target_language"] == expected_language
        assert draft_artifact["pages"][0]["json_ld"]["inLanguage"] == expected_schema_language

        approval = waiting_state["approvals"][0]
        approve_response = client.post(
            f"/api/geo/{start['task_id']}/approval",
            headers=_auth_headers(),
            params={"request_id": approval["request_id"]},
            json={"approved": True, "reviewer_name": "pytest"},
        )
        assert approve_response.status_code == 200

        completed = _wait_for_status(client, start["task_id"], TaskStatus.COMPLETED.value)
        page_url = completed["published_pages"][0]["page_url"]
        public_page = client.get(page_url.replace(runtime.urls.backend, ""))
        assert public_page.status_code == 200
        assert expected_html_lang in public_page.text
        assert expected_faq_label in public_page.text
        assert completed["report"] is not None
        assert expected_report_title in completed["report"]["markdown"]


def test_rewrite_command_updates_draft(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE", "1")
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:
        start = client.post(
            "/api/geo/execute",
            headers=_auth_headers(),
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
            headers=_auth_headers(),
            json={"command": "content.rewrite", "actor": "pytest", "comment": "请淡化比较表述"},
        )
        assert command_response.status_code == 200

        deadline = time.time() + 15
        while time.time() < deadline:
            artifact = client.get(
                f"/api/geo/{start['task_id']}/artifacts/content_draft_artifact",
                headers=_auth_headers(),
            )
            if artifact.status_code == 200 and "レビュー反映メモ" in artifact.text:
                break
            time.sleep(0.15)
        else:
            msg = "Draft was not rewritten"
            raise AssertionError(msg)


def test_agent_runtime_endpoints_expose_cards_and_stream(
    monkeypatch,
    tmp_path: Path,
) -> None:
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:
        agents_response = client.get("/api/agents", headers=_auth_headers())
        assert agents_response.status_code == 200
        agents_payload = agents_response.json()
        assert any(item["id"] == "BrandMemory" for item in agents_payload["agents"])

        card_response = client.get("/api/agents/BrandMemory/card", headers=_auth_headers())
        assert card_response.status_code == 200
        assert card_response.json()["name"] == "BrandMemory"

        schema_response = client.get("/api/agents/BrandMemory/schema", headers=_auth_headers())
        assert schema_response.status_code == 200
        assert schema_response.json()["input_schema"]["type"] == "object"

        invoke_response = client.post(
            "/api/agents/BrandMemory/invoke",
            headers=_auth_headers(),
            json={
                "input": {
                    "task_id": "task-1",
                    "request": {
                        "campaign_name": "demo",
                        "package": "assessment",
                        "targets": {
                            "industries": ["manufacturing"],
                            "legacy_stacks": ["COBOL"],
                            "regions": ["Japan"],
                        },
                    },
                }
            },
        )
        assert invoke_response.status_code == 200
        assert invoke_response.json()["success"] is True

        stream_response = client.post(
            "/api/agents/BrandMemory/stream",
            headers=_auth_headers(),
            json={
                "input": {
                    "task_id": "task-1",
                    "request": {
                        "campaign_name": "demo",
                        "package": "assessment",
                        "targets": {
                            "industries": ["manufacturing"],
                            "legacy_stacks": ["COBOL"],
                            "regions": ["Japan"],
                        },
                    },
                }
            },
        )
        assert stream_response.status_code == 200
        assert "flow.start" in stream_response.text


def test_geo_stream_endpoint_supports_eventsource_query_auth(
    monkeypatch,
    tmp_path: Path,
) -> None:
    monkeypatch.setenv("AUTH_SERVICE_URL", "http://auth.example")
    app = create_app(_build_settings(tmp_path), auth_client_factory=_StubAuthClient)

    with TestClient(app) as client:

        async def _stream_events(task_id: str) -> AsyncIterator[TaskEvent]:
            yield TaskEvent(
                event_type="flow.start",
                task_id=task_id,
                stage="assessment",
                message="Campaign started",
            )

        app.state.orchestrator.get_state = lambda _task_id: object()
        app.state.orchestrator.stream_events = _stream_events

        response = client.get(
            "/api/geo/task-1/stream?access_token=valid-token&tenant_id=tenant-a",
        )

        assert response.status_code == 200
        assert response.headers["content-type"].startswith("text/event-stream")
        assert '"event_type": "flow.start"' in response.text
        assert '"event_type": "a2ui.clear"' in response.text
