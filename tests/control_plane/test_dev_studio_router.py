"""dev_studio router のユニットテスト."""

from __future__ import annotations

from typing import TYPE_CHECKING

import httpx
from fastapi import FastAPI
from fastapi.testclient import TestClient

from control_plane.routers.dev_studio import init_dev_studio_services
from control_plane.routers.dev_studio import router as dev_studio_router
from control_plane.services.app_discovery import AppDiscoveryService


if TYPE_CHECKING:
    from pathlib import Path


def _build_client(tmp_path: Path, monkeypatch) -> TestClient:
    monkeypatch.chdir(tmp_path)
    discovery = AppDiscoveryService(tmp_path / "apps")
    init_dev_studio_services(discovery)
    app = FastAPI()
    app.include_router(dev_studio_router)
    return TestClient(app)


def test_proxy_passes_json_response(tmp_path, monkeypatch) -> None:
    """proxy は upstream の JSON を返す."""

    class _FakeAsyncClient:
        def __init__(self, *args, **kwargs) -> None:
            del args, kwargs

        async def __aenter__(self):
            return self

        async def __aexit__(self, exc_type, exc, tb) -> None:
            del exc_type, exc, tb

        async def request(self, method: str, url: str, **kwargs) -> httpx.Response:
            del kwargs
            return httpx.Response(
                200,
                json={"templates": [{"id": "simple"}], "total": 1},
                request=httpx.Request(method, url),
            )

    monkeypatch.setattr("control_plane.routers.dev_studio.httpx.AsyncClient", _FakeAsyncClient)
    client = _build_client(tmp_path, monkeypatch)

    response = client.get("/api/studios/framework/dev-studio/wizard/templates")

    assert response.status_code == 200
    assert response.json()["total"] == 1


def test_proxy_connect_error_returns_503(tmp_path, monkeypatch) -> None:
    """upstream 不達時は 503 を返す."""

    class _BrokenAsyncClient:
        def __init__(self, *args, **kwargs) -> None:
            del args, kwargs

        async def __aenter__(self):
            return self

        async def __aexit__(self, exc_type, exc, tb) -> None:
            del exc_type, exc, tb

        async def request(self, method: str, url: str, **kwargs) -> httpx.Response:
            del kwargs
            msg = "down"
            raise httpx.ConnectError(msg, request=httpx.Request(method, url))

    monkeypatch.setattr("control_plane.routers.dev_studio.httpx.AsyncClient", _BrokenAsyncClient)
    client = _build_client(tmp_path, monkeypatch)

    response = client.get("/api/studios/framework/dev-studio/wizard/templates")

    assert response.status_code == 503
    assert response.json()["detail"]["error_code"] == "DEV_STUDIO_UNAVAILABLE"


def test_materialize_creates_app_manifest(tmp_path, monkeypatch) -> None:
    """materialize は app を作成し、discovery で再検出できる."""
    client = _build_client(tmp_path, monkeypatch)

    response = client.post(
        "/api/studios/framework/dev-studio/materialize",
        json={
            "name": "ops builder",
            "display_name": "Ops Builder",
            "description": "Generated backend app",
            "template_id": "ops_automation",
            "goal": "Automate change reviews",
            "spec_kind": "agent",
            "spec": {
                "name": "OpsBuilderAgent",
                "description": "Review change requests",
                "capabilities": ["review", "planning"],
                "engine_type": "simple",
            },
            "generated_files": {
                "app.py": (
                    "from fastapi import FastAPI\n"
                    "app = FastAPI()\n"
                    "@app.get('/health')\n"
                    "async def health():\n"
                    "    return {'status': 'healthy'}\n"
                ),
                "requirements.txt": "fastapi\nuvicorn\n",
            },
            "output_type": "backend",
            "framework": "fastapi",
        },
    )

    assert response.status_code == 200
    payload = response.json()
    assert payload["success"] is True
    assert payload["app_name"] == "ops_builder"
    assert payload["ports"]["api"] >= 8100
    assert (tmp_path / "apps" / "ops_builder" / "app_config.json").is_file()
    assert (tmp_path / "apps" / "ops_builder" / "app.py").is_file()
