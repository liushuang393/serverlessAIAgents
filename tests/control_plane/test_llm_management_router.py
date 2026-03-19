"""Integration tests for LLM management router (legacy + new endpoints).

Note: Uses synchronous SyncASGIClient to avoid pytest-asyncio/anyio event-loop
shutdown hang that occurs with async generator fixtures + httpx.AsyncClient.
"""

from __future__ import annotations

import os
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING, Any

import httpx
import pytest
from control_plane.db.session import close_platform_db
from control_plane.main import create_app
from control_plane.routers.llm_management import init_llm_management_service
from control_plane.services.llm_management import LLMManagementService

if TYPE_CHECKING:
    from pathlib import Path

from tests.control_plane.conftest import SyncASGIClient


@pytest.fixture
def llm_client(tmp_path: Path) -> tuple[SyncASGIClient, LLMManagementService]:
    app = create_app()

    @asynccontextmanager
    async def _no_lifespan(_app: Any) -> Any:
        yield

    app.router.lifespan_context = _no_lifespan

    service = LLMManagementService(config_path=tmp_path / ".bizcore" / "llm_gateway.yaml")
    init_llm_management_service(service)

    client = SyncASGIClient(app)
    try:
        yield client, service
    finally:
        # close_platform_db shuts down the aiosqlite connection worker thread
        # that would otherwise keep the process alive after tests complete.
        import asyncio
        try:
            loop = asyncio.new_event_loop()
            loop.run_until_complete(close_platform_db())
            loop.close()
        except Exception:
            pass
        client.close()


def test_llm_new_endpoints_and_legacy_overview(llm_client: Any) -> None:
    client, _service = llm_client

    overview = client.get("/api/studios/framework/llm/overview")
    assert overview.status_code == 200
    overview_json = overview.json()
    assert "providers" in overview_json
    assert "routing_policy" in overview_json

    catalog = client.get("/api/studios/framework/llm/catalog")
    assert catalog.status_code == 200
    catalog_json = catalog.json()
    assert "providers" in catalog_json
    assert "backends" in catalog_json

    diagnostics = client.get("/api/studios/framework/llm/diagnostics")
    assert diagnostics.status_code == 200
    assert diagnostics.json()["route_count"] > 0


def test_setup_and_switch_preflight_failure_does_not_write_config(llm_client: Any) -> None:
    client, service = llm_client
    # Force initial load to create the default config file so version is stable.
    _ = service.get_config()
    before_version = service.get_config_version()
    previous = os.environ.pop("OPENAI_API_KEY", None)

    try:
        response = client.post(
            "/api/studios/framework/llm/setup-and-switch",
            json={
                "preflight": {
                    "providers": ["openai"],
                    "backends": [],
                    "auto_install": True,
                    "auto_start": True,
                    "health_check": True,
                    "dry_run": False,
                },
                "switch": {
                    "provider": "openai",
                    "model": "gpt-4o-mini",
                    "backend": "none",
                    "roles": ["reasoning"],
                    "auto_enable_provider": True,
                    "update_fallback_chain": True,
                    "validate_runtime": True,
                },
            },
        )

        assert response.status_code == 200
        payload = response.json()
        assert payload["success"] is False
        assert payload["switch"] is None

        after_version = service.get_config_version()
        assert after_version == before_version
    finally:
        if previous is not None:
            os.environ["OPENAI_API_KEY"] = previous


def test_legacy_put_registry_endpoint_still_supported(llm_client: Any) -> None:
    client, _service = llm_client

    get_before = client.get("/api/studios/framework/llm/registry")
    assert get_before.status_code == 200
    current = get_before.json()["registry"]
    assert "reasoning" in current

    put_resp = client.put(
        "/api/studios/framework/llm/registry",
        json={"registry": {"reasoning": current["reasoning"]}},
    )
    assert put_resp.status_code == 200
    assert put_resp.json()["registry"]["reasoning"] == current["reasoning"]


def test_service_diagnostics_route_missing_case(tmp_path: Path) -> None:
    service = LLMManagementService(config_path=tmp_path / ".bizcore" / "llm_gateway.yaml")
    diagnostics = service.get_diagnostics(has_llm_routes=False, route_count=0)

    assert diagnostics.has_llm_routes is False
    assert any("Platform backend" in hint for hint in diagnostics.hints)


def test_deploy_unknown_engine_returns_400(llm_client: Any) -> None:
    client, _service = llm_client

    response = client.post(
        "/api/studios/framework/llm/engines/unknown/deploy",
        json={},
    )

    assert response.status_code == 400
    assert "設定されていません" in response.json()["detail"]


def test_put_engines_port_conflict_returns_400(llm_client: Any) -> None:
    client, _service = llm_client

    response = client.put(
        "/api/studios/framework/llm/engines",
        json={
            "inference_engines": [
                {
                    "name": "vllm",
                    "engine_type": "vllm",
                    "base_url": "http://127.0.0.1:8001",
                    "health_path": "/health",
                    "metrics_path": "/metrics",
                    "model_list_path": "/v1/models",
                    "enabled": True,
                    "deployment_mode": "docker",
                    "docker_image": "vllm/vllm-openai:v0.8.5",
                    "served_model_name": "Qwen/Qwen2.5-0.5B-Instruct",
                    "container_name": "llm-vllm",
                    "host_port": 8001,
                    "public_base_url": None,
                    "gpu_enabled": False,
                    "gpu_devices": [],
                    "gpu_count": None,
                    "extra_env": {},
                    "deployment_status": None,
                    "deployment_error": None,
                    "compose_path": None,
                }
            ]
        },
    )

    assert response.status_code == 400
    assert "decision_governance_engine.api" in response.json()["detail"]
