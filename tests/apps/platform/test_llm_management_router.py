"""Integration tests for LLM management router (legacy + new endpoints)."""

from __future__ import annotations

import os
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING

import httpx
import pytest
from apps.platform.main import create_app
from apps.platform.routers.llm_management import init_llm_management_service
from apps.platform.services.llm_management import LLMManagementService


if TYPE_CHECKING:
    from pathlib import Path


@pytest.fixture
async def llm_client(tmp_path: Path):
    app = create_app()

    @asynccontextmanager
    async def _no_lifespan(_app):
        yield

    app.router.lifespan_context = _no_lifespan

    service = LLMManagementService(config_path=tmp_path / ".agentflow" / "llm_gateway.yaml")
    init_llm_management_service(service)

    transport = httpx.ASGITransport(app=app, raise_app_exceptions=False)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as client:
        yield client, service


async def test_llm_new_endpoints_and_legacy_overview(llm_client) -> None:
    client, _service = llm_client

    overview = await client.get("/api/studios/framework/llm/overview")
    assert overview.status_code == 200
    overview_json = overview.json()
    assert "providers" in overview_json
    assert "routing_policy" in overview_json

    catalog = await client.get("/api/studios/framework/llm/catalog")
    assert catalog.status_code == 200
    catalog_json = catalog.json()
    assert "providers" in catalog_json
    assert "backends" in catalog_json

    diagnostics = await client.get("/api/studios/framework/llm/diagnostics")
    assert diagnostics.status_code == 200
    assert diagnostics.json()["route_count"] > 0


async def test_setup_and_switch_preflight_failure_does_not_write_config(llm_client) -> None:
    client, service = llm_client
    before_version = service.get_config_version()
    previous = os.environ.pop("OPENAI_API_KEY", None)

    try:
        response = await client.post(
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


async def test_legacy_put_registry_endpoint_still_supported(llm_client) -> None:
    client, _service = llm_client

    get_before = await client.get("/api/studios/framework/llm/registry")
    assert get_before.status_code == 200
    current = get_before.json()["registry"]
    assert "reasoning" in current

    put_resp = await client.put(
        "/api/studios/framework/llm/registry",
        json={"registry": {"reasoning": current["reasoning"]}},
    )
    assert put_resp.status_code == 200
    assert put_resp.json()["registry"]["reasoning"] == current["reasoning"]


async def test_service_diagnostics_route_missing_case(tmp_path: Path) -> None:
    service = LLMManagementService(config_path=tmp_path / ".agentflow" / "llm_gateway.yaml")
    diagnostics = service.get_diagnostics(has_llm_routes=False, route_count=0)

    assert diagnostics.has_llm_routes is False
    assert any("Platform backend" in hint for hint in diagnostics.hints)


async def test_deploy_unknown_engine_returns_400(llm_client) -> None:
    client, _service = llm_client

    response = await client.post(
        "/api/studios/framework/llm/engines/unknown/deploy",
        json={},
    )

    assert response.status_code == 400
    assert "設定されていません" in response.json()["detail"]


async def test_put_engines_port_conflict_returns_400(llm_client) -> None:
    client, _service = llm_client

    response = await client.put(
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
