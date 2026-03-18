"""Platform LLMManagementService のテスト."""

from __future__ import annotations

import asyncio
import sqlite3
from pathlib import Path

import pytest
from control_plane.db import close_platform_db
from control_plane.schemas.llm_management_schemas import (
    LLMEngineDeployRequest,
    LLMInferenceEngineConfigPayload,
    LLMProviderSecretUpdateRequest,
)
from control_plane.services.llm_management import LLMManagementService
from control_plane.services.llm_management_persistence import PlatformEngineDeploymentRecord
from control_plane.services.llm_management_setup_manager import LLMSetupCommandResult
from control_plane.services.llm_runtime_status import resolve_provider_runtime_statuses

from infrastructure.llm.gateway import EngineRuntimeStatus, ProviderConfig


class StubDeployRunner:
    """常に成功を返す command runner."""

    async def run(
        self,
        command: list[str],
        *,
        cwd: str | None = None,
        timeout_seconds: float = 120.0,
    ) -> LLMSetupCommandResult:
        del cwd, timeout_seconds
        return LLMSetupCommandResult(
            command=command,
            return_code=0,
            stdout="ok",
            stderr="",
            allowed=True,
        )


class StubDeployRunnerWithStderr:
    """成功だが stderr を返す command runner."""

    async def run(
        self,
        command: list[str],
        *,
        cwd: str | None = None,
        timeout_seconds: float = 120.0,
    ) -> LLMSetupCommandResult:
        del cwd, timeout_seconds
        return LLMSetupCommandResult(
            command=command,
            return_code=0,
            stdout="container started",
            stderr="Creating network...\nStarting service...",
            allowed=True,
        )


async def _health_ok(*args, **kwargs) -> None:
    del args, kwargs


@pytest.mark.asyncio
async def test_get_overview_contains_required_sections(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)

    overview = await service.get_overview()
    payload = overview.model_dump()
    assert "providers" in payload
    assert "inference_engines" in payload
    assert "models" in payload
    assert "registry" in payload
    assert "routing_policy" in payload


@pytest.mark.asyncio
async def test_update_registry_normalizes_role_and_alias(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)

    updated = await service.update_registry({"Reasoning": "Coding_OpenAI", "cheap": "cheap_gemini"})
    assert updated["reasoning"] == "coding_openai"
    assert updated["cheap"] == "cheap_gemini"


def test_catalog_includes_local_provider_once(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)

    catalog = service.get_catalog()
    provider_names = [item.name.value for item in catalog.providers]

    assert provider_names.count("local") == 1
    assert provider_names.count("google") == 1
    assert "gpt-5.2" in next(item for item in catalog.providers if item.name.value == "openai").recommended_models


@pytest.mark.asyncio
async def test_provider_runtime_marks_missing_key_as_unavailable(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    monkeypatch.setenv("PLATFORM_RUNTIME_CACHE_DB", str(tmp_path / "runtime_cache.db"))
    monkeypatch.delenv("OPENAI_API_KEY", raising=False)
    service = LLMManagementService(config_path=config_path)

    statuses = await service.get_provider_runtime()
    by_name = {item.name: item for item in statuses}
    assert by_name["openai"].status == "unavailable"
    assert by_name["local"].status == "unavailable"


@pytest.mark.asyncio
async def test_update_engines_rejects_app_port_conflicts(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)

    with pytest.raises(ValueError, match=r"decision_governance_engine\.api"):
        await service.update_engines(
            [
                LLMInferenceEngineConfigPayload(
                    name="vllm",
                    engine_type="vllm",
                    base_url="http://127.0.0.1:8001",
                    health_path="/health",
                    metrics_path="/metrics",
                    model_list_path="/v1/models",
                    enabled=True,
                    deployment_mode="docker",
                    docker_image="vllm/vllm-openai:v0.8.5",
                    served_model_name="Qwen/Qwen2.5-0.5B-Instruct",
                    container_name="llm-vllm",
                    host_port=8001,
                    public_base_url=None,
                    gpu_enabled=False,
                    gpu_devices=[],
                    gpu_count=None,
                    extra_env={},
                    deployment_status=None,
                    deployment_error=None,
                    compose_path=None,
                )
            ]
        )


@pytest.mark.asyncio
async def test_save_provider_secret_encrypts_and_updates_overview(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    db_path = tmp_path / "control_plane.db"
    cache_path = tmp_path / "platform_runtime_cache.db"
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"

    await close_platform_db()
    monkeypatch.setenv("PLATFORM_DATABASE_URL", f"sqlite+aiosqlite:///{db_path}")
    monkeypatch.setenv("PLATFORM_RUNTIME_CACHE_DB", str(cache_path))
    monkeypatch.setenv("PLATFORM_SECRET_MASTER_KEY", "platform-secret-master-key-for-tests")

    service = LLMManagementService(config_path=config_path)

    response = await service.save_provider_secret(
        "openai",
        payload=LLMProviderSecretUpdateRequest(
            api_key_env="OPENAI_API_KEY",
            secret_value="sk-platform-secret",
        ),
    )

    assert response.secret_status.configured is True
    assert response.secret_status.source == "platform_encrypted"
    assert cache_path.is_file()

    overview = await service.get_overview()
    provider = next(item for item in overview.providers if item.name == "openai")
    assert provider.secret_status.source == "platform_encrypted"
    assert provider.secret_status.masked == response.secret_status.masked

    with sqlite3.connect(db_path) as connection:
        row = connection.execute(
            "SELECT encrypted_secret, secret_mask FROM llm_provider_secrets WHERE provider_name = ?",
            ("openai",),
        ).fetchone()
    assert row is not None
    assert row[0] != "sk-platform-secret"
    assert row[1] == response.secret_status.masked

    await close_platform_db()


@pytest.mark.asyncio
async def test_get_provider_runtime_marks_local_unavailable_when_linked_engine_is_unhealthy(
    tmp_path: Path,
) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)

    await service.update_engines(
        [
            LLMInferenceEngineConfigPayload(
                name="vllm",
                engine_type="vllm",
                base_url="http://127.0.0.1:18001",
                health_path="/health",
                metrics_path="/metrics",
                model_list_path="/v1/models",
                enabled=True,
                deployment_mode="docker",
                docker_image="vllm/vllm-openai:v0.8.5",
                served_model_name="Qwen/Qwen2.5-0.5B-Instruct",
                container_name="llm-vllm",
                host_port=18001,
                public_base_url=None,
                gpu_enabled=False,
                gpu_devices=[],
                gpu_count=None,
                extra_env={},
                deployment_status=None,
                deployment_error=None,
                compose_path=None,
            )
        ]
    )

    statuses = await service.get_provider_runtime()
    by_name = {item.name: item for item in statuses}

    assert by_name["local"].status == "unavailable"
    assert by_name["local"].last_error is not None
    assert by_name["local"].last_error.startswith("linked_engine_unhealthy:")


@pytest.mark.asyncio
async def test_resolve_provider_runtime_statuses_probes_custom_provider_success(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)
    config = service.get_config().model_copy(deep=True)
    config.providers.append(
        ProviderConfig(
            name="custom",
            api_base="http://127.0.0.1:19000/v1",
            api_key_env=None,
            enabled=True,
            models=[],
        )
    )

    async def _fake_get(self, url: str, *args, **kwargs):  # type: ignore[no-untyped-def]
        del self, args, kwargs
        class _Response:
            status_code = 200
        assert url.endswith("/v1/models")
        return _Response()

    monkeypatch.setattr("control_plane.services.llm_runtime_status.httpx.AsyncClient.get", _fake_get)

    statuses = await resolve_provider_runtime_statuses(config, config_path=config_path, engine_statuses=[])
    by_name = {item.name: item for item in statuses}

    assert by_name["custom"].status == "available"
    assert by_name["custom"].source == "probe:v1_models"
    assert by_name["custom"].last_error is None


@pytest.mark.asyncio
async def test_resolve_provider_runtime_statuses_probes_custom_provider_failure(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)
    config = service.get_config().model_copy(deep=True)
    config.providers.append(
        ProviderConfig(
            name="custom",
            api_base="http://127.0.0.1:19000/v1",
            api_key_env=None,
            enabled=True,
            models=[],
        )
    )

    async def _fake_get(self, url: str, *args, **kwargs):  # type: ignore[no-untyped-def]
        del self, args, kwargs
        class _Response:
            status_code = 503
        return _Response()

    monkeypatch.setattr("control_plane.services.llm_runtime_status.httpx.AsyncClient.get", _fake_get)

    statuses = await resolve_provider_runtime_statuses(config, config_path=config_path, engine_statuses=[])
    by_name = {item.name: item for item in statuses}

    assert by_name["custom"].status == "unavailable"
    assert by_name["custom"].last_error is not None
    assert "v1_models:status=503" in by_name["custom"].last_error


@pytest.mark.asyncio
async def test_resolve_provider_runtime_statuses_uses_linked_engine_without_direct_probe(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    config = LLMManagementService(config_path=config_path).get_config()
    error_message = "linked local provider should not use direct probe"

    async def _unexpected_probe(*args, **kwargs):  # type: ignore[no-untyped-def]
        del args, kwargs
        raise AssertionError(error_message)

    monkeypatch.setattr("control_plane.services.llm_runtime_status._probe_provider_runtime", _unexpected_probe)

    statuses = await resolve_provider_runtime_statuses(
        config,
        config_path=config_path,
        engine_statuses=[
            EngineRuntimeStatus(
                name="vllm",
                engine_type="vllm",
                status="unavailable",
                last_error="disabled",
            )
        ],
    )

    by_name = {item.name: item for item in statuses}
    assert by_name["local"].status == "unavailable"


@pytest.mark.asyncio
async def test_deploy_engine_generates_compose_and_persists_status(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    db_path = tmp_path / "control_plane.db"
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"

    await close_platform_db()
    monkeypatch.setenv("PLATFORM_DATABASE_URL", f"sqlite+aiosqlite:///{db_path}")
    monkeypatch.setenv("PLATFORM_RUNTIME_CACHE_DB", str(tmp_path / "platform_runtime_cache.db"))
    monkeypatch.setenv("PLATFORM_SECRET_MASTER_KEY", "platform-secret-master-key-for-tests")

    service = LLMManagementService(config_path=config_path)
    service._setup_manager._runner = StubDeployRunner()
    service._setup_manager._wait_for_engine_health = _health_ok  # type: ignore[method-assign]

    response = await service.deploy_engine(
        "vllm",
        LLMEngineDeployRequest(public_base_url="https://llm.example.com/v1"),
    )

    assert response.success is True
    assert response.engine.deployment_status == "running"
    assert response.engine.compose_path is not None

    compose_path = Path(response.engine.compose_path)
    assert await asyncio.to_thread(compose_path.is_file)
    compose_text = await asyncio.to_thread(compose_path.read_text, encoding="utf-8")
    assert "Qwen/Qwen2.5-0.5B-Instruct" in compose_text

    overview = await service.get_overview()
    engine = next(item for item in overview.inference_engines if item.name == "vllm")
    assert engine.deployment_status == "running"
    assert engine.public_base_url == "https://llm.example.com/v1"

    await close_platform_db()


@pytest.mark.asyncio
async def test_deploy_engine_success_with_stderr_does_not_persist_error_and_enables_engine(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    db_path = tmp_path / "control_plane.db"
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"

    await close_platform_db()
    monkeypatch.setenv("PLATFORM_DATABASE_URL", f"sqlite+aiosqlite:///{db_path}")
    monkeypatch.setenv("PLATFORM_RUNTIME_CACHE_DB", str(tmp_path / "platform_runtime_cache.db"))
    monkeypatch.setenv("PLATFORM_SECRET_MASTER_KEY", "platform-secret-master-key-for-tests")

    service = LLMManagementService(config_path=config_path)
    service._setup_manager._runner = StubDeployRunnerWithStderr()
    service._setup_manager._wait_for_engine_health = _health_ok  # type: ignore[method-assign]

    response = await service.deploy_engine(
        "vllm",
        LLMEngineDeployRequest(public_base_url="https://llm.example.com/v1"),
    )

    assert response.success is True
    assert response.engine.deployment_error is None
    overview = await service.get_overview()
    engine = next(item for item in overview.inference_engines if item.name == "vllm")
    assert engine.deployment_error is None
    enabled_engine = next(item for item in service.get_config().inference_engines if item.name == "vllm")
    assert enabled_engine.enabled is True

    await close_platform_db()


@pytest.mark.asyncio
async def test_stop_engine_success_with_stderr_does_not_persist_error(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    db_path = tmp_path / "control_plane.db"
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"

    await close_platform_db()
    monkeypatch.setenv("PLATFORM_DATABASE_URL", f"sqlite+aiosqlite:///{db_path}")
    monkeypatch.setenv("PLATFORM_RUNTIME_CACHE_DB", str(tmp_path / "platform_runtime_cache.db"))
    monkeypatch.setenv("PLATFORM_SECRET_MASTER_KEY", "platform-secret-master-key-for-tests")

    service = LLMManagementService(config_path=config_path)
    service._setup_manager._runner = StubDeployRunner()
    service._setup_manager._wait_for_engine_health = _health_ok  # type: ignore[method-assign]
    await service.deploy_engine("vllm", LLMEngineDeployRequest())

    service._setup_manager._runner = StubDeployRunnerWithStderr()
    response = await service.stop_engine("vllm")

    assert response.success is True
    assert response.engine.deployment_status == "stopped"
    assert response.engine.deployment_error is None

    await close_platform_db()


@pytest.mark.asyncio
async def test_get_overview_trims_oversized_deployment_error(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    db_path = tmp_path / "control_plane.db"
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"

    await close_platform_db()
    monkeypatch.setenv("PLATFORM_DATABASE_URL", f"sqlite+aiosqlite:///{db_path}")
    monkeypatch.setenv("PLATFORM_RUNTIME_CACHE_DB", str(tmp_path / "platform_runtime_cache.db"))
    monkeypatch.setenv("PLATFORM_SECRET_MASTER_KEY", "platform-secret-master-key-for-tests")

    service = LLMManagementService(config_path=config_path)
    long_error = "\n".join(f"line-{index}" for index in range(300))
    await service._persistence.upsert_engine_deployment(
        PlatformEngineDeploymentRecord(
            engine_name="vllm",
            engine_type="vllm",
            deployment_mode="docker",
            docker_image="vllm/vllm-openai:v0.8.5",
            served_model_name="Qwen/Qwen2.5-0.5B-Instruct",
            container_name="llm-vllm",
            host_port=18001,
            public_base_url="http://127.0.0.1:18001/v1",
            compose_path=str(tmp_path / "docker-compose.yml"),
            compose_yaml=None,
            gpu_enabled=False,
            gpu_devices=[],
            gpu_count=None,
            extra_env={},
            status="failed",
            last_error=long_error,
        )
    )

    overview = await service.get_overview()
    engine = next(item for item in overview.inference_engines if item.name == "vllm")

    assert engine.deployment_error is not None
    assert "... " in engine.deployment_error
    assert len(engine.deployment_error) <= 2000

    await close_platform_db()
