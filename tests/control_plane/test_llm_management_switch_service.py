"""Unit tests for transactional LLM switch service."""

from __future__ import annotations

from typing import TYPE_CHECKING

from control_plane.schemas.llm_management_schemas import (
    LLMBackendKind,
    LLMProviderKind,
    LLMSwitchRequest,
    LLMSwitchRuntimeCheck,
)
from control_plane.services.llm_management_config_store import LLMConfigStore
from control_plane.services.llm_management_switch_service import LLMSwitchService
from control_plane.services.llm_management_validator import LLMConfigValidationError, LLMConfigValidator
from infrastructure.llm.gateway import ProviderRuntimeStatus


if TYPE_CHECKING:
    from pathlib import Path


def _build_service(config_path: Path) -> tuple[LLMSwitchService, LLMConfigStore, LLMConfigValidator]:
    store = LLMConfigStore(config_path)
    store.load()
    validator = LLMConfigValidator()
    service = LLMSwitchService(
        config_store=store,
        validator=validator,
        config_path=config_path,
    )
    return service, store, validator


async def test_switch_success_updates_registry(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service, store, _validator = _build_service(config_path)

    async def _runtime_ok(*args, **kwargs) -> LLMSwitchRuntimeCheck:
        del args, kwargs
        return LLMSwitchRuntimeCheck(provider_status="available", backend_status=None, errors=[])

    service._runtime_check = _runtime_ok  # type: ignore[method-assign]

    response = await service.switch(
        LLMSwitchRequest(
            provider=LLMProviderKind.OPENAI,
            model="gpt-4o-mini",
            backend=LLMBackendKind.NONE,
            roles=["reasoning"],
            validate_runtime=True,
        )
    )

    assert response.success is True
    assert response.rolled_back is False
    assert response.applied_alias is not None
    current = store.load()
    assert current.registry["reasoning"] == response.applied_alias


async def test_switch_runtime_failure_rolls_back(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service, store, _validator = _build_service(config_path)
    before = store.load()
    before_alias = before.registry["reasoning"]

    async def _runtime_fail(*args, **kwargs) -> LLMSwitchRuntimeCheck:
        del args, kwargs
        return LLMSwitchRuntimeCheck(
            provider_status="unavailable",
            backend_status=None,
            errors=["provider unavailable"],
        )

    service._runtime_check = _runtime_fail  # type: ignore[method-assign]

    response = await service.switch(
        LLMSwitchRequest(
            provider=LLMProviderKind.OPENAI,
            model="gpt-4o-mini",
            backend=LLMBackendKind.NONE,
            roles=["reasoning"],
            validate_runtime=True,
        )
    )

    assert response.success is False
    assert response.rolled_back is True
    after = store.load()
    assert after.registry["reasoning"] == before_alias


async def test_switch_validation_failure_does_not_persist(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service, store, validator = _build_service(config_path)
    old_version = store.version()

    def _raise_validation(*args, **kwargs):
        del args, kwargs
        msg = "forced validation error"
        raise LLMConfigValidationError(msg)

    validator.prepare_config = _raise_validation  # type: ignore[method-assign]

    response = await service.switch(
        LLMSwitchRequest(
            provider=LLMProviderKind.OPENAI,
            model="gpt-4o-mini",
            backend=LLMBackendKind.NONE,
            roles=["reasoning"],
            validate_runtime=False,
        )
    )

    assert response.success is False
    assert response.rolled_back is False
    assert store.version() == old_version


async def test_runtime_check_uses_shared_provider_status_resolver(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    service, _store, _validator = _build_service(config_path)
    config = service._store.load()

    async def _provider_statuses(*args, **kwargs):  # type: ignore[no-untyped-def]
        del args, kwargs
        return [
            ProviderRuntimeStatus(
                name="openai",
                status="available",
                api_key_env="OPENAI_API_KEY",
                source="ENV",
                masked="sk-***",
                last_error=None,
            )
        ]

    async def _engine_statuses(*args, **kwargs):  # type: ignore[no-untyped-def]
        del args, kwargs
        return []

    monkeypatch.setattr(
        "control_plane.services.llm_management_switch_service.resolve_provider_runtime_statuses",
        _provider_statuses,
    )
    monkeypatch.setattr(
        "control_plane.services.llm_management_switch_service.LiteLLMGateway.get_engine_statuses",
        _engine_statuses,
    )

    runtime = await service._runtime_check(
        config,
        LLMSwitchRequest(
            provider=LLMProviderKind.OPENAI,
            model="gpt-5-mini",
            backend=LLMBackendKind.NONE,
            roles=["reasoning"],
            validate_runtime=True,
        ),
    )

    assert runtime.provider_status == "available"
    assert runtime.errors == []
