"""Unit tests for transactional LLM switch service."""

from __future__ import annotations

from pathlib import Path

from apps.platform.schemas.llm_management_schemas import (
    LLMBackendKind,
    LLMProviderKind,
    LLMSwitchRequest,
    LLMSwitchRuntimeCheck,
)
from apps.platform.services.llm_management_config_store import LLMConfigStore
from apps.platform.services.llm_management_switch_service import LLMSwitchService
from apps.platform.services.llm_management_validator import LLMConfigValidationError, LLMConfigValidator


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
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
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
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
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
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    service, store, validator = _build_service(config_path)
    old_version = store.version()

    def _raise_validation(*args, **kwargs):
        del args, kwargs
        raise LLMConfigValidationError("forced validation error")

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
