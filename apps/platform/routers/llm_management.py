"""LLM Management router."""

from __future__ import annotations

from typing import Any

from apps.platform.services.llm_management import (
    LLMManagementService,
    get_default_llm_management_service,
)
from fastapi import APIRouter
from pydantic import BaseModel, Field


router = APIRouter(prefix="/api/studios/framework/llm", tags=["llm-management"])

_service: LLMManagementService | None = None


class ProviderUpdateRequest(BaseModel):
    providers: list[dict[str, Any]] = Field(default_factory=list)


class EngineUpdateRequest(BaseModel):
    inference_engines: list[dict[str, Any]] = Field(default_factory=list)


class ModelUpdateRequest(BaseModel):
    models: list[dict[str, Any]] = Field(default_factory=list)


class RegistryUpdateRequest(BaseModel):
    registry: dict[str, str] = Field(default_factory=dict)


class RoutingPolicyUpdateRequest(BaseModel):
    routing_policy: dict[str, Any]


def init_llm_management_service(service: LLMManagementService) -> None:
    """Initialize dependency service from app lifespan."""
    global _service
    _service = service


def _get_service() -> LLMManagementService:
    global _service
    if _service is None:
        _service = get_default_llm_management_service()
    return _service


@router.get("/overview")
async def get_llm_overview() -> dict[str, Any]:
    service = _get_service()
    return service.get_overview()


@router.post("/reload")
async def reload_llm_config() -> dict[str, Any]:
    service = _get_service()
    config = service.reload()
    return {"reloaded": True, "gateway": config.gateway.model_dump()}


@router.get("/providers")
async def get_providers() -> dict[str, Any]:
    service = _get_service()
    config = service.get_config()
    return {"providers": [provider.model_dump() for provider in config.providers]}


@router.put("/providers")
async def put_providers(payload: ProviderUpdateRequest) -> dict[str, Any]:
    service = _get_service()
    providers = service.update_providers(payload.providers)
    return {"providers": providers}


@router.get("/providers/runtime")
async def get_provider_runtime() -> dict[str, Any]:
    service = _get_service()
    return {"providers_runtime": service.get_provider_runtime()}


@router.get("/engines")
async def get_engines() -> dict[str, Any]:
    service = _get_service()
    config = service.get_config()
    return {"inference_engines": [engine.model_dump() for engine in config.inference_engines]}


@router.put("/engines")
async def put_engines(payload: EngineUpdateRequest) -> dict[str, Any]:
    service = _get_service()
    engines = service.update_engines(payload.inference_engines)
    return {"inference_engines": engines}


@router.get("/engines/status")
async def get_engine_status() -> dict[str, Any]:
    service = _get_service()
    statuses = await service.get_engine_statuses()
    return {"engine_status": statuses}


@router.get("/models")
async def get_models() -> dict[str, Any]:
    service = _get_service()
    config = service.get_config()
    return {"models": [model.model_dump() for model in config.models]}


@router.put("/models")
async def put_models(payload: ModelUpdateRequest) -> dict[str, Any]:
    service = _get_service()
    models = service.update_models(payload.models)
    return {"models": models}


@router.get("/registry")
async def get_registry() -> dict[str, Any]:
    service = _get_service()
    return {"registry": dict(service.get_config().registry)}


@router.put("/registry")
async def put_registry(payload: RegistryUpdateRequest) -> dict[str, Any]:
    service = _get_service()
    registry = service.update_registry(payload.registry)
    return {"registry": registry}


@router.get("/routing-policy")
async def get_routing_policy() -> dict[str, Any]:
    service = _get_service()
    return {"routing_policy": service.get_config().routing_policy.model_dump()}


@router.put("/routing-policy")
async def put_routing_policy(payload: RoutingPolicyUpdateRequest) -> dict[str, Any]:
    service = _get_service()
    policy = service.update_routing_policy(payload.routing_policy)
    return {"routing_policy": policy}


@router.get("/cost-summary")
async def get_cost_summary() -> dict[str, Any]:
    service = _get_service()
    return service.get_cost_summary()
