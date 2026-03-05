"""LLM Management router."""

from __future__ import annotations

from fastapi import APIRouter, Request

from apps.platform.schemas.llm_management_schemas import (
    EngineUpdateRequest,
    LLMCatalogResponse,
    LLMCostSummary,
    LLMDiagnosticsResponse,
    LLMEngineStatusResponse,
    LLMEnginesResponse,
    LLMGatewayRuntimePayload,
    LLMManagementOverviewResponse,
    LLMModelsResponse,
    LLMPreflightReport,
    LLMPreflightRequest,
    LLMProvidersResponse,
    LLMProvidersRuntimeResponse,
    LLMRegistryResponse,
    LLMReloadResponse,
    LLMRoutingPolicyResponse,
    LLMSetupAndSwitchRequest,
    LLMSetupAndSwitchResponse,
    LLMSwitchRequest,
    LLMSwitchResponse,
    ModelUpdateRequest,
    ProviderUpdateRequest,
    RegistryUpdateRequest,
    RoutingPolicyUpdateRequest,
)
from apps.platform.services.llm_management import (
    LLMManagementService,
    get_default_llm_management_service,
)


router = APIRouter(prefix="/api/studios/framework/llm", tags=["llm-management"])

_service: LLMManagementService | None = None


def init_llm_management_service(service: LLMManagementService) -> None:
    """Initialize dependency service from app lifespan."""
    global _service
    _service = service


def _get_service() -> LLMManagementService:
    global _service
    if _service is None:
        _service = get_default_llm_management_service()
    return _service


@router.get("/overview", response_model=LLMManagementOverviewResponse)
async def get_llm_overview() -> LLMManagementOverviewResponse:
    service = _get_service()
    return service.get_overview()


@router.post("/reload", response_model=LLMReloadResponse)
async def reload_llm_config() -> LLMReloadResponse:
    service = _get_service()
    config = service.reload()
    return LLMReloadResponse(
        reloaded=True,
        gateway=LLMGatewayRuntimePayload.model_validate(config.gateway.model_dump()),
    )


@router.get("/providers", response_model=LLMProvidersResponse)
async def get_providers() -> LLMProvidersResponse:
    service = _get_service()
    overview = service.get_overview()
    return LLMProvidersResponse(providers=overview.providers)


@router.put("/providers", response_model=LLMProvidersResponse)
async def put_providers(payload: ProviderUpdateRequest) -> LLMProvidersResponse:
    service = _get_service()
    providers = service.update_providers(payload.providers)
    return LLMProvidersResponse(providers=providers)


@router.get("/providers/runtime", response_model=LLMProvidersRuntimeResponse)
async def get_provider_runtime() -> LLMProvidersRuntimeResponse:
    service = _get_service()
    return LLMProvidersRuntimeResponse(providers_runtime=service.get_provider_runtime())


@router.get("/engines", response_model=LLMEnginesResponse)
async def get_engines() -> LLMEnginesResponse:
    service = _get_service()
    overview = service.get_overview()
    return LLMEnginesResponse(inference_engines=overview.inference_engines)


@router.put("/engines", response_model=LLMEnginesResponse)
async def put_engines(payload: EngineUpdateRequest) -> LLMEnginesResponse:
    service = _get_service()
    engines = service.update_engines(payload.inference_engines)
    return LLMEnginesResponse(inference_engines=engines)


@router.get("/engines/status", response_model=LLMEngineStatusResponse)
async def get_engine_status() -> LLMEngineStatusResponse:
    service = _get_service()
    statuses = await service.get_engine_statuses()
    return LLMEngineStatusResponse(engine_status=statuses)


@router.get("/models", response_model=LLMModelsResponse)
async def get_models() -> LLMModelsResponse:
    service = _get_service()
    overview = service.get_overview()
    return LLMModelsResponse(models=overview.models)


@router.put("/models", response_model=LLMModelsResponse)
async def put_models(payload: ModelUpdateRequest) -> LLMModelsResponse:
    service = _get_service()
    models = service.update_models(payload.models)
    return LLMModelsResponse(models=models)


@router.get("/registry", response_model=LLMRegistryResponse)
async def get_registry() -> LLMRegistryResponse:
    service = _get_service()
    return LLMRegistryResponse(registry=dict(service.get_config().registry))


@router.put("/registry", response_model=LLMRegistryResponse)
async def put_registry(payload: RegistryUpdateRequest) -> LLMRegistryResponse:
    service = _get_service()
    registry = service.update_registry(payload.registry)
    return LLMRegistryResponse(registry=registry)


@router.get("/routing-policy", response_model=LLMRoutingPolicyResponse)
async def get_routing_policy() -> LLMRoutingPolicyResponse:
    service = _get_service()
    return LLMRoutingPolicyResponse(
        routing_policy=service.get_overview().routing_policy,
    )


@router.put("/routing-policy", response_model=LLMRoutingPolicyResponse)
async def put_routing_policy(payload: RoutingPolicyUpdateRequest) -> LLMRoutingPolicyResponse:
    service = _get_service()
    policy = service.update_routing_policy(payload.routing_policy)
    return LLMRoutingPolicyResponse(routing_policy=policy)


@router.get("/cost-summary", response_model=LLMCostSummary)
async def get_cost_summary() -> LLMCostSummary:
    service = _get_service()
    return service.get_cost_summary()


@router.get("/catalog", response_model=LLMCatalogResponse)
async def get_catalog() -> LLMCatalogResponse:
    service = _get_service()
    return service.get_catalog()


@router.post("/preflight", response_model=LLMPreflightReport)
async def run_preflight(payload: LLMPreflightRequest) -> LLMPreflightReport:
    service = _get_service()
    return await service.preflight(payload)


@router.post("/switch", response_model=LLMSwitchResponse)
async def switch_llm(payload: LLMSwitchRequest) -> LLMSwitchResponse:
    service = _get_service()
    return await service.switch(payload)


@router.post("/setup-and-switch", response_model=LLMSetupAndSwitchResponse)
async def setup_and_switch(payload: LLMSetupAndSwitchRequest) -> LLMSetupAndSwitchResponse:
    service = _get_service()
    return await service.setup_and_switch(payload)


@router.get("/diagnostics", response_model=LLMDiagnosticsResponse)
async def get_diagnostics(request: Request) -> LLMDiagnosticsResponse:
    service = _get_service()
    llm_routes = [
        route
        for route in request.app.routes
        if str(getattr(route, "path", "")).startswith("/api/studios/framework/llm")
    ]
    return service.get_diagnostics(
        has_llm_routes=bool(llm_routes),
        route_count=len(llm_routes),
    )
