"""Schemas for Platform LLM Management APIs."""

from __future__ import annotations

from datetime import UTC, datetime
from enum import StrEnum
from typing import Literal

from pydantic import BaseModel, Field, field_validator


class LLMProviderKind(StrEnum):
    """Supported provider identifiers for management APIs."""

    OPENAI = "openai"
    ANTHROPIC = "anthropic"
    GOOGLE = "google"
    HUGGINGFACE = "huggingface"
    AZURE_OPENAI = "azure_openai"
    OPENROUTER = "openrouter"
    DEEPSEEK = "deepseek"
    CUSTOM = "custom"
    LOCAL = "local"


class LLMBackendKind(StrEnum):
    """Supported backend identifiers for management APIs."""

    NONE = "none"
    VLLM = "vllm"
    SGLANG = "sglang"
    TGI = "tgi"
    OLLAMA = "ollama"


class LLMProviderSecretStatusPayload(BaseModel):
    """Provider secret の表示状態."""

    configured: bool = False
    masked: str | None = None
    source: str = "unavailable"
    available: bool = False
    last_error: str | None = None


class LLMProviderConfigPayload(BaseModel):
    """Typed provider payload used by PUT /providers."""

    name: str = Field(..., min_length=1)
    api_base: str | None = None
    api_key_env: str | None = None
    models: list[str] = Field(default_factory=list)
    enabled: bool = True
    secret_status: LLMProviderSecretStatusPayload = Field(default_factory=LLMProviderSecretStatusPayload)

    @field_validator("name")
    @classmethod
    def normalize_name(cls, value: str) -> str:
        return value.strip().lower()


class LLMProviderRuntimeStatusPayload(BaseModel):
    """Provider runtime status payload."""

    name: str
    status: Literal["available", "unavailable"]
    api_key_env: str | None = None
    source: str | None = None
    masked: str | None = None
    last_error: str | None = None


class LLMInferenceEngineConfigPayload(BaseModel):
    """Typed inference engine payload used by PUT /engines."""

    name: str = Field(..., min_length=1)
    engine_type: Literal["vllm", "sglang", "tgi", "ollama"]
    base_url: str = Field(..., min_length=1)
    health_path: str = "/health"
    metrics_path: str = "/metrics"
    model_list_path: str = "/v1/models"
    enabled: bool = True
    deployment_mode: Literal["manual", "docker", "native"] = "manual"
    docker_image: str | None = None
    served_model_name: str | None = None
    container_name: str | None = None
    host_port: int | None = Field(default=None, ge=1, le=65535)
    public_base_url: str | None = None
    gpu_enabled: bool = False
    gpu_devices: list[str] = Field(default_factory=list)
    gpu_count: int | None = Field(default=None, ge=1)
    extra_env: dict[str, str] = Field(default_factory=dict)
    deployment_status: str | None = None
    deployment_error: str | None = None
    compose_path: str | None = None

    @field_validator("name")
    @classmethod
    def normalize_name(cls, value: str) -> str:
        return value.strip().lower()


class LLMEngineRuntimeStatusPayload(BaseModel):
    """Engine runtime status payload."""

    name: str
    engine_type: Literal["vllm", "sglang", "tgi", "ollama"]
    status: Literal["available", "unavailable"]
    latency_ms: float | None = None
    gpu_usage: float | None = None
    loaded_models: list[str] = Field(default_factory=list)
    last_error: str | None = None


class LLMModelCostConfigPayload(BaseModel):
    """Cost payload used by PUT /models."""

    input_per_1k: float = Field(default=0.0, ge=0.0)
    output_per_1k: float = Field(default=0.0, ge=0.0)


class LLMModelConfigPayload(BaseModel):
    """Typed model payload used by PUT /models."""

    alias: str = Field(..., min_length=1)
    model_id: str | None = None
    provider: str = Field(..., min_length=1)
    model: str = Field(..., min_length=1)
    model_type: Literal["text", "embedding", "image", "speech_to_text", "text_to_speech"] = "text"
    api_base: str | None = None
    api_key_env: str | None = None
    engine: str | None = None
    enabled: bool = True
    modalities: list[str] = Field(default_factory=lambda: ["text"])
    quality_score: float = Field(default=0.5, ge=0.0, le=1.0)
    avg_latency_ms: float = Field(default=800.0, ge=0.0)
    cost: LLMModelCostConfigPayload = Field(default_factory=LLMModelCostConfigPayload)

    @field_validator("alias", "provider")
    @classmethod
    def normalize_lower(cls, value: str) -> str:
        return value.strip().lower()


class LLMRoutingPolicyConfigPayload(BaseModel):
    """Typed routing policy payload used by PUT /routing-policy."""

    priority: Literal["latency", "cost", "quality"] = "latency"
    fallback_chain: dict[str, list[str]] = Field(default_factory=dict)
    load_balance_strategy: Literal["round_robin", "least_latency", "random"] = "round_robin"
    cost_budget: float | None = Field(default=None, ge=0.0)


class LLMGatewayRuntimePayload(BaseModel):
    """Gateway runtime options payload used in overview responses."""

    default_role: str
    request_timeout_seconds: int
    max_retries: int


class LLMCostSummaryDetail(BaseModel):
    """Per-model cost summary detail row."""

    alias: str
    requests: int
    failures: int
    avg_latency_ms: float
    cost_usd: float


class LLMCostSummary(BaseModel):
    """Aggregate cost summary payload."""

    total_cost_usd: float
    details: list[LLMCostSummaryDetail] = Field(default_factory=list)
    cost_budget: float | None = None
    budget_exceeded: bool = False


class LLMManagementOverviewResponse(BaseModel):
    """Typed response for GET /overview."""

    gateway: LLMGatewayRuntimePayload
    providers: list[LLMProviderConfigPayload] = Field(default_factory=list)
    providers_runtime: list[LLMProviderRuntimeStatusPayload] = Field(default_factory=list)
    inference_engines: list[LLMInferenceEngineConfigPayload] = Field(default_factory=list)
    models: list[LLMModelConfigPayload] = Field(default_factory=list)
    registry: dict[str, str] = Field(default_factory=dict)
    routing_policy: LLMRoutingPolicyConfigPayload
    cost_summary: LLMCostSummary
    config_version: str | None = None


class ProviderUpdateRequest(BaseModel):
    """PUT /providers payload."""

    providers: list[LLMProviderConfigPayload] = Field(default_factory=list)


class EngineUpdateRequest(BaseModel):
    """PUT /engines payload."""

    inference_engines: list[LLMInferenceEngineConfigPayload] = Field(default_factory=list)


class ModelUpdateRequest(BaseModel):
    """PUT /models payload."""

    models: list[LLMModelConfigPayload] = Field(default_factory=list)


class RegistryUpdateRequest(BaseModel):
    """PUT /registry payload."""

    registry: dict[str, str] = Field(default_factory=dict)


class RoutingPolicyUpdateRequest(BaseModel):
    """PUT /routing-policy payload."""

    routing_policy: LLMRoutingPolicyConfigPayload


class LLMProvidersResponse(BaseModel):
    """Response payload for provider config endpoints."""

    providers: list[LLMProviderConfigPayload] = Field(default_factory=list)


class LLMProvidersRuntimeResponse(BaseModel):
    """Response payload for provider runtime endpoint."""

    providers_runtime: list[LLMProviderRuntimeStatusPayload] = Field(default_factory=list)


class LLMEnginesResponse(BaseModel):
    """Response payload for inference engine config endpoints."""

    inference_engines: list[LLMInferenceEngineConfigPayload] = Field(default_factory=list)


class LLMProviderSecretUpdateRequest(BaseModel):
    """Provider secret 保存リクエスト."""

    api_key_env: str | None = None
    secret_value: str = Field(..., min_length=1)


class LLMProviderSecretResponse(BaseModel):
    """Provider secret 保存/削除応答."""

    provider: str
    secret_status: LLMProviderSecretStatusPayload


class LLMEngineStatusResponse(BaseModel):
    """Response payload for inference engine status endpoint."""

    engine_status: list[LLMEngineRuntimeStatusPayload] = Field(default_factory=list)


class LLMEngineDeployRequest(BaseModel):
    """Engine deploy リクエスト."""

    public_base_url: str | None = None


class LLMEngineDeployResponse(BaseModel):
    """Engine deploy/stop 応答."""

    success: bool
    engine: LLMInferenceEngineConfigPayload
    message: str
    command: LLMSetupCommandResult | None = None


class LLMModelsResponse(BaseModel):
    """Response payload for model alias config endpoints."""

    models: list[LLMModelConfigPayload] = Field(default_factory=list)


class LLMRegistryResponse(BaseModel):
    """Response payload for registry endpoint."""

    registry: dict[str, str] = Field(default_factory=dict)


class LLMRoutingPolicyResponse(BaseModel):
    """Response payload for routing policy endpoint."""

    routing_policy: LLMRoutingPolicyConfigPayload


class LLMReloadResponse(BaseModel):
    """Response payload for POST /reload."""

    reloaded: bool = True
    gateway: LLMGatewayRuntimePayload


class LLMCatalogProvider(BaseModel):
    """Provider metadata for management catalog."""

    name: LLMProviderKind
    canonical_name: str
    aliases: list[str] = Field(default_factory=list)
    requires_api_key: bool = True
    default_api_key_env: str | None = None
    default_api_base: str | None = None
    recommended_models: list[str] = Field(default_factory=list)
    install_recipes: list[list[str]] = Field(default_factory=list)
    category: Literal["cloud", "local"] = "cloud"
    local_engines: list[LLMBackendKind] = Field(default_factory=list)


class LLMCatalogModel(BaseModel):
    """Model metadata for management catalog."""

    alias: str
    model_id: str | None = None
    provider: str
    model: str
    model_type: Literal["text", "embedding", "image", "speech_to_text", "text_to_speech"] = "text"
    capabilities: list[str] = Field(default_factory=list)
    context_window: int
    recommended_for: list[str] = Field(default_factory=list)


class LLMCatalogBackend(BaseModel):
    """Backend metadata for management catalog."""

    name: LLMBackendKind
    display_name: str
    default_base_url: str
    default_health_path: str
    install_recipes: list[list[str]] = Field(default_factory=list)
    start_recipes: list[list[str]] = Field(default_factory=list)


class LLMCatalogResponse(BaseModel):
    """Response for GET /catalog."""

    providers: list[LLMCatalogProvider] = Field(default_factory=list)
    backends: list[LLMCatalogBackend] = Field(default_factory=list)
    models: list[LLMCatalogModel] = Field(default_factory=list)
    generated_at: str = Field(default_factory=lambda: datetime.now(UTC).isoformat())


class LLMPreflightRequest(BaseModel):
    """Preflight request for setup/install/start/health."""

    providers: list[LLMProviderKind] = Field(default_factory=list)
    backends: list[LLMBackendKind] = Field(default_factory=list)
    auto_install: bool = True
    auto_start: bool = True
    health_check: bool = True
    dry_run: bool = False


class LLMSetupCommandResult(BaseModel):
    """Command execution result used by preflight report."""

    command: list[str] = Field(default_factory=list)
    cwd: str | None = None
    return_code: int | None = None
    stdout: str = ""
    stderr: str = ""
    timed_out: bool = False
    allowed: bool = True
    error: str | None = None


class LLMPreflightStep(BaseModel):
    """Single preflight step result."""

    category: Literal["provider", "backend"]
    target: str
    phase: Literal["detect", "install", "start", "health", "validate"]
    status: Literal["success", "failed", "skipped", "dry_run"]
    message: str
    command: LLMSetupCommandResult | None = None
    remediation: list[str] = Field(default_factory=list)


class LLMPreflightReport(BaseModel):
    """Structured preflight report."""

    status: Literal["success", "failed", "partial", "dry_run"]
    started_at: str = Field(default_factory=lambda: datetime.now(UTC).isoformat())
    completed_at: str = Field(default_factory=lambda: datetime.now(UTC).isoformat())
    request: LLMPreflightRequest
    steps: list[LLMPreflightStep] = Field(default_factory=list)
    summary: str = ""


class LLMSwitchRequest(BaseModel):
    """Atomic switch request."""

    provider: LLMProviderKind
    model: str = Field(..., min_length=1)
    backend: LLMBackendKind = LLMBackendKind.NONE
    roles: list[str] = Field(
        default_factory=lambda: ["reasoning", "coding", "cheap", "local"],
    )
    model_alias: str | None = None
    auto_enable_provider: bool = True
    update_fallback_chain: bool = True
    validate_runtime: bool = True

    @field_validator("roles", mode="before")
    @classmethod
    def normalize_roles(cls, value: list[str] | None) -> list[str]:
        if not value:
            return ["reasoning"]
        normalized = [str(item).strip().lower() for item in value if str(item).strip()]
        return normalized or ["reasoning"]


class LLMSwitchDiffItem(BaseModel):
    """Single diff entry for switch response."""

    field: str
    before: str | None = None
    after: str | None = None


class LLMSwitchRuntimeCheck(BaseModel):
    """Runtime check summary after applying switch."""

    provider_status: str | None = None
    backend_status: str | None = None
    model_status: str | None = None
    errors: list[str] = Field(default_factory=list)


class LLMSwitchResponse(BaseModel):
    """Response for switch endpoints."""

    success: bool
    rolled_back: bool = False
    config_version: str | None = None
    applied_alias: str | None = None
    registry: dict[str, str] = Field(default_factory=dict)
    diffs: list[LLMSwitchDiffItem] = Field(default_factory=list)
    runtime_check: LLMSwitchRuntimeCheck = Field(default_factory=LLMSwitchRuntimeCheck)
    message: str = ""


class LLMSetupAndSwitchRequest(BaseModel):
    """Combined request for setup + switch."""

    preflight: LLMPreflightRequest
    switch: LLMSwitchRequest


class LLMSetupAndSwitchResponse(BaseModel):
    """Combined response for setup + switch."""

    preflight: LLMPreflightReport
    switch: LLMSwitchResponse | None = None
    success: bool
    message: str


class LLMDiagnosticsResponse(BaseModel):
    """Diagnostics response for route/config visibility."""

    has_llm_routes: bool
    route_count: int = 0
    config_path: str
    config_exists: bool
    config_version: str | None = None
    last_preflight: LLMPreflightReport | None = None
    hints: list[str] = Field(default_factory=list)
    server_time: str = Field(default_factory=lambda: datetime.now(UTC).isoformat())
