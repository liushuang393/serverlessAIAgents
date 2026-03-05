"""LiteLLM gateway configuration models and persistence helpers.

Gateway canonical file: `.agentflow/llm_gateway.yaml`
Priority: environment variables > `.env` > YAML defaults.
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import Any, Literal

import yaml
from pydantic import BaseModel, Field, field_validator


_GATEWAY_RELATIVE_PATH = Path(".agentflow") / "llm_gateway.yaml"
_DOTENV_RELATIVE_PATH = Path(".env")

_AVAILABLE_STATUS = Literal["available", "unavailable"]


class ProviderConfig(BaseModel):
    """Model provider configuration."""

    name: str = Field(..., min_length=1)
    api_base: str | None = None
    api_key_env: str | None = None
    models: list[str] = Field(default_factory=list)
    enabled: bool = True

    @field_validator("name")
    @classmethod
    def _normalize_name(cls, value: str) -> str:
        return value.strip().lower()


class InferenceEngineConfig(BaseModel):
    """Local inference engine configuration."""

    name: str = Field(..., min_length=1)
    engine_type: Literal["vllm", "sglang", "tgi"]
    base_url: str
    health_path: str = "/health"
    metrics_path: str = "/metrics"
    model_list_path: str = "/v1/models"
    enabled: bool = True

    @field_validator("name")
    @classmethod
    def _normalize_name(cls, value: str) -> str:
        return value.strip().lower()


class ModelCostConfig(BaseModel):
    """Per-model cost estimation used for fallback and budget checks."""

    input_per_1k: float = Field(default=0.0, ge=0.0)
    output_per_1k: float = Field(default=0.0, ge=0.0)


class ModelConfig(BaseModel):
    """Gateway model alias definition."""

    alias: str = Field(..., min_length=1)
    provider: str = Field(..., min_length=1)
    model: str = Field(..., min_length=1)
    api_base: str | None = None
    api_key_env: str | None = None
    engine: str | None = None
    enabled: bool = True
    modalities: list[str] = Field(default_factory=lambda: ["text"])
    quality_score: float = Field(default=0.5, ge=0.0, le=1.0)
    avg_latency_ms: float = Field(default=800.0, ge=0.0)
    cost: ModelCostConfig = Field(default_factory=ModelCostConfig)

    @field_validator("alias", "provider")
    @classmethod
    def _normalize_lower(cls, value: str) -> str:
        return value.strip().lower()


class RoutingPolicyConfig(BaseModel):
    """Routing policy for the gateway."""

    priority: Literal["latency", "cost", "quality"] = "latency"
    fallback_chain: dict[str, list[str]] = Field(default_factory=dict)
    load_balance_strategy: Literal["round_robin", "least_latency", "random"] = "round_robin"
    cost_budget: float | None = Field(default=None, ge=0.0)


class GatewayRuntimeConfig(BaseModel):
    """Gateway runtime behavior options."""

    default_role: str = "reasoning"
    request_timeout_seconds: int = Field(default=120, gt=0)
    max_retries: int = Field(default=2, ge=0, le=5)


class LLMGatewayConfig(BaseModel):
    """Top-level gateway configuration."""

    gateway: GatewayRuntimeConfig = Field(default_factory=GatewayRuntimeConfig)
    providers: list[ProviderConfig] = Field(default_factory=list)
    inference_engines: list[InferenceEngineConfig] = Field(default_factory=list)
    models: list[ModelConfig] = Field(default_factory=list)
    registry: dict[str, str] = Field(default_factory=dict)
    routing_policy: RoutingPolicyConfig = Field(default_factory=RoutingPolicyConfig)


class ProviderRuntimeStatus(BaseModel):
    """Provider availability status for platform UI."""

    name: str
    status: _AVAILABLE_STATUS
    api_key_env: str | None = None
    source: str | None = None
    last_error: str | None = None


class EngineRuntimeStatus(BaseModel):
    """Inference engine runtime status for platform UI."""

    name: str
    engine_type: Literal["vllm", "sglang", "tgi"]
    status: _AVAILABLE_STATUS
    latency_ms: float | None = None
    gpu_usage: float | None = None
    loaded_models: list[str] = Field(default_factory=list)
    last_error: str | None = None


def _load_dotenv_values(dotenv_path: Path) -> dict[str, str]:
    if not dotenv_path.is_file():
        return {}

    values: dict[str, str] = {}
    for raw_line in dotenv_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        key = key.strip()
        if not key:
            continue
        values[key] = value.strip().strip('"').strip("'")
    return values


def _default_gateway_config() -> LLMGatewayConfig:
    return LLMGatewayConfig(
        providers=[
            ProviderConfig(name="openai", api_base="https://api.openai.com/v1", api_key_env="OPENAI_API_KEY", enabled=True),
            ProviderConfig(
                name="anthropic",
                api_base="https://api.anthropic.com",
                api_key_env="ANTHROPIC_API_KEY",
                enabled=True,
            ),
            ProviderConfig(name="google", api_base="https://generativelanguage.googleapis.com", api_key_env="GEMINI_API_KEY", enabled=True),
            ProviderConfig(name="local", api_base="http://127.0.0.1:4000", api_key_env=None, enabled=True),
        ],
        inference_engines=[
            InferenceEngineConfig(name="vllm", engine_type="vllm", base_url="http://127.0.0.1:8001", enabled=False),
            InferenceEngineConfig(name="sglang", engine_type="sglang", base_url="http://127.0.0.1:30000", enabled=False),
            InferenceEngineConfig(name="tgi", engine_type="tgi", base_url="http://127.0.0.1:8080", enabled=False),
        ],
        models=[
            ModelConfig(alias="reasoning_claude", provider="anthropic", model="claude-sonnet-4-20250514", modalities=["text", "tool_call"], quality_score=0.95, avg_latency_ms=1200.0, cost=ModelCostConfig(input_per_1k=0.003, output_per_1k=0.015)),
            ModelConfig(alias="coding_openai", provider="openai", model="gpt-4o", modalities=["text", "tool_call"], quality_score=0.9, avg_latency_ms=900.0, cost=ModelCostConfig(input_per_1k=0.005, output_per_1k=0.015)),
            ModelConfig(alias="cheap_gemini", provider="google", model="gemini-2.0-flash", modalities=["text", "tool_call"], quality_score=0.75, avg_latency_ms=600.0, cost=ModelCostConfig(input_per_1k=0.0002, output_per_1k=0.0008)),
            ModelConfig(alias="local_qwen2", provider="local", model="qwen2", api_base="http://127.0.0.1:8001/v1", modalities=["text", "tool_call", "embedding", "audio", "image"], quality_score=0.65, avg_latency_ms=500.0, cost=ModelCostConfig(input_per_1k=0.0, output_per_1k=0.0)),
        ],
        registry={
            "reasoning": "reasoning_claude",
            "coding": "coding_openai",
            "cheap": "cheap_gemini",
            "local": "local_qwen2",
        },
        routing_policy=RoutingPolicyConfig(
            priority="latency",
            fallback_chain={
                "reasoning": ["coding_openai", "cheap_gemini", "local_qwen2"],
                "coding": ["reasoning_claude", "cheap_gemini", "local_qwen2"],
                "cheap": ["coding_openai", "local_qwen2"],
                "local": ["cheap_gemini"],
            },
            load_balance_strategy="round_robin",
            cost_budget=None,
        ),
    )


def _resolve_config_path(config_path: Path | None = None) -> Path:
    if config_path is not None:
        return config_path
    return Path.cwd() / _GATEWAY_RELATIVE_PATH


def _resolve_dotenv_path(config_path: Path) -> Path:
    return config_path.parent.parent / _DOTENV_RELATIVE_PATH


def _apply_env_overrides(config: LLMGatewayConfig) -> LLMGatewayConfig:
    if priority := os.getenv("LLM_GATEWAY_PRIORITY"):
        config.routing_policy.priority = priority.strip().lower()  # type: ignore[assignment]
    if strategy := os.getenv("LLM_GATEWAY_LOAD_BALANCE"):
        config.routing_policy.load_balance_strategy = strategy.strip().lower()  # type: ignore[assignment]
    if role := os.getenv("LLM_GATEWAY_DEFAULT_ROLE"):
        config.gateway.default_role = role.strip().lower()
    if budget := os.getenv("LLM_GATEWAY_COST_BUDGET"):
        try:
            config.routing_policy.cost_budget = float(budget)
        except ValueError:
            config.routing_policy.cost_budget = None

    for role_name in list(config.registry.keys()):
        env_key = f"LLM_ROLE_{role_name.upper()}_ALIAS"
        if alias := os.getenv(env_key):
            config.registry[role_name] = alias.strip().lower()

    for model in config.models:
        env_key = f"LLM_MODEL_{model.alias.upper()}_NAME"
        if model_name := os.getenv(env_key):
            model.model = model_name.strip()

    return config


def load_gateway_config(config_path: Path | None = None) -> LLMGatewayConfig:
    """Load gateway configuration from YAML with defaults and ENV overrides."""
    resolved_path = _resolve_config_path(config_path)
    if not resolved_path.is_file():
        default = _default_gateway_config()
        save_gateway_config(default, resolved_path)
        return _apply_env_overrides(default)

    loaded = yaml.safe_load(resolved_path.read_text(encoding="utf-8"))
    payload = loaded if isinstance(loaded, dict) else {}
    config = LLMGatewayConfig.model_validate(payload)
    return _apply_env_overrides(config)


def save_gateway_config(config: LLMGatewayConfig, config_path: Path | None = None) -> Path:
    """Persist gateway configuration to YAML."""
    resolved_path = _resolve_config_path(config_path)
    resolved_path.parent.mkdir(parents=True, exist_ok=True)
    yaml.safe_dump(config.model_dump(mode="python"), resolved_path.open("w", encoding="utf-8"), sort_keys=False)
    return resolved_path


def resolve_secret(
    env_name: str | None,
    *,
    config_path: Path | None = None,
) -> tuple[str | None, str | None]:
    """Resolve secret by ENV first, then `.env` file.

    Returns:
        (value, source) where source is `ENV`, `.env`, or None.
    """
    if env_name is None or not env_name.strip():
        return None, None

    normalized = env_name.strip()
    if value := os.getenv(normalized):
        return value, "ENV"

    path = _resolve_config_path(config_path)
    dotenv_values = _load_dotenv_values(_resolve_dotenv_path(path))
    value = dotenv_values.get(normalized)
    if value:
        return value, ".env"
    return None, None


def build_provider_runtime_statuses(
    config: LLMGatewayConfig,
    *,
    config_path: Path | None = None,
) -> list[ProviderRuntimeStatus]:
    """Build provider availability status list."""
    statuses: list[ProviderRuntimeStatus] = []
    for provider in config.providers:
        if not provider.enabled:
            statuses.append(
                ProviderRuntimeStatus(
                    name=provider.name,
                    status="unavailable",
                    api_key_env=provider.api_key_env,
                    source=None,
                    last_error="disabled",
                )
            )
            continue

        if provider.api_key_env is None:
            statuses.append(
                ProviderRuntimeStatus(
                    name=provider.name,
                    status="available",
                    api_key_env=None,
                    source="N/A",
                    last_error=None,
                )
            )
            continue

        secret, source = resolve_secret(provider.api_key_env, config_path=config_path)
        statuses.append(
            ProviderRuntimeStatus(
                name=provider.name,
                status="available" if secret else "unavailable",
                api_key_env=provider.api_key_env,
                source=source,
                last_error=None if secret else "api_key_not_configured",
            )
        )
    return statuses


__all__ = [
    "EngineRuntimeStatus",
    "GatewayRuntimeConfig",
    "InferenceEngineConfig",
    "LLMGatewayConfig",
    "ModelConfig",
    "ModelCostConfig",
    "ProviderConfig",
    "ProviderRuntimeStatus",
    "RoutingPolicyConfig",
    "build_provider_runtime_statuses",
    "load_gateway_config",
    "resolve_secret",
    "save_gateway_config",
]
