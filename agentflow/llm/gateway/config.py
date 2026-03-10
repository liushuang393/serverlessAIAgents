"""LiteLLM gateway configuration models and persistence helpers.

Gateway canonical file: `.agentflow/llm_gateway.yaml`
Priority: Platform encrypted secret > environment variables > `.env` > YAML defaults.
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import Any, Literal

import yaml
from pydantic import BaseModel, Field, field_validator, model_validator


_GATEWAY_RELATIVE_PATH = Path(".agentflow") / "llm_gateway.yaml"
_DOTENV_RELATIVE_PATH = Path(".env")

_AVAILABLE_STATUS = Literal["available", "unavailable"]
_MODEL_TYPE = Literal["text", "embedding", "image", "speech_to_text", "text_to_speech"]


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
    deployment_mode: Literal["manual", "docker"] = "manual"
    docker_image: str | None = None
    served_model_name: str | None = None
    container_name: str | None = None
    host_port: int | None = None
    public_base_url: str | None = None
    gpu_enabled: bool = False
    gpu_devices: list[str] = Field(default_factory=list)
    gpu_count: int | None = None
    extra_env: dict[str, str] = Field(default_factory=dict)

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
    model_id: str | None = None
    provider: str = Field(..., min_length=1)
    model: str = Field(..., min_length=1)
    model_type: _MODEL_TYPE = "text"
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

    @field_validator("model_id", mode="before")
    @classmethod
    def _normalize_model_id(cls, value: str | None) -> str | None:
        if value is None:
            return None
        text = value.strip().lower()
        return text or None

    @model_validator(mode="after")
    def _ensure_model_identity(self) -> ModelConfig:
        if self.model_id is None:
            self.model_id = self.alias
        if self.model_type == "text":
            modalities = {item.strip().lower() for item in self.modalities}
            if "embedding" in modalities and modalities == {"embedding"}:
                self.model_type = "embedding"
            elif "image" in modalities and modalities == {"image"}:
                self.model_type = "image"
            elif "speech_to_text" in modalities and modalities == {"speech_to_text"}:
                self.model_type = "speech_to_text"
            elif "text_to_speech" in modalities and modalities == {"text_to_speech"}:
                self.model_type = "text_to_speech"
        return self


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
    masked: str | None = None
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
            ProviderConfig(
                name="openai", api_base="https://api.openai.com/v1", api_key_env="OPENAI_API_KEY", enabled=True
            ),
            ProviderConfig(
                name="anthropic",
                api_base="https://api.anthropic.com",
                api_key_env="ANTHROPIC_API_KEY",
                enabled=True,
            ),
            ProviderConfig(
                name="google",
                api_base="https://generativelanguage.googleapis.com",
                api_key_env="GEMINI_API_KEY",
                enabled=True,
            ),
            ProviderConfig(name="local", api_base="http://127.0.0.1:18001", api_key_env=None, enabled=True),
        ],
        inference_engines=[
            InferenceEngineConfig(
                name="vllm",
                engine_type="vllm",
                base_url="http://127.0.0.1:18001",
                enabled=False,
                deployment_mode="docker",
                docker_image="vllm/vllm-openai:v0.8.5",
                served_model_name="Qwen/Qwen2.5-0.5B-Instruct",
                container_name="llm-vllm",
                host_port=18001,
            ),
            InferenceEngineConfig(
                name="sglang",
                engine_type="sglang",
                base_url="http://127.0.0.1:18002",
                enabled=False,
                deployment_mode="docker",
                docker_image="lmsysorg/sglang:v0.4.9.post4-cu126",
                served_model_name="Qwen/Qwen2.5-0.5B-Instruct",
                container_name="llm-sglang",
                host_port=18002,
            ),
            InferenceEngineConfig(
                name="tgi",
                engine_type="tgi",
                base_url="http://127.0.0.1:18003",
                enabled=False,
                deployment_mode="docker",
                docker_image="ghcr.io/huggingface/text-generation-inference:3.3.7",
                served_model_name="Qwen/Qwen2.5-0.5B-Instruct",
                container_name="llm-tgi",
                host_port=18003,
            ),
        ],
        models=[
            ModelConfig(
                alias="reasoning_claude",
                model_id="reasoning_claude",
                provider="anthropic",
                model="claude-sonnet-4-6",
                model_type="text",
                modalities=["text", "tool_call"],
                quality_score=0.95,
                avg_latency_ms=1200.0,
                cost=ModelCostConfig(input_per_1k=0.003, output_per_1k=0.015),
            ),
            ModelConfig(
                alias="coding_openai",
                model_id="coding_openai",
                provider="openai",
                model="gpt-5.2",
                model_type="text",
                modalities=["text", "tool_call"],
                quality_score=0.9,
                avg_latency_ms=850.0,
                cost=ModelCostConfig(input_per_1k=0.0004, output_per_1k=0.0016),
            ),
            ModelConfig(
                alias="cheap_gemini",
                model_id="cheap_gemini",
                provider="google",
                model="gemini-3.1-flash-lite-preview",
                model_type="text",
                modalities=["text", "tool_call"],
                quality_score=0.8,
                avg_latency_ms=650.0,
                cost=ModelCostConfig(input_per_1k=0.0002, output_per_1k=0.0008),
            ),
            ModelConfig(
                alias="platform_text_default",
                model_id="platform_text_default",
                provider="openai",
                model="gpt-5-mini",
                model_type="text",
                modalities=["text", "tool_call"],
                quality_score=0.9,
                avg_latency_ms=850.0,
                cost=ModelCostConfig(input_per_1k=0.0004, output_per_1k=0.0016),
            ),
            ModelConfig(
                alias="platform_embedding_default",
                model_id="platform_embedding_default",
                provider="openai",
                model="text-embedding-3-small",
                model_type="embedding",
                modalities=["embedding"],
                quality_score=0.82,
                avg_latency_ms=350.0,
                cost=ModelCostConfig(input_per_1k=0.00002, output_per_1k=0.0),
            ),
            ModelConfig(
                alias="platform_image_default",
                model_id="platform_image_default",
                provider="openai",
                model="gpt-image-1",
                model_type="image",
                modalities=["image"],
                quality_score=0.8,
                avg_latency_ms=1800.0,
                cost=ModelCostConfig(input_per_1k=0.01, output_per_1k=0.0),
            ),
            ModelConfig(
                alias="platform_speech_to_text_default",
                model_id="platform_speech_to_text_default",
                provider="openai",
                model="gpt-4o-transcribe",
                model_type="speech_to_text",
                modalities=["speech_to_text"],
                quality_score=0.82,
                avg_latency_ms=900.0,
                cost=ModelCostConfig(input_per_1k=0.0, output_per_1k=0.0),
            ),
            ModelConfig(
                alias="platform_text_to_speech_default",
                model_id="platform_text_to_speech_default",
                provider="openai",
                model="gpt-4o-mini-tts",
                model_type="text_to_speech",
                modalities=["text_to_speech"],
                quality_score=0.78,
                avg_latency_ms=950.0,
                cost=ModelCostConfig(input_per_1k=0.0, output_per_1k=0.0),
            ),
            ModelConfig(
                alias="local_vllm_default",
                model_id="local_vllm_default",
                provider="local",
                model="Qwen/Qwen2.5-0.5B-Instruct",
                api_base="http://127.0.0.1:18001/v1",
                engine="vllm",
                model_type="text",
                modalities=["text", "tool_call"],
                quality_score=0.7,
                avg_latency_ms=550.0,
                cost=ModelCostConfig(input_per_1k=0.0, output_per_1k=0.0),
            ),
        ],
        registry={
            "reasoning": "reasoning_claude",
            "coding": "coding_openai",
            "cheap": "cheap_gemini",
            "local": "local_vllm_default",
        },
        routing_policy=RoutingPolicyConfig(
            priority="latency",
            fallback_chain={
                "reasoning": ["coding_openai", "cheap_gemini", "local_vllm_default"],
                "coding": ["reasoning_claude", "cheap_gemini", "local_vllm_default"],
                "cheap": ["coding_openai", "local_vllm_default"],
                "local": ["cheap_gemini"],
            },
            load_balance_strategy="round_robin",
            cost_budget=None,
        ),
    )


def _merge_provider_defaults(
    provider: ProviderConfig,
    default_provider: ProviderConfig,
) -> tuple[ProviderConfig, bool]:
    updates: dict[str, Any] = {}
    if provider.api_base is None and default_provider.api_base is not None:
        updates["api_base"] = default_provider.api_base
    if provider.api_key_env is None and default_provider.api_key_env is not None:
        updates["api_key_env"] = default_provider.api_key_env
    if not provider.models and default_provider.models:
        updates["models"] = list(default_provider.models)
    if not updates:
        return provider, False
    return provider.model_copy(update=updates), True


def _merge_engine_defaults(
    engine: InferenceEngineConfig,
    default_engine: InferenceEngineConfig,
) -> tuple[InferenceEngineConfig, bool]:
    updates: dict[str, Any] = {}
    if not engine.base_url:
        updates["base_url"] = default_engine.base_url
    if engine.health_path == "/health" and default_engine.health_path != "/health":
        updates["health_path"] = default_engine.health_path
    if engine.metrics_path == "/metrics" and default_engine.metrics_path != "/metrics":
        updates["metrics_path"] = default_engine.metrics_path
    if engine.model_list_path == "/v1/models" and default_engine.model_list_path != "/v1/models":
        updates["model_list_path"] = default_engine.model_list_path
    if engine.deployment_mode == "manual" and default_engine.deployment_mode != "manual":
        updates["deployment_mode"] = default_engine.deployment_mode
    if engine.docker_image is None and default_engine.docker_image is not None:
        updates["docker_image"] = default_engine.docker_image
    if engine.served_model_name is None and default_engine.served_model_name is not None:
        updates["served_model_name"] = default_engine.served_model_name
    if engine.container_name is None and default_engine.container_name is not None:
        updates["container_name"] = default_engine.container_name
    if engine.host_port is None and default_engine.host_port is not None:
        updates["host_port"] = default_engine.host_port
    if engine.public_base_url is None and default_engine.public_base_url is not None:
        updates["public_base_url"] = default_engine.public_base_url
    if not engine.gpu_enabled and default_engine.gpu_enabled:
        updates["gpu_enabled"] = default_engine.gpu_enabled
    if not engine.gpu_devices and default_engine.gpu_devices:
        updates["gpu_devices"] = list(default_engine.gpu_devices)
    if engine.gpu_count is None and default_engine.gpu_count is not None:
        updates["gpu_count"] = default_engine.gpu_count
    if not engine.extra_env and default_engine.extra_env:
        updates["extra_env"] = dict(default_engine.extra_env)
    if not updates:
        return engine, False
    return engine.model_copy(update=updates), True


def _merge_model_defaults(
    model: ModelConfig,
    default_model: ModelConfig,
) -> tuple[ModelConfig, bool]:
    updates: dict[str, Any] = {}
    if model.model_id is None and default_model.model_id is not None:
        updates["model_id"] = default_model.model_id
    if model.api_base is None and default_model.api_base is not None:
        updates["api_base"] = default_model.api_base
    if model.api_key_env is None and default_model.api_key_env is not None:
        updates["api_key_env"] = default_model.api_key_env
    if model.engine is None and default_model.engine is not None:
        updates["engine"] = default_model.engine
    if model.model_type == "text" and default_model.model_type != "text" and model.modalities == ["text"]:
        updates["model_type"] = default_model.model_type
        updates["modalities"] = list(default_model.modalities)
    if not updates:
        return model, False
    return model.model_copy(update=updates), True


def _merge_with_default_gateway_config(config: LLMGatewayConfig) -> tuple[LLMGatewayConfig, bool]:
    default_config = _default_gateway_config()
    merged = config.model_copy(deep=True)
    changed = False

    default_providers = {provider.name: provider for provider in default_config.providers}
    merged_providers: list[ProviderConfig] = []
    seen_provider_names: set[str] = set()
    for provider in merged.providers:
        default_provider = default_providers.get(provider.name)
        if default_provider is not None:
            provider, provider_changed = _merge_provider_defaults(provider, default_provider)
            changed = changed or provider_changed
        merged_providers.append(provider)
        seen_provider_names.add(provider.name)
    for default_provider in default_config.providers:
        if default_provider.name in seen_provider_names:
            continue
        merged_providers.append(default_provider.model_copy(deep=True))
        seen_provider_names.add(default_provider.name)
        changed = True
    merged.providers = merged_providers

    default_engines = {engine.name: engine for engine in default_config.inference_engines}
    merged_engines: list[InferenceEngineConfig] = []
    seen_engine_names: set[str] = set()
    for engine in merged.inference_engines:
        default_engine = default_engines.get(engine.name)
        if default_engine is not None:
            engine, engine_changed = _merge_engine_defaults(engine, default_engine)
            changed = changed or engine_changed
        merged_engines.append(engine)
        seen_engine_names.add(engine.name)
    for default_engine in default_config.inference_engines:
        if default_engine.name in seen_engine_names:
            continue
        merged_engines.append(default_engine.model_copy(deep=True))
        seen_engine_names.add(default_engine.name)
        changed = True
    merged.inference_engines = merged_engines

    default_models_by_alias = {model.alias: model for model in default_config.models}
    default_model_ids = {str(model.model_id or model.alias): model for model in default_config.models}
    merged_models: list[ModelConfig] = []
    seen_model_aliases: set[str] = set()
    seen_model_ids: set[str] = set()
    for model in merged.models:
        lookup_id = str(model.model_id or model.alias)
        default_model = default_models_by_alias.get(model.alias) or default_model_ids.get(lookup_id)
        if default_model is not None:
            model, model_changed = _merge_model_defaults(model, default_model)
            changed = changed or model_changed
        merged_models.append(model)
        seen_model_aliases.add(model.alias)
        seen_model_ids.add(str(model.model_id or model.alias))
    for default_model in default_config.models:
        default_model_id = str(default_model.model_id or default_model.alias)
        if default_model.alias in seen_model_aliases or default_model_id in seen_model_ids:
            continue
        merged_models.append(default_model.model_copy(deep=True))
        seen_model_aliases.add(default_model.alias)
        seen_model_ids.add(default_model_id)
        changed = True
    merged.models = merged_models

    merged_registry = dict(merged.registry)
    for role, alias in default_config.registry.items():
        if role not in merged_registry:
            merged_registry[role] = alias
            changed = True
    merged.registry = merged_registry

    merged_fallback_chain = dict(merged.routing_policy.fallback_chain)
    for role, aliases in default_config.routing_policy.fallback_chain.items():
        if role not in merged_fallback_chain:
            merged_fallback_chain[role] = list(aliases)
            changed = True
    merged.routing_policy.fallback_chain = merged_fallback_chain

    return merged, changed


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
    config, changed = _merge_with_default_gateway_config(config)
    if changed:
        save_gateway_config(config, resolved_path)
    return _apply_env_overrides(config)


def save_gateway_config(config: LLMGatewayConfig, config_path: Path | None = None) -> Path:
    """Persist gateway configuration to YAML."""
    resolved_path = _resolve_config_path(config_path)
    resolved_path.parent.mkdir(parents=True, exist_ok=True)
    yaml.safe_dump(config.model_dump(mode="python"), resolved_path.open("w", encoding="utf-8"), sort_keys=False)
    return resolved_path


# ---------------------------------------------------------------------------
# プラットフォーム暗号化シークレット解決コールバック
# agentflow → apps の逆依存を回避するため、コールバック登録方式を採用。
# apps/platform が起動時に register_platform_secret_resolver() を呼ぶ。
# ---------------------------------------------------------------------------
from collections.abc import Callable


PlatformSecretResolverT = Callable[[str], tuple[str | None, Any]]
_platform_secret_resolver: PlatformSecretResolverT | None = None


def register_platform_secret_resolver(
    resolver: PlatformSecretResolverT,
) -> None:
    """プラットフォーム暗号化シークレット解決コールバックを登録.

    apps/platform が起動時にこの関数を呼び出して、
    resolve_platform_cached_secret 相当の関数を注入する。

    Args:
        resolver: (provider_name) -> (secret_value, status_object)
    """
    global _platform_secret_resolver
    _platform_secret_resolver = resolver


def resolve_secret(
    env_name: str | None,
    *,
    provider_name: str | None = None,
    config_path: Path | None = None,
) -> tuple[str | None, str | None]:
    """Resolve secret by Platform encrypted secret first, then ENV, then `.env`.

    Returns:
        (value, source) where source is `platform_encrypted`, `ENV`, `.env`, or None.
    """
    if provider_name is not None and _platform_secret_resolver is not None:
        try:
            cached_secret, secret_status = _platform_secret_resolver(provider_name)
            if secret_status.configured:
                if cached_secret:
                    return cached_secret, secret_status.source
                return None, secret_status.source
        except Exception:
            pass

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


def resolve_secret_status(
    *,
    provider_name: str | None,
    env_name: str | None,
    config_path: Path | None = None,
) -> dict[str, Any]:
    """Provider secret 状態を返す."""
    if provider_name is not None and _platform_secret_resolver is not None:
        try:
            cached_secret, secret_status = _platform_secret_resolver(provider_name)
            if secret_status.configured:
                return {
                    "configured": secret_status.configured,
                    "masked": secret_status.masked,
                    "source": secret_status.source,
                    "available": bool(cached_secret),
                    "last_error": secret_status.last_error,
                }
        except Exception:
            pass

    if env_name is None or not env_name.strip():
        return {
            "configured": False,
            "masked": None,
            "source": "unavailable",
            "available": False,
            "last_error": "api_key_env_not_configured",
        }

    secret, source = resolve_secret(env_name, provider_name=None, config_path=config_path)
    if secret:
        masked = "*" * max(len(secret.strip()) - 4, 0) + secret.strip()[-4:]
        return {
            "configured": True,
            "masked": masked,
            "source": source or "ENV",
            "available": True,
            "last_error": None,
        }
    return {
        "configured": False,
        "masked": None,
        "source": "unavailable",
        "available": False,
        "last_error": "api_key_not_configured",
    }


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
            provider_name = provider.name.strip().lower()
            if provider_name == "local":
                enabled_models = [
                    model
                    for model in config.models
                    if model.enabled and model.provider.strip().lower() == provider_name
                ]
                if not enabled_models:
                    statuses.append(
                        ProviderRuntimeStatus(
                            name=provider.name,
                            status="unavailable",
                            api_key_env=None,
                            source="local",
                            masked=None,
                            last_error="no_enabled_local_models",
                        )
                    )
                    continue

                referenced_engines = sorted(
                    {
                        engine_name.strip().lower()
                        for model in enabled_models
                        if isinstance(model.engine, str) and model.engine.strip()
                        for engine_name in [model.engine]
                    }
                )
                if referenced_engines:
                    enabled_engine_names = {
                        engine.name.strip().lower() for engine in config.inference_engines if engine.enabled
                    }
                    missing_or_disabled = [
                        engine_name for engine_name in referenced_engines if engine_name not in enabled_engine_names
                    ]
                    if missing_or_disabled:
                        statuses.append(
                            ProviderRuntimeStatus(
                                name=provider.name,
                                status="unavailable",
                                api_key_env=None,
                                source=f"engine:{','.join(referenced_engines)}",
                                masked=None,
                                last_error=f"linked_engine_disabled:{','.join(missing_or_disabled)}",
                            )
                        )
                        continue

                    statuses.append(
                        ProviderRuntimeStatus(
                            name=provider.name,
                            status="available",
                            api_key_env=None,
                            source=f"engine:{','.join(referenced_engines)}",
                            masked=None,
                            last_error=None,
                        )
                    )
                    continue

                if provider.api_base:
                    statuses.append(
                        ProviderRuntimeStatus(
                            name=provider.name,
                            status="available",
                            api_key_env=None,
                            source="api_base",
                            masked=None,
                            last_error=None,
                        )
                    )
                    continue

                statuses.append(
                    ProviderRuntimeStatus(
                        name=provider.name,
                        status="unavailable",
                        api_key_env=None,
                        source="local",
                        masked=None,
                        last_error="local_api_base_missing",
                    )
                )
                continue

            statuses.append(
                ProviderRuntimeStatus(
                    name=provider.name,
                    status="available",
                    api_key_env=None,
                    source="N/A",
                    masked=None,
                    last_error=None,
                )
            )
            continue

        secret_status = resolve_secret_status(
            provider_name=provider.name,
            env_name=provider.api_key_env,
            config_path=config_path,
        )
        secret = None
        if secret_status["available"]:
            secret, _source = resolve_secret(
                provider.api_key_env,
                provider_name=provider.name,
                config_path=config_path,
            )
        statuses.append(
            ProviderRuntimeStatus(
                name=provider.name,
                status="available" if secret else "unavailable",
                api_key_env=provider.api_key_env,
                source=str(secret_status["source"]),
                masked=secret_status["masked"],
                last_error=None if secret else str(secret_status["last_error"]),
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
    "register_platform_secret_resolver",
    "resolve_secret",
    "resolve_secret_status",
    "save_gateway_config",
]
