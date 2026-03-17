"""Catalog generator for providers/models/backends used by LLM management."""

from __future__ import annotations

from datetime import UTC, datetime
from typing import Any, get_args

from infrastructure.llm.gateway import load_gateway_config
from infrastructure.llm.models import MODELS
from apps.platform.schemas.llm_management_schemas import (
    LLMBackendKind,
    LLMCatalogBackend,
    LLMCatalogModel,
    LLMCatalogProvider,
    LLMCatalogResponse,
    LLMProviderKind,
)
from apps.platform.schemas.provisioning_schemas import AppCreateRequest
from apps.platform.services.llm_management_validator import (
    provider_default_api_base,
    provider_default_api_key_env,
)


_PROVIDER_ALIASES: dict[str, list[str]] = {
    "google": ["gemini"],
}

_PROVIDER_INSTALL_RECIPES: dict[str, list[list[str]]] = {
    "ollama": [["ollama", "--version"]],
    "openai": [],
    "anthropic": [],
    "google": [],
    "azure_openai": [],
    "openrouter": [],
    "deepseek": [],
    "custom": [],
    "local": [],
}

_PROVIDER_PRIORITY_MODELS: dict[str, list[str]] = {
    "openai": ["gpt-5.2", "gpt-5-mini", "gpt-5-nano"],
    "anthropic": ["claude-opus-4-6", "claude-sonnet-4-6", "claude-haiku-4-5"],
    "google": ["gemini-3.1-pro-preview", "gemini-3-flash-preview", "gemini-3.1-flash-lite-preview"],
    "ollama": ["llama3.3:70b", "qwen2.5:72b", "qwen2.5-coder:32b"],
    "local": ["Qwen/Qwen2.5-0.5B-Instruct"],
}

_BACKEND_METADATA: dict[LLMBackendKind, dict[str, Any]] = {
    LLMBackendKind.VLLM: {
        "display_name": "vLLM",
        "base_url": "http://127.0.0.1:18001",
        "health_path": "/health",
        "install": [["python", "-m", "pip", "install", "vllm"]],
        "start": [["docker", "compose", "up", "-d"]],
    },
    LLMBackendKind.SGLANG: {
        "display_name": "SGLang",
        "base_url": "http://127.0.0.1:18002",
        "health_path": "/health",
        "install": [["python", "-m", "pip", "install", "sglang"]],
        "start": [["docker", "compose", "up", "-d"]],
    },
    LLMBackendKind.TGI: {
        "display_name": "TGI",
        "base_url": "http://127.0.0.1:18003",
        "health_path": "/health",
        "install": [["docker", "pull", "ghcr.io/huggingface/text-generation-inference:3.3.7"]],
        "start": [["docker", "compose", "up", "-d"]],
    },
}


class LLMCatalogService:
    """Build provider/model/backend catalog for UI and automation."""

    def build(self) -> LLMCatalogResponse:
        providers = self._provider_catalog()
        models = self._model_catalog()
        backends = self._backend_catalog()
        return LLMCatalogResponse(
            providers=providers,
            models=models,
            backends=backends,
            generated_at=datetime.now(UTC).isoformat(),
        )

    def _provider_catalog(self) -> list[LLMCatalogProvider]:
        provider_values = self._provider_values_from_provisioning()
        provider_values.extend(self._provider_values_from_gateway())
        result: list[LLMCatalogProvider] = []
        seen_provider_names: set[str] = set()
        for raw in sorted(set(provider_values)):
            if raw == "auto":
                continue
            provider_name = "google" if raw == "gemini" else raw
            if provider_name not in {item.value for item in LLMProviderKind}:
                continue
            if provider_name in seen_provider_names:
                continue
            seen_provider_names.add(provider_name)
            kind = LLMProviderKind(provider_name)
            result.append(
                LLMCatalogProvider(
                    name=kind,
                    canonical_name=provider_name,
                    aliases=_PROVIDER_ALIASES.get(provider_name, []),
                    requires_api_key=provider_name not in {"ollama", "local", "custom"},
                    default_api_key_env=provider_default_api_key_env(provider_name),
                    default_api_base=provider_default_api_base(provider_name),
                    recommended_models=self._recommended_models(provider_name),
                    install_recipes=_PROVIDER_INSTALL_RECIPES.get(provider_name, []),
                )
            )
        return sorted(result, key=lambda item: item.name.value)

    def _model_catalog(self) -> list[LLMCatalogModel]:
        models: list[LLMCatalogModel] = []
        seen_ids: set[str] = set()
        for alias, info in MODELS.items():
            recommended_for: list[str] = []
            capability_values = [cap.value for cap in info.capabilities]
            if "reasoning" in capability_values:
                recommended_for.append("reasoning")
            if "code" in capability_values:
                recommended_for.append("coding")
            if info.input_cost_per_1k <= 0.001 and info.output_cost_per_1k <= 0.002:
                recommended_for.append("cheap")
            if info.provider in {"ollama", "local"}:
                recommended_for.append("local")
            models.append(
                LLMCatalogModel(
                    alias=alias,
                    model_id=alias,
                    provider=info.provider,
                    model=info.name,
                    model_type=self._infer_model_type(capability_values),
                    capabilities=capability_values,
                    context_window=info.context_window,
                    recommended_for=sorted(set(recommended_for)),
                )
            )
            seen_ids.add(alias)

        gateway_config = load_gateway_config()
        for model in gateway_config.models:
            model_id = model.model_id or model.alias
            if model_id in seen_ids:
                continue
            models.append(
                LLMCatalogModel(
                    alias=model.alias,
                    model_id=model_id,
                    provider=model.provider,
                    model=model.model,
                    model_type=model.model_type,
                    capabilities=list(model.modalities),
                    context_window=0,
                    recommended_for=[],
                )
            )
            seen_ids.add(model_id)
        return sorted(models, key=lambda item: (item.provider, item.model))

    @staticmethod
    def _infer_model_type(capabilities: list[str]) -> str:
        if "embedding" in capabilities:
            return "embedding"
        if "image" in capabilities:
            return "image"
        if "speech_to_text" in capabilities:
            return "speech_to_text"
        if "text_to_speech" in capabilities:
            return "text_to_speech"
        return "text"

    def _backend_catalog(self) -> list[LLMCatalogBackend]:
        backends: list[LLMCatalogBackend] = []
        for backend, meta in _BACKEND_METADATA.items():
            backends.append(
                LLMCatalogBackend(
                    name=backend,
                    display_name=str(meta["display_name"]),
                    default_base_url=str(meta["base_url"]),
                    default_health_path=str(meta["health_path"]),
                    install_recipes=list(meta.get("install", [])),
                    start_recipes=list(meta.get("start", [])),
                )
            )
        return sorted(backends, key=lambda item: item.name.value)

    def _recommended_models(self, provider_name: str) -> list[str]:
        recommended: list[str] = []
        for model_name in _PROVIDER_PRIORITY_MODELS.get(provider_name, []):
            if model_name not in recommended:
                recommended.append(model_name)

        gateway_config = load_gateway_config()
        for model in gateway_config.models:
            if model.provider != provider_name:
                continue
            if model.model not in recommended:
                recommended.append(model.model)

        for info in MODELS.values():
            if info.provider != provider_name:
                continue
            if info.name not in recommended:
                recommended.append(info.name)
            if len(recommended) >= 8:
                break
        return recommended

    @staticmethod
    def _provider_values_from_provisioning() -> list[str]:
        field = AppCreateRequest.model_fields.get("llm_provider")
        if field is None:
            return []
        annotation = field.annotation
        literals = get_args(annotation)
        return [str(item) for item in literals if isinstance(item, str)]

    @staticmethod
    def _provider_values_from_gateway() -> list[str]:
        config = load_gateway_config()
        return [provider.name for provider in config.providers]
