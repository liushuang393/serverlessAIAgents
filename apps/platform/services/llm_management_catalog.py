"""Catalog generator for providers/models/backends used by LLM management."""

from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, get_args

from agentflow.llm.models import MODELS
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

_BACKEND_METADATA: dict[LLMBackendKind, dict[str, Any]] = {
    LLMBackendKind.VLLM: {
        "display_name": "vLLM",
        "base_url": "http://127.0.0.1:8001",
        "health_path": "/health",
        "install": [["python", "-m", "pip", "install", "vllm"]],
        "start": [["docker", "compose", "up", "-d"]],
    },
    LLMBackendKind.SGLANG: {
        "display_name": "SGLang",
        "base_url": "http://127.0.0.1:30000",
        "health_path": "/health",
        "install": [["python", "-m", "pip", "install", "sglang"]],
        "start": [["docker", "compose", "up", "-d"]],
    },
    LLMBackendKind.TGI: {
        "display_name": "TGI",
        "base_url": "http://127.0.0.1:8080",
        "health_path": "/health",
        "install": [["docker", "pull", "ghcr.io/huggingface/text-generation-inference:latest"]],
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
            generated_at=datetime.now(timezone.utc).isoformat(),
        )

    def _provider_catalog(self) -> list[LLMCatalogProvider]:
        provider_values = self._provider_values_from_provisioning()
        result: list[LLMCatalogProvider] = []
        for raw in provider_values:
            if raw == "auto":
                continue
            provider_name = "google" if raw == "gemini" else raw
            if provider_name not in {item.value for item in LLMProviderKind}:
                continue
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
                    provider=info.provider,
                    model=info.name,
                    capabilities=capability_values,
                    context_window=info.context_window,
                    recommended_for=sorted(set(recommended_for)),
                )
            )
        return sorted(models, key=lambda item: (item.provider, item.model))

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
        values = [str(item) for item in literals if isinstance(item, str)]
        return values
