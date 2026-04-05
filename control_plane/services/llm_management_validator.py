"""LLM Gateway 設定の正規化と検証."""

from __future__ import annotations

from typing import TYPE_CHECKING
from urllib.parse import urlparse


if TYPE_CHECKING:
    from collections.abc import Iterable

    from infrastructure.llm.gateway import LLMGatewayConfig, ModelConfig, ProviderConfig


_CANONICAL_PROVIDER_MAP: dict[str, str] = {
    "gemini": "google",
    "ollama": "local",
}

_SUPPORTED_PROVIDERS: set[str] = {
    "openai",
    "anthropic",
    "google",
    "huggingface",
    "azure_openai",
    "openrouter",
    "deepseek",
    "custom",
    "local",
}


class LLMConfigValidationError(ValueError):
    """Raised when gateway config violates cross-field constraints."""


class LLMConfigValidator:
    """Apply canonicalization and cross-field validation rules."""

    @staticmethod
    def canonical_provider_name(name: str) -> str:
        normalized = name.strip().lower()
        return _CANONICAL_PROVIDER_MAP.get(normalized, normalized)

    def prepare_config(self, config: LLMGatewayConfig) -> LLMGatewayConfig:
        """Return canonicalized copy ready for persistence."""
        prepared = config.model_copy(deep=True)
        prepared.providers = self._canonicalize_providers(prepared.providers)
        prepared.models = self._canonicalize_models(prepared.models)
        prepared.registry = {
            str(role).strip().lower(): str(alias).strip().lower() for role, alias in prepared.registry.items()
        }
        prepared.routing_policy.fallback_chain = {
            str(role).strip().lower(): [str(alias).strip().lower() for alias in aliases]
            for role, aliases in prepared.routing_policy.fallback_chain.items()
        }
        self.validate_or_raise(prepared)
        return prepared

    def validate_or_raise(self, config: LLMGatewayConfig) -> None:
        """Raise LLMConfigValidationError if config is invalid."""
        errors = self.validate(config)
        if errors:
            raise LLMConfigValidationError("; ".join(errors))

    def validate(self, config: LLMGatewayConfig) -> list[str]:
        """Validate cross-field constraints and return errors."""
        errors: list[str] = []

        provider_names = [self.canonical_provider_name(item.name) for item in config.providers]
        if len(provider_names) != len(set(provider_names)):
            errors.append("provider 名は正規化後に一意である必要があります。")

        unknown_providers = sorted(name for name in provider_names if name not in _SUPPORTED_PROVIDERS)
        if unknown_providers:
            errors.append(f"未対応 provider があります: {', '.join(unknown_providers)}")

        engine_names = [engine.name.strip().lower() for engine in config.inference_engines]
        if len(engine_names) != len(set(engine_names)):
            errors.append("engine 名は一意である必要があります。")

        host_ports = [engine.host_port for engine in config.inference_engines if engine.host_port is not None]
        if len(host_ports) != len(set(host_ports)):
            errors.append("engine の host_port は重複できません。")

        engine_name_set = set(engine_names)
        for engine in config.inference_engines:
            if engine.host_port is None:
                continue
            parsed = urlparse(engine.base_url)
            port = parsed.port
            if port is not None and port != engine.host_port:
                errors.append(
                    f"engine '{engine.name}' の base_url port ({port}) と host_port ({engine.host_port}) が一致しません。"
                )

        model_aliases: list[str] = [item.alias.strip().lower() for item in config.models]
        if len(model_aliases) != len(set(model_aliases)):
            errors.append("model alias は一意である必要があります。")
        model_ids: list[str] = [str(item.model_id or item.alias).strip().lower() for item in config.models]
        if len(model_ids) != len(set(model_ids)):
            errors.append("model_id は一意である必要があります。")
        alias_to_model = {item.alias.strip().lower(): item for item in config.models}
        enabled_aliases = {alias for alias, model in alias_to_model.items() if model.enabled}

        providers_set = set(provider_names)
        for model in config.models:
            provider_name = self.canonical_provider_name(model.provider)
            if provider_name not in providers_set and provider_name not in _SUPPORTED_PROVIDERS:
                errors.append(
                    f"model '{model.alias}' が未定義 provider '{model.provider}' を参照しています。",
                )
            if model.engine:
                normalized_engine_name = model.engine.strip().lower()
                if normalized_engine_name not in engine_name_set:
                    errors.append(f"model '{model.alias}' が存在しない engine '{model.engine}' を参照しています。")

        for role, alias in config.registry.items():
            normalized_alias = alias.strip().lower()
            if normalized_alias not in alias_to_model:
                errors.append(f"registry の role '{role}' が存在しない alias '{alias}' を参照しています。")
                continue
            if normalized_alias not in enabled_aliases:
                errors.append(f"registry の role '{role}' が無効な alias '{alias}' を参照しています。")

        for role, aliases in config.routing_policy.fallback_chain.items():
            for alias in aliases:
                normalized_alias = alias.strip().lower()
                if normalized_alias not in alias_to_model:
                    errors.append(f"fallback role '{role}' に存在しない alias '{alias}' が含まれています。")
                    continue
                if normalized_alias not in enabled_aliases:
                    errors.append(f"fallback role '{role}' に無効な alias '{alias}' が含まれています。")

        return errors

    def _canonicalize_providers(self, providers: Iterable[ProviderConfig]) -> list[ProviderConfig]:
        normalized: list[ProviderConfig] = []
        for provider in providers:
            normalized_name = self.canonical_provider_name(provider.name)
            normalized.append(provider.model_copy(update={"name": normalized_name}))
        return normalized

    def _canonicalize_models(self, models: Iterable[ModelConfig]) -> list[ModelConfig]:
        normalized: list[ModelConfig] = []
        for model in models:
            original_provider = model.provider.strip().lower()
            normalized_provider = self.canonical_provider_name(model.provider)
            updates: dict[str, str] = {
                "alias": model.alias.strip().lower(),
                "provider": normalized_provider,
            }
            # 後方互換: provider="ollama" → provider="local", engine="ollama"
            if original_provider == "ollama" and model.engine is None:
                updates["engine"] = "ollama"
            normalized.append(model.model_copy(update=updates))
        return normalized


def provider_default_api_key_env(provider: str) -> str | None:
    """Return conventional API key env name for provider."""
    mapping: dict[str, str] = {
        "openai": "OPENAI_API_KEY",
        "anthropic": "ANTHROPIC_API_KEY",
        "google": "GEMINI_API_KEY",
        "huggingface": "HF_TOKEN",
        "azure_openai": "AZURE_OPENAI_API_KEY",
        "openrouter": "OPENROUTER_API_KEY",
        "deepseek": "DEEPSEEK_API_KEY",
    }
    return mapping.get(LLMConfigValidator.canonical_provider_name(provider))


def provider_default_api_base(provider: str) -> str | None:
    """Return default API base for known providers."""
    mapping: dict[str, str] = {
        "openai": "https://api.openai.com/v1",
        "anthropic": "https://api.anthropic.com",
        "google": "https://generativelanguage.googleapis.com",
        "huggingface": "https://router.huggingface.co/v1",
        "azure_openai": "https://{resource}.openai.azure.com",
        "openrouter": "https://openrouter.ai/api/v1",
        "deepseek": "https://api.deepseek.com/v1",
        "local": "http://127.0.0.1:18001/v1",
    }
    return mapping.get(LLMConfigValidator.canonical_provider_name(provider))
