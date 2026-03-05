"""Validation and canonicalization for LLM gateway config."""

from __future__ import annotations

from typing import Iterable

from agentflow.llm.gateway import LLMGatewayConfig, ModelConfig, ProviderConfig


_CANONICAL_PROVIDER_MAP: dict[str, str] = {
    "gemini": "google",
}

_SUPPORTED_PROVIDERS: set[str] = {
    "openai",
    "anthropic",
    "google",
    "ollama",
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
            str(role).strip().lower(): str(alias).strip().lower()
            for role, alias in prepared.registry.items()
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
            errors.append("provider names must be unique after canonicalization")

        unknown_providers = sorted(name for name in provider_names if name not in _SUPPORTED_PROVIDERS)
        if unknown_providers:
            errors.append(f"unsupported provider(s): {', '.join(unknown_providers)}")

        model_aliases: list[str] = [item.alias.strip().lower() for item in config.models]
        if len(model_aliases) != len(set(model_aliases)):
            errors.append("model aliases must be unique")
        alias_to_model = {item.alias.strip().lower(): item for item in config.models}
        enabled_aliases = {alias for alias, model in alias_to_model.items() if model.enabled}

        providers_set = set(provider_names)
        for model in config.models:
            provider_name = self.canonical_provider_name(model.provider)
            if provider_name not in providers_set and provider_name not in _SUPPORTED_PROVIDERS:
                errors.append(
                    f"model '{model.alias}' uses unknown provider '{model.provider}'",
                )

        for role, alias in config.registry.items():
            normalized_alias = alias.strip().lower()
            if normalized_alias not in alias_to_model:
                errors.append(f"registry role '{role}' points to missing alias '{alias}'")
                continue
            if normalized_alias not in enabled_aliases:
                errors.append(f"registry role '{role}' points to disabled alias '{alias}'")

        for role, aliases in config.routing_policy.fallback_chain.items():
            for alias in aliases:
                normalized_alias = alias.strip().lower()
                if normalized_alias not in alias_to_model:
                    errors.append(f"fallback role '{role}' contains missing alias '{alias}'")
                    continue
                if normalized_alias not in enabled_aliases:
                    errors.append(f"fallback role '{role}' contains disabled alias '{alias}'")

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
            normalized_provider = self.canonical_provider_name(model.provider)
            normalized.append(
                model.model_copy(
                    update={
                        "alias": model.alias.strip().lower(),
                        "provider": normalized_provider,
                    }
                )
            )
        return normalized


def provider_default_api_key_env(provider: str) -> str | None:
    """Return conventional API key env name for provider."""
    mapping: dict[str, str] = {
        "openai": "OPENAI_API_KEY",
        "anthropic": "ANTHROPIC_API_KEY",
        "google": "GEMINI_API_KEY",
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
        "azure_openai": "https://{resource}.openai.azure.com",
        "openrouter": "https://openrouter.ai/api/v1",
        "deepseek": "https://api.deepseek.com/v1",
        "ollama": "http://127.0.0.1:11434/v1",
        "local": "http://127.0.0.1:4000/v1",
    }
    return mapping.get(LLMConfigValidator.canonical_provider_name(provider))
