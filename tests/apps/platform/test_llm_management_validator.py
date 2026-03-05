"""Unit tests for LLM config validator."""

from __future__ import annotations

from agentflow.llm.gateway import LLMGatewayConfig, ModelConfig, ProviderConfig, RoutingPolicyConfig
from apps.platform.services.llm_management_validator import LLMConfigValidationError, LLMConfigValidator


def _base_config() -> LLMGatewayConfig:
    return LLMGatewayConfig(
        providers=[
            ProviderConfig(name="openai", api_key_env="OPENAI_API_KEY", enabled=True),
            ProviderConfig(name="gemini", api_key_env="GEMINI_API_KEY", enabled=True),
        ],
        models=[
            ModelConfig(alias="reasoning_main", provider="gemini", model="gemini-2.0-flash", enabled=True),
            ModelConfig(alias="coding_main", provider="openai", model="gpt-4o-mini", enabled=True),
        ],
        registry={"reasoning": "reasoning_main"},
        routing_policy=RoutingPolicyConfig(
            fallback_chain={"reasoning": ["coding_main"]},
        ),
    )


def test_prepare_config_canonicalizes_provider_names() -> None:
    validator = LLMConfigValidator()
    prepared = validator.prepare_config(_base_config())
    provider_names = {provider.name for provider in prepared.providers}
    assert "google" in provider_names
    assert "gemini" not in provider_names
    assert prepared.models[0].provider == "google"


def test_validate_reports_registry_and_fallback_errors() -> None:
    validator = LLMConfigValidator()
    config = _base_config().model_copy(deep=True)
    config.models[1].enabled = False
    config.registry["coding"] = "missing_alias"
    config.routing_policy.fallback_chain["reasoning"] = ["coding_main"]

    errors = validator.validate(config)
    assert any("missing alias" in item for item in errors)
    assert any("disabled alias" in item for item in errors)


def test_validate_or_raise_rejects_unknown_provider() -> None:
    validator = LLMConfigValidator()
    config = _base_config().model_copy(deep=True)
    config.models[0].provider = "unknown_provider"

    try:
        validator.validate_or_raise(config)
    except LLMConfigValidationError as exc:
        assert "unknown provider" in str(exc)
    else:
        raise AssertionError("expected LLMConfigValidationError")
