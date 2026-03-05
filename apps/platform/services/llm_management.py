"""LLM Management service for Platform."""

from __future__ import annotations

from pathlib import Path
from typing import Any

from agentflow.llm.gateway import (
    InferenceEngineConfig,
    LLMGatewayConfig,
    LiteLLMGateway,
    ModelConfig,
    ProviderConfig,
    RoutingPolicyConfig,
    build_provider_runtime_statuses,
    load_gateway_config,
    save_gateway_config,
)


class LLMManagementService:
    """Read/update/validate LLM gateway configuration."""

    def __init__(self, config_path: Path | None = None) -> None:
        self._config_path = config_path
        self._gateway = LiteLLMGateway(config_path=config_path)

    def reload(self) -> LLMGatewayConfig:
        """Reload gateway configuration from disk."""
        self._gateway.reload()
        return self._gateway.config

    def get_config(self) -> LLMGatewayConfig:
        """Return current gateway configuration."""
        return self._gateway.config

    def get_overview(self) -> dict[str, Any]:
        """Return complete management payload for UI."""
        config = self.get_config()
        provider_statuses = build_provider_runtime_statuses(config, config_path=self._config_path)
        return {
            "gateway": config.gateway.model_dump(),
            "providers": [provider.model_dump() for provider in config.providers],
            "providers_runtime": [status.model_dump() for status in provider_statuses],
            "inference_engines": [engine.model_dump() for engine in config.inference_engines],
            "models": [model.model_dump() for model in config.models],
            "registry": dict(config.registry),
            "routing_policy": config.routing_policy.model_dump(),
            "cost_summary": self._gateway.get_cost_summary(),
        }

    def update_providers(self, providers: list[dict[str, Any]]) -> list[dict[str, Any]]:
        """Replace provider configurations."""
        config = self.get_config()
        config.providers = [ProviderConfig.model_validate(item) for item in providers]
        save_gateway_config(config, self._config_path)
        self.reload()
        return [provider.model_dump() for provider in self.get_config().providers]

    def update_engines(self, engines: list[dict[str, Any]]) -> list[dict[str, Any]]:
        """Replace inference engine configurations."""
        config = self.get_config()
        config.inference_engines = [InferenceEngineConfig.model_validate(item) for item in engines]
        save_gateway_config(config, self._config_path)
        self.reload()
        return [engine.model_dump() for engine in self.get_config().inference_engines]

    def update_models(self, models: list[dict[str, Any]]) -> list[dict[str, Any]]:
        """Replace model alias configurations."""
        config = self.get_config()
        config.models = [ModelConfig.model_validate(item) for item in models]
        save_gateway_config(config, self._config_path)
        self.reload()
        return [model.model_dump() for model in self.get_config().models]

    def update_registry(self, registry: dict[str, str]) -> dict[str, str]:
        """Replace role->model registry mapping."""
        config = self.get_config()
        normalized = {str(role).strip().lower(): str(alias).strip().lower() for role, alias in registry.items()}
        config.registry = normalized
        save_gateway_config(config, self._config_path)
        self.reload()
        return dict(self.get_config().registry)

    def update_routing_policy(self, routing_policy: dict[str, Any]) -> dict[str, Any]:
        """Update routing policy config."""
        config = self.get_config()
        config.routing_policy = RoutingPolicyConfig.model_validate(routing_policy)
        save_gateway_config(config, self._config_path)
        self.reload()
        return self.get_config().routing_policy.model_dump()

    async def get_engine_statuses(self) -> list[dict[str, Any]]:
        """Return runtime statuses for configured inference engines."""
        statuses = await self._gateway.get_engine_statuses()
        return [status.model_dump() for status in statuses]

    def get_provider_runtime(self) -> list[dict[str, Any]]:
        """Return provider availability statuses."""
        statuses = build_provider_runtime_statuses(self.get_config(), config_path=self._config_path)
        return [status.model_dump() for status in statuses]

    def get_cost_summary(self) -> dict[str, Any]:
        """Return gateway cost-tracking summary."""
        return self._gateway.get_cost_summary()


def get_default_llm_management_service() -> LLMManagementService:
    """Create management service from canonical config path."""
    config_path = Path.cwd() / ".agentflow" / "llm_gateway.yaml"
    if not config_path.exists():
        load_gateway_config(config_path)
    return LLMManagementService(config_path=config_path)
