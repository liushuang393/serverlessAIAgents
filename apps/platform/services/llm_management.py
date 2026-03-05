"""LLM Management service for Platform."""

from __future__ import annotations

from pathlib import Path

from agentflow.llm.gateway import (
    InferenceEngineConfig,
    LLMGatewayConfig,
    LiteLLMGateway,
    ModelConfig,
    ProviderConfig,
    RoutingPolicyConfig,
    build_provider_runtime_statuses,
    load_gateway_config,
)
from apps.platform.schemas.llm_management_schemas import (
    LLMCatalogResponse,
    LLMCostSummary,
    LLMDiagnosticsResponse,
    LLMEngineRuntimeStatusPayload,
    LLMInferenceEngineConfigPayload,
    LLMGatewayRuntimePayload,
    LLMManagementOverviewResponse,
    LLMModelConfigPayload,
    LLMPreflightReport,
    LLMPreflightRequest,
    LLMProviderConfigPayload,
    LLMProviderRuntimeStatusPayload,
    LLMRoutingPolicyConfigPayload,
    LLMSetupAndSwitchRequest,
    LLMSetupAndSwitchResponse,
    LLMSwitchRequest,
    LLMSwitchResponse,
)
from apps.platform.services.llm_management_catalog import LLMCatalogService
from apps.platform.services.llm_management_config_store import LLMConfigStore
from apps.platform.services.llm_management_setup_manager import LLMSetupManager
from apps.platform.services.llm_management_switch_service import LLMSwitchService
from apps.platform.services.llm_management_validator import LLMConfigValidator


class LLMManagementService:
    """Read/update/validate LLM gateway configuration."""

    def __init__(self, config_path: Path | None = None) -> None:
        self._config_path = config_path or (Path.cwd() / ".agentflow" / "llm_gateway.yaml")
        self._store = LLMConfigStore(self._config_path)
        self._validator = LLMConfigValidator()
        self._catalog = LLMCatalogService()
        self._setup_manager = LLMSetupManager(config_path=self._config_path)
        self._switch_service = LLMSwitchService(
            config_store=self._store,
            validator=self._validator,
            config_path=self._config_path,
        )
        self._gateway = LiteLLMGateway(config_path=self._config_path)
        self._last_preflight: LLMPreflightReport | None = None

    @property
    def config_path(self) -> Path:
        """Return canonical gateway config path."""
        return self._config_path

    def reload(self) -> LLMGatewayConfig:
        """Reload gateway configuration from disk."""
        self._gateway.reload()
        return self._gateway.config

    def get_config(self) -> LLMGatewayConfig:
        """Return current gateway configuration."""
        return self._gateway.config

    def get_config_version(self) -> str | None:
        """Return current config version digest."""
        return self._store.version()

    def get_overview(self) -> LLMManagementOverviewResponse:
        """Return complete management payload for UI."""
        config = self.get_config()
        provider_statuses = build_provider_runtime_statuses(config, config_path=self._config_path)
        return LLMManagementOverviewResponse.model_validate(
            {
                "gateway": LLMGatewayRuntimePayload.model_validate(config.gateway.model_dump()),
                "providers": [provider.model_dump() for provider in config.providers],
                "providers_runtime": [status.model_dump() for status in provider_statuses],
                "inference_engines": [engine.model_dump() for engine in config.inference_engines],
                "models": [model.model_dump() for model in config.models],
                "registry": dict(config.registry),
                "routing_policy": config.routing_policy.model_dump(),
                "cost_summary": self._gateway.get_cost_summary(),
                "config_version": self.get_config_version(),
            }
        )

    def update_providers(self, providers: list[LLMProviderConfigPayload]) -> list[LLMProviderConfigPayload]:
        """Replace provider configurations."""
        config = self.get_config().model_copy(deep=True)
        config.providers = [
            ProviderConfig.model_validate(item.model_dump(mode="python"))
            for item in providers
        ]
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return [
            LLMProviderConfigPayload.model_validate(provider.model_dump())
            for provider in self.get_config().providers
        ]

    def update_engines(
        self,
        engines: list[LLMInferenceEngineConfigPayload],
    ) -> list[LLMInferenceEngineConfigPayload]:
        """Replace inference engine configurations."""
        config = self.get_config().model_copy(deep=True)
        config.inference_engines = [
            InferenceEngineConfig.model_validate(item.model_dump(mode="python"))
            for item in engines
        ]
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return [
            LLMInferenceEngineConfigPayload.model_validate(engine.model_dump())
            for engine in self.get_config().inference_engines
        ]

    def update_models(self, models: list[LLMModelConfigPayload]) -> list[LLMModelConfigPayload]:
        """Replace model alias configurations."""
        config = self.get_config().model_copy(deep=True)
        config.models = [ModelConfig.model_validate(item.model_dump(mode="python")) for item in models]
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return [LLMModelConfigPayload.model_validate(model.model_dump()) for model in self.get_config().models]

    def update_registry(self, registry: dict[str, str]) -> dict[str, str]:
        """Replace role->model registry mapping."""
        config = self.get_config().model_copy(deep=True)
        normalized = {
            str(role).strip().lower(): str(alias).strip().lower()
            for role, alias in registry.items()
        }
        config.registry = normalized
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return dict(self.get_config().registry)

    def update_routing_policy(
        self,
        routing_policy: LLMRoutingPolicyConfigPayload,
    ) -> LLMRoutingPolicyConfigPayload:
        """Update routing policy config."""
        config = self.get_config().model_copy(deep=True)
        config.routing_policy = RoutingPolicyConfig.model_validate(routing_policy.model_dump(mode="python"))
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return LLMRoutingPolicyConfigPayload.model_validate(self.get_config().routing_policy.model_dump())

    async def get_engine_statuses(self) -> list[LLMEngineRuntimeStatusPayload]:
        """Return runtime statuses for configured inference engines."""
        statuses = await self._gateway.get_engine_statuses()
        return [LLMEngineRuntimeStatusPayload.model_validate(status.model_dump()) for status in statuses]

    def get_provider_runtime(self) -> list[LLMProviderRuntimeStatusPayload]:
        """Return provider availability statuses."""
        statuses = build_provider_runtime_statuses(self.get_config(), config_path=self._config_path)
        return [LLMProviderRuntimeStatusPayload.model_validate(status.model_dump()) for status in statuses]

    def get_cost_summary(self) -> LLMCostSummary:
        """Return gateway cost-tracking summary."""
        return LLMCostSummary.model_validate(self._gateway.get_cost_summary())

    def get_catalog(self) -> LLMCatalogResponse:
        """Return provider/model/backend catalog metadata."""
        return self._catalog.build()

    async def preflight(self, request: LLMPreflightRequest) -> LLMPreflightReport:
        """Run setup preflight workflow."""
        config = self._store.load()
        report = await self._setup_manager.preflight(request, config)
        self._last_preflight = report
        return report

    async def switch(self, request: LLMSwitchRequest) -> LLMSwitchResponse:
        """Apply atomic switch workflow."""
        result = await self._switch_service.switch(request)
        self.reload()
        return result

    async def setup_and_switch(
        self,
        request: LLMSetupAndSwitchRequest,
    ) -> LLMSetupAndSwitchResponse:
        """Run setup preflight and then switch when preflight is successful."""
        preflight_report = await self.preflight(request.preflight)
        if preflight_report.status in {"failed", "dry_run"}:
            return LLMSetupAndSwitchResponse(
                preflight=preflight_report,
                switch=None,
                success=False,
                message="preflight did not reach executable success; switch skipped",
            )

        switch_response = await self.switch(request.switch)
        return LLMSetupAndSwitchResponse(
            preflight=preflight_report,
            switch=switch_response,
            success=switch_response.success,
            message=(
                "setup and switch completed"
                if switch_response.success
                else "switch failed after preflight"
            ),
        )

    def get_diagnostics(
        self,
        *,
        has_llm_routes: bool,
        route_count: int,
    ) -> LLMDiagnosticsResponse:
        """Return diagnostics summary for route/config visibility."""
        config_exists = self._store.exists()
        version = self._store.version()
        hints: list[str] = []
        if not has_llm_routes:
            hints.append("LLM routes are missing. Restart platform backend with latest code.")
        if not config_exists:
            hints.append("Gateway config file is missing. Reload endpoint will recreate default config.")
        if self._last_preflight is not None and self._last_preflight.status == "failed":
            hints.append("Last preflight failed. Resolve remediation hints before switching.")
        if not hints:
            hints.append("LLM management routes and config are healthy.")

        return LLMDiagnosticsResponse(
            has_llm_routes=has_llm_routes,
            route_count=route_count,
            config_path=str(self._config_path),
            config_exists=config_exists,
            config_version=version,
            last_preflight=self._last_preflight,
            hints=hints,
        )


def get_default_llm_management_service() -> LLMManagementService:
    """Create management service from canonical config path."""
    config_path = Path.cwd() / ".agentflow" / "llm_gateway.yaml"
    if not config_path.exists():
        load_gateway_config(config_path)
    return LLMManagementService(config_path=config_path)
