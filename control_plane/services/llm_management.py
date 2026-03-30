"""Platform 向け LLM Management サービス."""

from __future__ import annotations

import json
from datetime import UTC, datetime
from pathlib import Path

from control_plane.schemas.llm_management_schemas import (
    LLMCatalogResponse,
    LLMCostSummary,
    LLMDiagnosticsResponse,
    LLMEngineDeployRequest,
    LLMEngineDeployResponse,
    LLMEngineRuntimeStatusPayload,
    LLMGatewayRuntimePayload,
    LLMInferenceEngineConfigPayload,
    LLMManagementOverviewResponse,
    LLMModelConfigPayload,
    LLMPreflightReport,
    LLMPreflightRequest,
    LLMProviderConfigPayload,
    LLMProviderRuntimeStatusPayload,
    LLMProviderSecretResponse,
    LLMProviderSecretStatusPayload,
    LLMProviderSecretUpdateRequest,
    LLMRoutingPolicyConfigPayload,
    LLMSetupAndSwitchRequest,
    LLMSetupAndSwitchResponse,
    LLMSwitchRequest,
    LLMSwitchResponse,
)
from control_plane.services.llm_management_catalog import LLMCatalogService
from control_plane.services.llm_management_config_store import LLMConfigStore
from control_plane.services.llm_management_persistence import (
    LLMManagementPersistence,
    PlatformEngineDeploymentRecord,
    resolve_platform_cached_secret,
)
from control_plane.services.llm_management_setup_manager import LLMSetupManager
from control_plane.services.llm_management_switch_service import LLMSwitchService
from control_plane.services.llm_management_validator import LLMConfigValidator
from control_plane.services.llm_runtime_status import resolve_provider_runtime_statuses
from infrastructure.llm.gateway import (
    InferenceEngineConfig,
    LiteLLMGateway,
    LLMGatewayConfig,
    ModelConfig,
    ProviderConfig,
    ProviderRuntimeStatus,
    RoutingPolicyConfig,
    load_gateway_config,
    resolve_secret_status,
)
from infrastructure.llm.gateway.config import register_platform_secret_resolver


class LLMManagementService:
    """Read/update/validate LLM gateway configuration."""

    def __init__(self, config_path: Path | None = None) -> None:
        self._config_path = config_path or _resolve_default_gateway_config_path()
        self._store = LLMConfigStore(self._config_path)
        self._validator = LLMConfigValidator()
        self._catalog = LLMCatalogService()
        self._persistence = LLMManagementPersistence()
        register_platform_secret_resolver(resolve_platform_cached_secret)
        self._setup_manager = LLMSetupManager(config_path=self._config_path)
        self._switch_service = LLMSwitchService(
            config_store=self._store,
            validator=self._validator,
            config_path=self._config_path,
        )
        self._last_preflight: LLMPreflightReport | None = None

    @property
    def config_path(self) -> Path:
        """Return canonical gateway config path."""
        return self._config_path

    def reload(self) -> LLMGatewayConfig:
        """Reload gateway configuration from disk."""
        return self._store.load()

    def get_config(self) -> LLMGatewayConfig:
        """Return current gateway configuration."""
        return self._store.load()

    def get_config_version(self) -> str | None:
        """Return current config version digest."""
        return self._store.version()

    async def get_overview(self) -> LLMManagementOverviewResponse:
        """Return complete management payload for UI."""
        config = self.get_config()
        provider_statuses = await self._build_provider_runtime_statuses(config)
        deployments = await self._persistence.list_engine_deployments()
        providers_payload: list[dict[str, object]] = []
        for provider in config.providers:
            secret_status = resolve_secret_status(
                provider_name=provider.name,
                env_name=provider.api_key_env,
                config_path=self._config_path,
            )
            provider_payload = provider.model_dump(mode="python")
            provider_payload["secret_status"] = secret_status
            providers_payload.append(provider_payload)

        engines_payload: list[dict[str, object]] = []
        for engine in config.inference_engines:
            engine_payload = engine.model_dump(mode="python")
            deployment = deployments.get(engine.name)
            if deployment is not None:
                engine_payload["deployment_status"] = deployment.status
                engine_payload["deployment_error"] = self._summarize_deployment_error(deployment.last_error)
                engine_payload["compose_path"] = deployment.compose_path
                engine_payload["public_base_url"] = deployment.public_base_url or engine.public_base_url
            engines_payload.append(engine_payload)

        return LLMManagementOverviewResponse.model_validate(
            {
                "gateway": LLMGatewayRuntimePayload.model_validate(config.gateway.model_dump()),
                "providers": providers_payload,
                "providers_runtime": [status.model_dump() for status in provider_statuses],
                "inference_engines": engines_payload,
                "models": [model.model_dump() for model in config.models],
                "registry": dict(config.registry),
                "routing_policy": config.routing_policy.model_dump(),
                "cost_summary": LiteLLMGateway(config=config, config_path=self._config_path).get_cost_summary(),
                "config_version": self.get_config_version(),
            }
        )

    async def update_providers(self, providers: list[LLMProviderConfigPayload]) -> list[LLMProviderConfigPayload]:
        """Replace provider configurations."""
        config = self.get_config().model_copy(deep=True)
        config.providers = [ProviderConfig.model_validate(item.model_dump(mode="python")) for item in providers]
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return [
            LLMProviderConfigPayload.model_validate(
                {
                    **provider.model_dump(mode="python"),
                    "secret_status": resolve_secret_status(
                        provider_name=provider.name,
                        env_name=provider.api_key_env,
                        config_path=self._config_path,
                    ),
                }
            )
            for provider in self.get_config().providers
        ]

    async def update_engines(
        self,
        engines: list[LLMInferenceEngineConfigPayload],
    ) -> list[LLMInferenceEngineConfigPayload]:
        """Replace inference engine configurations."""
        config = self.get_config().model_copy(deep=True)
        config.inference_engines = [
            InferenceEngineConfig.model_validate(item.model_dump(mode="python")) for item in engines
        ]
        self._validate_engine_host_ports(config.inference_engines)
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return [
            LLMInferenceEngineConfigPayload.model_validate(engine.model_dump(mode="python"))
            for engine in self.get_config().inference_engines
        ]

    async def update_models(self, models: list[LLMModelConfigPayload]) -> list[LLMModelConfigPayload]:
        """Replace model alias configurations."""
        config = self.get_config().model_copy(deep=True)
        config.models = [ModelConfig.model_validate(item.model_dump(mode="python")) for item in models]
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return [LLMModelConfigPayload.model_validate(model.model_dump()) for model in self.get_config().models]

    async def update_registry(self, registry: dict[str, str]) -> dict[str, str]:
        """Replace role->model registry mapping."""
        config = self.get_config().model_copy(deep=True)
        normalized = {str(role).strip().lower(): str(alias).strip().lower() for role, alias in registry.items()}
        config.registry = normalized
        prepared = self._validator.prepare_config(config)
        self._store.save(prepared)
        self.reload()
        return dict(self.get_config().registry)

    async def update_routing_policy(
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
        gateway = LiteLLMGateway(config=self.get_config(), config_path=self._config_path)
        statuses = await gateway.get_engine_statuses()
        return [LLMEngineRuntimeStatusPayload.model_validate(status.model_dump()) for status in statuses]

    async def get_provider_runtime(self) -> list[LLMProviderRuntimeStatusPayload]:
        """Return provider availability statuses."""
        statuses = await self._build_provider_runtime_statuses(self.get_config())
        return [LLMProviderRuntimeStatusPayload.model_validate(status.model_dump()) for status in statuses]

    def get_cost_summary(self) -> LLMCostSummary:
        """Return gateway cost-tracking summary."""
        gateway = LiteLLMGateway(config=self.get_config(), config_path=self._config_path)
        return LLMCostSummary.model_validate(gateway.get_cost_summary())

    def get_catalog(self) -> LLMCatalogResponse:
        """Return provider/model/backend catalog metadata."""
        return self._catalog.build()

    async def save_provider_secret(
        self,
        provider_name: str,
        payload: LLMProviderSecretUpdateRequest,
    ) -> LLMProviderSecretResponse:
        """Provider API key を暗号化保存する."""
        status = await self._persistence.upsert_provider_secret(
            provider_name=provider_name,
            api_key_env=payload.api_key_env,
            secret_value=payload.secret_value,
        )

        if payload.api_key_env:
            config = self.get_config().model_copy(deep=True)
            updated = False
            providers: list[ProviderConfig] = []
            for provider in config.providers:
                if provider.name == provider_name.strip().lower():
                    providers.append(provider.model_copy(update={"api_key_env": payload.api_key_env.strip()}))
                    updated = True
                else:
                    providers.append(provider)
            if updated:
                config.providers = providers
                prepared = self._validator.prepare_config(config)
                self._store.save(prepared)
                self.reload()

        return LLMProviderSecretResponse(
            provider=provider_name.strip().lower(),
            secret_status=LLMProviderSecretStatusPayload.model_validate(status.model_dump()),
        )

    async def delete_provider_secret(self, provider_name: str) -> LLMProviderSecretResponse:
        """保存済み provider secret を削除する."""
        await self._persistence.delete_provider_secret(provider_name)
        return LLMProviderSecretResponse(
            provider=provider_name.strip().lower(),
            secret_status=LLMProviderSecretStatusPayload(),
        )

    async def deploy_engine(
        self,
        engine_name: str,
        payload: LLMEngineDeployRequest,
    ) -> LLMEngineDeployResponse:
        """Engine 設定から docker compose 配備を実行する."""
        config = self.get_config().model_copy(deep=True)
        engine = next((item for item in config.inference_engines if item.name == engine_name.strip().lower()), None)
        if engine is None:
            msg = f"engine '{engine_name}' は設定されていません。"
            raise ValueError(msg)
        self._validate_engine_host_ports(config.inference_engines)

        if payload.public_base_url:
            engine = engine.model_copy(update={"public_base_url": payload.public_base_url.strip()})
            config.inference_engines = [
                engine if item.name == engine.name else item for item in config.inference_engines
            ]
            self._validate_engine_host_ports(config.inference_engines)
            prepared = self._validator.prepare_config(config)
            self._store.save(prepared)
            self.reload()

        command_result, compose_path, compose_yaml = await self._setup_manager.deploy_engine(engine)
        success = bool(command_result.error is None and command_result.return_code == 0)
        public_base_url = engine.public_base_url or engine.base_url
        if success:
            self._enable_engine(engine.name)
        deployment = PlatformEngineDeploymentRecord(
            engine_name=engine.name,
            engine_type=engine.engine_type,
            deployment_mode=engine.deployment_mode,
            docker_image=engine.docker_image,
            served_model_name=engine.served_model_name,
            container_name=engine.container_name,
            host_port=engine.host_port,
            public_base_url=public_base_url,
            compose_path=str(compose_path),
            compose_yaml=compose_yaml,
            gpu_enabled=engine.gpu_enabled,
            gpu_devices=list(engine.gpu_devices),
            gpu_count=engine.gpu_count,
            extra_env=dict(engine.extra_env),
            status="running" if success else "failed",
            last_error=(
                None
                if success
                else self._summarize_deployment_error(command_result.error or command_result.stderr or None)
            ),
            deployed_at=datetime.now(UTC).isoformat() if success else None,
            stopped_at=None,
        )
        await self._persistence.upsert_engine_deployment(deployment)
        return LLMEngineDeployResponse(
            success=success,
            engine=LLMInferenceEngineConfigPayload.model_validate(
                {
                    **engine.model_dump(mode="python"),
                    "deployment_status": deployment.status,
                    "deployment_error": deployment.last_error,
                    "compose_path": deployment.compose_path,
                    "public_base_url": deployment.public_base_url,
                }
            ),
            message="engine の配備が完了しました。" if success else "engine の配備に失敗しました。",
            command=command_result,
        )

    async def stop_engine(self, engine_name: str) -> LLMEngineDeployResponse:
        """Engine compose を停止する."""
        config = self.get_config()
        engine = next((item for item in config.inference_engines if item.name == engine_name.strip().lower()), None)
        if engine is None:
            msg = f"engine '{engine_name}' は設定されていません。"
            raise ValueError(msg)

        command_result = await self._setup_manager.stop_engine(engine.name)
        success = bool(command_result.error is None and command_result.return_code == 0)
        deployment = PlatformEngineDeploymentRecord(
            engine_name=engine.name,
            engine_type=engine.engine_type,
            deployment_mode=engine.deployment_mode,
            docker_image=engine.docker_image,
            served_model_name=engine.served_model_name,
            container_name=engine.container_name,
            host_port=engine.host_port,
            public_base_url=engine.public_base_url or engine.base_url,
            compose_path=str(self._config_path.parent / "llm_backends" / engine.name / "docker-compose.yml"),
            compose_yaml=None,
            gpu_enabled=engine.gpu_enabled,
            gpu_devices=list(engine.gpu_devices),
            gpu_count=engine.gpu_count,
            extra_env=dict(engine.extra_env),
            status="stopped" if success else "stop_failed",
            last_error=(
                None
                if success
                else self._summarize_deployment_error(command_result.error or command_result.stderr or None)
            ),
            deployed_at=None,
            stopped_at=datetime.now(UTC).isoformat(),
        )
        await self._persistence.upsert_engine_deployment(deployment)
        return LLMEngineDeployResponse(
            success=success,
            engine=LLMInferenceEngineConfigPayload.model_validate(
                {
                    **engine.model_dump(mode="python"),
                    "deployment_status": deployment.status,
                    "deployment_error": deployment.last_error,
                    "compose_path": deployment.compose_path,
                    "public_base_url": deployment.public_base_url,
                }
            ),
            message="engine を停止しました。" if success else "engine の停止に失敗しました。",
            command=command_result,
        )

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
                message="preflight が実行可能な成功状態に到達しなかったため、切替を中止しました。",
            )

        switch_response = await self.switch(request.switch)
        return LLMSetupAndSwitchResponse(
            preflight=preflight_report,
            switch=switch_response,
            success=switch_response.success,
            message=(
                "セットアップと切替が完了しました。"
                if switch_response.success
                else "preflight 後の切替に失敗しました。"
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
            hints.append("LLM 管理ルートが見つかりません。Platform backend を最新コードで再起動してください。")
        if not config_exists:
            hints.append("Gateway 設定ファイルがありません。Reload を実行すると既定設定を再生成します。")
        if self._last_preflight is not None and self._last_preflight.status == "failed":
            hints.append("直近の preflight が失敗しています。修正ヒントを解消してから切り替えてください。")
        if not hints:
            hints.append("LLM 管理ルートと設定は正常です。")

        return LLMDiagnosticsResponse(
            has_llm_routes=has_llm_routes,
            route_count=route_count,
            config_path=str(self._config_path),
            config_exists=config_exists,
            config_version=version,
            last_preflight=self._last_preflight,
            hints=hints,
        )

    def _validate_engine_host_ports(self, engines: list[InferenceEngineConfig]) -> None:
        """Reject engine host ports that collide with app runtime ports."""
        reserved_ports = self._collect_reserved_app_ports()
        seen_engine_ports: dict[int, str] = {}

        for engine in engines:
            if engine.host_port is None:
                continue
            if engine.host_port in seen_engine_ports:
                msg = f"engine port {engine.host_port} が '{seen_engine_ports[engine.host_port]}' と重複しています。"
                raise ValueError(msg)
            seen_engine_ports[engine.host_port] = engine.name

            owner = reserved_ports.get(engine.host_port)
            if owner is None:
                continue
            msg = f"engine '{engine.name}' の host_port {engine.host_port} は app 側の '{owner}' と衝突します。"
            raise ValueError(msg)

    @staticmethod
    def _collect_reserved_app_ports() -> dict[int, str]:
        """Collect app ports from app_config.json files."""
        apps_dir = Path.cwd() / "apps"
        reserved: dict[int, str] = {}
        if not apps_dir.is_dir():
            return reserved

        for config_path in sorted(apps_dir.glob("*/app_config.json")):
            try:
                payload = json.loads(config_path.read_text(encoding="utf-8"))
            except (OSError, json.JSONDecodeError):
                continue

            app_name = str(payload.get("name") or config_path.parent.name)
            ports = payload.get("ports")
            if not isinstance(ports, dict):
                continue

            for port_type in ("api", "frontend", "db", "redis"):
                raw_port = ports.get(port_type)
                if isinstance(raw_port, int):
                    reserved.setdefault(raw_port, f"{app_name}.{port_type}")
        return reserved

    async def _build_provider_runtime_statuses(
        self,
        config: LLMGatewayConfig,
    ) -> list[ProviderRuntimeStatus]:
        """Build provider runtime statuses with shared linked-engine and probe rules."""
        return await resolve_provider_runtime_statuses(config, config_path=self._config_path)

    def _enable_engine(self, engine_name: str) -> None:
        """Enable an engine in gateway config after a successful deploy."""
        config = self.get_config().model_copy(deep=True)
        updated = False
        config.inference_engines = [
            engine.model_copy(update={"enabled": True}) if engine.name == engine_name and not engine.enabled else engine
            for engine in config.inference_engines
        ]
        for engine in config.inference_engines:
            if engine.name == engine_name and engine.enabled:
                updated = True
                break
        if updated:
            prepared = self._validator.prepare_config(config)
            self._store.save(prepared)

    @staticmethod
    def _summarize_deployment_error(error: str | None) -> str | None:
        """Trim oversized deployment logs to a compact summary."""
        if error is None:
            return None
        text = error.strip()
        if not text:
            return None
        if len(text) <= 1500:
            return text

        lines = [line.rstrip() for line in text.splitlines() if line.strip()]
        if not lines:
            return text[:1500]

        head = lines[:10]
        tail = lines[-10:] if len(lines) > 10 else []
        omitted_lines = max(len(lines) - len(head) - len(tail), 0)
        pieces = [*head]
        if omitted_lines > 0:
            pieces.append(f"... {omitted_lines} 行を省略 ...")
        pieces.extend(tail)
        return "\n".join(pieces)[:2000]


def get_default_llm_management_service() -> LLMManagementService:
    """Create management service from canonical config path."""
    config_path = _resolve_default_gateway_config_path()
    if not config_path.exists():
        load_gateway_config(config_path)
    return LLMManagementService(config_path=config_path)


def _resolve_default_gateway_config_path() -> Path:
    return Path.cwd() / ".bizcore" / "llm_gateway.yaml"
