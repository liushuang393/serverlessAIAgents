"""Provider / model / backend の原子的切替サービス."""

from __future__ import annotations

import re
from typing import TYPE_CHECKING

from infrastructure.llm.gateway import (
    LiteLLMGateway,
    LLMGatewayConfig,
    ModelConfig,
    ProviderConfig,
)
from apps.platform.schemas.llm_management_schemas import (
    LLMBackendKind,
    LLMSwitchDiffItem,
    LLMSwitchRequest,
    LLMSwitchResponse,
    LLMSwitchRuntimeCheck,
)
from apps.platform.services.llm_management_validator import (
    LLMConfigValidator,
    provider_default_api_base,
    provider_default_api_key_env,
)
from apps.platform.services.llm_runtime_status import resolve_provider_runtime_statuses


if TYPE_CHECKING:
    from pathlib import Path

    from apps.platform.services.llm_management_config_store import LLMConfigStore


class LLMSwitchService:
    """Apply switch request transactionally with rollback on runtime failure."""

    def __init__(
        self,
        *,
        config_store: LLMConfigStore,
        validator: LLMConfigValidator,
        config_path: Path,
    ) -> None:
        self._store = config_store
        self._validator = validator
        self._config_path = config_path

    async def switch(self, request: LLMSwitchRequest) -> LLMSwitchResponse:
        """Apply provider/model/backend switch with runtime check and rollback."""
        current_config, _old_version = self._store.load_with_version()
        original = current_config.model_copy(deep=True)
        diffs: list[LLMSwitchDiffItem] = []

        try:
            updated, applied_alias, diffs = self._apply_switch(current_config, request)
            validated = self._validator.prepare_config(updated)
        except Exception as exc:
            return LLMSwitchResponse(
                success=False,
                rolled_back=False,
                message=f"検証に失敗しました: {exc}",
                registry=dict(original.registry),
                diffs=diffs,
            )

        version = self._store.save(validated)
        runtime_check = await self._runtime_check(validated, request)

        runtime_errors = list(runtime_check.errors)
        if request.validate_runtime and runtime_errors:
            self._store.save(original)
            return LLMSwitchResponse(
                success=False,
                rolled_back=True,
                config_version=self._store.version(),
                applied_alias=applied_alias,
                registry=dict(original.registry),
                diffs=diffs,
                runtime_check=runtime_check,
                message="実行時チェックに失敗したため、直前の設定へロールバックしました。",
            )

        return LLMSwitchResponse(
            success=True,
            rolled_back=False,
            config_version=version,
            applied_alias=applied_alias,
            registry=dict(validated.registry),
            diffs=diffs,
            runtime_check=runtime_check,
            message="切替を反映しました。",
        )

    def _apply_switch(
        self,
        config: LLMGatewayConfig,
        request: LLMSwitchRequest,
    ) -> tuple[LLMGatewayConfig, str, list[LLMSwitchDiffItem]]:
        provider_name = self._validator.canonical_provider_name(request.provider.value)
        backend_name = None if request.backend == LLMBackendKind.NONE else request.backend.value
        target_alias = self._resolve_alias(request, provider_name)

        diffs: list[LLMSwitchDiffItem] = []
        updated = config.model_copy(deep=True)

        provider_before = {item.name: item for item in updated.providers}
        if provider_name not in provider_before:
            if not request.auto_enable_provider:
                msg = f"provider '{provider_name}' は設定されていません。"
                raise ValueError(msg)
            updated.providers.append(
                ProviderConfig(
                    name=provider_name,
                    api_base=provider_default_api_base(provider_name),
                    api_key_env=provider_default_api_key_env(provider_name),
                    enabled=True,
                    models=[],
                )
            )
            diffs.append(LLMSwitchDiffItem(field=f"providers.{provider_name}", before=None, after="enabled"))
        else:
            existing = provider_before[provider_name]
            if not existing.enabled and request.auto_enable_provider:
                updated.providers = [
                    item.model_copy(update={"enabled": True}) if item.name == provider_name else item
                    for item in updated.providers
                ]
                diffs.append(
                    LLMSwitchDiffItem(field=f"providers.{provider_name}.enabled", before="false", after="true")
                )

        existing_model = None
        for item in updated.models:
            if item.alias == target_alias:
                existing_model = item
                break

        if existing_model is None:
            api_base = self._resolve_backend_api_base(updated, backend_name)
            updated.models.append(
                ModelConfig(
                    alias=target_alias,
                    model_id=target_alias,
                    provider=provider_name,
                    model=request.model.strip(),
                    api_base=api_base,
                    engine=backend_name,
                    model_type="text",
                    enabled=True,
                )
            )
            diffs.append(LLMSwitchDiffItem(field=f"models.{target_alias}", before=None, after=request.model.strip()))
        else:
            before = f"{existing_model.provider}/{existing_model.model}"
            api_base = self._resolve_backend_api_base(updated, backend_name) or existing_model.api_base
            updated.models = [
                item.model_copy(
                    update={
                        "model_id": item.model_id or item.alias,
                        "provider": provider_name,
                        "model": request.model.strip(),
                        "api_base": api_base,
                        "engine": backend_name,
                        "model_type": "text",
                        "enabled": True,
                    }
                )
                if item.alias == target_alias
                else item
                for item in updated.models
            ]
            after = f"{provider_name}/{request.model.strip()}"
            if before != after:
                diffs.append(LLMSwitchDiffItem(field=f"models.{target_alias}", before=before, after=after))

        for role in request.roles:
            previous = updated.registry.get(role)
            updated.registry[role] = target_alias
            if previous != target_alias:
                diffs.append(
                    LLMSwitchDiffItem(
                        field=f"registry.{role}",
                        before=previous,
                        after=target_alias,
                    )
                )

        if request.update_fallback_chain:
            for role in request.roles:
                existing = updated.routing_policy.fallback_chain.get(role, [])
                merged = [target_alias, *[item for item in existing if item != target_alias]]
                updated.routing_policy.fallback_chain[role] = merged

        return updated, target_alias, diffs

    def _resolve_alias(self, request: LLMSwitchRequest, provider_name: str) -> str:
        if request.model_alias:
            return request.model_alias.strip().lower()
        role = request.roles[0] if request.roles else "reasoning"
        model_fragment = re.sub(r"[^a-zA-Z0-9]+", "_", request.model.strip().lower()).strip("_")
        if not model_fragment:
            model_fragment = "model"
        return f"{role}_{provider_name}_{model_fragment}"[:96]

    @staticmethod
    def _resolve_backend_api_base(
        config: LLMGatewayConfig,
        backend_name: str | None,
    ) -> str | None:
        if backend_name is None:
            return None
        for engine in config.inference_engines:
            if engine.name != backend_name:
                continue
            base = engine.public_base_url or engine.base_url
            if base.endswith("/v1"):
                return base
            return f"{base.rstrip('/')}/v1"
        return None

    async def _runtime_check(
        self,
        config: LLMGatewayConfig,
        request: LLMSwitchRequest,
    ) -> LLMSwitchRuntimeCheck:
        runtime = LLMSwitchRuntimeCheck()
        gateway = LiteLLMGateway(config=config, config_path=self._config_path)
        engine_statuses = await gateway.get_engine_statuses()
        by_engine = {item.name: item for item in engine_statuses}

        provider_name = self._validator.canonical_provider_name(request.provider.value)
        provider_statuses = await resolve_provider_runtime_statuses(
            config,
            config_path=self._config_path,
            engine_statuses=engine_statuses,
        )
        by_provider = {item.name: item for item in provider_statuses}
        provider_status = by_provider.get(provider_name)
        if provider_status is not None:
            provider_runtime_status = provider_status.status
            provider_last_error = provider_status.last_error

            runtime.provider_status = provider_runtime_status
            if request.validate_runtime and provider_runtime_status != "available":
                runtime.errors.append(
                    f"provider '{provider_name}' は利用不可です: {provider_last_error or 'unknown'}",
                )
        else:
            runtime.errors.append(f"provider '{provider_name}' の実行時状態が見つかりません。")

        if request.backend != LLMBackendKind.NONE:
            backend_name = request.backend.value
            engine_status = by_engine.get(backend_name)
            if engine_status is None:
                runtime.errors.append(f"backend '{backend_name}' の実行時状態が見つかりません。")
            else:
                runtime.backend_status = engine_status.status
                if request.validate_runtime and engine_status.status != "available":
                    runtime.errors.append(
                        f"backend '{backend_name}' は利用不可です: {engine_status.last_error or 'unknown'}",
                    )

        return runtime
