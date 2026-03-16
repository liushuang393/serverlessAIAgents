"""Shared provider runtime resolution for Platform LLM management."""

from __future__ import annotations

import asyncio
from dataclasses import dataclass
from typing import TYPE_CHECKING, Literal
from urllib.parse import urlparse, urlunparse

import httpx

from agentflow.llm.gateway import (
    EngineRuntimeStatus,
    LiteLLMGateway,
    LLMGatewayConfig,
    ModelConfig,
    ProviderConfig,
    ProviderRuntimeStatus,
    build_provider_runtime_statuses,
)


if TYPE_CHECKING:
    from pathlib import Path


_PROBE_TIMEOUT = httpx.Timeout(2.0, connect=1.0)
_ACTIVE_PROBE_PROVIDERS = {"local", "custom", "ollama"}


@dataclass(frozen=True, slots=True)
class _ProbeAttempt:
    label: str
    url: str


@dataclass(frozen=True, slots=True)
class _ProbeResult:
    status: Literal["available", "unavailable"]
    source: str
    last_error: str | None


async def resolve_provider_runtime_statuses(
    config: LLMGatewayConfig,
    *,
    config_path: Path | None = None,
    engine_statuses: list[EngineRuntimeStatus] | None = None,
) -> list[ProviderRuntimeStatus]:
    """Resolve provider runtime statuses with linked-engine and active-probe rules."""
    statuses = build_provider_runtime_statuses(config, config_path=config_path)
    status_by_name = {item.name.strip().lower(): item for item in statuses}
    resolved_engine_statuses = engine_statuses
    if resolved_engine_statuses is None:
        gateway = LiteLLMGateway(config=config, config_path=config_path)
        resolved_engine_statuses = await gateway.get_engine_statuses()

    engine_by_name = {item.name.strip().lower(): item for item in resolved_engine_statuses}
    linked_engines = _linked_engines_by_provider(config.models)

    probe_tasks: dict[str, asyncio.Task[_ProbeResult]] = {}
    ordered_statuses: list[ProviderRuntimeStatus] = []
    for provider in config.providers:
        provider_name = provider.name.strip().lower()
        base_status = status_by_name.get(provider_name)
        if base_status is None:
            continue

        if not provider.enabled:
            ordered_statuses.append(base_status)
            continue

        provider_linked_engines = linked_engines.get(provider_name, [])
        if provider_linked_engines:
            ordered_statuses.append(
                _resolve_linked_engine_status(base_status, provider_linked_engines, engine_by_name)
            )
            continue

        if provider.api_key_env is None and provider_name in _ACTIVE_PROBE_PROVIDERS:
            probe_tasks[provider_name] = asyncio.create_task(
                _probe_provider_runtime(provider, config.models),
                name=f"provider-runtime:{provider_name}",
            )
            ordered_statuses.append(base_status)
            continue

        ordered_statuses.append(base_status)

    if not probe_tasks:
        return ordered_statuses

    probe_results = await asyncio.gather(*probe_tasks.values())
    result_by_name = dict(zip(probe_tasks.keys(), probe_results, strict=False))

    normalized: list[ProviderRuntimeStatus] = []
    for status in ordered_statuses:
        probe_result = result_by_name.get(status.name.strip().lower())
        if probe_result is None:
            normalized.append(status)
            continue
        normalized.append(
            status.model_copy(
                update={
                    "status": probe_result.status,
                    "source": probe_result.source,
                    "last_error": probe_result.last_error,
                }
            )
        )
    return normalized


def _linked_engines_by_provider(models: list[ModelConfig]) -> dict[str, list[str]]:
    linked: dict[str, set[str]] = {}
    for model in models:
        provider_name = model.provider.strip().lower()
        if not model.enabled or not isinstance(model.engine, str) or not model.engine.strip():
            continue
        linked.setdefault(provider_name, set()).add(model.engine.strip().lower())
    return {provider_name: sorted(engine_names) for provider_name, engine_names in linked.items()}


def _resolve_linked_engine_status(
    status: ProviderRuntimeStatus,
    linked_engines: list[str],
    engine_by_name: dict[str, EngineRuntimeStatus],
) -> ProviderRuntimeStatus:
    unhealthy: list[str] = []
    for engine_name in linked_engines:
        engine_status = engine_by_name.get(engine_name)
        if engine_status is not None and engine_status.status == "available":
            continue
        reason = engine_status.last_error if engine_status is not None else "status_missing"
        unhealthy.append(f"{engine_name}:{reason or 'unavailable'}")

    if unhealthy:
        return status.model_copy(
            update={
                "status": "unavailable",
                "source": f"engine:{','.join(linked_engines)}",
                "last_error": f"linked_engine_unhealthy:{' / '.join(unhealthy)}",
            }
        )

    return status.model_copy(
        update={
            "status": "available",
            "source": f"engine:{','.join(linked_engines)}",
            "last_error": None,
        }
    )


async def _probe_provider_runtime(provider: ProviderConfig, models: list[ModelConfig]) -> _ProbeResult:
    base_url = _resolve_provider_base_url(provider, models)
    if base_url is None:
        return _ProbeResult(status="unavailable", source="probe:missing_api_base", last_error="api_base_missing")

    attempts = _probe_attempts(provider.name.strip().lower(), base_url)
    errors: list[str] = []
    async with httpx.AsyncClient(timeout=_PROBE_TIMEOUT) as client:
        for attempt in attempts:
            try:
                response = await client.get(attempt.url)
            except httpx.HTTPError as exc:
                errors.append(f"{attempt.label}:{exc}")
                continue
            if response.status_code < 300:
                return _ProbeResult(status="available", source=f"probe:{attempt.label}", last_error=None)
            errors.append(f"{attempt.label}:status={response.status_code}")

    summary = " / ".join(errors) if errors else "probe_failed"
    return _ProbeResult(status="unavailable", source=f"probe:{attempts[0].label}", last_error=summary)


def _resolve_provider_base_url(provider: ProviderConfig, models: list[ModelConfig]) -> str | None:
    candidates = [provider.api_base]
    candidates.extend(model.api_base for model in models if model.provider.strip().lower() == provider.name)
    for candidate in candidates:
        if isinstance(candidate, str) and candidate.strip():
            return candidate.strip().rstrip("/")
    return None


def _probe_attempts(provider_name: str, base_url: str) -> list[_ProbeAttempt]:
    root_url = _root_api_url(base_url)
    openai_models = _openai_models_url(base_url)
    health_url = f"{root_url}/health"
    if provider_name == "ollama":
        return [
            _ProbeAttempt(label="api_tags", url=f"{root_url}/api/tags"),
            _ProbeAttempt(label="v1_models", url=openai_models),
            _ProbeAttempt(label="health", url=health_url),
        ]
    return [
        _ProbeAttempt(label="v1_models", url=openai_models),
        _ProbeAttempt(label="health", url=health_url),
    ]


def _openai_models_url(base_url: str) -> str:
    if base_url.endswith("/v1"):
        return f"{base_url}/models"
    return f"{base_url}/v1/models"


def _root_api_url(base_url: str) -> str:
    parsed = urlparse(base_url)
    path = parsed.path or ""
    if path.endswith("/v1"):
        path = path[: -len("/v1")] or "/"
    normalized_path = path.rstrip("/")
    return urlunparse(parsed._replace(path=normalized_path, params="", query="", fragment="")).rstrip("/")


__all__ = ["resolve_provider_runtime_statuses"]
