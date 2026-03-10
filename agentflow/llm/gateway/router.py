"""Embedded LiteLLM gateway router for AgentFlow.

All upper layers must call this gateway instead of provider SDKs.
"""

from __future__ import annotations

import base64
import asyncio
import contextlib
import json
import logging
import random
import time
from collections import defaultdict
from typing import TYPE_CHECKING, Any

import httpx
from pydantic import BaseModel, Field

from agentflow.llm.contracts import resolve_contract_model_alias
from agentflow.llm.gateway.config import (
    EngineRuntimeStatus,
    LLMGatewayConfig,
    ModelConfig,
    load_gateway_config,
    resolve_secret,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from pathlib import Path


logger = logging.getLogger(__name__)

try:  # pragma: no cover - optional dependency
    import litellm
except ImportError:  # pragma: no cover - optional dependency
    litellm = None


class GatewayToolCall(BaseModel):
    """Tool call payload returned by the gateway."""

    id: str
    name: str
    arguments: str | dict[str, Any]


class GatewayResponse(BaseModel):
    """Normalized response payload for text/tool generation."""

    content: str | None = None
    model: str
    usage: dict[str, int] = Field(default_factory=dict)
    finish_reason: str | None = None
    tool_calls: list[GatewayToolCall] = Field(default_factory=list)
    cost_usd: float = 0.0
    latency_ms: float = 0.0


class LiteLLMGateway:
    """AgentFlow embedded LiteLLM gateway."""

    def __init__(
        self,
        config: LLMGatewayConfig | None = None,
        *,
        config_path: Path | None = None,
    ) -> None:
        self._config_path = config_path
        self._config = config or load_gateway_config(config_path)
        self._latency_by_alias: dict[str, float] = {}
        self._cost_by_alias: dict[str, float] = defaultdict(float)
        self._request_count_by_alias: dict[str, int] = defaultdict(int)
        self._failure_count_by_alias: dict[str, int] = defaultdict(int)
        self._round_robin_cursor: dict[str, int] = defaultdict(int)

    @property
    def config(self) -> LLMGatewayConfig:
        return self._config

    def reload(self) -> None:
        """Reload gateway configuration from YAML."""
        self._config = load_gateway_config(self._config_path)

    def _model_map(self) -> dict[str, ModelConfig]:
        return {m.alias: m for m in self._config.models}

    def _resolve_contract_alias(
        self,
        *,
        modality: str,
        metadata: dict[str, Any] | None = None,
    ) -> str | None:
        app_name = None
        agent_name = None
        if metadata is not None:
            raw_app_name = metadata.get("app_name")
            if isinstance(raw_app_name, str) and raw_app_name.strip():
                app_name = raw_app_name.strip()
            raw_agent_name = metadata.get("agent_name")
            if isinstance(raw_agent_name, str) and raw_agent_name.strip():
                agent_name = raw_agent_name.strip()
        return resolve_contract_model_alias(
            modality=modality,  # type: ignore[arg-type]
            app_name=app_name,
            agent_name=agent_name,
            gateway_config_path=self._config_path,
        )

    def _resolve_role_alias(
        self,
        role: str | None,
        model_alias: str | None,
        *,
        modality: str = "text",
        metadata: dict[str, Any] | None = None,
    ) -> str:
        alias = model_alias
        if alias:
            return alias.strip().lower()

        contract_alias = self._resolve_contract_alias(modality=modality, metadata=metadata)
        if contract_alias:
            return contract_alias.strip().lower()

        normalized_role = (role or self._config.gateway.default_role).strip().lower()
        resolved = self._config.registry.get(normalized_role)
        if resolved:
            return resolved
        if normalized_role in self._model_map():
            return normalized_role
        default_alias = self._config.registry.get(self._config.gateway.default_role)
        if default_alias:
            return default_alias
        if self._config.models:
            return self._config.models[0].alias
        msg = "No models are configured in LLM gateway"
        raise RuntimeError(msg)

    def _candidate_aliases(
        self,
        role: str | None,
        model_alias: str | None,
        *,
        modality: str = "text",
        metadata: dict[str, Any] | None = None,
    ) -> list[str]:
        primary = self._resolve_role_alias(role, model_alias, modality=modality, metadata=metadata)
        normalized_role = (role or self._config.gateway.default_role).strip().lower()
        fallback = self._config.routing_policy.fallback_chain.get(normalized_role, [])
        candidates: list[str] = []
        for alias in [primary, *fallback]:
            normalized = alias.strip().lower()
            if normalized in candidates:
                continue
            model_cfg = self._model_map().get(normalized)
            if model_cfg is None or not model_cfg.enabled:
                continue
            candidates.append(normalized)
        if not candidates:
            msg = f"No enabled model for role={normalized_role}"
            raise RuntimeError(msg)
        return self._apply_routing(candidates, normalized_role)

    def _apply_routing(self, aliases: list[str], role: str) -> list[str]:
        model_map = self._model_map()

        if self._config.routing_policy.priority == "cost":
            aliases = sorted(
                aliases,
                key=lambda alias: model_map[alias].cost.input_per_1k + model_map[alias].cost.output_per_1k,
            )
        elif self._config.routing_policy.priority == "quality":
            aliases = sorted(aliases, key=lambda alias: model_map[alias].quality_score, reverse=True)
        else:
            aliases = sorted(
                aliases,
                key=lambda alias: self._latency_by_alias.get(alias, model_map[alias].avg_latency_ms),
            )

        strategy = self._config.routing_policy.load_balance_strategy
        if strategy == "random":
            random.shuffle(aliases)
            return aliases

        if strategy == "least_latency":
            return sorted(aliases, key=lambda alias: self._latency_by_alias.get(alias, model_map[alias].avg_latency_ms))

        if not aliases:
            return aliases

        cursor = self._round_robin_cursor[role] % len(aliases)
        self._round_robin_cursor[role] += 1
        return aliases[cursor:] + aliases[:cursor]

    def _resolve_api_base(self, model_cfg: ModelConfig) -> str | None:
        if model_cfg.api_base:
            return model_cfg.api_base.rstrip("/")
        for provider in self._config.providers:
            if provider.name == model_cfg.provider and provider.api_base:
                return provider.api_base.rstrip("/")
        return None

    def _resolve_api_key(self, model_cfg: ModelConfig) -> str | None:
        key_env = model_cfg.api_key_env
        if key_env is None:
            for provider in self._config.providers:
                if provider.name == model_cfg.provider:
                    key_env = provider.api_key_env
                    break
        secret, _source = resolve_secret(
            key_env,
            provider_name=model_cfg.provider,
            config_path=self._config_path,
        )
        return secret

    def _to_litellm_model(self, model_cfg: ModelConfig) -> str:
        if "/" in model_cfg.model:
            return model_cfg.model
        provider = model_cfg.provider
        if provider in {"openai", "anthropic", "google", "gemini", "bedrock", "vertex_ai", "azure"}:
            normalized_provider = "google" if provider == "gemini" else provider
            return f"{normalized_provider}/{model_cfg.model}"
        return model_cfg.model

    def _ensure_openai_compatible_api_base(self, model_cfg: ModelConfig) -> str:
        api_base = self._resolve_api_base(model_cfg)
        if api_base is None:
            if model_cfg.provider == "openai":
                return "https://api.openai.com/v1"
            msg = f"model alias '{model_cfg.alias}' requires openai-compatible api_base for passthrough endpoint"
            raise RuntimeError(msg)
        if api_base.endswith("/v1"):
            return api_base
        return f"{api_base}/v1"

    @staticmethod
    def _build_messages(prompt: str | None, messages: list[dict[str, Any]] | None) -> list[dict[str, Any]]:
        if messages:
            return messages
        if prompt is None:
            return []
        return [{"role": "user", "content": prompt}]

    @staticmethod
    def _extract_usage(response: Any) -> dict[str, int]:
        usage = response.get("usage") if isinstance(response, dict) else getattr(response, "usage", None)
        if usage is None:
            return {}
        if isinstance(usage, dict):
            return {
                "prompt_tokens": int(usage.get("prompt_tokens", 0) or 0),
                "completion_tokens": int(usage.get("completion_tokens", 0) or 0),
                "total_tokens": int(usage.get("total_tokens", 0) or 0),
            }

        return {
            "prompt_tokens": int(getattr(usage, "prompt_tokens", 0) or 0),
            "completion_tokens": int(getattr(usage, "completion_tokens", 0) or 0),
            "total_tokens": int(getattr(usage, "total_tokens", 0) or 0),
        }

    @staticmethod
    def _extract_choice(response: Any) -> Any:
        choices = response.get("choices") if isinstance(response, dict) else getattr(response, "choices", [])
        if isinstance(choices, list) and choices:
            return choices[0]
        return None

    @classmethod
    def _extract_content(cls, response: Any) -> str | None:
        choice = cls._extract_choice(response)
        if choice is None:
            return None

        message = choice.get("message") if isinstance(choice, dict) else getattr(choice, "message", None)
        if isinstance(message, dict):
            content = message.get("content")
            if isinstance(content, str):
                return content
            return None

        content = getattr(message, "content", None)
        if isinstance(content, str):
            return content
        return None

    @classmethod
    def _extract_finish_reason(cls, response: Any) -> str | None:
        choice = cls._extract_choice(response)
        if choice is None:
            return None
        if isinstance(choice, dict):
            finish_reason = choice.get("finish_reason")
            return finish_reason if isinstance(finish_reason, str) else None
        finish_reason = getattr(choice, "finish_reason", None)
        return finish_reason if isinstance(finish_reason, str) else None

    @classmethod
    def _extract_tool_calls(cls, response: Any) -> list[GatewayToolCall]:
        choice = cls._extract_choice(response)
        if choice is None:
            return []

        message = choice.get("message") if isinstance(choice, dict) else getattr(choice, "message", None)
        if message is None:
            return []

        raw_calls = message.get("tool_calls") if isinstance(message, dict) else getattr(message, "tool_calls", None)
        if not raw_calls:
            return []

        normalized: list[GatewayToolCall] = []
        for call in raw_calls:
            if isinstance(call, dict):
                function_data = call.get("function", {})
                arguments = function_data.get("arguments", "{}")
                normalized.append(
                    GatewayToolCall(
                        id=str(call.get("id", "")),
                        name=str(function_data.get("name", "")),
                        arguments=arguments,
                    )
                )
                continue

            function_data = getattr(call, "function", None)
            if function_data is None:
                continue
            normalized.append(
                GatewayToolCall(
                    id=str(getattr(call, "id", "")),
                    name=str(getattr(function_data, "name", "")),
                    arguments=getattr(function_data, "arguments", "{}"),
                )
            )

        return normalized

    def _estimate_cost(self, alias: str, usage: dict[str, int]) -> float:
        model_cfg = self._model_map()[alias]
        prompt_tokens = usage.get("prompt_tokens", 0)
        completion_tokens = usage.get("completion_tokens", 0)
        return (prompt_tokens / 1000.0) * model_cfg.cost.input_per_1k + (
            completion_tokens / 1000.0
        ) * model_cfg.cost.output_per_1k

    def _record_success(self, alias: str, *, latency_ms: float, cost_usd: float) -> None:
        self._latency_by_alias[alias] = latency_ms
        self._cost_by_alias[alias] += cost_usd
        self._request_count_by_alias[alias] += 1

    def _record_failure(self, alias: str) -> None:
        self._failure_count_by_alias[alias] += 1

    async def _call_completion(
        self,
        model_cfg: ModelConfig,
        *,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]] | None,
        tool_choice: str | dict[str, Any] | None,
        temperature: float | None,
        max_tokens: int | None,
        metadata: dict[str, Any] | None,
    ) -> Any:
        kwargs: dict[str, Any] = {
            "messages": messages,
            "temperature": temperature,
            "max_tokens": max_tokens,
            "timeout": self._config.gateway.request_timeout_seconds,
        }
        kwargs = {k: v for k, v in kwargs.items() if v is not None}

        if tools:
            kwargs["tools"] = tools
            if tool_choice is not None:
                kwargs["tool_choice"] = tool_choice
        if metadata:
            kwargs["metadata"] = metadata

        if litellm is not None:
            kwargs["model"] = self._to_litellm_model(model_cfg)
            if api_base := self._resolve_api_base(model_cfg):
                kwargs["api_base"] = api_base
            if api_key := self._resolve_api_key(model_cfg):
                kwargs["api_key"] = api_key
            return await litellm.acompletion(**kwargs)

        openai_base = self._ensure_openai_compatible_api_base(model_cfg)
        headers: dict[str, str] = {"Content-Type": "application/json"}
        if api_key := self._resolve_api_key(model_cfg):
            headers["Authorization"] = f"Bearer {api_key}"

        payload: dict[str, Any] = {
            "model": model_cfg.model,
            "messages": messages,
            "temperature": temperature,
            "max_tokens": max_tokens,
        }
        if tools:
            payload["tools"] = tools
            if tool_choice is not None:
                payload["tool_choice"] = tool_choice
        payload = {k: v for k, v in payload.items() if v is not None}

        async with httpx.AsyncClient(timeout=self._config.gateway.request_timeout_seconds) as client:
            response = await client.post(f"{openai_base}/chat/completions", headers=headers, json=payload)
            response.raise_for_status()
            return response.json()

    async def generate(
        self,
        *,
        role: str,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        temperature: float | None = None,
        max_tokens: int | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> GatewayResponse:
        """Generate text via role-driven routing."""
        request_messages = self._build_messages(prompt, messages)
        candidates = self._candidate_aliases(
            role,
            model_alias,
            modality="text",
            metadata=metadata,
        )
        last_error: Exception | None = None

        for alias in candidates:
            model_cfg = self._model_map()[alias]
            start = time.perf_counter()
            try:
                response = await self._call_completion(
                    model_cfg,
                    messages=request_messages,
                    tools=tools,
                    tool_choice=None,
                    temperature=temperature,
                    max_tokens=max_tokens,
                    metadata=metadata,
                )
                latency_ms = (time.perf_counter() - start) * 1000.0
                usage = self._extract_usage(response)
                cost_usd = self._estimate_cost(alias, usage)
                self._record_success(alias, latency_ms=latency_ms, cost_usd=cost_usd)
                return GatewayResponse(
                    content=self._extract_content(response),
                    model=str(self._to_litellm_model(model_cfg)),
                    usage=usage,
                    finish_reason=self._extract_finish_reason(response),
                    tool_calls=self._extract_tool_calls(response),
                    cost_usd=cost_usd,
                    latency_ms=latency_ms,
                )
            except Exception as exc:  # pragma: no cover - network/runtime variability
                self._record_failure(alias)
                last_error = exc
                logger.warning("Gateway generate failed for alias=%s: %s", alias, exc)
                continue

        msg = f"Gateway generate failed after trying aliases={candidates}"
        raise RuntimeError(msg) from last_error

    async def tool_call(
        self,
        *,
        role: str,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]],
        tool_choice: str | dict[str, Any] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> GatewayResponse:
        """Generate tool call response via role-driven routing."""
        candidates = self._candidate_aliases(
            role,
            model_alias,
            modality="text",
            metadata=metadata,
        )
        last_error: Exception | None = None

        for alias in candidates:
            model_cfg = self._model_map()[alias]
            start = time.perf_counter()
            try:
                response = await self._call_completion(
                    model_cfg,
                    messages=messages,
                    tools=tools,
                    tool_choice=tool_choice,
                    temperature=None,
                    max_tokens=None,
                    metadata=metadata,
                )
                latency_ms = (time.perf_counter() - start) * 1000.0
                usage = self._extract_usage(response)
                cost_usd = self._estimate_cost(alias, usage)
                self._record_success(alias, latency_ms=latency_ms, cost_usd=cost_usd)
                return GatewayResponse(
                    content=self._extract_content(response),
                    model=str(self._to_litellm_model(model_cfg)),
                    usage=usage,
                    finish_reason=self._extract_finish_reason(response),
                    tool_calls=self._extract_tool_calls(response),
                    cost_usd=cost_usd,
                    latency_ms=latency_ms,
                )
            except Exception as exc:  # pragma: no cover - network/runtime variability
                self._record_failure(alias)
                last_error = exc
                logger.warning("Gateway tool_call failed for alias=%s: %s", alias, exc)
                continue

        msg = f"Gateway tool_call failed after trying aliases={candidates}"
        raise RuntimeError(msg) from last_error

    async def _stream_completion(
        self,
        model_cfg: ModelConfig,
        *,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]] | None,
        temperature: float | None,
        max_tokens: int | None,
        metadata: dict[str, Any] | None,
    ) -> AsyncIterator[str]:
        kwargs: dict[str, Any] = {
            "messages": messages,
            "stream": True,
            "temperature": temperature,
            "max_tokens": max_tokens,
            "timeout": self._config.gateway.request_timeout_seconds,
        }
        kwargs = {k: v for k, v in kwargs.items() if v is not None}

        if tools:
            kwargs["tools"] = tools
        if metadata:
            kwargs["metadata"] = metadata

        if litellm is not None:
            kwargs["model"] = self._to_litellm_model(model_cfg)
            if api_base := self._resolve_api_base(model_cfg):
                kwargs["api_base"] = api_base
            if api_key := self._resolve_api_key(model_cfg):
                kwargs["api_key"] = api_key

            stream = await litellm.acompletion(**kwargs)
            async for chunk in stream:
                choices = chunk.get("choices") if isinstance(chunk, dict) else getattr(chunk, "choices", [])
                if not choices:
                    continue
                choice = choices[0]
                delta = choice.get("delta") if isinstance(choice, dict) else getattr(choice, "delta", None)
                if delta is None:
                    continue
                content = delta.get("content") if isinstance(delta, dict) else getattr(delta, "content", None)
                if isinstance(content, str) and content:
                    yield content
            return

        openai_base = self._ensure_openai_compatible_api_base(model_cfg)
        headers: dict[str, str] = {"Content-Type": "application/json"}
        if api_key := self._resolve_api_key(model_cfg):
            headers["Authorization"] = f"Bearer {api_key}"

        payload: dict[str, Any] = {
            "model": model_cfg.model,
            "messages": messages,
            "stream": True,
            "temperature": temperature,
            "max_tokens": max_tokens,
        }
        if tools:
            payload["tools"] = tools
        payload = {k: v for k, v in payload.items() if v is not None}

        async with (
            httpx.AsyncClient(timeout=self._config.gateway.request_timeout_seconds) as client,
            client.stream("POST", f"{openai_base}/chat/completions", headers=headers, json=payload) as response,
        ):
            response.raise_for_status()
            async for line in response.aiter_lines():
                if not line.startswith("data: "):
                    continue
                raw = line[6:].strip()
                if raw == "[DONE]":
                    break
                try:
                    chunk = json.loads(raw)
                except json.JSONDecodeError:
                    continue
                choices = chunk.get("choices", [])
                if not choices:
                    continue
                delta = choices[0].get("delta", {})
                content = delta.get("content")
                if isinstance(content, str) and content:
                    yield content

    async def stream(
        self,
        *,
        role: str,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        temperature: float | None = None,
        max_tokens: int | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> AsyncIterator[str]:
        """Stream text via role-driven routing."""
        request_messages = self._build_messages(prompt, messages)
        candidates = self._candidate_aliases(
            role,
            model_alias,
            modality="text",
            metadata=metadata,
        )
        last_error: Exception | None = None

        for alias in candidates:
            model_cfg = self._model_map()[alias]
            start = time.perf_counter()
            try:
                async for chunk in self._stream_completion(
                    model_cfg,
                    messages=request_messages,
                    tools=tools,
                    temperature=temperature,
                    max_tokens=max_tokens,
                    metadata=metadata,
                ):
                    yield chunk
                latency_ms = (time.perf_counter() - start) * 1000.0
                self._record_success(alias, latency_ms=latency_ms, cost_usd=0.0)
                return
            except Exception as exc:  # pragma: no cover - network/runtime variability
                self._record_failure(alias)
                last_error = exc
                logger.warning("Gateway stream failed for alias=%s: %s", alias, exc)
                continue

        msg = f"Gateway stream failed after trying aliases={candidates}"
        raise RuntimeError(msg) from last_error

    async def embedding(
        self,
        *,
        role: str,
        input_texts: list[str],
        model_alias: str | None = None,
        model: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> list[list[float]]:
        """Generate embeddings via gateway."""
        if not input_texts:
            return []

        alias = self._resolve_role_alias(role, model_alias, modality="embedding", metadata=metadata)
        model_cfg = self._model_map()[alias]
        target_model = model or model_cfg.model

        if litellm is not None:
            kwargs: dict[str, Any] = {
                "model": self._to_litellm_model(
                    model_cfg if model is None else model_cfg.model_copy(update={"model": model})
                ),
                "input": input_texts,
                "timeout": self._config.gateway.request_timeout_seconds,
            }
            if api_base := self._resolve_api_base(model_cfg):
                kwargs["api_base"] = api_base
            if api_key := self._resolve_api_key(model_cfg):
                kwargs["api_key"] = api_key
            response = await litellm.aembedding(**kwargs)
            data = response.get("data") if isinstance(response, dict) else getattr(response, "data", [])
            vectors: list[list[float]] = []
            for item in data:
                embedding = item.get("embedding") if isinstance(item, dict) else getattr(item, "embedding", None)
                if isinstance(embedding, list):
                    vectors.append([float(x) for x in embedding])
            return vectors

        openai_base = self._ensure_openai_compatible_api_base(model_cfg)
        headers: dict[str, str] = {"Content-Type": "application/json"}
        if api_key := self._resolve_api_key(model_cfg):
            headers["Authorization"] = f"Bearer {api_key}"

        payload = {"model": target_model, "input": input_texts}
        async with httpx.AsyncClient(timeout=self._config.gateway.request_timeout_seconds) as client:
            response = await client.post(f"{openai_base}/embeddings", headers=headers, json=payload)
            response.raise_for_status()
            data = response.json().get("data", [])
            return [[float(x) for x in item.get("embedding", [])] for item in data if isinstance(item, dict)]

    async def _audio_form_post(
        self,
        *,
        model_cfg: ModelConfig,
        path: str,
        files: dict[str, tuple[str, bytes, str]],
        data: dict[str, Any],
    ) -> httpx.Response:
        base_url = self._ensure_openai_compatible_api_base(model_cfg)
        headers: dict[str, str] = {}
        if api_key := self._resolve_api_key(model_cfg):
            headers["Authorization"] = f"Bearer {api_key}"

        async with httpx.AsyncClient(timeout=self._config.gateway.request_timeout_seconds) as client:
            response = await client.post(
                f"{base_url}/{path.lstrip('/')}",
                headers=headers,
                files=files,
                data=data,
            )
            response.raise_for_status()
            return response

    async def transcribe(
        self,
        *,
        role: str,
        audio_bytes: bytes,
        language: str | None = None,
        prompt: str | None = None,
        response_format: str = "text",
        model: str | None = None,
        model_alias: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> str:
        """Transcribe audio via gateway passthrough endpoint."""
        alias = self._resolve_role_alias(
            role,
            model_alias,
            modality="speech_to_text",
            metadata=metadata,
        )
        model_cfg = self._model_map()[alias]
        data: dict[str, Any] = {
            "model": model or model_cfg.model,
            "response_format": response_format,
        }
        if language:
            data["language"] = language
        if prompt:
            data["prompt"] = prompt

        response = await self._audio_form_post(
            model_cfg=model_cfg,
            path="audio/transcriptions",
            files={"file": ("audio.mp3", audio_bytes, "audio/mpeg")},
            data=data,
        )

        if response_format == "text":
            return response.text
        payload = response.json()
        return str(payload.get("text", "")) if isinstance(payload, dict) else ""

    async def translate_audio(
        self,
        *,
        role: str,
        audio_bytes: bytes,
        prompt: str | None = None,
        model: str | None = None,
        model_alias: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> str:
        """Translate audio to text via gateway passthrough endpoint."""
        alias = self._resolve_role_alias(
            role,
            model_alias,
            modality="speech_to_text",
            metadata=metadata,
        )
        model_cfg = self._model_map()[alias]
        data: dict[str, Any] = {"model": model or model_cfg.model}
        if prompt:
            data["prompt"] = prompt

        response = await self._audio_form_post(
            model_cfg=model_cfg,
            path="audio/translations",
            files={"file": ("audio.mp3", audio_bytes, "audio/mpeg")},
            data=data,
        )
        payload = response.json()
        return str(payload.get("text", "")) if isinstance(payload, dict) else ""

    async def synthesize(
        self,
        *,
        role: str,
        text: str,
        voice: str,
        speed: float = 1.0,
        output_format: str = "mp3",
        model: str | None = None,
        model_alias: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> bytes:
        """Synthesize text to speech via gateway passthrough endpoint."""
        alias = self._resolve_role_alias(
            role,
            model_alias,
            modality="text_to_speech",
            metadata=metadata,
        )
        model_cfg = self._model_map()[alias]
        base_url = self._ensure_openai_compatible_api_base(model_cfg)
        headers: dict[str, str] = {"Content-Type": "application/json"}
        if api_key := self._resolve_api_key(model_cfg):
            headers["Authorization"] = f"Bearer {api_key}"

        payload: dict[str, Any] = {
            "model": model or model_cfg.model,
            "input": text,
            "voice": voice,
            "speed": speed,
            "response_format": output_format,
        }

        async with httpx.AsyncClient(timeout=self._config.gateway.request_timeout_seconds) as client:
            response = await client.post(f"{base_url}/audio/speech", headers=headers, json=payload)
            response.raise_for_status()
            return response.content

    async def generate_image(
        self,
        *,
        role: str,
        prompt: str,
        size: str,
        quality: str = "high",
        output_format: str = "png",
        model_alias: str | None = None,
        model: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> bytes:
        """Generate image via gateway passthrough endpoint."""
        alias = self._resolve_role_alias(
            role,
            model_alias,
            modality="image",
            metadata=metadata,
        )
        model_cfg = self._model_map()[alias]
        base_url = self._ensure_openai_compatible_api_base(model_cfg)
        headers: dict[str, str] = {"Content-Type": "application/json"}
        if api_key := self._resolve_api_key(model_cfg):
            headers["Authorization"] = f"Bearer {api_key}"

        payload: dict[str, Any] = {
            "model": model or model_cfg.model,
            "prompt": prompt,
            "n": 1,
            "size": size,
            "quality": quality,
            "output_format": output_format,
        }

        async with httpx.AsyncClient(timeout=self._config.gateway.request_timeout_seconds) as client:
            response = await client.post(f"{base_url}/images/generations", headers=headers, json=payload)
            response.raise_for_status()
            body = response.json()
            data = body.get("data", []) if isinstance(body, dict) else []
            if not data:
                msg = "Image generation response missing data"
                raise RuntimeError(msg)
            item = data[0]
            if not isinstance(item, dict):
                msg = "Image generation response is malformed"
                raise RuntimeError(msg)
            if b64 := item.get("b64_json"):
                return base64.b64decode(str(b64))
            if url := item.get("url"):
                image_resp = await client.get(str(url))
                image_resp.raise_for_status()
                return image_resp.content
            msg = "Image generation response contains neither b64_json nor url"
            raise RuntimeError(msg)

    async def get_engine_statuses(self) -> list[EngineRuntimeStatus]:
        """Collect local inference engine statuses for platform management UI."""
        return list(await asyncio.gather(*(self._probe_engine_status(engine) for engine in self._config.inference_engines)))

    async def _probe_engine_status(self, engine: InferenceEngineConfig) -> EngineRuntimeStatus:
        if not engine.enabled:
            return EngineRuntimeStatus(
                name=engine.name,
                engine_type=engine.engine_type,
                status="unavailable",
                last_error="disabled",
            )

        health_url = f"{engine.base_url.rstrip('/')}/{engine.health_path.lstrip('/')}"
        model_url = f"{engine.base_url.rstrip('/')}/{engine.model_list_path.lstrip('/')}"
        metrics_url = f"{engine.base_url.rstrip('/')}/{engine.metrics_path.lstrip('/')}"
        timeout = httpx.Timeout(2.0, connect=1.0)

        start = time.perf_counter()
        try:
            async with httpx.AsyncClient(timeout=timeout) as client:
                health_response = await client.get(health_url)
                latency_ms = (time.perf_counter() - start) * 1000.0
                if health_response.status_code >= 400:
                    return EngineRuntimeStatus(
                        name=engine.name,
                        engine_type=engine.engine_type,
                        status="unavailable",
                        latency_ms=latency_ms,
                        last_error=f"health status={health_response.status_code}",
                    )

                loaded_models: list[str] = []
                try:
                    model_response = await client.get(model_url)
                    if model_response.status_code < 400:
                        body = model_response.json()
                        raw_models = body.get("data", []) if isinstance(body, dict) else []
                        for item in raw_models:
                            if isinstance(item, dict):
                                model_id = item.get("id")
                                if isinstance(model_id, str):
                                    loaded_models.append(model_id)
                except Exception:
                    loaded_models = []

                gpu_usage: float | None = None
                try:
                    metrics_response = await client.get(metrics_url)
                    if metrics_response.status_code < 400:
                        gpu_usage = self._parse_gpu_usage(metrics_response.text)
                except Exception:
                    gpu_usage = None

            return EngineRuntimeStatus(
                name=engine.name,
                engine_type=engine.engine_type,
                status="available",
                latency_ms=latency_ms,
                gpu_usage=gpu_usage,
                loaded_models=loaded_models,
            )
        except Exception as exc:  # pragma: no cover - runtime/network variability
            return EngineRuntimeStatus(
                name=engine.name,
                engine_type=engine.engine_type,
                status="unavailable",
                last_error=str(exc),
            )

    @staticmethod
    def _parse_gpu_usage(metrics_text: str) -> float | None:
        for raw_line in metrics_text.splitlines():
            line = raw_line.strip()
            if "gpu" not in line.lower():
                continue
            numbers: list[float] = []
            token = ""
            for ch in line:
                if ch.isdigit() or ch == ".":
                    token += ch
                    continue
                if token:
                    with contextlib.suppress(ValueError):
                        numbers.append(float(token))
                    token = ""
            if token:
                with contextlib.suppress(ValueError):
                    numbers.append(float(token))
            if numbers:
                return max(0.0, min(numbers[0], 100.0))
        return None

    def get_cost_summary(self) -> dict[str, Any]:
        """Return aggregate cost and request statistics."""
        aliases = sorted(self._model_map().keys())
        details = []
        total_cost = 0.0
        for alias in aliases:
            alias_cost = float(self._cost_by_alias.get(alias, 0.0))
            total_cost += alias_cost
            details.append(
                {
                    "alias": alias,
                    "requests": int(self._request_count_by_alias.get(alias, 0)),
                    "failures": int(self._failure_count_by_alias.get(alias, 0)),
                    "avg_latency_ms": float(self._latency_by_alias.get(alias, 0.0)),
                    "cost_usd": alias_cost,
                }
            )

        return {
            "total_cost_usd": total_cost,
            "details": details,
            "cost_budget": self._config.routing_policy.cost_budget,
            "budget_exceeded": bool(
                self._config.routing_policy.cost_budget is not None
                and total_cost > self._config.routing_policy.cost_budget
            ),
        }


__all__ = ["GatewayResponse", "GatewayToolCall", "LiteLLMGateway"]
