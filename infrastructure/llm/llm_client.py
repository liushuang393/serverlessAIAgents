"""LLM client facade backed by AgentFlow LiteLLM gateway.

This module keeps compatibility models (`LLMMessage`, `LLMResponse`) while routing
all calls through `legacy LLM gateway facade`.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from infrastructure.llm.gateway import LiteLLMGateway


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


class LLMMessage(BaseModel):
    """Message payload for chat completion."""

    role: str = Field(..., description="message role")
    content: str | None = Field(default=None, description="message content")
    tool_call_id: str | None = Field(default=None, description="tool call id")
    name: str | None = Field(default=None, description="tool name")


class ToolCall(BaseModel):
    """Tool invocation payload."""

    id: str = Field(..., description="tool call id")
    name: str = Field(..., description="tool function name")
    arguments: str | dict[str, Any] = Field(..., description="tool arguments")

    def get_arguments_dict(self) -> dict[str, Any]:
        """Return JSON arguments as a dictionary."""
        if isinstance(self.arguments, dict):
            return self.arguments
        loaded = json.loads(self.arguments)
        if isinstance(loaded, dict):
            return loaded
        return {}


class LLMResponse(BaseModel):
    """Unified LLM response payload."""

    content: str | None = Field(default=None)
    model: str = Field(...)
    usage: dict[str, int] = Field(default_factory=dict)
    finish_reason: str | None = Field(default=None)
    tool_calls: list[ToolCall] = Field(default_factory=list)
    cost_usd: float = Field(default=0.0)
    latency_ms: float = Field(default=0.0)

    def has_tool_calls(self) -> bool:
        return len(self.tool_calls) > 0


class LLMConfig(BaseModel):
    """LLM client config.

    `role`/`model_alias` are the preferred configuration. `provider`/`model`
    are retained for backward compatibility and resolved via registry if possible.
    """

    role: str = Field(default="reasoning")
    model_alias: str | None = Field(default=None)

    provider: str | None = Field(default=None)
    model: str | None = Field(default=None)

    api_key: str | None = Field(default=None)
    base_url: str | None = Field(default=None)
    temperature: float = Field(default=0.7, ge=0.0, le=2.0)
    max_tokens: int = Field(default=2000, gt=0)
    timeout: int = Field(default=360, gt=0)

    gateway_config_path: str | None = Field(default=None)


class LLMClient:
    """Gateway-backed LLM client."""

    def __init__(self, config: LLMConfig) -> None:
        self._config = config
        gateway_path = Path(config.gateway_config_path) if config.gateway_config_path else None
        self._gateway = LiteLLMGateway(config_path=gateway_path)

    def _resolve_role(self, role: str | None = None) -> str:
        if role:
            return role
        return self._config.role

    def _resolve_model_alias(self, model_alias: str | None = None) -> str | None:
        if model_alias:
            return model_alias

        if self._config.model_alias:
            return self._config.model_alias

        if self._config.model:
            model_name = self._config.model.strip().lower()
            model_map = {m.alias: m for m in self._gateway.config.models}
            if model_name in model_map:
                return model_name

            provider = (self._config.provider or "").strip().lower()
            for alias, cfg in model_map.items():
                if cfg.model.strip().lower() == model_name and (not provider or cfg.provider == provider):
                    return alias
        return None

    @staticmethod
    def _to_message_dicts(messages: list[LLMMessage]) -> list[dict[str, Any]]:
        return [{key: value for key, value in msg.model_dump().items() if value is not None} for msg in messages]

    @staticmethod
    def _to_response(payload: Any) -> LLMResponse:
        tool_calls_raw = payload.tool_calls if hasattr(payload, "tool_calls") else payload.get("tool_calls", [])
        tool_calls = [
            ToolCall(id=call.id, name=call.name, arguments=call.arguments)
            if hasattr(call, "id")
            else ToolCall.model_validate(call)
            for call in tool_calls_raw
        ]

        return LLMResponse(
            content=getattr(payload, "content", None) if hasattr(payload, "content") else payload.get("content"),
            model=getattr(payload, "model", "unknown")
            if hasattr(payload, "model")
            else payload.get("model", "unknown"),
            usage=getattr(payload, "usage", {}) if hasattr(payload, "usage") else payload.get("usage", {}),
            finish_reason=getattr(payload, "finish_reason", None)
            if hasattr(payload, "finish_reason")
            else payload.get("finish_reason"),
            tool_calls=tool_calls,
            cost_usd=float(
                getattr(payload, "cost_usd", 0.0) if hasattr(payload, "cost_usd") else payload.get("cost_usd", 0.0)
            ),
            latency_ms=float(
                getattr(payload, "latency_ms", 0.0)
                if hasattr(payload, "latency_ms")
                else payload.get("latency_ms", 0.0)
            ),
        )

    async def generate(
        self,
        role: str,
        prompt: str | None = None,
        *,
        messages: list[LLMMessage] | None = None,
        tools: list[dict[str, Any]] | None = None,
        temperature: float | None = None,
        max_tokens: int | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> LLMResponse:
        """Preferred API: role-driven generation."""
        gateway_response = await self._gateway.generate(
            role=self._resolve_role(role),
            prompt=prompt,
            messages=self._to_message_dicts(messages) if messages else None,
            tools=tools,
            temperature=temperature if temperature is not None else self._config.temperature,
            max_tokens=max_tokens if max_tokens is not None else self._config.max_tokens,
            metadata=metadata,
            model_alias=self._resolve_model_alias(model_alias),
        )
        return self._to_response(gateway_response)

    async def tool_call(
        self,
        role: str,
        messages: list[LLMMessage],
        tools: list[dict[str, Any]],
        *,
        tool_choice: str | dict[str, Any] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> LLMResponse:
        """Preferred API: explicit tool calling."""
        gateway_response = await self._gateway.tool_call(
            role=self._resolve_role(role),
            messages=self._to_message_dicts(messages),
            tools=tools,
            tool_choice=tool_choice,
            metadata=metadata,
            model_alias=self._resolve_model_alias(model_alias),
        )
        return self._to_response(gateway_response)

    async def complete(self, prompt: str, **kwargs: Any) -> LLMResponse:
        """Backward-compatible prompt completion."""
        role = self._resolve_role(kwargs.pop("role", None))
        model_alias = self._resolve_model_alias(kwargs.pop("model_alias", None))
        return await self.generate(
            role=role,
            prompt=prompt,
            temperature=kwargs.pop("temperature", None),
            max_tokens=kwargs.pop("max_tokens", None),
            metadata=kwargs.pop("metadata", None),
            model_alias=model_alias,
        )

    async def chat(self, messages: list[LLMMessage], **kwargs: Any) -> LLMResponse:
        """Backward-compatible chat API."""
        role = self._resolve_role(kwargs.pop("role", None))
        model_alias = self._resolve_model_alias(kwargs.pop("model_alias", None))
        tools = kwargs.pop("tools", None)
        tool_choice = kwargs.pop("tool_choice", None)

        if tools:
            return await self.tool_call(
                role=role,
                messages=messages,
                tools=tools,
                tool_choice=tool_choice,
                metadata=kwargs.pop("metadata", None),
                model_alias=model_alias,
            )

        return await self.generate(
            role=role,
            messages=messages,
            temperature=kwargs.pop("temperature", None),
            max_tokens=kwargs.pop("max_tokens", None),
            metadata=kwargs.pop("metadata", None),
            model_alias=model_alias,
        )

    async def stream(
        self,
        role_or_messages: str | list[LLMMessage],
        prompt: str | None = None,
        *,
        messages: list[LLMMessage] | None = None,
        tools: list[dict[str, Any]] | None = None,
        temperature: float | None = None,
        max_tokens: int | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> AsyncIterator[str]:
        """Stream API (supports both new and legacy call styles).

        New style:
            stream("reasoning", prompt="...")

        Legacy style:
            stream([LLMMessage(...)])
        """
        role = role_or_messages if isinstance(role_or_messages, str) else self._resolve_role(None)

        request_messages = messages
        request_prompt = prompt

        if isinstance(role_or_messages, list):
            request_messages = role_or_messages
            request_prompt = None

        async for chunk in self._gateway.stream(
            role=self._resolve_role(role),
            prompt=request_prompt,
            messages=self._to_message_dicts(request_messages) if request_messages else None,
            tools=tools,
            temperature=temperature if temperature is not None else self._config.temperature,
            max_tokens=max_tokens if max_tokens is not None else self._config.max_tokens,
            metadata=metadata,
            model_alias=self._resolve_model_alias(model_alias),
        ):
            yield chunk
