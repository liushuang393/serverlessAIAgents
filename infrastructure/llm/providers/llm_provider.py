"""Gateway-first LLM provider facade.

Public API is role-driven:
- generate()
- stream()
- tool_call()

Legacy wrappers (`chat`, `complete`) are kept for compatibility and internally
forward to the new gateway-based APIs.
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from shared.config import AgentFlowSettings
    from infrastructure.llm.llm_client import LLMClient
    from contracts.runtime.context import RuntimeContext


logger = logging.getLogger(__name__)

_default_llm: LLMProvider | None = None


class LLMProviderConfig(BaseModel):
    """LLM provider runtime defaults."""

    default_role: str = Field(default="reasoning", description="default model role")
    temperature: float = Field(default=0.7, ge=0.0, le=2.0, description="temperature")
    max_tokens: int = Field(default=2000, gt=0, description="max tokens")
    timeout: int = Field(default=360, gt=0, description="timeout seconds")
    gateway_config_path: str | None = Field(default=None, description="optional gateway yaml path")


def _detect_provider_from_env(
    settings: AgentFlowSettings | None = None,
) -> tuple[str, str, str | None, str | None, int]:
    """Detect active provider info from settings for diagnostics/compatibility."""
    from shared.config import get_settings

    active_settings = settings or get_settings()
    llm_config = active_settings.get_active_llm_config()

    provider = str(llm_config.get("provider", "mock"))
    model = str(llm_config.get("model", "mock"))
    api_key = llm_config.get("api_key")
    base_url = llm_config.get("base_url")
    timeout = int(llm_config.get("timeout", 180) or 180)

    if provider == "mock":
        logger.warning("No LLM API key found in environment. Gateway fallback may use local/mock paths.")

    return provider, model, api_key, base_url, timeout


class LLMProvider:
    """Role-driven provider facade backed by `LLMClient`."""

    def __init__(
        self,
        config: LLMProviderConfig | None = None,
        *,
        role: str | None = None,
        temperature: float | None = None,
        max_tokens: int | None = None,
        settings: AgentFlowSettings | None = None,
        context: RuntimeContext | None = None,
    ) -> None:
        self._config = config or LLMProviderConfig()
        self._role_override = role
        self._temperature_override = temperature
        self._max_tokens_override = max_tokens
        self._settings = settings
        self._context = context
        self._provider_info: tuple[str, str, str | None, str | None] | None = None
        self._client: LLMClient | None = None
        self._initialize_client()

    def _initialize_client(self) -> None:
        from infrastructure.llm.llm_client import LLMClient, LLMConfig

        provider, model, api_key, base_url, timeout = _detect_provider_from_env(self._settings)
        self._provider_info = (provider, model, api_key, base_url)

        self._client = LLMClient(
            LLMConfig(
                role=(self._role_override or self._config.default_role),
                model=model,
                provider=provider,
                api_key=api_key,
                base_url=base_url,
                temperature=self._temperature_override or self._config.temperature,
                max_tokens=self._max_tokens_override or self._config.max_tokens,
                timeout=timeout,
                gateway_config_path=self._config.gateway_config_path,
            )
        )
        logger.info(
            "LLMProvider initialized via gateway: role=%s, provider=%s, model=%s",
            self._role_override or self._config.default_role,
            provider,
            model,
        )

    def _require_client(self) -> LLMClient:
        client = self._client
        if client is None:
            msg = "LLM client is not initialized"
            raise RuntimeError(msg)
        return client

    def _merge_metadata(self, metadata: dict[str, Any] | None) -> dict[str, Any] | None:
        payload: dict[str, Any] = {}
        if metadata:
            payload.update(metadata)
        if self._context is not None:
            app_name = self._context.metadata.get("app_name")
            if isinstance(app_name, str) and app_name.strip():
                payload.setdefault("app_name", app_name.strip())
            agent_name = self._context.metadata.get("agent_name")
            if isinstance(agent_name, str) and agent_name.strip():
                payload.setdefault("agent_name", agent_name.strip())
        return payload or None

    async def generate(
        self,
        role: str,
        prompt: str | None = None,
        *,
        messages: list[dict[str, str]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        temperature: float | None = None,
        max_tokens: int | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> dict[str, Any]:
        """Generate response via model role."""
        from infrastructure.llm.llm_client import LLMMessage

        llm_messages = [LLMMessage(**message) for message in messages] if messages else None
        response = await self._require_client().generate(
            role=role,
            prompt=prompt,
            messages=llm_messages,
            tools=tools,
            temperature=temperature,
            max_tokens=max_tokens,
            metadata=self._merge_metadata(metadata),
            model_alias=model_alias,
        )
        return response.model_dump()

    async def stream(
        self,
        role: str | list[dict[str, str]],
        prompt: str | None = None,
        *,
        messages: list[dict[str, str]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        temperature: float | None = None,
        max_tokens: int | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> AsyncIterator[str]:
        """Stream response tokens.

        New style:
            stream(role="reasoning", prompt="...")

        Legacy style:
            stream([{"role": "user", "content": "..."}])
        """
        from infrastructure.llm.llm_client import LLMMessage

        if isinstance(role, list):
            llm_messages = [LLMMessage(**message) for message in role]
            async for chunk in self._require_client().stream(
                llm_messages,
                tools=tools,
                temperature=temperature,
                max_tokens=max_tokens,
                metadata=self._merge_metadata(metadata),
                model_alias=model_alias,
            ):
                yield chunk
            return

        llm_messages = [LLMMessage(**message) for message in messages] if messages else None
        async for chunk in self._require_client().stream(
            role,
            prompt,
            messages=llm_messages,
            tools=tools,
            temperature=temperature,
            max_tokens=max_tokens,
            metadata=self._merge_metadata(metadata),
            model_alias=model_alias,
        ):
            yield chunk

    async def tool_call(
        self,
        role: str,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]],
        *,
        tool_choice: str | dict[str, Any] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> dict[str, Any]:
        """Generate tool-call response."""
        from infrastructure.llm.llm_client import LLMMessage

        llm_messages = [LLMMessage(**message) for message in messages]
        response = await self._require_client().tool_call(
            role=role,
            messages=llm_messages,
            tools=tools,
            tool_choice=tool_choice,
            metadata=self._merge_metadata(metadata),
            model_alias=model_alias,
        )
        return response.model_dump()

    async def chat(self, messages: list[dict[str, str]], **kwargs: Any) -> dict[str, Any]:
        """Legacy wrapper for chat API.

        Internally this routes to `generate`/`tool_call` via the gateway.
        """
        role = str(kwargs.pop("role", self._role_override or self._config.default_role))
        tools = kwargs.get("tools")
        if tools:
            return await self.tool_call(
                role=role,
                messages=messages,
                tools=tools,
                tool_choice=kwargs.get("tool_choice"),
                metadata=kwargs.get("metadata"),
                model_alias=kwargs.get("model_alias"),
            )
        return await self.generate(
            role=role,
            messages=messages,
            tools=None,
            temperature=kwargs.get("temperature"),
            max_tokens=kwargs.get("max_tokens"),
            metadata=kwargs.get("metadata"),
            model_alias=kwargs.get("model_alias"),
        )

    async def complete(self, prompt: str, **kwargs: Any) -> dict[str, Any]:
        """Legacy wrapper for completion API."""
        role = str(kwargs.pop("role", self._role_override or self._config.default_role))
        return await self.generate(
            role=role,
            prompt=prompt,
            tools=kwargs.get("tools"),
            temperature=kwargs.get("temperature"),
            max_tokens=kwargs.get("max_tokens"),
            metadata=kwargs.get("metadata"),
            model_alias=kwargs.get("model_alias"),
        )

    @property
    def config(self) -> LLMProviderConfig:
        return self._config


def get_llm(
    *,
    role: str | None = None,
    temperature: float | None = None,
    max_tokens: int | None = None,
    context: RuntimeContext | None = None,
    _new_instance: bool = False,
) -> LLMProvider:
    """Get default LLM provider instance."""
    global _default_llm

    if context is not None or role is not None or temperature is not None or max_tokens is not None or _new_instance:
        from kernel.runtime import resolve_settings

        return LLMProvider(
            role=role,
            temperature=temperature,
            max_tokens=max_tokens,
            settings=resolve_settings(context),
            context=context,
        )

    if _default_llm is None:
        _default_llm = LLMProvider()

    return _default_llm


def reset_llm() -> None:
    """Reset global singleton instance (tests)."""
    global _default_llm
    _default_llm = None
