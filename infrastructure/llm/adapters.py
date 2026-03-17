"""LLM backend の具体アダプタ.

NOTE: L1 (infrastructure) は上位層 (agentflow.providers) を直接 import しない。
LLMProvider は TYPE_CHECKING 専用、get_llm は関数内遅延 import で境界を維持。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from infrastructure.llm.providers.llm_provider import LLMProvider


class NoOpLLMBackend:
    """機能無効時に使う no-op backend."""

    async def generate(
        self,
        *,
        role: str,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> dict[str, Any]:
        del role, prompt, messages, tools, metadata, model_alias
        return {"content": "", "provider": "noop", "model": "noop", "disabled": True}

    async def stream(
        self,
        *,
        role: str,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> AsyncIterator[str]:
        del role, prompt, messages, tools, metadata, model_alias
        chunks: tuple[str, ...] = ()
        for chunk in chunks:
            yield chunk

    async def tool_call(
        self,
        *,
        role: str,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]],
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> dict[str, Any]:
        del role, messages, tools, metadata, model_alias
        return {"content": "", "tool_calls": [], "provider": "noop", "disabled": True}


class MockLLMBackend(NoOpLLMBackend):
    """テスト向け mock backend."""

    async def generate(
        self,
        *,
        role: str,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> dict[str, Any]:
        del tools, metadata
        return {
            "content": prompt or ((messages or [{"content": ""}])[-1].get("content", "")),
            "provider": "mock",
            "role": role,
            "model": model_alias or "mock",
        }


class AgentFlowLLMBackend:
    """既存 AgentFlow LLM facade を包む backend."""

    def __init__(self, provider: LLMProvider | None = None) -> None:
        if provider is None:
            # 遅延インポート: L1 → 上位層への静的依存を回避
            from infrastructure.llm.providers.llm_provider import get_llm

            provider = get_llm(_new_instance=True)
        self._provider = provider

    async def generate(
        self,
        *,
        role: str,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> dict[str, Any]:
        return await self._provider.generate(
            role=role,
            prompt=prompt,
            messages=messages,
            tools=tools,
            metadata=metadata,
            model_alias=model_alias,
        )

    async def stream(
        self,
        *,
        role: str,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> AsyncIterator[str]:
        async for chunk in self._provider.stream(
            role=role,
            prompt=prompt,
            messages=messages,
            tools=tools,
            metadata=metadata,
            model_alias=model_alias,
        ):
            yield chunk

    async def tool_call(
        self,
        *,
        role: str,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]],
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> dict[str, Any]:
        return await self._provider.tool_call(
            role=role,
            messages=messages,
            tools=tools,
            metadata=metadata,
            model_alias=model_alias,
        )
