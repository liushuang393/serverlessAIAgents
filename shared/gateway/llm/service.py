"""Layer 2 の LLM Gateway サービス."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from shared.registry import ComponentToggle


class SharedLLMGateway:
    """LLM backend を隠蔽する共有 Gateway."""

    def __init__(self, toggle: ComponentToggle | None = None) -> None:
        # 遅延 import: infrastructure 依存をトップレベルから排除
        from infrastructure.llm import get_llm_backend

        self._backend = get_llm_backend(toggle)

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
        return await self._backend.generate(
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
        async for chunk in self._backend.stream(
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
        return await self._backend.tool_call(
            role=role,
            messages=messages,
            tools=tools,
            metadata=metadata,
            model_alias=model_alias,
        )
