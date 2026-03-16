"""Layer 1 の LLM アダプタ抽象契約."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Protocol


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


class LLMBackend(Protocol):
    """共有 Gateway から利用する LLM backend 契約."""

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
        """応答を生成する."""

    def stream(
        self,
        *,
        role: str,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        tools: list[dict[str, Any]] | None = None,
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> AsyncIterator[str]:
        """ストリーム応答を返す."""

    async def tool_call(
        self,
        *,
        role: str,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]],
        metadata: dict[str, Any] | None = None,
        model_alias: str | None = None,
    ) -> dict[str, Any]:
        """tool calling を実行する."""
