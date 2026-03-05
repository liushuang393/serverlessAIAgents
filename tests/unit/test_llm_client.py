"""LLMClient unit tests (gateway-first implementation)."""

from __future__ import annotations

from types import SimpleNamespace
from typing import TYPE_CHECKING
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.llm.gateway.config import ModelConfig
from agentflow.llm.llm_client import LLMClient, LLMConfig, LLMMessage


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


def _gateway_models() -> list[ModelConfig]:
    return [
        ModelConfig(alias="reasoning_claude", provider="anthropic", model="claude-sonnet-4-20250514"),
        ModelConfig(alias="coding_openai", provider="openai", model="gpt-4o"),
    ]


@pytest.mark.asyncio
async def test_generate_routes_to_gateway() -> None:
    gateway = MagicMock()
    gateway.config = SimpleNamespace(models=_gateway_models())
    gateway.generate = AsyncMock(
        return_value={
            "content": "ok",
            "model": "anthropic/claude-sonnet-4-20250514",
            "usage": {"prompt_tokens": 1, "completion_tokens": 2, "total_tokens": 3},
            "finish_reason": "stop",
            "tool_calls": [],
            "cost_usd": 0.01,
            "latency_ms": 10.0,
        }
    )

    with patch("agentflow.llm.llm_client.LiteLLMGateway", return_value=gateway):
        client = LLMClient(LLMConfig(role="reasoning"))
        response = await client.generate(
            role="reasoning",
            messages=[LLMMessage(role="user", content="hello")],
        )

    assert response.content == "ok"
    assert response.model == "anthropic/claude-sonnet-4-20250514"
    gateway.generate.assert_awaited_once()


@pytest.mark.asyncio
async def test_chat_with_tools_uses_tool_call() -> None:
    gateway = MagicMock()
    gateway.config = SimpleNamespace(models=_gateway_models())
    gateway.tool_call = AsyncMock(
        return_value={
            "content": None,
            "model": "openai/gpt-4o",
            "usage": {"prompt_tokens": 3, "completion_tokens": 1, "total_tokens": 4},
            "finish_reason": "tool_calls",
            "tool_calls": [{"id": "call_1", "name": "search_docs", "arguments": "{}"}],
            "cost_usd": 0.0,
            "latency_ms": 20.0,
        }
    )

    with patch("agentflow.llm.llm_client.LiteLLMGateway", return_value=gateway):
        client = LLMClient(LLMConfig(role="coding"))
        response = await client.chat(
            [LLMMessage(role="user", content="find docs")],
            tools=[{"type": "function", "function": {"name": "search_docs"}}],
        )

    assert response.has_tool_calls() is True
    assert response.tool_calls[0].name == "search_docs"
    gateway.tool_call.assert_awaited_once()


@pytest.mark.asyncio
async def test_complete_uses_default_role() -> None:
    gateway = MagicMock()
    gateway.config = SimpleNamespace(models=_gateway_models())
    gateway.generate = AsyncMock(
        return_value={
            "content": "done",
            "model": "openai/gpt-4o",
            "usage": {},
            "finish_reason": "stop",
            "tool_calls": [],
            "cost_usd": 0.0,
            "latency_ms": 1.0,
        }
    )

    with patch("agentflow.llm.llm_client.LiteLLMGateway", return_value=gateway):
        client = LLMClient(LLMConfig(role="coding"))
        response = await client.complete("write code")

    assert response.content == "done"
    kwargs = gateway.generate.await_args.kwargs
    assert kwargs["role"] == "coding"
    assert kwargs["prompt"] == "write code"


@pytest.mark.asyncio
async def test_stream_supports_legacy_message_style() -> None:
    async def _stream() -> AsyncIterator[str]:
        for chunk in ["A", "B", "C"]:
            yield chunk

    gateway = MagicMock()
    gateway.config = SimpleNamespace(models=_gateway_models())
    gateway.stream = MagicMock(return_value=_stream())

    with patch("agentflow.llm.llm_client.LiteLLMGateway", return_value=gateway):
        client = LLMClient(LLMConfig(role="reasoning"))
        out: list[str] = []
        async for token in client.stream([LLMMessage(role="user", content="hello")]):
            out.append(token)

    assert out == ["A", "B", "C"]
    kwargs = gateway.stream.call_args.kwargs
    assert kwargs["role"] == "reasoning"
    assert kwargs["messages"] == [{"role": "user", "content": "hello"}]
