"""LLMProvider unit tests (role-first gateway API)."""

from __future__ import annotations

from typing import TYPE_CHECKING
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.providers.llm_provider import LLMProvider, _detect_provider_from_env, get_llm, reset_llm


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


@pytest.fixture(autouse=True)
def _reset_singleton() -> None:
    reset_llm()
    yield
    reset_llm()


@patch("agentflow.config.get_settings")
def test_detect_provider_from_env(mock_get_settings: MagicMock) -> None:
    mock_settings = MagicMock()
    mock_settings.get_active_llm_config.return_value = {
        "provider": "openai",
        "model": "gpt-4o",
        "api_key": "sk-test",
        "base_url": "https://api.openai.com/v1",
        "timeout": 120,
    }
    mock_get_settings.return_value = mock_settings

    provider, model, api_key, base_url, timeout = _detect_provider_from_env()
    assert provider == "openai"
    assert model == "gpt-4o"
    assert api_key == "sk-test"
    assert base_url == "https://api.openai.com/v1"
    assert timeout == 120


@pytest.mark.asyncio
@patch("agentflow.config.get_settings")
@patch("agentflow.llm.llm_client.LLMClient")
async def test_generate_uses_llm_client(mock_llm_client: MagicMock, mock_get_settings: MagicMock) -> None:
    mock_settings = MagicMock()
    mock_settings.get_active_llm_config.return_value = {"provider": "openai", "model": "gpt-4o"}
    mock_get_settings.return_value = mock_settings

    mock_response = MagicMock()
    mock_response.model_dump.return_value = {"content": "hello", "model": "openai/gpt-4o", "usage": {}}
    client = MagicMock()
    client.generate = AsyncMock(return_value=mock_response)
    mock_llm_client.return_value = client

    provider = LLMProvider()
    result = await provider.generate(
        role="reasoning",
        messages=[{"role": "user", "content": "hi"}],
    )

    assert result["content"] == "hello"
    client.generate.assert_awaited_once()


@pytest.mark.asyncio
@patch("agentflow.config.get_settings")
@patch("agentflow.llm.llm_client.LLMClient")
async def test_chat_wrapper_with_tools_calls_tool_call(
    mock_llm_client: MagicMock,
    mock_get_settings: MagicMock,
) -> None:
    mock_settings = MagicMock()
    mock_settings.get_active_llm_config.return_value = {"provider": "openai", "model": "gpt-4o"}
    mock_get_settings.return_value = mock_settings

    mock_response = MagicMock()
    mock_response.model_dump.return_value = {"content": None, "tool_calls": [{"id": "x", "name": "search", "arguments": "{}"}]}
    client = MagicMock()
    client.tool_call = AsyncMock(return_value=mock_response)
    mock_llm_client.return_value = client

    provider = LLMProvider()
    result = await provider.chat(
        [{"role": "user", "content": "search docs"}],
        tools=[{"type": "function", "function": {"name": "search"}}],
    )

    assert "tool_calls" in result
    client.tool_call.assert_awaited_once()


@pytest.mark.asyncio
@patch("agentflow.config.get_settings")
@patch("agentflow.llm.llm_client.LLMClient")
async def test_complete_wrapper_calls_generate(
    mock_llm_client: MagicMock,
    mock_get_settings: MagicMock,
) -> None:
    mock_settings = MagicMock()
    mock_settings.get_active_llm_config.return_value = {"provider": "openai", "model": "gpt-4o"}
    mock_get_settings.return_value = mock_settings

    mock_response = MagicMock()
    mock_response.model_dump.return_value = {"content": "done", "model": "openai/gpt-4o", "usage": {}}
    client = MagicMock()
    client.generate = AsyncMock(return_value=mock_response)
    mock_llm_client.return_value = client

    provider = LLMProvider()
    result = await provider.complete("test prompt")

    assert result["content"] == "done"
    client.generate.assert_awaited_once()


@pytest.mark.asyncio
@patch("agentflow.config.get_settings")
@patch("agentflow.llm.llm_client.LLMClient")
async def test_stream_supports_legacy_style(
    mock_llm_client: MagicMock,
    mock_get_settings: MagicMock,
) -> None:
    mock_settings = MagicMock()
    mock_settings.get_active_llm_config.return_value = {"provider": "openai", "model": "gpt-4o"}
    mock_get_settings.return_value = mock_settings

    async def _stream(*_args: object, **_kwargs: object) -> AsyncIterator[str]:
        for token in ["Hello", " ", "World"]:
            yield token

    client = MagicMock()
    client.stream = _stream
    mock_llm_client.return_value = client

    provider = LLMProvider()
    chunks: list[str] = []
    async for token in provider.stream([{"role": "user", "content": "hi"}]):
        chunks.append(token)

    assert chunks == ["Hello", " ", "World"]


@patch("agentflow.config.get_settings")
@patch("agentflow.llm.llm_client.LLMClient")
def test_get_llm_singleton_behavior(
    mock_llm_client: MagicMock,
    mock_get_settings: MagicMock,
) -> None:
    mock_settings = MagicMock()
    mock_settings.get_active_llm_config.return_value = {"provider": "openai", "model": "gpt-4o"}
    mock_get_settings.return_value = mock_settings
    mock_llm_client.return_value = MagicMock()

    first = get_llm()
    second = get_llm()
    fresh = get_llm(_new_instance=True)

    assert first is second
    assert first is not fresh
