"""LLMClient のユニットテスト."""

from __future__ import annotations

from unittest.mock import AsyncMock

import httpx
import pytest

from agentflow.llm.llm_client import LLMClient, LLMConfig, LLMMessage, LLMResponse


def _sample_messages() -> list[LLMMessage]:
    return [LLMMessage(role="user", content="hello")]


def test_initialize_ollama_normalizes_base_url() -> None:
    """OLLAMA_BASE_URL が /v1 付きでも重複しない."""
    client = LLMClient(
        LLMConfig(
            provider="ollama",
            model="qwen3:4b",
            base_url="http://localhost:11434/v1",
        )
    )

    assert client._base_url == "http://localhost:11434/v1"
    assert client._ollama_native_base_url == "http://localhost:11434"


@pytest.mark.asyncio
async def test_ollama_chat_falls_back_to_native_api_on_404(monkeypatch: pytest.MonkeyPatch) -> None:
    """OpenAI互換 endpoint が 404 の場合に /api/chat へフォールバックする."""

    async def _mock_post(self: httpx.AsyncClient, url: str, json: dict) -> httpx.Response:
        request = httpx.Request("POST", url, json=json)
        return httpx.Response(status_code=404, request=request)

    monkeypatch.setattr(httpx.AsyncClient, "post", _mock_post)

    client = LLMClient(
        LLMConfig(
            provider="ollama",
            model="qwen3:4b",
            base_url="http://localhost:11434",
        )
    )
    fallback_response = LLMResponse(
        content="fallback",
        model="qwen3:4b",
        usage={},
        finish_reason="stop",
    )
    client._chat_ollama_native = AsyncMock(return_value=fallback_response)  # type: ignore[method-assign]

    response = await client._chat_openai_compatible(_sample_messages())

    assert response.content == "fallback"
    client._chat_ollama_native.assert_awaited_once()  # type: ignore[attr-defined]


@pytest.mark.asyncio
async def test_localai_chat_does_not_fallback_on_404(monkeypatch: pytest.MonkeyPatch) -> None:
    """LocalAI は 404 をそのまま例外化する."""

    async def _mock_post(self: httpx.AsyncClient, url: str, json: dict) -> httpx.Response:
        request = httpx.Request("POST", url, json=json)
        return httpx.Response(status_code=404, request=request)

    monkeypatch.setattr(httpx.AsyncClient, "post", _mock_post)

    client = LLMClient(
        LLMConfig(
            provider="localai",
            model="local-model",
            base_url="http://localhost:8080",
        )
    )

    with pytest.raises(httpx.HTTPStatusError):
        await client._chat_openai_compatible(_sample_messages())


@pytest.mark.asyncio
async def test_ollama_model_not_found_does_not_fallback(monkeypatch: pytest.MonkeyPatch) -> None:
    """Ollama の model not found はネイティブAPIへフォールバックしない."""

    async def _mock_post(self: httpx.AsyncClient, url: str, json: dict) -> httpx.Response:
        request = httpx.Request("POST", url, json=json)
        return httpx.Response(
            status_code=404,
            request=request,
            json={"error": {"message": "model 'qwen3:4b' not found"}},
        )

    monkeypatch.setattr(httpx.AsyncClient, "post", _mock_post)

    client = LLMClient(
        LLMConfig(
            provider="ollama",
            model="qwen3:4b",
            base_url="http://localhost:11434",
        )
    )
    client._chat_ollama_native = AsyncMock()  # type: ignore[method-assign]

    with pytest.raises(httpx.HTTPStatusError):
        await client._chat_openai_compatible(_sample_messages())
    client._chat_ollama_native.assert_not_awaited()  # type: ignore[attr-defined]
