"""Embedding provider unit tests."""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock, patch

import httpx
import pytest

from agentflow.providers.embedding_provider import OllamaEmbeddingProvider


def _build_async_client(responses: list[httpx.Response]) -> MagicMock:
    """httpx.AsyncClient をモック化したテスト用クライアントを作る."""
    client = MagicMock()
    client.__aenter__ = AsyncMock(return_value=client)
    client.__aexit__ = AsyncMock(return_value=None)
    client.post = AsyncMock(side_effect=responses)
    return client


def _response(url: str, status_code: int, payload: dict[str, object]) -> httpx.Response:
    """httpx.Response ヘルパー."""
    request = httpx.Request("POST", url)
    return httpx.Response(status_code=status_code, json=payload, request=request)


@pytest.mark.asyncio
@patch("httpx.AsyncClient")
async def test_ollama_embed_uses_modern_endpoint(mock_async_client: MagicMock) -> None:
    """通常は /api/embed を使用する."""
    modern = _response(
        "http://localhost:11434/api/embed",
        200,
        {"embeddings": [[0.1, 0.2], [0.3, 0.4]]},
    )
    mock_client = _build_async_client([modern])
    mock_async_client.return_value = mock_client

    provider = OllamaEmbeddingProvider(model="nomic-embed-text:latest")
    result = await provider.embed_batch(["alpha", "beta"])

    assert result == [[0.1, 0.2], [0.3, 0.4]]
    assert len(mock_client.post.await_args_list) == 1
    assert mock_client.post.await_args_list[0].args[0].endswith("/api/embed")


@pytest.mark.asyncio
@patch("httpx.AsyncClient")
async def test_ollama_embed_falls_back_to_legacy_on_404(mock_async_client: MagicMock) -> None:
    """404 時は /api/embeddings へ自動フォールバックする."""
    modern_404 = _response(
        "http://localhost:11434/api/embed",
        404,
        {"error": "Not Found"},
    )
    legacy_1 = _response(
        "http://localhost:11434/api/embeddings",
        200,
        {"embedding": [1, 2]},
    )
    legacy_2 = _response(
        "http://localhost:11434/api/embeddings",
        200,
        {"embedding": [3, 4]},
    )
    legacy_3 = _response(
        "http://localhost:11434/api/embeddings",
        200,
        {"embedding": [5, 6]},
    )
    mock_client = _build_async_client([modern_404, legacy_1, legacy_2, legacy_3])
    mock_async_client.return_value = mock_client

    provider = OllamaEmbeddingProvider(model="nomic-embed-text:latest")
    first = await provider.embed_batch(["one", "two"])
    second = await provider.embed_batch(["three"])

    assert first == [[1.0, 2.0], [3.0, 4.0]]
    assert second == [[5.0, 6.0]]

    endpoints = [call.args[0] for call in mock_client.post.await_args_list]
    assert endpoints[0].endswith("/api/embed")
    assert endpoints[1].endswith("/api/embeddings")
    assert endpoints[2].endswith("/api/embeddings")
    assert endpoints[3].endswith("/api/embeddings")
    assert endpoints.count("http://localhost:11434/api/embed") == 1


@pytest.mark.asyncio
@patch("httpx.AsyncClient")
async def test_ollama_embed_batch_empty_input_skips_http(mock_async_client: MagicMock) -> None:
    """空入力では HTTP 呼び出しを行わない."""
    provider = OllamaEmbeddingProvider(model="nomic-embed-text:latest")
    result = await provider.embed_batch([])

    assert result == []
    mock_async_client.assert_not_called()


@pytest.mark.asyncio
@patch("httpx.AsyncClient")
async def test_ollama_embed_model_not_found_raises_clear_error(mock_async_client: MagicMock) -> None:
    """モデル未取得の 404 は明示的エラーとして返す."""
    modern_404_model_missing = _response(
        "http://localhost:11434/api/embed",
        404,
        {"error": 'model "nomic-embed-text:latest" not found, try pulling it first'},
    )
    mock_client = _build_async_client([modern_404_model_missing])
    mock_async_client.return_value = mock_client

    provider = OllamaEmbeddingProvider(model="nomic-embed-text:latest")

    with pytest.raises(httpx.HTTPStatusError, match="Run `ollama pull nomic-embed-text:latest`"):
        await provider.embed_batch(["hello"])

    assert len(mock_client.post.await_args_list) == 1
    assert mock_client.post.await_args_list[0].args[0].endswith("/api/embed")
