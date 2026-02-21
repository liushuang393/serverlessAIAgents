"""OpenAIEmbeddingsのテスト."""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.memory.embeddings.openai_embeddings import OpenAIEmbeddings


@pytest.fixture
def openai_embeddings():
    """OpenAIEmbeddingsのフィクスチャ."""
    with patch("agentflow.memory.embeddings.openai_embeddings.AsyncOpenAI") as mock_openai:
        mock_client = AsyncMock()
        mock_openai.return_value = mock_client
        embeddings = OpenAIEmbeddings(api_key="test-api-key", model="text-embedding-3-small", dimension=1536)
        embeddings._client = mock_client
        return embeddings


@pytest.mark.asyncio
async def test_embed_text(openai_embeddings):
    """テキスト埋め込みテスト."""
    # モックレスポンスを準備
    mock_response = MagicMock()
    mock_response.data = [MagicMock(embedding=[0.1] * 1536)]
    openai_embeddings._client.embeddings.create = AsyncMock(return_value=mock_response)

    result = await openai_embeddings.embed_text("テストテキスト")

    assert len(result) == 1536
    assert all(isinstance(x, float) for x in result)
    openai_embeddings._client.embeddings.create.assert_called_once()


@pytest.mark.asyncio
async def test_embed_text_empty(openai_embeddings):
    """空のテキスト埋め込みテスト."""
    with pytest.raises(ValueError, match="Text cannot be empty"):
        await openai_embeddings.embed_text("")


@pytest.mark.asyncio
async def test_embed_batch(openai_embeddings):
    """バッチ埋め込みテスト."""
    # モックレスポンスを準備
    mock_response = MagicMock()
    mock_response.data = [
        MagicMock(embedding=[0.1] * 1536),
        MagicMock(embedding=[0.2] * 1536),
        MagicMock(embedding=[0.3] * 1536),
    ]
    openai_embeddings._client.embeddings.create = AsyncMock(return_value=mock_response)

    texts = ["テキスト1", "テキスト2", "テキスト3"]
    results = await openai_embeddings.embed_batch(texts)

    assert len(results) == 3
    assert all(len(emb) == 1536 for emb in results)
    openai_embeddings._client.embeddings.create.assert_called_once()


@pytest.mark.asyncio
async def test_embed_batch_empty_list(openai_embeddings):
    """空のリスト埋め込みテスト."""
    results = await openai_embeddings.embed_batch([])
    assert results == []


@pytest.mark.asyncio
async def test_embed_batch_all_empty(openai_embeddings):
    """全て空のテキストのバッチ埋め込みテスト."""
    with pytest.raises(ValueError, match="All texts are empty"):
        await openai_embeddings.embed_batch(["", "  ", ""])


@pytest.mark.asyncio
async def test_embed_text_api_error(openai_embeddings):
    """API エラーテスト."""
    openai_embeddings._client.embeddings.create = AsyncMock(side_effect=Exception("API Error"))

    with pytest.raises(RuntimeError, match="Failed to generate embedding"):
        await openai_embeddings.embed_text("テストテキスト")


def test_get_dimension(openai_embeddings):
    """次元数取得テスト."""
    assert openai_embeddings.get_dimension() == 1536


def test_get_model_name(openai_embeddings):
    """モデル名取得テスト."""
    assert openai_embeddings.get_model_name() == "text-embedding-3-small"


def test_init_without_openai():
    """OpenAIパッケージなしの初期化テスト."""
    with (
        patch("agentflow.memory.embeddings.openai_embeddings.AsyncOpenAI", side_effect=ImportError),
        pytest.raises(ImportError, match="openai package is required"),
    ):
        OpenAIEmbeddings(api_key="test-api-key")


def test_default_dimensions():
    """デフォルト次元数テスト."""
    with patch("agentflow.memory.embeddings.openai_embeddings.AsyncOpenAI"):
        # text-embedding-3-small
        emb1 = OpenAIEmbeddings(api_key="test", model="text-embedding-3-small")
        assert emb1.get_dimension() == 1536

        # text-embedding-3-large
        emb2 = OpenAIEmbeddings(api_key="test", model="text-embedding-3-large")
        assert emb2.get_dimension() == 3072

        # text-embedding-ada-002
        emb3 = OpenAIEmbeddings(api_key="test", model="text-embedding-ada-002")
        assert emb3.get_dimension() == 1536
