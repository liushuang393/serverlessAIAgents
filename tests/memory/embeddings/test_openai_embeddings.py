"""OpenAIEmbeddingsのテスト.

実装は LiteLLMGateway を使用してエンベディングを生成する。
"""

from unittest.mock import AsyncMock, patch

import pytest

from infrastructure.memory.embeddings.openai_embeddings import OpenAIEmbeddings


@pytest.fixture
def openai_embeddings():
    """OpenAIEmbeddingsのフィクスチャ（Gateway をモック化）."""
    with patch("infrastructure.memory.embeddings.openai_embeddings.LiteLLMGateway") as mock_gw_cls:
        mock_gateway = AsyncMock()
        mock_gw_cls.return_value = mock_gateway
        embeddings = OpenAIEmbeddings(api_key="test-api-key", model="text-embedding-3-small", dimension=1536)
        # _gateway を直接差し替え
        embeddings._gateway = mock_gateway
        return embeddings


@pytest.mark.asyncio
async def test_embed_text(openai_embeddings):
    """テキスト埋め込みテスト."""
    openai_embeddings._gateway.embedding = AsyncMock(return_value=[[0.1] * 1536])

    result = await openai_embeddings.embed_text("テストテキスト")

    assert len(result) == 1536
    assert all(isinstance(x, float) for x in result)
    openai_embeddings._gateway.embedding.assert_called_once()


@pytest.mark.asyncio
async def test_embed_text_empty(openai_embeddings):
    """空のテキスト埋め込みテスト."""
    with pytest.raises(ValueError, match="Text cannot be empty"):
        await openai_embeddings.embed_text("")


@pytest.mark.asyncio
async def test_embed_batch(openai_embeddings):
    """バッチ埋め込みテスト."""
    openai_embeddings._gateway.embedding = AsyncMock(
        return_value=[[0.1] * 1536, [0.2] * 1536, [0.3] * 1536],
    )

    texts = ["テキスト1", "テキスト2", "テキスト3"]
    results = await openai_embeddings.embed_batch(texts)

    assert len(results) == 3
    assert all(len(emb) == 1536 for emb in results)
    openai_embeddings._gateway.embedding.assert_called_once()


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
    openai_embeddings._gateway.embedding = AsyncMock(side_effect=Exception("API Error"))

    with pytest.raises(RuntimeError, match="Failed to generate embedding"):
        await openai_embeddings.embed_text("テストテキスト")


def test_get_dimension(openai_embeddings):
    """次元数取得テスト."""
    assert openai_embeddings.get_dimension() == 1536


def test_get_model_name(openai_embeddings):
    """モデル名取得テスト."""
    assert openai_embeddings.get_model_name() == "text-embedding-3-small"


def test_default_dimensions():
    """デフォルト次元数テスト."""
    with patch("infrastructure.memory.embeddings.openai_embeddings.LiteLLMGateway"):
        # text-embedding-3-small
        emb1 = OpenAIEmbeddings(api_key="test", model="text-embedding-3-small")
        assert emb1.get_dimension() == 1536

        # text-embedding-3-large
        emb2 = OpenAIEmbeddings(api_key="test", model="text-embedding-3-large")
        assert emb2.get_dimension() == 3072

        # text-embedding-ada-002
        emb3 = OpenAIEmbeddings(api_key="test", model="text-embedding-ada-002")
        assert emb3.get_dimension() == 1536
