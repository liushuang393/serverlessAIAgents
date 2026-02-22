"""SentenceTransformerEmbeddingsのテスト."""

from unittest.mock import MagicMock, patch

import numpy as np
import pytest

from agentflow.memory.embeddings.sentence_transformer_embeddings import (
    SentenceTransformerEmbeddings,
)


@pytest.fixture
def sentence_transformer_embeddings():
    """SentenceTransformerEmbeddingsのフィクスチャ."""
    with patch("agentflow.memory.embeddings.sentence_transformer_embeddings.SentenceTransformer") as mock_st:
        mock_model = MagicMock()
        mock_model.get_sentence_embedding_dimension.return_value = 384
        mock_st.return_value = mock_model
        embeddings = SentenceTransformerEmbeddings(model_name="all-MiniLM-L6-v2", device="cpu")
        embeddings._model = mock_model
        return embeddings


@pytest.mark.asyncio
async def test_embed_text(sentence_transformer_embeddings):
    """テキスト埋め込みテスト."""
    # モックレスポンスを準備
    mock_embedding = np.array([0.1] * 384)
    sentence_transformer_embeddings._model.encode = MagicMock(return_value=mock_embedding)

    result = await sentence_transformer_embeddings.embed_text("テストテキスト")

    assert len(result) == 384
    assert all(isinstance(x, float) for x in result)
    sentence_transformer_embeddings._model.encode.assert_called_once()


@pytest.mark.asyncio
async def test_embed_text_empty(sentence_transformer_embeddings):
    """空のテキスト埋め込みテスト."""
    with pytest.raises(ValueError, match="Text cannot be empty"):
        await sentence_transformer_embeddings.embed_text("")


@pytest.mark.asyncio
async def test_embed_batch(sentence_transformer_embeddings):
    """バッチ埋め込みテスト."""
    # モックレスポンスを準備
    mock_embeddings = np.array([[0.1] * 384, [0.2] * 384, [0.3] * 384])
    sentence_transformer_embeddings._model.encode = MagicMock(return_value=mock_embeddings)

    texts = ["テキスト1", "テキスト2", "テキスト3"]
    results = await sentence_transformer_embeddings.embed_batch(texts)

    assert len(results) == 3
    assert all(len(emb) == 384 for emb in results)
    sentence_transformer_embeddings._model.encode.assert_called_once()


@pytest.mark.asyncio
async def test_embed_batch_empty_list(sentence_transformer_embeddings):
    """空のリスト埋め込みテスト."""
    results = await sentence_transformer_embeddings.embed_batch([])
    assert results == []


@pytest.mark.asyncio
async def test_embed_batch_all_empty(sentence_transformer_embeddings):
    """全て空のテキストのバッチ埋め込みテスト."""
    with pytest.raises(ValueError, match="All texts are empty"):
        await sentence_transformer_embeddings.embed_batch(["", "  ", ""])


@pytest.mark.asyncio
async def test_embed_text_error(sentence_transformer_embeddings):
    """エンコードエラーテスト."""
    sentence_transformer_embeddings._model.encode = MagicMock(side_effect=Exception("Encoding Error"))

    with pytest.raises(RuntimeError, match="Failed to generate embedding"):
        await sentence_transformer_embeddings.embed_text("テストテキスト")


def test_get_dimension(sentence_transformer_embeddings):
    """次元数取得テスト."""
    assert sentence_transformer_embeddings.get_dimension() == 384


def test_get_model_name(sentence_transformer_embeddings):
    """モデル名取得テスト."""
    assert sentence_transformer_embeddings.get_model_name() == "all-MiniLM-L6-v2"


def test_init_without_sentence_transformers():
    """Sentence Transformersパッケージなしの初期化テスト."""
    with (
        patch(
            "agentflow.memory.embeddings.sentence_transformer_embeddings.SentenceTransformer",
            side_effect=ImportError,
        ),
        pytest.raises(ImportError, match="sentence-transformers package is required"),
    ):
        SentenceTransformerEmbeddings(model_name="all-MiniLM-L6-v2")


def test_init_with_different_models():
    """異なるモデルでの初期化テスト."""
    with patch("agentflow.memory.embeddings.sentence_transformer_embeddings.SentenceTransformer") as mock_st:
        # all-MiniLM-L6-v2
        mock_model1 = MagicMock()
        mock_model1.get_sentence_embedding_dimension.return_value = 384
        mock_st.return_value = mock_model1
        emb1 = SentenceTransformerEmbeddings(model_name="all-MiniLM-L6-v2")
        assert emb1.get_dimension() == 384

        # all-mpnet-base-v2
        mock_model2 = MagicMock()
        mock_model2.get_sentence_embedding_dimension.return_value = 768
        mock_st.return_value = mock_model2
        emb2 = SentenceTransformerEmbeddings(model_name="all-mpnet-base-v2")
        assert emb2.get_dimension() == 768
