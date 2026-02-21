"""PineconeDBのテスト."""

from datetime import datetime
from unittest.mock import MagicMock, patch

import pytest

from agentflow.memory.types import MemoryEntry, MemoryType
from agentflow.memory.vector_db.pinecone_db import PineconeDB


@pytest.fixture
def pinecone_db():
    """PineconeDBのフィクスチャ."""
    with patch("agentflow.memory.vector_db.pinecone_db.Pinecone"):
        db = PineconeDB(api_key="test-api-key", environment="us-west1-gcp", index_name="test-index")
        db._index = MagicMock()
        db._connected = True
        return db


@pytest.fixture
def sample_entry():
    """サンプル記憶エントリ."""
    return MemoryEntry(
        id="test-001",
        content="テスト記憶",
        topic="test",
        timestamp=datetime.now(),
        memory_type=MemoryType.LONG_TERM,
        importance_score=0.8,
        metadata={"source": "test"},
    )


@pytest.fixture
def sample_embedding():
    """サンプル埋め込みベクトル."""
    return [0.1] * 1536


@pytest.mark.asyncio
async def test_connect():
    """接続テスト."""
    with patch("agentflow.memory.vector_db.pinecone_db.Pinecone") as mock_pinecone:
        mock_pc = MagicMock()
        mock_index = MagicMock()
        mock_pc.Index.return_value = mock_index
        mock_pinecone.return_value = mock_pc

        db = PineconeDB(api_key="test-api-key", environment="us-west1-gcp", index_name="test-index")
        await db.connect()

        assert db._connected is True
        mock_pc.Index.assert_called_once_with("test-index")


@pytest.mark.asyncio
async def test_connect_import_error():
    """Pineconeパッケージなしの接続テスト."""
    with patch("agentflow.memory.vector_db.pinecone_db.Pinecone", side_effect=ImportError):
        db = PineconeDB(api_key="test-api-key", environment="us-west1-gcp", index_name="test-index")

        with pytest.raises(ImportError, match="pinecone-client package is required"):
            await db.connect()


@pytest.mark.asyncio
async def test_disconnect(pinecone_db):
    """切断テスト."""
    await pinecone_db.disconnect()
    assert pinecone_db._connected is False


@pytest.mark.asyncio
async def test_upsert(pinecone_db, sample_entry, sample_embedding):
    """挿入/更新テスト."""
    await pinecone_db.upsert(sample_entry, sample_embedding)

    pinecone_db._index.upsert.assert_called_once()
    call_args = pinecone_db._index.upsert.call_args
    vectors = call_args[1]["vectors"]
    assert len(vectors) == 1
    assert vectors[0][0] == sample_entry.id
    assert vectors[0][1] == sample_embedding


@pytest.mark.asyncio
async def test_upsert_not_connected(sample_entry, sample_embedding):
    """未接続時の挿入/更新テスト."""
    db = PineconeDB(api_key="test-api-key", environment="us-west1-gcp", index_name="test-index")

    with pytest.raises(ConnectionError, match="Not connected to Pinecone"):
        await db.upsert(sample_entry, sample_embedding)


@pytest.mark.asyncio
async def test_search(pinecone_db, sample_entry, sample_embedding):
    """検索テスト."""
    # モックレスポンスを準備
    mock_match = MagicMock()
    mock_match.id = sample_entry.id
    mock_match.score = 0.95
    mock_match.metadata = {
        "content": sample_entry.content,
        "topic": sample_entry.topic,
        "timestamp": sample_entry.timestamp.isoformat(),
        "memory_type": sample_entry.memory_type.value,
        "importance_score": sample_entry.importance_score,
        "metadata": "{}",
    }

    mock_results = MagicMock()
    mock_results.matches = [mock_match]
    pinecone_db._index.query.return_value = mock_results

    results = await pinecone_db.search(
        query_embedding=sample_embedding, limit=10, min_similarity=0.5
    )

    assert len(results) == 1
    entry, score = results[0]
    assert entry.id == sample_entry.id
    assert score == 0.95


@pytest.mark.asyncio
async def test_search_with_topic(pinecone_db, sample_embedding):
    """トピック指定検索テスト."""
    mock_results = MagicMock()
    mock_results.matches = []
    pinecone_db._index.query.return_value = mock_results

    await pinecone_db.search(query_embedding=sample_embedding, limit=10, topic="test")

    call_args = pinecone_db._index.query.call_args
    assert call_args[1]["filter"] == {"topic": "test"}


@pytest.mark.asyncio
async def test_search_min_similarity_filter(pinecone_db, sample_entry, sample_embedding):
    """最小類似度フィルタテスト."""
    # 類似度が低いマッチ
    mock_match = MagicMock()
    mock_match.id = sample_entry.id
    mock_match.score = 0.3  # min_similarity=0.5より低い
    mock_match.metadata = {
        "content": sample_entry.content,
        "topic": sample_entry.topic,
        "timestamp": sample_entry.timestamp.isoformat(),
        "memory_type": sample_entry.memory_type.value,
        "importance_score": sample_entry.importance_score,
        "metadata": "{}",
    }

    mock_results = MagicMock()
    mock_results.matches = [mock_match]
    pinecone_db._index.query.return_value = mock_results

    results = await pinecone_db.search(
        query_embedding=sample_embedding, limit=10, min_similarity=0.5
    )

    # 類似度が低いのでフィルタされる
    assert len(results) == 0


@pytest.mark.asyncio
async def test_delete(pinecone_db):
    """削除テスト."""
    result = await pinecone_db.delete("test-001")

    assert result is True
    pinecone_db._index.delete.assert_called_once_with(ids=["test-001"])


@pytest.mark.asyncio
async def test_clear_all(pinecone_db):
    """全削除テスト."""
    await pinecone_db.clear()

    pinecone_db._index.delete.assert_called_once_with(delete_all=True)


@pytest.mark.asyncio
async def test_clear_by_topic(pinecone_db):
    """トピック指定削除テスト."""
    await pinecone_db.clear(topic="test")

    pinecone_db._index.delete.assert_called_once_with(filter={"topic": "test"})


def test_get_status(pinecone_db):
    """状態取得テスト."""
    status = pinecone_db.get_status()

    assert status["database"] == "pinecone"
    assert status["environment"] == "us-west1-gcp"
    assert status["index_name"] == "test-index"
    assert status["connected"] is True
