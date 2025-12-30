"""QdrantDBのテスト."""

from datetime import datetime
from unittest.mock import MagicMock, patch

import pytest

from agentflow.memory.types import MemoryEntry, MemoryType
from agentflow.memory.vector_db.qdrant_db import QdrantDB


@pytest.fixture
def qdrant_db():
    """QdrantDBのフィクスチャ."""
    with patch("agentflow.memory.vector_db.qdrant_db.QdrantClient"):
        db = QdrantDB(host="localhost", port=6333, collection_name="test-collection", dimension=384)
        db._client = MagicMock()
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
    return [0.1] * 384


@pytest.mark.asyncio
async def test_connect():
    """接続テスト."""
    with patch("agentflow.memory.vector_db.qdrant_db.QdrantClient") as mock_qdrant:
        mock_client = MagicMock()
        mock_collections = MagicMock()
        mock_collections.collections = []
        mock_client.get_collections.return_value = mock_collections
        mock_qdrant.return_value = mock_client

        db = QdrantDB(host="localhost", port=6333, collection_name="test-collection", dimension=384)
        await db.connect()

        assert db._connected is True
        mock_client.create_collection.assert_called_once()


@pytest.mark.asyncio
async def test_connect_existing_collection():
    """既存コレクションへの接続テスト."""
    with patch("agentflow.memory.vector_db.qdrant_db.QdrantClient") as mock_qdrant:
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_collection.name = "test-collection"
        mock_collections = MagicMock()
        mock_collections.collections = [mock_collection]
        mock_client.get_collections.return_value = mock_collections
        mock_qdrant.return_value = mock_client

        db = QdrantDB(host="localhost", port=6333, collection_name="test-collection", dimension=384)
        await db.connect()

        assert db._connected is True
        # 既存コレクションなので作成されない
        mock_client.create_collection.assert_not_called()


@pytest.mark.asyncio
async def test_connect_import_error():
    """Qdrantパッケージなしの接続テスト."""
    with patch("agentflow.memory.vector_db.qdrant_db.QdrantClient", side_effect=ImportError):
        db = QdrantDB(host="localhost", port=6333, collection_name="test-collection", dimension=384)

        with pytest.raises(ImportError, match="qdrant-client package is required"):
            await db.connect()


@pytest.mark.asyncio
async def test_disconnect(qdrant_db):
    """切断テスト."""
    await qdrant_db.disconnect()
    assert qdrant_db._connected is False


@pytest.mark.asyncio
async def test_upsert(qdrant_db, sample_entry, sample_embedding):
    """挿入/更新テスト."""
    await qdrant_db.upsert(sample_entry, sample_embedding)

    qdrant_db._client.upsert.assert_called_once()
    call_args = qdrant_db._client.upsert.call_args
    assert call_args[1]["collection_name"] == "test-collection"
    points = call_args[1]["points"]
    assert len(points) == 1


@pytest.mark.asyncio
async def test_upsert_not_connected(sample_entry, sample_embedding):
    """未接続時の挿入/更新テスト."""
    db = QdrantDB(host="localhost", port=6333, collection_name="test-collection", dimension=384)

    with pytest.raises(ConnectionError, match="Not connected to Qdrant"):
        await db.upsert(sample_entry, sample_embedding)


@pytest.mark.asyncio
async def test_search(qdrant_db, sample_entry, sample_embedding):
    """検索テスト."""
    # モックレスポンスを準備
    mock_result = MagicMock()
    mock_result.id = sample_entry.id
    mock_result.score = 0.95
    mock_result.payload = {
        "content": sample_entry.content,
        "topic": sample_entry.topic,
        "timestamp": sample_entry.timestamp.isoformat(),
        "memory_type": sample_entry.memory_type.value,
        "importance_score": sample_entry.importance_score,
        "metadata": "{}",
    }

    qdrant_db._client.search.return_value = [mock_result]

    results = await qdrant_db.search(query_embedding=sample_embedding, limit=10, min_similarity=0.5)

    assert len(results) == 1
    entry, score = results[0]
    assert entry.id == sample_entry.id
    assert score == 0.95


@pytest.mark.asyncio
async def test_search_with_topic(qdrant_db, sample_embedding):
    """トピック指定検索テスト."""
    qdrant_db._client.search.return_value = []

    await qdrant_db.search(query_embedding=sample_embedding, limit=10, topic="test")

    call_args = qdrant_db._client.search.call_args
    assert call_args[1]["query_filter"] is not None


@pytest.mark.asyncio
async def test_delete(qdrant_db):
    """削除テスト."""
    result = await qdrant_db.delete("test-001")

    assert result is True
    qdrant_db._client.delete.assert_called_once()


@pytest.mark.asyncio
async def test_clear_all(qdrant_db):
    """全削除テスト."""
    await qdrant_db.clear()

    qdrant_db._client.delete_collection.assert_called_once_with(collection_name="test-collection")
    qdrant_db._client.create_collection.assert_called_once()


@pytest.mark.asyncio
async def test_clear_by_topic(qdrant_db):
    """トピック指定削除テスト."""
    await qdrant_db.clear(topic="test")

    call_args = qdrant_db._client.delete.call_args
    assert call_args[1]["collection_name"] == "test-collection"
    assert call_args[1]["points_selector"] is not None


def test_get_status(qdrant_db):
    """状態取得テスト."""
    status = qdrant_db.get_status()

    assert status["database"] == "qdrant"
    assert status["host"] == "localhost"
    assert status["port"] == 6333
    assert status["collection_name"] == "test-collection"
    assert status["dimension"] == 384
    assert status["connected"] is True

