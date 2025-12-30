"""RedisBackendのテスト."""

import json
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.memory.distributed.redis_backend import RedisBackend
from agentflow.memory.types import MemoryEntry, MemoryType


@pytest.fixture
def redis_backend():
    """RedisBackendのフィクスチャ."""
    return RedisBackend(host="localhost", port=6379, ttl=3600)


@pytest.fixture
def sample_entry():
    """サンプル記憶エントリ."""
    return MemoryEntry(
        id="test-001",
        content="テスト記憶",
        topic="test",
        timestamp=datetime.now(),
        memory_type=MemoryType.SHORT_TERM,
        importance_score=0.8,
        metadata={"source": "test"},
    )


@pytest.mark.asyncio
async def test_connect(redis_backend):
    """接続テスト."""
    with patch("agentflow.memory.distributed.redis_backend.redis.asyncio.Redis") as mock_redis:
        mock_client = AsyncMock()
        mock_redis.return_value = mock_client
        mock_client.ping = AsyncMock()

        await redis_backend.connect()

        assert redis_backend._connected is True
        mock_client.ping.assert_called_once()


@pytest.mark.asyncio
async def test_disconnect(redis_backend):
    """切断テスト."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True

    await redis_backend.disconnect()

    assert redis_backend._connected is False
    redis_backend._client.close.assert_called_once()


@pytest.mark.asyncio
async def test_save(redis_backend, sample_entry):
    """保存テスト."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True

    await redis_backend.save(sample_entry)

    # setexが呼ばれたことを確認
    redis_backend._client.setex.assert_called_once()
    call_args = redis_backend._client.setex.call_args
    assert call_args[0][0] == f"memory:{sample_entry.id}"
    assert call_args[0][1] == 3600

    # saddが呼ばれたことを確認（トピックインデックス）
    redis_backend._client.sadd.assert_called_once()


@pytest.mark.asyncio
async def test_load_found(redis_backend, sample_entry):
    """読み込みテスト（見つかった場合）."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True

    # モックデータを準備
    entry_data = {
        "id": sample_entry.id,
        "content": sample_entry.content,
        "topic": sample_entry.topic,
        "timestamp": sample_entry.timestamp.isoformat(),
        "memory_type": sample_entry.memory_type.value,
        "importance_score": sample_entry.importance_score,
        "metadata": sample_entry.metadata,
    }
    redis_backend._client.get.return_value = json.dumps(entry_data, ensure_ascii=False)

    result = await redis_backend.load(sample_entry.id)

    assert result is not None
    assert result.id == sample_entry.id
    assert result.content == sample_entry.content
    assert result.topic == sample_entry.topic


@pytest.mark.asyncio
async def test_load_not_found(redis_backend):
    """読み込みテスト（見つからない場合）."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True
    redis_backend._client.get.return_value = None

    result = await redis_backend.load("non-existent")

    assert result is None


@pytest.mark.asyncio
async def test_delete(redis_backend):
    """削除テスト."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True
    redis_backend._client.delete.return_value = 1

    result = await redis_backend.delete("test-001")

    assert result is True
    redis_backend._client.delete.assert_called_once_with("memory:test-001")


@pytest.mark.asyncio
async def test_search_by_topic(redis_backend, sample_entry):
    """トピック検索テスト."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True

    # モックデータを準備
    redis_backend._client.smembers.return_value = {sample_entry.id}
    entry_data = {
        "id": sample_entry.id,
        "content": sample_entry.content,
        "topic": sample_entry.topic,
        "timestamp": sample_entry.timestamp.isoformat(),
        "memory_type": sample_entry.memory_type.value,
        "importance_score": sample_entry.importance_score,
        "metadata": sample_entry.metadata,
    }
    redis_backend._client.get.return_value = json.dumps(entry_data, ensure_ascii=False)

    results = await redis_backend.search(topic="test", limit=10)

    assert len(results) == 1
    assert results[0].id == sample_entry.id


@pytest.mark.asyncio
async def test_exists(redis_backend):
    """存在確認テスト."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True
    redis_backend._client.exists.return_value = 1

    result = await redis_backend.exists("test-001")

    assert result is True


@pytest.mark.asyncio
async def test_count(redis_backend):
    """カウントテスト."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True
    redis_backend._client.scard = AsyncMock(return_value=3)

    result = await redis_backend.count(topic="test")

    assert result == 3


@pytest.mark.asyncio
async def test_clear(redis_backend):
    """クリアテスト."""
    redis_backend._client = AsyncMock()
    redis_backend._connected = True
    redis_backend._client.smembers = AsyncMock(return_value={"id1", "id2"})
    redis_backend._client.delete = AsyncMock(return_value=1)

    result = await redis_backend.clear(topic="test")

    assert result == 2

