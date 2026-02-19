"""PostgresBackendのテスト."""

import json
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.memory.distributed.postgres_backend import PostgresBackend
from agentflow.memory.types import MemoryEntry, MemoryType


@pytest.fixture
def postgres_backend():
    """PostgresBackendのフィクスチャ."""
    return PostgresBackend(
        host="localhost",
        port=5432,
        database="test_db",
        user="test_user",
        password="test_pass",
    )


@pytest.fixture
def sample_entry():
    """サンプル記憶エントリ."""
    return MemoryEntry(
        id="test-001",
        content="テスト記憶",
        topic="test",
        timestamp=datetime.now(),
        memory_type=MemoryType.LONG_TERM,
        importance_score=0.9,
        metadata={"source": "test"},
    )


@pytest.mark.asyncio
async def test_connect(postgres_backend):
    """接続テスト。asyncpg.create_pool は async 関数なので new_callable=AsyncMock を使う。"""
    with patch(
        "agentflow.memory.distributed.postgres_backend.asyncpg.create_pool",
        new_callable=AsyncMock,
    ) as mock_create_pool:
        mock_pool = MagicMock()
        mock_create_pool.return_value = mock_pool
        mock_conn = AsyncMock()
        mock_pool.acquire.return_value.__aenter__.return_value = mock_conn

        await postgres_backend.connect()

        assert postgres_backend._connected is True
        mock_create_pool.assert_called_once()


@pytest.mark.asyncio
async def test_disconnect(postgres_backend):
    """切断テスト."""
    postgres_backend._pool = AsyncMock()
    postgres_backend._connected = True

    await postgres_backend.disconnect()

    assert postgres_backend._connected is False
    postgres_backend._pool.close.assert_called_once()


@pytest.mark.asyncio
async def test_save(postgres_backend, sample_entry):
    """保存テスト."""
    postgres_backend._pool = MagicMock()
    postgres_backend._connected = True
    mock_conn = AsyncMock()

    # acquire()をAsyncMockに設定
    mock_acquire = AsyncMock()
    mock_acquire.__aenter__.return_value = mock_conn
    postgres_backend._pool.acquire.return_value = mock_acquire

    await postgres_backend.save(sample_entry)

    # executeが呼ばれたことを確認
    mock_conn.execute.assert_called_once()
    call_args = mock_conn.execute.call_args[0]
    assert "INSERT INTO memories" in call_args[0]


@pytest.mark.asyncio
async def test_load_found(postgres_backend, sample_entry):
    """読み込みテスト（見つかった場合）."""
    postgres_backend._pool = MagicMock()
    postgres_backend._connected = True
    mock_conn = AsyncMock()

    mock_acquire = AsyncMock()
    mock_acquire.__aenter__.return_value = mock_conn
    postgres_backend._pool.acquire.return_value = mock_acquire

    # モックデータを準備
    mock_record = {
        "id": sample_entry.id,
        "content": sample_entry.content,
        "topic": sample_entry.topic,
        "timestamp": sample_entry.timestamp,
        "memory_type": sample_entry.memory_type.value,
        "importance_score": sample_entry.importance_score,
        "metadata": json.dumps(sample_entry.metadata),
    }
    mock_conn.fetchrow.return_value = mock_record

    result = await postgres_backend.load(sample_entry.id)

    assert result is not None
    assert result.id == sample_entry.id
    assert result.content == sample_entry.content


@pytest.mark.asyncio
async def test_load_not_found(postgres_backend):
    """読み込みテスト（見つからない場合）."""
    postgres_backend._pool = MagicMock()
    postgres_backend._connected = True
    mock_conn = AsyncMock()

    mock_acquire = AsyncMock()
    mock_acquire.__aenter__.return_value = mock_conn
    postgres_backend._pool.acquire.return_value = mock_acquire
    mock_conn.fetchrow.return_value = None

    result = await postgres_backend.load("non-existent")

    assert result is None


@pytest.mark.asyncio
async def test_delete(postgres_backend):
    """削除テスト."""
    postgres_backend._pool = MagicMock()
    postgres_backend._connected = True
    mock_conn = AsyncMock()

    mock_acquire = AsyncMock()
    mock_acquire.__aenter__.return_value = mock_conn
    postgres_backend._pool.acquire.return_value = mock_acquire
    mock_conn.execute.return_value = "DELETE 1"

    result = await postgres_backend.delete("test-001")

    assert result is True


@pytest.mark.asyncio
async def test_search_by_topic(postgres_backend, sample_entry):
    """トピック検索テスト."""
    postgres_backend._pool = MagicMock()
    postgres_backend._connected = True
    mock_conn = AsyncMock()

    mock_acquire = AsyncMock()
    mock_acquire.__aenter__.return_value = mock_conn
    postgres_backend._pool.acquire.return_value = mock_acquire

    # モックデータを準備
    mock_record = {
        "id": sample_entry.id,
        "content": sample_entry.content,
        "topic": sample_entry.topic,
        "timestamp": sample_entry.timestamp,
        "memory_type": sample_entry.memory_type.value,
        "importance_score": sample_entry.importance_score,
        "metadata": json.dumps(sample_entry.metadata),
    }
    mock_conn.fetch.return_value = [mock_record]

    results = await postgres_backend.search(topic="test", limit=10)

    assert len(results) == 1
    assert results[0].id == sample_entry.id


@pytest.mark.asyncio
async def test_exists(postgres_backend):
    """存在確認テスト."""
    postgres_backend._pool = MagicMock()
    postgres_backend._connected = True
    mock_conn = AsyncMock()

    mock_acquire = AsyncMock()
    mock_acquire.__aenter__.return_value = mock_conn
    postgres_backend._pool.acquire.return_value = mock_acquire
    mock_conn.fetchval.return_value = True

    result = await postgres_backend.exists("test-001")

    assert result is True


@pytest.mark.asyncio
async def test_count(postgres_backend):
    """カウントテスト."""
    postgres_backend._pool = MagicMock()
    postgres_backend._connected = True
    mock_conn = AsyncMock()

    mock_acquire = AsyncMock()
    mock_acquire.__aenter__.return_value = mock_conn
    postgres_backend._pool.acquire.return_value = mock_acquire
    mock_conn.fetchval.return_value = 5

    result = await postgres_backend.count(topic="test")

    assert result == 5


@pytest.mark.asyncio
async def test_clear(postgres_backend):
    """クリアテスト."""
    postgres_backend._pool = MagicMock()
    postgres_backend._connected = True
    mock_conn = AsyncMock()

    mock_acquire = AsyncMock()
    mock_acquire.__aenter__.return_value = mock_conn
    postgres_backend._pool.acquire.return_value = mock_acquire
    mock_conn.execute.return_value = "DELETE 3"

    result = await postgres_backend.clear(topic="test")

    assert result == 3
