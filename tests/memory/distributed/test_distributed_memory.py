"""DistributedMemoryManagerのテスト."""

from datetime import datetime
from unittest.mock import AsyncMock

import pytest

from agentflow.memory.distributed.distributed_memory import DistributedMemoryManager
from agentflow.memory.types import MemoryEntry, MemoryType


@pytest.fixture
def mock_cache():
    """モックキャッシュバックエンド."""
    cache = AsyncMock()
    cache.connect = AsyncMock()
    cache.disconnect = AsyncMock()
    cache.save = AsyncMock()
    cache.load = AsyncMock()
    cache.delete = AsyncMock()
    cache.search = AsyncMock()
    cache.exists = AsyncMock()
    cache.count = AsyncMock()
    cache.clear = AsyncMock()
    cache.get_status = lambda: {"backend": "redis"}
    return cache


@pytest.fixture
def mock_storage():
    """モックストレージバックエンド."""
    storage = AsyncMock()
    storage.connect = AsyncMock()
    storage.disconnect = AsyncMock()
    storage.save = AsyncMock()
    storage.load = AsyncMock()
    storage.delete = AsyncMock()
    storage.search = AsyncMock()
    storage.exists = AsyncMock()
    storage.count = AsyncMock()
    storage.clear = AsyncMock()
    storage.get_status = lambda: {"backend": "postgres"}
    return storage


@pytest.fixture
def distributed_manager(mock_cache, mock_storage):
    """DistributedMemoryManagerのフィクスチャ."""
    return DistributedMemoryManager(cache_backend=mock_cache, storage_backend=mock_storage)


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


@pytest.mark.asyncio
async def test_start(distributed_manager, mock_cache, mock_storage):
    """開始テスト."""
    await distributed_manager.start()

    mock_cache.connect.assert_called_once()
    mock_storage.connect.assert_called_once()


@pytest.mark.asyncio
async def test_stop(distributed_manager, mock_cache, mock_storage):
    """停止テスト."""
    await distributed_manager.stop()

    mock_cache.disconnect.assert_called_once()
    mock_storage.disconnect.assert_called_once()


@pytest.mark.asyncio
async def test_save(distributed_manager, mock_cache, mock_storage, sample_entry):
    """保存テスト."""
    await distributed_manager.save(sample_entry)

    # キャッシュとストレージの両方に保存
    mock_cache.save.assert_called_once_with(sample_entry)
    mock_storage.save.assert_called_once_with(sample_entry)


@pytest.mark.asyncio
async def test_load_from_cache(distributed_manager, mock_cache, mock_storage, sample_entry):
    """キャッシュから読み込みテスト."""
    mock_cache.load.return_value = sample_entry
    mock_storage.load.return_value = None

    result = await distributed_manager.load(sample_entry.id)

    assert result == sample_entry
    mock_cache.load.assert_called_once_with(sample_entry.id)
    # キャッシュにあるのでストレージは呼ばれない
    mock_storage.load.assert_not_called()


@pytest.mark.asyncio
async def test_load_from_storage(distributed_manager, mock_cache, mock_storage, sample_entry):
    """ストレージから読み込みテスト."""
    mock_cache.load.return_value = None
    mock_storage.load.return_value = sample_entry

    result = await distributed_manager.load(sample_entry.id)

    assert result == sample_entry
    mock_cache.load.assert_called_once_with(sample_entry.id)
    mock_storage.load.assert_called_once_with(sample_entry.id)
    # ストレージから取得したのでキャッシュに保存
    mock_cache.save.assert_called_once_with(sample_entry)


@pytest.mark.asyncio
async def test_load_not_found(distributed_manager, mock_cache, mock_storage):
    """読み込みテスト（見つからない場合）."""
    mock_cache.load.return_value = None
    mock_storage.load.return_value = None

    result = await distributed_manager.load("non-existent")

    assert result is None


@pytest.mark.asyncio
async def test_delete(distributed_manager, mock_cache, mock_storage):
    """削除テスト."""
    mock_cache.delete.return_value = True
    mock_storage.delete.return_value = True

    result = await distributed_manager.delete("test-001")

    assert result is True
    mock_cache.delete.assert_called_once_with("test-001")
    mock_storage.delete.assert_called_once_with("test-001")


@pytest.mark.asyncio
async def test_search_from_cache(distributed_manager, mock_cache, mock_storage, sample_entry):
    """キャッシュから検索テスト."""
    # キャッシュで十分な結果が得られた場合
    mock_cache.search.return_value = [sample_entry] * 10
    mock_storage.search.return_value = []

    results = await distributed_manager.search(topic="test", limit=10)

    assert len(results) == 10
    mock_cache.search.assert_called_once()
    # キャッシュで十分なのでストレージは呼ばれない
    mock_storage.search.assert_not_called()


@pytest.mark.asyncio
async def test_search_from_both(distributed_manager, mock_cache, mock_storage, sample_entry):
    """キャッシュとストレージから検索テスト."""
    # キャッシュで不十分な結果の場合
    cache_entry = MemoryEntry(
        id="cache-001",
        content="キャッシュ記憶",
        topic="test",
        timestamp=datetime.now(),
        memory_type=MemoryType.SHORT_TERM,
        importance_score=0.7,
        metadata={},
    )
    storage_entry = MemoryEntry(
        id="storage-001",
        content="ストレージ記憶",
        topic="test",
        timestamp=datetime.now(),
        memory_type=MemoryType.LONG_TERM,
        importance_score=0.9,
        metadata={},
    )

    mock_cache.search.return_value = [cache_entry]
    mock_storage.search.return_value = [storage_entry]

    results = await distributed_manager.search(topic="test", limit=10)

    assert len(results) == 2
    mock_cache.search.assert_called_once()
    mock_storage.search.assert_called_once()


@pytest.mark.asyncio
async def test_exists(distributed_manager, mock_cache, mock_storage):
    """存在確認テスト."""
    mock_cache.exists.return_value = True
    mock_storage.exists.return_value = False

    result = await distributed_manager.exists("test-001")

    assert result is True


@pytest.mark.asyncio
async def test_count(distributed_manager, mock_storage):
    """カウントテスト."""
    mock_storage.count.return_value = 100

    result = await distributed_manager.count(topic="test")

    assert result == 100
    mock_storage.count.assert_called_once_with("test")


@pytest.mark.asyncio
async def test_clear(distributed_manager, mock_cache, mock_storage):
    """クリアテスト."""
    mock_cache.clear.return_value = 5
    mock_storage.clear.return_value = 10

    result = await distributed_manager.clear(topic="test")

    assert result == 10  # max(5, 10)
    mock_cache.clear.assert_called_once_with("test")
    mock_storage.clear.assert_called_once_with("test")


def test_get_status(distributed_manager):
    """状態取得テスト."""
    status = distributed_manager.get_status()

    assert "cache" in status
    assert "storage" in status
    assert status["cache"]["backend"] == "redis"
    assert status["storage"]["backend"] == "postgres"

