"""Layer 1 の storage 公開 API.

統一ストレージバックエンド + バイナリストレージ + レジストリを提供。
"""

from infrastructure.storage.adapters import InMemoryBinaryStorage, NoOpBinaryStorage
from infrastructure.storage.backend import (
    BaseStorageBackend,
    StorageBackend,
    get_backend,
)
from infrastructure.storage.memory_backend import MemoryStorageBackend
from infrastructure.storage.ports import BinaryStorage
from infrastructure.storage.registry import StorageRegistry


__all__ = [
    "BaseStorageBackend",
    "BinaryStorage",
    "InMemoryBinaryStorage",
    "MemoryStorageBackend",
    "NoOpBinaryStorage",
    "StorageBackend",
    "StorageRegistry",
    "get_backend",
]
