"""統一ストレージバックエンド（後方互換 re-export）.

実装は infrastructure.storage に移動済み。
このモジュールは後方互換のための re-export のみ。
"""

from infrastructure.storage.backend import (
    BaseStorageBackend,
    StorageBackend,
    get_backend,
)
from infrastructure.storage.memory_backend import MemoryStorageBackend


__all__ = [
    "BaseStorageBackend",
    "MemoryStorageBackend",
    "StorageBackend",
    "get_backend",
]
