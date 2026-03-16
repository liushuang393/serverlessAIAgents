"""統一ストレージバックエンド協議（後方互換 re-export）.

実装は infrastructure.storage.backend に移動済み。
"""

from infrastructure.storage.backend import (  # noqa: F401
    BaseStorageBackend,
    StorageBackend,
    get_backend,
)
