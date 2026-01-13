# -*- coding: utf-8 -*-
"""統一ストレージバックエンド.

DeepAgentCoordinator の RuntimeStore/EvolutionStore を
統一的に管理するストレージ抽象層。

Backends:
- MemoryBackend: インメモリ（開発/テスト用）
- RedisBackend: Redis（高速アクセス、セッションデータ用）
- PostgresBackend: PostgreSQL（永続化、進化データ用）

Example:
    >>> from agentflow.storage import StorageBackend, get_backend
    >>>
    >>> # 環境変数から自動選択
    >>> backend = get_backend()
    >>>
    >>> # 明示的に指定
    >>> backend = get_backend("redis://localhost:6379/0")
    >>>
    >>> # 基本操作
    >>> await backend.set("key", {"data": "value"})
    >>> data = await backend.get("key")
"""

from agentflow.storage.backend import (
    StorageBackend,
    get_backend,
)
from agentflow.storage.memory_backend import MemoryStorageBackend

__all__ = [
    "StorageBackend",
    "MemoryStorageBackend",
    "get_backend",
]
