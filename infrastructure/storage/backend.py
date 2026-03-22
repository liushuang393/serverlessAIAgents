"""統一ストレージバックエンド協議.

RuntimeStore/EvolutionStore の共通インターフェースを定義。
infrastructure(L1) 層に配置。
"""

from __future__ import annotations

import logging
import os
from abc import ABC
from typing import Any, Protocol, runtime_checkable


logger = logging.getLogger(__name__)


@runtime_checkable
class StorageBackend(Protocol):
    """統一ストレージバックエンドプロトコル.

    RuntimeStore と EvolutionStore の共通操作を定義。
    Key-Value ストレージ + Virtual Filesystem をサポート。

    Implementations:
    - MemoryStorageBackend: インメモリ（デフォルト）
    - RedisStorageBackend: Redis（本番推奨）
    - PostgresStorageBackend: PostgreSQL（進化データ用）
    """

    # 接続管理
    async def connect(self) -> None: ...
    async def disconnect(self) -> None: ...
    async def is_connected(self) -> bool: ...
    async def ping(self) -> bool: ...

    # Key-Value 操作
    async def get(self, key: str) -> Any | None: ...
    async def set(self, key: str, value: Any, ttl: int | None = None) -> None: ...
    async def delete(self, key: str) -> bool: ...
    async def exists(self, key: str) -> bool: ...
    async def keys(self, pattern: str = "*") -> list[str]: ...
    async def clear(self, pattern: str = "*") -> int: ...

    # バッチ操作
    async def mget(self, keys: list[str]) -> dict[str, Any]: ...
    async def mset(self, items: dict[str, Any], ttl: int | None = None) -> None: ...

    # Virtual Filesystem
    async def write_file(
        self,
        path: str,
        content: bytes | str,
        metadata: dict[str, Any] | None = None,
    ) -> None: ...
    async def read_file(self, path: str) -> bytes | None: ...
    async def list_files(self, prefix: str = "") -> list[dict[str, Any]]: ...
    async def delete_file(self, path: str) -> bool: ...
    async def file_exists(self, path: str) -> bool: ...

    # ステータス
    async def get_stats(self) -> dict[str, Any]: ...


class BaseStorageBackend(ABC):
    """ストレージバックエンド基底クラス.

    共通の実装を提供する抽象基底クラス。
    """

    def __init__(self, namespace: str = "default") -> None:
        """初期化.

        Args:
            namespace: 名前空間（データ分離用）
        """
        self._namespace = namespace
        self._connected = False
        self._logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")

    def _prefixed_key(self, key: str) -> str:
        """名前空間付きキーを生成."""
        return f"{self._namespace}:{key}"

    def _strip_prefix(self, key: str) -> str:
        """名前空間プレフィックスを除去."""
        prefix = f"{self._namespace}:"
        if key.startswith(prefix):
            return key[len(prefix) :]
        return key

    async def connect(self) -> None:
        """接続（デフォルトは何もしない）."""
        self._connected = True

    async def disconnect(self) -> None:
        """切断（デフォルトは何もしない）."""
        self._connected = False

    async def is_connected(self) -> bool:
        """接続状態を確認."""
        return self._connected

    async def ping(self) -> bool:
        """疎通確認."""
        return self._connected

    async def get(self, key: str) -> Any | None:
        """値を取得（サブクラスで実装）."""
        msg = f"{self.__class__.__name__}.get is not implemented"
        raise NotImplementedError(msg)

    async def set(self, key: str, value: Any, ttl: int | None = None) -> None:
        """値を設定（サブクラスで実装）."""
        del key, value, ttl
        msg = f"{self.__class__.__name__}.set is not implemented"
        raise NotImplementedError(msg)

    # バッチ操作のデフォルト実装（個別呼び出し）
    async def mget(self, keys: list[str]) -> dict[str, Any]:
        """複数キーを一括取得（デフォルト実装）."""
        result: dict[str, Any] = {}
        for key in keys:
            value = await self.get(key)
            if value is not None:
                result[key] = value
        return result

    async def mset(self, items: dict[str, Any], ttl: int | None = None) -> None:
        """複数キーを一括設定（デフォルト実装）."""
        for key, value in items.items():
            await self.set(key, value, ttl)


def get_backend(
    url: str | None = None,
    namespace: str = "default",
) -> StorageBackend:
    """ストレージバックエンドを取得.

    StorageRegistry に登録済みの scheme を使ってインスタンスを生成する。
    未登録 scheme の場合は ValueError を送出。

    Args:
        url: 接続URL（None の場合は環境変数 AGENTFLOW_STORAGE_URL から取得）
        namespace: 名前空間

    Returns:
        StorageBackend インスタンス

    Raises:
        ValueError: 未登録の scheme
    """
    from infrastructure.storage.registry import StorageRegistry

    if url is None:
        url = os.environ.get("AGENTFLOW_STORAGE_URL", "memory://")

    scheme = url.split("://")[0].lower() if "://" in url else "memory"

    if StorageRegistry.is_registered(scheme):
        return StorageRegistry.create(scheme, namespace=namespace)

    msg = f"Unknown storage scheme: {scheme}. Available: {StorageRegistry.available_schemes()}"
    raise ValueError(msg)
