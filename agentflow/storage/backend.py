"""統一ストレージバックエンド協議.

RuntimeStore/EvolutionStore の共通インターフェースを定義。
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

    # =========================================================================
    # 接続管理
    # =========================================================================

    async def connect(self) -> None:
        """バックエンドに接続."""
        ...

    async def disconnect(self) -> None:
        """バックエンドから切断."""
        ...

    async def is_connected(self) -> bool:
        """接続状態を確認."""
        ...

    async def ping(self) -> bool:
        """疎通確認."""
        ...

    # =========================================================================
    # Key-Value 操作
    # =========================================================================

    async def get(self, key: str) -> Any | None:
        """値を取得.

        Args:
            key: キー

        Returns:
            値（存在しない場合はNone）
        """
        ...

    async def set(
        self,
        key: str,
        value: Any,
        ttl: int | None = None,
    ) -> None:
        """値を設定.

        Args:
            key: キー
            value: 値（JSON シリアライズ可能）
            ttl: 有効期限（秒）、Noneで無期限
        """
        ...

    async def delete(self, key: str) -> bool:
        """値を削除.

        Args:
            key: キー

        Returns:
            削除成功の場合True
        """
        ...

    async def exists(self, key: str) -> bool:
        """キーの存在確認."""
        ...

    async def keys(self, pattern: str = "*") -> list[str]:
        """キー一覧を取得.

        Args:
            pattern: パターン（*でワイルドカード）

        Returns:
            マッチするキーのリスト
        """
        ...

    async def clear(self, pattern: str = "*") -> int:
        """キーをクリア.

        Args:
            pattern: パターン（*で全削除）

        Returns:
            削除したキーの数
        """
        ...

    # =========================================================================
    # バッチ操作
    # =========================================================================

    async def mget(self, keys: list[str]) -> dict[str, Any]:
        """複数キーを一括取得.

        Args:
            keys: キーリスト

        Returns:
            {key: value} の辞書（存在しないキーは含まない）
        """
        ...

    async def mset(
        self,
        items: dict[str, Any],
        ttl: int | None = None,
    ) -> None:
        """複数キーを一括設定.

        Args:
            items: {key: value} の辞書
            ttl: 共通の有効期限
        """
        ...

    # =========================================================================
    # Virtual Filesystem
    # =========================================================================

    async def write_file(
        self,
        path: str,
        content: bytes | str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """仮想ファイルを書き込み."""
        ...

    async def read_file(self, path: str) -> bytes | None:
        """仮想ファイルを読み込み."""
        ...

    async def list_files(self, prefix: str = "") -> list[dict[str, Any]]:
        """仮想ファイル一覧を取得."""
        ...

    async def delete_file(self, path: str) -> bool:
        """仮想ファイルを削除."""
        ...

    async def file_exists(self, path: str) -> bool:
        """仮想ファイルの存在確認."""
        ...

    # =========================================================================
    # ステータス
    # =========================================================================

    async def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        ...


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
            return key[len(prefix):]
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

    # バッチ操作のデフォルト実装（個別呼び出し）
    async def mget(self, keys: list[str]) -> dict[str, Any]:
        """複数キーを一括取得（デフォルト実装）."""
        result: dict[str, Any] = {}
        for key in keys:
            value = await self.get(key)
            if value is not None:
                result[key] = value
        return result

    async def mset(
        self,
        items: dict[str, Any],
        ttl: int | None = None,
    ) -> None:
        """複数キーを一括設定（デフォルト実装）."""
        for key, value in items.items():
            await self.set(key, value, ttl)


def get_backend(
    url: str | None = None,
    namespace: str = "default",
) -> StorageBackend:
    """ストレージバックエンドを取得.

    Args:
        url: 接続URL（None の場合は環境変数 AGENTFLOW_STORAGE_URL から取得）
             - "memory://": メモリバックエンド
             - "redis://host:port/db": Redis
             - "postgres://user:pass@host:port/db": PostgreSQL
        namespace: 名前空間

    Returns:
        StorageBackend インスタンス
    """
    from agentflow.storage.memory_backend import MemoryStorageBackend

    if url is None:
        url = os.environ.get("AGENTFLOW_STORAGE_URL", "memory://")

    scheme = url.split("://")[0].lower() if "://" in url else "memory"

    if scheme == "memory":
        return MemoryStorageBackend(namespace=namespace)

    if scheme == "redis":
        # Redis バックエンドは将来実装
        logger.warning("Redis backend not yet implemented, using memory")
        return MemoryStorageBackend(namespace=namespace)

    if scheme in ("postgres", "postgresql"):
        # PostgreSQL バックエンドは将来実装
        logger.warning("PostgreSQL backend not yet implemented, using memory")
        return MemoryStorageBackend(namespace=namespace)

    msg = f"Unknown storage scheme: {scheme}"
    raise ValueError(msg)
