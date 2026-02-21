"""メモリストレージバックエンド.

開発・テスト用のインメモリ実装。
"""

from __future__ import annotations

import fnmatch
from datetime import datetime
from typing import Any

from agentflow.storage.backend import BaseStorageBackend


class MemoryStorageBackend(BaseStorageBackend):
    """メモリベースのストレージバックエンド.

    開発・テスト環境向け。プロセス終了でデータは消失。

    Features:
    - Key-Value ストレージ
    - TTL サポート（簡易実装）
    - Virtual Filesystem
    - パターンマッチング

    Example:
        >>> backend = MemoryStorageBackend()
        >>> await backend.connect()
        >>> await backend.set("user:123", {"name": "Alice"})
        >>> data = await backend.get("user:123")
    """

    def __init__(self, namespace: str = "default") -> None:
        """初期化."""
        super().__init__(namespace=namespace)
        self._data: dict[str, Any] = {}
        self._ttl: dict[str, datetime] = {}  # キー → 有効期限
        self._files: dict[str, bytes] = {}
        self._file_metadata: dict[str, dict[str, Any]] = {}

    async def connect(self) -> None:
        """接続（メモリなので何もしない）."""
        self._connected = True

    async def disconnect(self) -> None:
        """切断（メモリなので何もしない）."""
        self._connected = False

    async def ping(self) -> bool:
        """疎通確認."""
        return True

    # =========================================================================
    # Key-Value 操作
    # =========================================================================

    async def get(self, key: str) -> Any | None:
        """値を取得."""
        prefixed = self._prefixed_key(key)
        self._cleanup_expired(prefixed)
        return self._data.get(prefixed)

    async def set(
        self,
        key: str,
        value: Any,
        ttl: int | None = None,
    ) -> None:
        """値を設定."""
        prefixed = self._prefixed_key(key)
        self._data[prefixed] = value

        if ttl is not None:
            from datetime import timedelta

            self._ttl[prefixed] = datetime.now() + timedelta(seconds=ttl)
        elif prefixed in self._ttl:
            del self._ttl[prefixed]

    async def delete(self, key: str) -> bool:
        """値を削除."""
        prefixed = self._prefixed_key(key)
        if prefixed in self._data:
            del self._data[prefixed]
            if prefixed in self._ttl:
                del self._ttl[prefixed]
            return True
        return False

    async def exists(self, key: str) -> bool:
        """キーの存在確認."""
        prefixed = self._prefixed_key(key)
        self._cleanup_expired(prefixed)
        return prefixed in self._data

    async def keys(self, pattern: str = "*") -> list[str]:
        """キー一覧を取得."""
        # まず期限切れをクリーンアップ
        for key in list(self._data.keys()):
            self._cleanup_expired(key)

        # パターンマッチング
        prefix = f"{self._namespace}:"
        full_pattern = f"{prefix}{pattern}"

        matched: list[str] = []
        for key in self._data:
            if fnmatch.fnmatch(key, full_pattern):
                matched.append(self._strip_prefix(key))

        return sorted(matched)

    async def clear(self, pattern: str = "*") -> int:
        """キーをクリア."""
        keys_to_delete = await self.keys(pattern)
        count = 0
        for key in keys_to_delete:
            if await self.delete(key):
                count += 1
        return count

    def _cleanup_expired(self, prefixed_key: str) -> None:
        """期限切れデータを削除."""
        if prefixed_key in self._ttl and datetime.now() > self._ttl[prefixed_key]:
            if prefixed_key in self._data:
                del self._data[prefixed_key]
            del self._ttl[prefixed_key]

    # =========================================================================
    # バッチ操作（最適化版）
    # =========================================================================

    async def mget(self, keys: list[str]) -> dict[str, Any]:
        """複数キーを一括取得."""
        result: dict[str, Any] = {}
        for key in keys:
            prefixed = self._prefixed_key(key)
            self._cleanup_expired(prefixed)
            if prefixed in self._data:
                result[key] = self._data[prefixed]
        return result

    async def mset(
        self,
        items: dict[str, Any],
        ttl: int | None = None,
    ) -> None:
        """複数キーを一括設定."""
        from datetime import timedelta

        expire_at = None
        if ttl is not None:
            expire_at = datetime.now() + timedelta(seconds=ttl)

        for key, value in items.items():
            prefixed = self._prefixed_key(key)
            self._data[prefixed] = value
            if expire_at:
                self._ttl[prefixed] = expire_at
            elif prefixed in self._ttl:
                del self._ttl[prefixed]

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
        normalized = self._normalize_path(path)
        prefixed = self._prefixed_key(f"_file:{normalized}")

        if isinstance(content, str):
            content = content.encode("utf-8")

        self._files[prefixed] = content
        self._file_metadata[prefixed] = {
            "path": normalized,
            "size": len(content),
            "created_at": datetime.now().isoformat(),
            **(metadata or {}),
        }

    async def read_file(self, path: str) -> bytes | None:
        """仮想ファイルを読み込み."""
        normalized = self._normalize_path(path)
        prefixed = self._prefixed_key(f"_file:{normalized}")
        return self._files.get(prefixed)

    async def list_files(self, prefix: str = "") -> list[dict[str, Any]]:
        """仮想ファイル一覧を取得."""
        normalized_prefix = self._normalize_path(prefix) if prefix else "/"
        ns_prefix = self._prefixed_key("_file:")

        results: list[dict[str, Any]] = []
        for key, metadata in self._file_metadata.items():
            if key.startswith(ns_prefix):
                file_path = metadata.get("path", "")
                if file_path.startswith(normalized_prefix):
                    results.append(metadata.copy())

        return sorted(results, key=lambda x: x.get("path", ""))

    async def delete_file(self, path: str) -> bool:
        """仮想ファイルを削除."""
        normalized = self._normalize_path(path)
        prefixed = self._prefixed_key(f"_file:{normalized}")

        if prefixed in self._files:
            del self._files[prefixed]
            del self._file_metadata[prefixed]
            return True
        return False

    async def file_exists(self, path: str) -> bool:
        """仮想ファイルの存在確認."""
        normalized = self._normalize_path(path)
        prefixed = self._prefixed_key(f"_file:{normalized}")
        return prefixed in self._files

    def _normalize_path(self, path: str) -> str:
        """パスを正規化."""
        if not path.startswith("/"):
            path = "/" + path
        while "//" in path:
            path = path.replace("//", "/")
        return path

    # =========================================================================
    # ステータス
    # =========================================================================

    async def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        # 期限切れをクリーンアップ
        for key in list(self._data.keys()):
            self._cleanup_expired(key)

        prefix = f"{self._namespace}:"
        ns_keys = [k for k in self._data if k.startswith(prefix)]
        ns_files = [k for k in self._files if k.startswith(prefix)]

        return {
            "backend": "memory",
            "namespace": self._namespace,
            "connected": self._connected,
            "key_count": len(ns_keys),
            "file_count": len(ns_files),
            "total_file_size": sum(len(v) for k, v in self._files.items() if k.startswith(prefix)),
            "ttl_keys": len([k for k in self._ttl if k.startswith(prefix)]),
        }
