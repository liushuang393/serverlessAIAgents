"""ストレージバックエンド レジストリ.

バックエンド実装を scheme 名で登録・取得するプラグイン可能なレジストリ。
infrastructure(L1) 層に配置。

使用例:
    ```python
    from infrastructure.storage.registry import StorageRegistry

    # カスタムバックエンドを登録
    StorageRegistry.register("redis", RedisStorageBackend)

    # scheme 名で取得
    backend = StorageRegistry.create("redis", namespace="app")

    # 登録済み scheme 一覧
    schemes = StorageRegistry.available_schemes()
    ```
"""

from __future__ import annotations

import logging
from typing import Any

from infrastructure.storage.backend import BaseStorageBackend, StorageBackend


logger = logging.getLogger(__name__)

# バックエンドクラスの型（BaseStorageBackend のサブクラス）
type BackendFactory = type[BaseStorageBackend]


class StorageRegistry:
    """ストレージバックエンドのプラグインレジストリ.

    scheme 名（"memory", "redis", "postgres" 等）をキーに
    バックエンドクラスを登録・生成する。
    """

    _backends: dict[str, BackendFactory] = {}

    @classmethod
    def register(cls, scheme: str, backend_cls: BackendFactory) -> None:
        """バックエンドクラスを登録.

        Args:
            scheme: URL scheme 名（例: "memory", "redis"）
            backend_cls: BaseStorageBackend のサブクラス
        """
        if not isinstance(backend_cls, type) or not issubclass(
            backend_cls,
            BaseStorageBackend,
        ):
            msg = f"backend_cls は BaseStorageBackend のサブクラスである必要があります: {backend_cls}"
            raise TypeError(msg)
        cls._backends[scheme.lower()] = backend_cls
        logger.info("ストレージバックエンド登録: scheme=%s, cls=%s", scheme, backend_cls.__name__)

    @classmethod
    def unregister(cls, scheme: str) -> bool:
        """バックエンドクラスの登録を解除.

        Args:
            scheme: URL scheme 名

        Returns:
            解除できた場合 True
        """
        return cls._backends.pop(scheme.lower(), None) is not None

    @classmethod
    def create(cls, scheme: str, **kwargs: Any) -> StorageBackend:
        """登録済みバックエンドのインスタンスを生成.

        Args:
            scheme: URL scheme 名
            **kwargs: バックエンドコンストラクタに渡す引数

        Returns:
            StorageBackend インスタンス

        Raises:
            KeyError: 未登録の scheme
        """
        scheme_lower = scheme.lower()
        backend_cls = cls._backends.get(scheme_lower)
        if backend_cls is None:
            available = ", ".join(sorted(cls._backends.keys())) or "(なし)"
            msg = f"未登録のストレージ scheme: '{scheme_lower}'. 登録済み: {available}"
            raise KeyError(msg)
        return backend_cls(**kwargs)

    @classmethod
    def available_schemes(cls) -> list[str]:
        """登録済み scheme 一覧を取得.

        Returns:
            ソート済み scheme 名リスト
        """
        return sorted(cls._backends.keys())

    @classmethod
    def is_registered(cls, scheme: str) -> bool:
        """scheme が登録済みか確認.

        Args:
            scheme: URL scheme 名

        Returns:
            登録済みなら True
        """
        return scheme.lower() in cls._backends

    @classmethod
    def reset(cls) -> None:
        """全登録をクリア（テスト用）."""
        cls._backends.clear()


def _register_builtins() -> None:
    """組み込みバックエンドを登録."""
    from infrastructure.storage.memory_backend import MemoryStorageBackend

    if not StorageRegistry.is_registered("memory"):
        StorageRegistry.register("memory", MemoryStorageBackend)


# モジュールロード時に組み込みバックエンドを自動登録
_register_builtins()
