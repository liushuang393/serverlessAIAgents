"""統一レジストリ基類 - Protocol/Skill/Coordinator 共用.

このモジュールは汎用的なレジストリパターンを提供します：
- 型安全な登録・取得
- スレッドセーフな操作
- 自動発見機能（オプション）

設計原則：
- 簡単：シンプルな API
- 柔軟：ジェネリック対応
- 拡張：サブクラスでカスタマイズ可能
"""

import logging
import threading
from abc import ABC, abstractmethod
from typing import Any, Generic, TypeVar

# 型変数定義
T = TypeVar("T")


class Registry(ABC, Generic[T]):
    """汎用レジストリ基底クラス.

    Protocol、Skill、Coordinatorなど様々なコンポーネントの
    登録・取得を統一的に管理します。

    Example:
        >>> class MyRegistry(Registry[MyClass]):
        ...     pass
        >>> registry = MyRegistry()
        >>> registry.register("item1", MyClass())
        >>> item = registry.get("item1")
    """

    def __init__(self) -> None:
        """レジストリを初期化."""
        self._items: dict[str, T] = {}
        self._lock = threading.RLock()
        self._logger = logging.getLogger(self.__class__.__name__)

    def register(self, name: str, item: T) -> None:
        """アイテムを登録.

        Args:
            name: アイテム名（一意識別子）
            item: 登録するアイテム

        Raises:
            ValueError: 名前が空の場合
            TypeError: 名前が文字列でない場合、またはアイテムが None の場合
        """
        if not isinstance(name, str):
            msg = f"Name must be a string, got {type(name).__name__}"
            raise TypeError(msg)
        if not name or not name.strip():
            msg = "Name cannot be empty or whitespace only"
            raise ValueError(msg)
        if item is None:
            msg = "Item cannot be None"
            raise TypeError(msg)

        with self._lock:
            if name in self._items:
                self._logger.warning(f"Overwriting existing item: {name}")
            self._items[name] = item
            self._logger.debug(f"Registered: {name}")

    def get(self, name: str) -> T | None:
        """アイテムを取得.

        Args:
            name: アイテム名

        Returns:
            アイテム、存在しない場合 None
        """
        with self._lock:
            return self._items.get(name)

    def get_or_raise(self, name: str) -> T:
        """アイテムを取得（存在しない場合は例外）.

        Args:
            name: アイテム名

        Returns:
            アイテム

        Raises:
            KeyError: アイテムが存在しない場合
        """
        item = self.get(name)
        if item is None:
            msg = f"Item not found: {name}"
            raise KeyError(msg)
        return item

    def unregister(self, name: str) -> bool:
        """アイテムを削除.

        Args:
            name: アイテム名

        Returns:
            削除成功した場合 True
        """
        with self._lock:
            if name in self._items:
                del self._items[name]
                self._logger.debug(f"Unregistered: {name}")
                return True
            return False

    def list_names(self) -> list[str]:
        """登録済みアイテム名一覧を取得.

        Returns:
            アイテム名のリスト
        """
        with self._lock:
            return list(self._items.keys())

    def list_all(self) -> dict[str, T]:
        """全アイテムを取得.

        Returns:
            アイテム辞書のコピー
        """
        with self._lock:
            return dict(self._items)

    def clear(self) -> None:
        """全アイテムを削除."""
        with self._lock:
            self._items.clear()
            self._logger.debug("Registry cleared")

    def __len__(self) -> int:
        """登録済みアイテム数を取得."""
        with self._lock:
            return len(self._items)

    def __contains__(self, name: str) -> bool:
        """アイテムの存在確認."""
        with self._lock:
            return name in self._items


class ProtocolRegistry(Registry[Any]):
    """プロトコルレジストリ - MCP/A2A/AG-UI/A2UI 用."""

    _instance: "ProtocolRegistry | None" = None
    _instance_lock = threading.Lock()

    def __new__(cls) -> "ProtocolRegistry":
        """シングルトンパターン."""
        with cls._instance_lock:
            if cls._instance is None:
                cls._instance = super().__new__(cls)
            return cls._instance

