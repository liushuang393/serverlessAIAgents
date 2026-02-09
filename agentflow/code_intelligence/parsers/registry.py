"""Parser Registry - パーサーレジストリ.

パーサーの登録と取得を管理します。
"""

from __future__ import annotations

import logging
import threading
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.code_intelligence.parsers.base import CodeParser


_logger = logging.getLogger(__name__)


class ParserRegistry:
    """パーサーレジストリ.

    言語名でパーサーを登録・取得します。

    Example:
        >>> registry = ParserRegistry()
        >>> registry.register("python", PythonParser)
        >>> parser = registry.get("python")
    """

    _instance: ParserRegistry | None = None
    _instance_lock = threading.Lock()

    def __new__(cls) -> ParserRegistry:
        """シングルトンパターン."""
        with cls._instance_lock:
            if cls._instance is None:
                cls._instance = super().__new__(cls)
                cls._instance._initialized = False
            return cls._instance

    def __init__(self) -> None:
        """初期化."""
        if getattr(self, "_initialized", False):
            return

        self._parsers: dict[str, type[CodeParser]] = {}
        self._instances: dict[str, CodeParser] = {}
        self._lock = threading.RLock()
        self._initialized = True

    def register(self, language: str, parser_class: type[CodeParser]) -> None:
        """パーサーを登録.

        Args:
            language: 言語名
            parser_class: パーサークラス
        """
        with self._lock:
            language_lower = language.lower()
            if language_lower in self._parsers:
                _logger.warning(f"Overwriting parser for language: {language}")
            self._parsers[language_lower] = parser_class
            _logger.debug(f"Registered parser for: {language}")

    def get(self, language: str) -> CodeParser | None:
        """パーサーを取得.

        インスタンスはキャッシュされます。

        Args:
            language: 言語名

        Returns:
            パーサーインスタンス（存在しない場合は None）
        """
        with self._lock:
            language_lower = language.lower()

            # キャッシュされたインスタンスを返す
            if language_lower in self._instances:
                return self._instances[language_lower]

            # クラスからインスタンスを作成
            parser_class = self._parsers.get(language_lower)
            if parser_class is None:
                return None

            instance = parser_class()
            self._instances[language_lower] = instance
            return instance

    def get_or_raise(self, language: str) -> CodeParser:
        """パーサーを取得（存在しない場合は例外）.

        Args:
            language: 言語名

        Returns:
            パーサーインスタンス

        Raises:
            ValueError: パーサーが登録されていない場合
        """
        parser = self.get(language)
        if parser is None:
            available = ", ".join(self.list_languages())
            msg = f"Parser not found for language: {language}. Available: {available}"
            raise ValueError(msg)
        return parser

    def list_languages(self) -> list[str]:
        """登録済み言語リストを取得.

        Returns:
            言語名のリスト
        """
        with self._lock:
            return list(self._parsers.keys())

    def is_supported(self, language: str) -> bool:
        """言語がサポートされているか確認.

        Args:
            language: 言語名

        Returns:
            サポートされている場合 True
        """
        with self._lock:
            return language.lower() in self._parsers

    def unregister(self, language: str) -> bool:
        """パーサーを登録解除.

        Args:
            language: 言語名

        Returns:
            解除成功した場合 True
        """
        with self._lock:
            language_lower = language.lower()
            if language_lower in self._parsers:
                del self._parsers[language_lower]
                if language_lower in self._instances:
                    del self._instances[language_lower]
                return True
            return False


# グローバルレジストリ
_default_registry: ParserRegistry | None = None


def _get_registry() -> ParserRegistry:
    """デフォルトレジストリを取得."""
    global _default_registry
    if _default_registry is None:
        _default_registry = ParserRegistry()
    return _default_registry


def get_parser(language: str) -> CodeParser:
    """パーサーを取得.

    Args:
        language: 言語名

    Returns:
        パーサーインスタンス

    Raises:
        ValueError: パーサーが登録されていない場合
    """
    return _get_registry().get_or_raise(language)


def register_parser(language: str, parser_class: type[CodeParser]) -> None:
    """パーサーを登録.

    Args:
        language: 言語名
        parser_class: パーサークラス
    """
    _get_registry().register(language, parser_class)


__all__ = [
    "ParserRegistry",
    "get_parser",
    "register_parser",
]
