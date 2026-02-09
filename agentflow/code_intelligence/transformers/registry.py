"""Transformer Registry - 変換レジストリ."""

from __future__ import annotations

import logging
import threading
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.code_intelligence.transformers.base import CodeTransformer


_logger = logging.getLogger(__name__)


class TransformerRegistry:
    """変換レジストリ.

    ソース言語-ターゲット言語のペアで変換器を管理します。
    """

    _instance: TransformerRegistry | None = None
    _instance_lock = threading.Lock()

    def __new__(cls) -> TransformerRegistry:
        with cls._instance_lock:
            if cls._instance is None:
                cls._instance = super().__new__(cls)
                cls._instance._initialized = False
            return cls._instance

    def __init__(self) -> None:
        if getattr(self, "_initialized", False):
            return
        self._transformers: dict[str, type[CodeTransformer]] = {}
        self._instances: dict[str, CodeTransformer] = {}
        self._lock = threading.RLock()
        self._initialized = True

    def _make_key(self, source: str, target: str) -> str:
        return f"{source.lower()}->{target.lower()}"

    def register(
        self,
        source_language: str,
        target_language: str,
        transformer_class: type[CodeTransformer],
    ) -> None:
        """変換器を登録."""
        with self._lock:
            key = self._make_key(source_language, target_language)
            self._transformers[key] = transformer_class
            _logger.debug(f"Registered transformer: {key}")

    def get(self, source_language: str, target_language: str) -> CodeTransformer | None:
        """変換器を取得."""
        with self._lock:
            key = self._make_key(source_language, target_language)
            if key in self._instances:
                return self._instances[key]
            transformer_class = self._transformers.get(key)
            if transformer_class is None:
                return None
            instance = transformer_class()
            self._instances[key] = instance
            return instance

    def get_or_raise(self, source_language: str, target_language: str) -> CodeTransformer:
        """変換器を取得（存在しない場合は例外）."""
        transformer = self.get(source_language, target_language)
        if transformer is None:
            msg = f"Transformer not found: {source_language} -> {target_language}"
            raise ValueError(msg)
        return transformer

    def list_transformers(self) -> list[tuple[str, str]]:
        """登録済み変換ペアを取得."""
        with self._lock:
            pairs = []
            for key in self._transformers:
                parts = key.split("->")
                if len(parts) == 2:
                    pairs.append((parts[0], parts[1]))
            return pairs


_default_registry: TransformerRegistry | None = None


def _get_registry() -> TransformerRegistry:
    global _default_registry
    if _default_registry is None:
        _default_registry = TransformerRegistry()
    return _default_registry


def get_transformer(source_language: str, target_language: str) -> CodeTransformer:
    """変換器を取得."""
    return _get_registry().get_or_raise(source_language, target_language)


def register_transformer(
    source_language: str,
    target_language: str,
    transformer_class: type[CodeTransformer],
) -> None:
    """変換器を登録."""
    _get_registry().register(source_language, target_language, transformer_class)


__all__ = [
    "TransformerRegistry",
    "get_transformer",
    "register_transformer",
]
