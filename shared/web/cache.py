"""Web 取得キャッシュ."""

from __future__ import annotations

import time
from dataclasses import dataclass
from typing import Any


@dataclass
class _CacheEntry:
    value: Any
    expires_at: float


class WebContentCache:
    """単純なインメモリキャッシュ."""

    def __init__(self, ttl_seconds: int = 300) -> None:
        self._ttl_seconds = ttl_seconds
        self._store: dict[str, _CacheEntry] = {}

    def get(self, key: str) -> Any | None:
        """キーで値を取得する."""
        entry = self._store.get(key)
        if entry is None:
            return None
        if entry.expires_at < time.monotonic():
            self._store.pop(key, None)
            return None
        return entry.value

    def set(self, key: str, value: Any) -> None:
        """キーに値を格納する."""
        expires_at = time.monotonic() + self._ttl_seconds
        self._store[key] = _CacheEntry(value=value, expires_at=expires_at)

    def clear(self) -> None:
        """キャッシュを全削除する."""
        self._store.clear()


_DEFAULT_WEB_CONTENT_CACHE: WebContentCache | None = None


def get_web_content_cache(ttl_seconds: int = 300) -> WebContentCache:
    """共有 Web キャッシュを取得する."""
    global _DEFAULT_WEB_CONTENT_CACHE
    if _DEFAULT_WEB_CONTENT_CACHE is None:
        _DEFAULT_WEB_CONTENT_CACHE = WebContentCache(ttl_seconds=ttl_seconds)
    return _DEFAULT_WEB_CONTENT_CACHE
