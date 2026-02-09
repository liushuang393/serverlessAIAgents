"""RSS フィード取得."""

from __future__ import annotations

import asyncio
import logging
from typing import Any

import feedparser
import httpx


class RSSFetcher:
    """RSS フィード取得クライアント."""

    def __init__(self, max_retries: int = 2) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._max_retries = max_retries

    async def fetch(self, feed_url: str) -> list[dict[str, Any]]:
        """フィードを取得."""
        for attempt in range(self._max_retries + 1):
            try:
                async with httpx.AsyncClient(timeout=20.0) as client:
                    response = await client.get(feed_url)

                if response.status_code == 200:
                    feed = await asyncio.to_thread(feedparser.parse, response.text)
                    return list(feed.entries)

                self._logger.warning("RSS取得失敗: %s", response.status_code)
            except Exception as exc:
                self._logger.warning("RSS取得エラー: %s", exc)

            await asyncio.sleep(0.5 * (attempt + 1))

        return []
