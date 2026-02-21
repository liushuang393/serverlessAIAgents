"""arXiv API 連携."""

from __future__ import annotations

import asyncio
import logging
from typing import Any

import feedparser
import httpx
from apps.market_trend_monitor.backend.integrations.rate_limiter import rate_limiter


class ArxivAPIClient:
    """arXiv API クライアント."""

    def __init__(
        self,
        base_url: str = "https://export.arxiv.org/api/query",
        max_retries: int = 2,
    ) -> None:
        """初期化."""
        self._base_url = base_url
        self._logger = logging.getLogger(self.__class__.__name__)
        self._max_retries = max_retries

    async def search(self, query: str, max_results: int = 5) -> list[dict[str, Any]]:
        """論文検索."""
        params = {
            "search_query": f"all:{query}",
            "start": 0,
            "max_results": max_results,
            "sortBy": "submittedDate",
            "sortOrder": "descending",
        }

        for attempt in range(self._max_retries + 1):
            try:
                await rate_limiter.acquire("arxiv")
                async with httpx.AsyncClient(timeout=20.0) as client:
                    response = await client.get(self._base_url, params=params)

                if response.status_code == 200:
                    feed = await asyncio.to_thread(feedparser.parse, response.text)
                    return list(feed.entries)

                self._logger.warning("arXiv APIエラー: status=%s", response.status_code)
            except Exception as exc:
                self._logger.warning("arXiv API 失敗: %s", exc)

            await asyncio.sleep(0.5 * (attempt + 1))

        return []
