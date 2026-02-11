"""GitHub API 連携."""

from __future__ import annotations

import asyncio
import logging
from datetime import datetime
from typing import Any

import httpx

from apps.market_trend_monitor.backend.integrations.rate_limiter import rate_limiter


class GitHubAPIClient:
    """GitHub API クライアント."""

    def __init__(
        self,
        token: str | None = None,
        base_url: str = "https://api.github.com",
        max_retries: int = 2,
    ) -> None:
        """初期化."""
        self._token = token
        self._base_url = base_url
        self._logger = logging.getLogger(self.__class__.__name__)
        self._max_retries = max_retries

    async def search_repositories(
        self,
        query: str,
        per_page: int = 5,
    ) -> list[dict[str, Any]]:
        """リポジトリ検索."""
        headers = {"Accept": "application/vnd.github+json"}
        if self._token:
            headers["Authorization"] = f"Bearer {self._token}"

        params = {
            "q": query,
            "per_page": per_page,
            "sort": "updated",
            "order": "desc",
        }

        url = f"{self._base_url}/search/repositories"

        for attempt in range(self._max_retries + 1):
            try:
                await rate_limiter.acquire("github")
                async with httpx.AsyncClient(timeout=20.0) as client:
                    response = await client.get(url, params=params, headers=headers)

                if response.status_code == 200:
                    payload = response.json()
                    return payload.get("items", [])

                if response.status_code == 403:
                    reset = response.headers.get("X-RateLimit-Reset")
                    if reset:
                        now = datetime.now().timestamp()
                        wait = max(0.0, float(reset) - now)
                        self._logger.warning("GitHubレート制限、%ss 待機", int(wait))
                        await asyncio.sleep(min(wait, 10.0))
                        continue

                self._logger.warning(
                    "GitHub APIエラー: status=%s", response.status_code
                )
            except Exception as exc:
                self._logger.warning("GitHub API 失敗: %s", exc)

            await asyncio.sleep(0.5 * (attempt + 1))

        return []
