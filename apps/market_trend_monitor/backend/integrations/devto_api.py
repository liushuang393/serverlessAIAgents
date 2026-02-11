"""DEV.to API クライアント.

Phase 12: DEV.to API（認証不要）を使用して技術記事を収集。
"""

from __future__ import annotations

import logging
from typing import Any

import httpx


class DevToAPIClient:
    """DEV.to API クライアント.

    認証不要。レート制限: 30 req/30s。
    """

    BASE_URL = "https://dev.to/api"

    def __init__(self, *, api_key: str | None = None) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)
        self._api_key = api_key

    async def search_articles(
        self,
        tag: str,
        per_page: int = 10,
        page: int = 1,
    ) -> list[dict[str, Any]]:
        """タグベースで記事を検索.

        Args:
            tag: 検索タグ
            per_page: 1ページの件数
            page: ページ番号

        Returns:
            記事リスト
        """
        params = {
            "tag": tag,
            "per_page": min(per_page, 30),
            "page": page,
        }
        headers: dict[str, str] = {}
        if self._api_key:
            headers["api-key"] = self._api_key

        try:
            async with httpx.AsyncClient(timeout=15.0) as client:
                resp = await client.get(
                    f"{self.BASE_URL}/articles",
                    params=params,
                    headers=headers,
                )
                resp.raise_for_status()
                return resp.json()
        except Exception as e:
            self._logger.warning("DEV.to API失敗: %s", e)
            return []
