"""StackOverflow API クライアント.

Phase 12: StackExchange API（認証不要）を使用して
[cobol] [java] [migration] タグの質問動向を収集。
"""

from __future__ import annotations

import logging
from typing import Any

import httpx


class StackOverflowAPIClient:
    """StackExchange API クライアント.

    認証不要。レート制限: 300 req/day (匿名)。
    """

    BASE_URL = "https://api.stackexchange.com/2.3"

    def __init__(self, *, key: str | None = None) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)
        self._key = key  # オプション: API key で日次枠を10,000に拡張

    async def search_questions(
        self,
        tagged: str,
        page_size: int = 10,
        sort: str = "activity",
    ) -> list[dict[str, Any]]:
        """タグ付き質問を検索.

        Args:
            tagged: タグ（セミコロン区切りで複数指定可）
            page_size: 取得件数
            sort: ソート順 (activity, votes, creation)

        Returns:
            質問リスト
        """
        params: dict[str, Any] = {
            "tagged": tagged,
            "site": "stackoverflow",
            "pagesize": min(page_size, 30),
            "sort": sort,
            "order": "desc",
            "filter": "withbody",
        }
        if self._key:
            params["key"] = self._key

        try:
            async with httpx.AsyncClient(timeout=15.0) as client:
                resp = await client.get(f"{self.BASE_URL}/questions", params=params)
                resp.raise_for_status()
                data = resp.json()
                return data.get("items", [])
        except Exception as e:
            self._logger.warning("StackOverflow API失敗: %s", e)
            return []

    async def get_tag_info(self, tags: list[str]) -> list[dict[str, Any]]:
        """タグ情報（質問数・利用者数）を取得.

        Args:
            tags: タグ名リスト

        Returns:
            タグ情報リスト
        """
        tag_str = ";".join(tags[:5])
        params: dict[str, Any] = {
            "site": "stackoverflow",
            "filter": "default",
        }
        if self._key:
            params["key"] = self._key

        try:
            async with httpx.AsyncClient(timeout=15.0) as client:
                resp = await client.get(
                    f"{self.BASE_URL}/tags/{tag_str}/info", params=params,
                )
                resp.raise_for_status()
                data = resp.json()
                return data.get("items", [])
        except Exception as e:
            self._logger.warning("StackOverflow タグ情報取得失敗: %s", e)
            return []
