"""Web search provider 実装."""

from __future__ import annotations

import os
from typing import Any
from urllib.parse import urlparse

import httpx

from infrastructure.providers.web.search.base import SearchItem, SearchProvider


class InternalSearchProvider(SearchProvider):
    """テスト・URL 直指定用の最小 provider."""

    name = "internal_search"

    async def search(self, query: str, *, top_k: int = 5) -> list[SearchItem]:
        normalized = query.strip()
        if not normalized:
            return []
        if normalized.startswith(("http://", "https://")):
            return [SearchItem(url=normalized, title=normalized, snippet="direct_url", score=1.0)]
        _ = top_k
        return []


class ExternalSearchAPIProvider(SearchProvider):
    """環境変数に応じて外部検索 API を使う provider."""

    name = "external_search_api"

    def __init__(self) -> None:
        self._bing_api_key = os.getenv("BING_SEARCH_API_KEY", "").strip()
        self._bing_endpoint = os.getenv("BING_SEARCH_ENDPOINT", "https://api.bing.microsoft.com/v7.0/search")
        self._serpapi_api_key = os.getenv("SERPAPI_API_KEY", "").strip()
        self._serpapi_endpoint = os.getenv("SERPAPI_ENDPOINT", "https://serpapi.com/search.json")
        self._tavily_api_key = os.getenv("TAVILY_API_KEY", "").strip()
        self._tavily_endpoint = os.getenv("TAVILY_SEARCH_ENDPOINT", "https://api.tavily.com/search")

    async def search(self, query: str, *, top_k: int = 5) -> list[SearchItem]:
        normalized = query.strip()
        if not normalized:
            return []
        if normalized.startswith(("http://", "https://")):
            return [SearchItem(url=normalized, title=normalized, snippet="direct_url", score=1.0)]

        providers = [
            self._search_tavily,
            self._search_bing,
            self._search_serpapi,
        ]
        for provider in providers:
            results = await provider(normalized, top_k=top_k)
            if results:
                return self._dedupe(results)[:top_k]
        return []

    async def _search_bing(self, query: str, *, top_k: int) -> list[SearchItem]:
        if not self._bing_api_key:
            return []
        headers = {"Ocp-Apim-Subscription-Key": self._bing_api_key}
        params: dict[str, str | int] = {"q": query, "count": max(1, min(top_k, 10)), "mkt": "en-US"}
        async with httpx.AsyncClient(timeout=20.0) as client:
            response = await client.get(self._bing_endpoint, headers=headers, params=params)
        if response.status_code != 200:
            return []
        payload = response.json()
        rows = payload.get("webPages", {}).get("value")
        if not isinstance(rows, list):
            return []
        return [
            SearchItem(
                url=str(row.get("url", "")).strip(),
                title=self._clean_text(row.get("name")),
                snippet=self._clean_text(row.get("snippet")),
                score=self._score_from_rank(index),
                metadata={"provider": "bing"},
            )
            for index, row in enumerate(rows, start=1)
            if isinstance(row, dict) and str(row.get("url", "")).strip()
        ]

    async def _search_serpapi(self, query: str, *, top_k: int) -> list[SearchItem]:
        if not self._serpapi_api_key:
            return []
        params: dict[str, str | int] = {
            "engine": "google",
            "q": query,
            "api_key": self._serpapi_api_key,
            "num": max(1, min(top_k, 10)),
        }
        async with httpx.AsyncClient(timeout=20.0) as client:
            response = await client.get(self._serpapi_endpoint, params=params)
        if response.status_code != 200:
            return []
        payload = response.json()
        rows = payload.get("organic_results")
        if not isinstance(rows, list):
            return []
        return [
            SearchItem(
                url=str(row.get("link", "")).strip(),
                title=self._clean_text(row.get("title")),
                snippet=self._clean_text(row.get("snippet")),
                score=self._score_from_rank(index),
                metadata={"provider": "serpapi"},
            )
            for index, row in enumerate(rows, start=1)
            if isinstance(row, dict) and str(row.get("link", "")).strip()
        ]

    async def _search_tavily(self, query: str, *, top_k: int) -> list[SearchItem]:
        if not self._tavily_api_key:
            return []
        payload = {
            "api_key": self._tavily_api_key,
            "query": query,
            "search_depth": "basic",
            "max_results": max(1, min(top_k, 10)),
        }
        async with httpx.AsyncClient(timeout=20.0) as client:
            response = await client.post(self._tavily_endpoint, json=payload)
        if response.status_code != 200:
            return []
        body = response.json()
        rows = body.get("results")
        if not isinstance(rows, list):
            return []
        return [
            SearchItem(
                url=str(row.get("url", "")).strip(),
                title=self._clean_text(row.get("title")),
                snippet=self._clean_text(row.get("content")),
                score=self._score_from_rank(index),
                metadata={"provider": "tavily"},
            )
            for index, row in enumerate(rows, start=1)
            if isinstance(row, dict) and str(row.get("url", "")).strip()
        ]

    @staticmethod
    def _clean_text(value: Any) -> str | None:
        text = str(value).strip() if value is not None else ""
        return text or None

    @staticmethod
    def _score_from_rank(rank: int) -> float:
        return max(0.1, min(1.0, 1.0 - ((rank - 1) * 0.08)))

    @staticmethod
    def _dedupe(items: list[SearchItem]) -> list[SearchItem]:
        seen: set[str] = set()
        unique: list[SearchItem] = []
        for item in items:
            normalized = urlparse(item.url)._replace(fragment="", query="").geturl()
            if normalized in seen:
                continue
            seen.add(normalized)
            unique.append(item)
        return unique
