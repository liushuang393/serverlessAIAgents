"""NewsAPI Integration.

このモジュールはNewsAPIとの統合を提供します。

主な機能:
    - ニュース記事の検索
    - トップヘッドラインの取得
    - ソース別ニュースの取得
    - レート制限対応
"""

import asyncio
import logging
from datetime import datetime, timedelta
from typing import Any

import aiohttp
from apps.market_trend_monitor.backend.integrations.rate_limiter import rate_limiter


class NewsAPIClient:
    """NewsAPIクライアント.

    NewsAPI (https://newsapi.org/) からニュース記事を取得します。

    Attributes:
        api_key: NewsAPI APIキー
        base_url: NewsAPI ベースURL
        rate_limit_delay: レート制限時の待機時間（秒）
        max_retries: 最大リトライ回数
    """

    def __init__(
        self,
        api_key: str | None = None,
        rate_limit_delay: float = 1.0,
        max_retries: int = 3,
    ) -> None:
        """NewsAPIClientを初期化.

        Args:
            api_key: NewsAPI APIキー（Noneの場合はモックモード）
            rate_limit_delay: レート制限時の待機時間（秒）
            max_retries: 最大リトライ回数
        """
        self._api_key = api_key
        self._base_url = "https://newsapi.org/v2"
        self._logger = logging.getLogger(__name__)
        self._mock_mode = api_key is None
        self._rate_limit_delay = rate_limit_delay
        self._max_retries = max_retries
        self._last_request_time: float = 0.0

    async def _wait_for_rate_limit(self) -> None:
        """レート制限のための待機.

        共有トークンバケットを使用。
        """
        await rate_limiter.acquire("news")

    async def search_everything(
        self,
        query: str,
        languages: list[str] | None = None,
        sort_by: str = "publishedAt",
        page_size: int = 10,
        from_date: datetime | None = None,
    ) -> list[dict[str, Any]]:
        """ニュース記事を検索.

        Args:
            query: 検索クエリ
            languages: 言語コードリスト（en, ja, zh等）。Noneの場合は 'en'
            sort_by: ソート順（publishedAt, relevancy, popularity）
            page_size: 取得件数
            from_date: 開始日時（Noneの場合は7日前）

        Returns:
            ニュース記事リスト
        """
        if languages is None:
            languages = ["en"]

        all_articles = []
        for lang in languages:
            lang_articles = await self._search_single_lang(
                query=query,
                language=lang,
                sort_by=sort_by,
                page_size=page_size,
                from_date=from_date,
            )
            all_articles.extend(lang_articles)

        return all_articles

    async def _search_single_lang(
        self,
        query: str,
        language: str = "en",
        sort_by: str = "publishedAt",
        page_size: int = 10,
        from_date: datetime | None = None,
    ) -> list[dict[str, Any]]:
        if self._mock_mode:
            return self._generate_mock_articles(query, page_size)

        if from_date is None:
            from_date = datetime.now() - timedelta(days=7)

        params = {
            "q": query,
            "language": language,
            "sortBy": sort_by,
            "pageSize": page_size,
            "from": from_date.strftime("%Y-%m-%d"),
            "apiKey": self._api_key,
        }

        # リトライ機構付きでリクエスト
        for attempt in range(self._max_retries):
            try:
                # レート制限待機
                await self._wait_for_rate_limit()

                async with (
                    aiohttp.ClientSession() as session,
                    session.get(
                        f"{self._base_url}/everything",
                        params=params,
                        timeout=aiohttp.ClientTimeout(total=30),
                    ) as response,
                ):
                    if response.status == 200:
                        data = await response.json()
                        return data.get("articles", [])
                    if response.status == 429:
                        # レート制限エラー
                        self._logger.warning(
                            f"Rate limit exceeded (attempt {attempt + 1}/{self._max_retries})"
                        )
                        if attempt < self._max_retries - 1:
                            wait_time = self._rate_limit_delay * (2**attempt)
                            await asyncio.sleep(wait_time)
                            continue
                    else:
                        self._logger.error(f"NewsAPI error: {response.status}")
                        if attempt < self._max_retries - 1:
                            await asyncio.sleep(1.0)
                            continue
                        return self._generate_mock_articles(query, page_size)
            except TimeoutError:
                self._logger.exception(
                    f"NewsAPI timeout (attempt {attempt + 1}/{self._max_retries})"
                )
                if attempt < self._max_retries - 1:
                    await asyncio.sleep(1.0)
                    continue
            except Exception as e:
                self._logger.exception(f"Failed to fetch news from NewsAPI: {e}")
                if attempt < self._max_retries - 1:
                    await asyncio.sleep(1.0)
                    continue

        # 全てのリトライが失敗した場合はモック記事を返す
        return self._generate_mock_articles(query, page_size)

    async def get_top_headlines(
        self,
        country: str = "us",
        category: str | None = None,
        page_size: int = 10,
    ) -> list[dict[str, Any]]:
        """トップヘッドラインを取得.

        Args:
            country: 国コード（us, jp等）
            category: カテゴリ（business, technology等）
            page_size: 取得件数

        Returns:
            ニュース記事リスト
        """
        if self._mock_mode:
            return self._generate_mock_articles(f"top headlines {country}", page_size)

        params = {
            "country": country,
            "pageSize": page_size,
            "apiKey": self._api_key,
        }

        if category:
            params["category"] = category

        # リトライ機構付きでリクエスト
        for attempt in range(self._max_retries):
            try:
                # レート制限待機
                await self._wait_for_rate_limit()

                async with (
                    aiohttp.ClientSession() as session,
                    session.get(
                        f"{self._base_url}/top-headlines",
                        params=params,
                        timeout=aiohttp.ClientTimeout(total=30),
                    ) as response,
                ):
                    if response.status == 200:
                        data = await response.json()
                        return data.get("articles", [])
                    if response.status == 429:
                        # レート制限エラー
                        self._logger.warning(
                            f"Rate limit exceeded (attempt {attempt + 1}/{self._max_retries})"
                        )
                        if attempt < self._max_retries - 1:
                            wait_time = self._rate_limit_delay * (2**attempt)
                            await asyncio.sleep(wait_time)
                            continue
                    else:
                        self._logger.error(f"NewsAPI error: {response.status}")
                        if attempt < self._max_retries - 1:
                            await asyncio.sleep(1.0)
                            continue
                        return self._generate_mock_articles(f"top headlines {country}", page_size)
            except TimeoutError:
                self._logger.exception(
                    f"NewsAPI timeout (attempt {attempt + 1}/{self._max_retries})"
                )
                if attempt < self._max_retries - 1:
                    await asyncio.sleep(1.0)
                    continue
            except Exception as e:
                self._logger.exception(f"Failed to fetch top headlines from NewsAPI: {e}")
                if attempt < self._max_retries - 1:
                    await asyncio.sleep(1.0)
                    continue

        # 全てのリトライが失敗した場合はモック記事を返す
        return self._generate_mock_articles(f"top headlines {country}", page_size)

    def _generate_mock_articles(self, query: str, count: int) -> list[dict[str, Any]]:
        """モック記事を生成.

        Args:
            query: 検索クエリ
            count: 生成件数

        Returns:
            モック記事リスト
        """
        articles = []
        for i in range(count):
            articles.append(
                {
                    "source": {"id": None, "name": f"Mock Source {i + 1}"},
                    "author": f"Mock Author {i + 1}",
                    "title": f"Mock Article about {query} - {i + 1}",
                    "description": f"This is a mock article about {query}. Article number {i + 1}.",
                    "url": f"https://example.com/article-{i + 1}",
                    "urlToImage": None,
                    "publishedAt": datetime.now().isoformat(),
                    "content": f"Mock content for article {i + 1} about {query}.",
                }
            )
        return articles
