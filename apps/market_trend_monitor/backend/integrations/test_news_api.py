"""NewsAPIClient 単元テスト.

このモジュールは、NewsAPIClientのエラーハンドリングとレート制限対応をテストします。
"""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from apps.market_trend_monitor.backend.integrations.news_api import NewsAPIClient


@pytest.mark.asyncio
async def test_news_api_mock_mode() -> None:
    """NewsAPIClientのモックモードをテスト."""
    client = NewsAPIClient(api_key=None)

    # モック記事が返ることを確認
    articles = await client.search_everything("AI", page_size=5)
    assert len(articles) == 5
    assert "AI" in articles[0]["title"]


@pytest.mark.asyncio
async def test_news_api_rate_limit() -> None:
    """NewsAPIClientのレート制限対応をテスト."""
    client = NewsAPIClient(api_key="test-key", rate_limit_delay=0.5, max_retries=3)

    with patch("aiohttp.ClientSession") as mock_session:
        # レート制限エラー（429）をシミュレート
        mock_response = AsyncMock()
        mock_response.status = 429
        mock_response.__aenter__.return_value = mock_response
        mock_response.__aexit__.return_value = None

        mock_get = AsyncMock(return_value=mock_response)
        mock_session.return_value.__aenter__.return_value.get = mock_get
        mock_session.return_value.__aexit__.return_value = None

        # レート制限エラー後、モック記事が返ることを確認
        articles = await client.search_everything("AI", page_size=5)
        assert len(articles) == 5
        # リトライが実行されたことを確認
        assert mock_get.call_count == 3


@pytest.mark.asyncio
async def test_news_api_timeout() -> None:
    """NewsAPIClientのタイムアウト処理をテスト."""
    client = NewsAPIClient(api_key="test-key", max_retries=2)

    with patch("aiohttp.ClientSession") as mock_session:
        # タイムアウトをシミュレート
        mock_get = AsyncMock(side_effect=TimeoutError("Request timeout"))
        mock_session.return_value.__aenter__.return_value.get = mock_get
        mock_session.return_value.__aexit__.return_value = None

        # タイムアウト後、モック記事が返ることを確認
        articles = await client.search_everything("AI", page_size=5)
        assert len(articles) == 5
        # リトライが実行されたことを確認
        assert mock_get.call_count == 2


@pytest.mark.asyncio
async def test_news_api_successful_call() -> None:
    """NewsAPIClientの正常な呼び出しをテスト."""
    client = NewsAPIClient(api_key="test-key")

    with patch("apps.market_trend_monitor.backend.integrations.news_api.aiohttp.ClientSession") as mock_session:
        # 正常なレスポンスをシミュレート
        mock_response = MagicMock()
        mock_response.status = 200
        mock_response.json = AsyncMock(
            return_value={
                "articles": [
                    {
                        "source": {"id": "test", "name": "Test Source"},
                        "author": "Test Author",
                        "title": "Test Article about AI",
                        "description": "Test description",
                        "url": "https://example.com/article",
                        "urlToImage": None,
                        "publishedAt": "2025-11-21T00:00:00Z",
                        "content": "Test content",
                    }
                ]
            }
        )
        mock_response.__aenter__ = AsyncMock(return_value=mock_response)
        mock_response.__aexit__ = AsyncMock(return_value=None)

        mock_session_instance = MagicMock()
        mock_session_instance.get = MagicMock(return_value=mock_response)
        mock_session_instance.__aenter__ = AsyncMock(return_value=mock_session_instance)
        mock_session_instance.__aexit__ = AsyncMock(return_value=None)
        mock_session.return_value = mock_session_instance

        # 正常なレスポンスが返ることを確認
        articles = await client.search_everything("AI", page_size=1)
        assert len(articles) == 1
        assert articles[0]["title"] == "Test Article about AI"


@pytest.mark.asyncio
async def test_news_api_top_headlines() -> None:
    """NewsAPIClientのトップヘッドライン取得をテスト."""
    client = NewsAPIClient(api_key="test-key")

    with patch("apps.market_trend_monitor.backend.integrations.news_api.aiohttp.ClientSession") as mock_session:
        # 正常なレスポンスをシミュレート
        mock_response = MagicMock()
        mock_response.status = 200
        mock_response.json = AsyncMock(
            return_value={
                "articles": [
                    {
                        "source": {"id": "test", "name": "Test Source"},
                        "author": "Test Author",
                        "title": "Top Headline",
                        "description": "Test description",
                        "url": "https://example.com/headline",
                        "urlToImage": None,
                        "publishedAt": "2025-11-21T00:00:00Z",
                        "content": "Test content",
                    }
                ]
            }
        )
        mock_response.__aenter__ = AsyncMock(return_value=mock_response)
        mock_response.__aexit__ = AsyncMock(return_value=None)

        mock_session_instance = MagicMock()
        mock_session_instance.get = MagicMock(return_value=mock_response)
        mock_session_instance.__aenter__ = AsyncMock(return_value=mock_session_instance)
        mock_session_instance.__aexit__ = AsyncMock(return_value=None)
        mock_session.return_value = mock_session_instance

        # 正常なレスポンスが返ることを確認
        articles = await client.get_top_headlines(country="us", page_size=1)
        assert len(articles) == 1
        assert articles[0]["title"] == "Top Headline"


@pytest.mark.asyncio
async def test_news_api_error_fallback() -> None:
    """NewsAPIClientのエラー時のフォールバックをテスト."""
    client = NewsAPIClient(api_key="test-key", max_retries=1)

    with patch("aiohttp.ClientSession") as mock_session:
        # APIエラーをシミュレート
        mock_get = AsyncMock(side_effect=Exception("API Error"))
        mock_session.return_value.__aenter__.return_value.get = mock_get
        mock_session.return_value.__aexit__.return_value = None

        # エラー後、モック記事が返ることを確認
        articles = await client.search_everything("AI", page_size=3)
        assert len(articles) == 3


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
