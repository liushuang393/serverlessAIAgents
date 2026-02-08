# -*- coding: utf-8 -*-
"""External API Integrations.

このモジュールは外部APIとの統合を提供します。
"""

from apps.market_trend_monitor.backend.integrations.arxiv_api import ArxivAPIClient
from apps.market_trend_monitor.backend.integrations.github_api import GitHubAPIClient
from apps.market_trend_monitor.backend.integrations.news_api import NewsAPIClient
from apps.market_trend_monitor.backend.integrations.rss_fetcher import RSSFetcher

__all__ = ["ArxivAPIClient", "GitHubAPIClient", "NewsAPIClient", "RSSFetcher"]
