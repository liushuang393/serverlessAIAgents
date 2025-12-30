# -*- coding: utf-8 -*-
"""External API Integrations.

このモジュールは外部APIとの統合を提供します。
"""

from apps.market_trend_monitor.backend.integrations.news_api import NewsAPIClient

__all__ = ["NewsAPIClient"]

