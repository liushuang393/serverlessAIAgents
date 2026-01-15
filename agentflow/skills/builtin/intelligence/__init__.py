# -*- coding: utf-8 -*-
"""情報収集・分析 Skills パッケージ.

このパッケージは市場/技術/競合情報の収集と分析スキルを提供します。

含まれるスキル:
- WebCrawler: ウェブ情報収集
- TrendAnalyzer: トレンド分析
- ReportBuilder: レポート構築

使用例:
    >>> from agentflow.skills.builtin.intelligence import (
    ...     WebCrawler,
    ...     TrendAnalyzer,
    ...     ReportBuilder,
    ... )
"""

from agentflow.skills.builtin.intelligence.web_crawler import (
    WebCrawler,
    CrawlConfig,
    CrawledPage,
    CrawlResult,
)
from agentflow.skills.builtin.intelligence.trend_analyzer import (
    TrendAnalyzer,
    TrendReport,
    TrendTopic,
    TrendDirection,
)
from agentflow.skills.builtin.intelligence.report_builder import (
    ReportBuilder,
    IntelReport,
    ReportConfig,
)

__all__ = [
    # ウェブ収集
    "WebCrawler",
    "CrawlConfig",
    "CrawledPage",
    "CrawlResult",
    # トレンド分析
    "TrendAnalyzer",
    "TrendReport",
    "TrendTopic",
    "TrendDirection",
    # レポート構築
    "ReportBuilder",
    "IntelReport",
    "ReportConfig",
]
