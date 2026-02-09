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

from agentflow.skills.builtin.intelligence.report_builder import (
    IntelReport,
    ReportBuilder,
    ReportConfig,
)
from agentflow.skills.builtin.intelligence.trend_analyzer import (
    TrendAnalyzer,
    TrendDirection,
    TrendReport,
    TrendTopic,
)
from agentflow.skills.builtin.intelligence.web_crawler import (
    CrawlConfig,
    CrawledPage,
    CrawlResult,
    WebCrawler,
)


__all__ = [
    "CrawlConfig",
    "CrawlResult",
    "CrawledPage",
    "IntelReport",
    # レポート構築
    "ReportBuilder",
    "ReportConfig",
    # トレンド分析
    "TrendAnalyzer",
    "TrendDirection",
    "TrendReport",
    "TrendTopic",
    # ウェブ収集
    "WebCrawler",
]
