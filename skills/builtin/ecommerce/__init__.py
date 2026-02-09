"""跨境電商運営 Skills パッケージ.

このパッケージは跨境電商運営に必要なスキルセットを提供します。

含まれるスキル:
- competitor_scraper: 競合商品情報の収集
- price_analyzer: 価格・在庫分析
- listing_generator: 商品リスティング生成
- inventory_adjuster: 在庫・価格調整
- ad_monitor: 広告パフォーマンス監視
- daily_report: 日次レポート生成

ワークフロー:
    競品採集 → 価格/在庫分析 → Listing生成 → 在庫/調価 → 広告監視 → 日報生成

使用例:
    >>> from agentflow.skills.builtin.ecommerce import (
    ...     CompetitorScraper,
    ...     PriceAnalyzer,
    ...     ListingGenerator,
    ... )
    >>>
    >>> scraper = CompetitorScraper()
    >>> products = await scraper.scrape_competitors(
    ...     keywords=["wireless earbuds"],
    ...     platforms=["amazon", "ebay"],
    ... )
"""

from agentflow.skills.builtin.ecommerce.ad_monitor import (
    AdAlert,
    AdMonitor,
    AdPerformance,
)
from agentflow.skills.builtin.ecommerce.competitor_scraper import (
    CompetitorScraper,
    ScrapeConfig,
    ScrapedProduct,
)
from agentflow.skills.builtin.ecommerce.daily_report import (
    DailyReport,
    DailyReportGenerator,
    ReportSection,
)
from agentflow.skills.builtin.ecommerce.inventory_adjuster import (
    AdjustmentPlan,
    AdjustmentType,
    InventoryAdjuster,
)
from agentflow.skills.builtin.ecommerce.listing_generator import (
    GeneratedListing,
    ListingConfig,
    ListingGenerator,
)
from agentflow.skills.builtin.ecommerce.price_analyzer import (
    MarketPosition,
    PriceAnalysis,
    PriceAnalyzer,
    PriceStrategy,
)


__all__ = [
    "AdAlert",
    # 広告監視
    "AdMonitor",
    "AdPerformance",
    "AdjustmentPlan",
    "AdjustmentType",
    # 競品採集
    "CompetitorScraper",
    "DailyReport",
    # 日報生成
    "DailyReportGenerator",
    "GeneratedListing",
    # 在庫調整
    "InventoryAdjuster",
    "ListingConfig",
    # Listing生成
    "ListingGenerator",
    "MarketPosition",
    "PriceAnalysis",
    # 価格分析
    "PriceAnalyzer",
    "PriceStrategy",
    "ReportSection",
    "ScrapeConfig",
    "ScrapedProduct",
]
