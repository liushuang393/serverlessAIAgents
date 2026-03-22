"""ReportBuilder - kernel/reporter からの再エクスポートshim.

本体は kernel/reporter/service.py に移行済み。
後方互換性のためこのモジュールからも全シンボルをインポート可能。
"""

from kernel.reporter.service import (
    ChartData,
    ExecutiveSummary,
    OutputFormat,
    ReportBuilder,
    ReportSection,
    SectionedReportBuilder,
    SimpleReportBuilder,
    create_bar_chart,
    create_line_chart,
    create_pie_chart,
)


__all__ = [
    "ChartData",
    "ExecutiveSummary",
    "OutputFormat",
    "ReportBuilder",
    "ReportSection",
    "SectionedReportBuilder",
    "SimpleReportBuilder",
    "create_bar_chart",
    "create_line_chart",
    "create_pie_chart",
]
