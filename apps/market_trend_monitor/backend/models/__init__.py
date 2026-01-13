# -*- coding: utf-8 -*-
"""データモデルパッケージ."""

from apps.market_trend_monitor.backend.models.schemas import (
    Article,
    Notification,
    NotificationPriority,
    Report,
    ReportSection,
    SentimentType,
    SourceType,
    Trend,
)
from apps.market_trend_monitor.backend.models.agent_schemas import (
    AnalyzerInput,
    AnalyzerOutput,
    ArticleSchema,
    CollectorInput,
    CollectorOutput,
    DateRange,
    NotificationSchema,
    NotifierInput,
    NotifierOutput,
    ReporterInput,
    ReporterOutput,
    ReportSchema,
    ReportSectionSchema,
    TrendSchema,
)

__all__ = [
    # dataclass モデル
    "Article",
    "Notification",
    "NotificationPriority",
    "Report",
    "ReportSection",
    "SentimentType",
    "SourceType",
    "Trend",
    # Pydantic スキーマ（Agent I/O）
    "AnalyzerInput",
    "AnalyzerOutput",
    "ArticleSchema",
    "CollectorInput",
    "CollectorOutput",
    "DateRange",
    "NotificationSchema",
    "NotifierInput",
    "NotifierOutput",
    "ReporterInput",
    "ReporterOutput",
    "ReportSchema",
    "ReportSectionSchema",
    "TrendSchema",
]

