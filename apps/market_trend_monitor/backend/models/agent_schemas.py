# -*- coding: utf-8 -*-
"""Agent I/O スキーマ定義.

Market Trend Monitor の Agent 入出力スキーマを Pydantic で定義。
ResilientAgent の型安全性を確保します。
"""

from datetime import datetime

from pydantic import BaseModel, Field

from apps.market_trend_monitor.backend.models.schemas import (
    NotificationPriority,
    SentimentType,
    SourceType,
)


# ============================================================
# CollectorAgent スキーマ
# ============================================================

class DateRange(BaseModel):
    """日付範囲."""

    start: str | None = None
    end: str | None = None


class CollectorInput(BaseModel):
    """CollectorAgent 入力スキーマ."""

    keywords: list[str] = Field(default_factory=list, description="検索キーワード")
    sources: list[str] = Field(default=["news"], description="データソース")
    date_range: DateRange | None = Field(default=None, description="日付範囲")


class ArticleSchema(BaseModel):
    """記事スキーマ."""

    id: str
    title: str
    url: str
    source: str
    published_at: str
    content: str
    keywords: list[str] = Field(default_factory=list)
    collected_at: str
    metadata: dict = Field(default_factory=dict)


class CollectorOutput(BaseModel):
    """CollectorAgent 出力スキーマ."""

    articles: list[ArticleSchema] = Field(default_factory=list)
    total_count: int = 0
    sources_stats: dict[str, int] = Field(default_factory=dict)


# ============================================================
# AnalyzerAgent スキーマ
# ============================================================

class AnalyzerInput(BaseModel):
    """AnalyzerAgent 入力スキーマ."""

    articles: list[ArticleSchema] = Field(default_factory=list)
    enable_sentiment: bool = True


class TrendSchema(BaseModel):
    """トレンドスキーマ."""

    id: str
    topic: str
    score: float
    articles_count: int
    keywords: list[str]
    sentiment: str
    growth_rate: float
    created_at: str
    metadata: dict = Field(default_factory=dict)


class AnalyzerOutput(BaseModel):
    """AnalyzerAgent 出力スキーマ."""

    trends: list[TrendSchema] = Field(default_factory=list)
    summary: str = ""
    keywords_stats: dict[str, int] = Field(default_factory=dict)


# ============================================================
# ReporterAgent スキーマ
# ============================================================

class ReporterInput(BaseModel):
    """ReporterAgent 入力スキーマ."""

    trends: list[TrendSchema] = Field(default_factory=list)
    summary: str = ""
    period: str = ""


class ReportSectionSchema(BaseModel):
    """レポートセクションスキーマ."""

    title: str
    content: str
    charts: list[dict] = Field(default_factory=list)
    metadata: dict = Field(default_factory=dict)


class ReportSchema(BaseModel):
    """レポートスキーマ."""

    id: str
    title: str
    sections: list[ReportSectionSchema]
    generated_at: str
    period: str = ""
    metadata: dict = Field(default_factory=dict)


class ReporterOutput(BaseModel):
    """ReporterAgent 出力スキーマ."""

    report: ReportSchema
    formats: list[str] = Field(default=["markdown", "html"])


# ============================================================
# NotifierAgent スキーマ
# ============================================================

class NotifierInput(BaseModel):
    """NotifierAgent 入力スキーマ."""

    trends: list[TrendSchema] = Field(default_factory=list)
    alert_threshold: float = 0.3


class NotificationSchema(BaseModel):
    """通知スキーマ."""

    id: str
    type: str
    priority: str
    message: str
    timestamp: str
    metadata: dict = Field(default_factory=dict)


class NotifierOutput(BaseModel):
    """NotifierAgent 出力スキーマ."""

    notifications: list[NotificationSchema] = Field(default_factory=list)
    alerts_count: int = 0

