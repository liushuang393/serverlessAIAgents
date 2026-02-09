"""設定API."""

from apps.market_trend_monitor.backend.config import config
from fastapi import APIRouter
from pydantic import BaseModel, Field


router = APIRouter(tags=["設定"])


class SettingsUpdateRequest(BaseModel):
    """設定更新リクエスト."""

    keywords: list[str] | None = None
    sources: list[str] | None = None
    interval_seconds: int | None = Field(default=None, ge=60)
    trend_score_threshold: float | None = Field(default=None, ge=0.0, le=1.0)
    alert_growth_rate_threshold: float | None = Field(default=None, ge=0.0, le=1.0)
    trend_window_days: int | None = Field(default=None, ge=1, le=90)


@router.get("/api/settings")
async def get_settings() -> dict:
    """設定を取得."""
    return {
        "collector": {
            "keywords": config.collector.keywords,
            "sources": config.collector.sources,
            "interval_seconds": config.collector.interval_seconds,
        },
        "analyzer": {
            "trend_score_threshold": config.analyzer.trend_score_threshold,
            "enable_sentiment_analysis": config.analyzer.enable_sentiment_analysis,
            "trend_window_days": config.analyzer.trend_window_days,
        },
        "notifier": {
            "alert_growth_rate_threshold": config.notifier.alert_growth_rate_threshold,
            "rate_limit_seconds": config.notifier.rate_limit_seconds,
        },
    }


@router.put("/api/settings")
async def update_settings(request: SettingsUpdateRequest) -> dict:
    """設定を更新."""
    if request.keywords is not None:
        config.collector.keywords = request.keywords
    if request.sources is not None:
        config.collector.sources = request.sources
    if request.interval_seconds is not None:
        config.collector.interval_seconds = request.interval_seconds
    if request.trend_score_threshold is not None:
        config.analyzer.trend_score_threshold = request.trend_score_threshold
    if request.trend_window_days is not None:
        config.analyzer.trend_window_days = request.trend_window_days
    if request.alert_growth_rate_threshold is not None:
        config.notifier.alert_growth_rate_threshold = request.alert_growth_rate_threshold

    return await get_settings()
