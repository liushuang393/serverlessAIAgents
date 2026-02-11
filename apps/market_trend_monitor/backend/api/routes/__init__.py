"""API ルーター集約."""

from apps.market_trend_monitor.backend.api.routes.collect import router as collect_router
from apps.market_trend_monitor.backend.api.routes.competitors import router as competitors_router
from apps.market_trend_monitor.backend.api.routes.evidence import router as evidence_router
from apps.market_trend_monitor.backend.api.routes.metrics import router as metrics_router
from apps.market_trend_monitor.backend.api.routes.predictions import router as predictions_router
from apps.market_trend_monitor.backend.api.routes.settings import router as settings_router
from apps.market_trend_monitor.backend.api.routes.signals import router as signals_router
from apps.market_trend_monitor.backend.api.routes.sources import router as sources_router
from apps.market_trend_monitor.backend.api.routes.trends import router as trends_router


__all__ = [
    "collect_router",
    "competitors_router",
    "evidence_router",
    "metrics_router",
    "predictions_router",
    "settings_router",
    "signals_router",
    "sources_router",
    "trends_router",
]
