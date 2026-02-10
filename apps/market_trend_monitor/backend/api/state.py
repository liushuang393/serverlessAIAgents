"""API 共有状態.

各ルーターで使用するサービスインスタンスを集約します。
"""

from apps.market_trend_monitor.backend.services import MarketStore
from apps.market_trend_monitor.backend.services.registry import (
    evidence_service,
    prediction_service,
    signal_service,
    source_registry_service,
)


store = MarketStore(evidence_service=evidence_service)

# Re-export services from registry
__all__ = [
    "store",
    "source_registry_service",
    "prediction_service",
    "evidence_service",
    "signal_service",
]
