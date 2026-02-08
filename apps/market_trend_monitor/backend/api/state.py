# -*- coding: utf-8 -*-
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

store = MarketStore()
