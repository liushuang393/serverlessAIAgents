# -*- coding: utf-8 -*-
"""サービス共有レジストリ.

エージェントとAPIで同一インスタンスを共有するためのレジストリです。
"""

from apps.market_trend_monitor.backend.services.evidence_service import EvidenceService
from apps.market_trend_monitor.backend.services.prediction_service import PredictionService
from apps.market_trend_monitor.backend.services.signal_service import SignalService
from apps.market_trend_monitor.backend.services.source_registry import SourceRegistryService


evidence_service = EvidenceService()
signal_service = SignalService()
prediction_service = PredictionService()
source_registry_service = SourceRegistryService()
