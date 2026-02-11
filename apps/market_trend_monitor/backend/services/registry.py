"""サービス共有レジストリ.

エージェントとAPIで同一インスタンスを共有するためのレジストリです。
Phase 10: サービス間の依存関係を正しく注入。
Phase 12: 情報源信頼度追跡の統合。
Phase 13: 推奨エンジンの追加。
"""

from apps.market_trend_monitor.backend.services.adaptive_scoring_service import (
    AdaptiveScoringService,
)
from apps.market_trend_monitor.backend.services.bayesian_confidence_service import (
    BayesianConfidenceService,
)
from apps.market_trend_monitor.backend.services.evidence_service import EvidenceService
from apps.market_trend_monitor.backend.services.prediction_service import PredictionService
from apps.market_trend_monitor.backend.services.recommendation_service import (
    RecommendationService,
)
from apps.market_trend_monitor.backend.services.signal_service import SignalService
from apps.market_trend_monitor.backend.services.source_registry import SourceRegistryService
from apps.market_trend_monitor.backend.services.source_reliability_tracker import (
    SourceReliabilityTracker,
)


# Phase 9 サービス（他サービスの依存先）
adaptive_scoring_service = AdaptiveScoringService()
bayesian_confidence_service = BayesianConfidenceService()

# Phase 12: 情報源信頼度動的追跡
source_reliability_tracker = SourceReliabilityTracker()

# Phase 10/12: 依存関係を注入
evidence_service = EvidenceService(
    bayesian_confidence_service=bayesian_confidence_service,
    source_reliability_tracker=source_reliability_tracker,
)
signal_service = SignalService(
    adaptive_scoring_service=adaptive_scoring_service,
    bayesian_confidence_service=bayesian_confidence_service,
)
prediction_service = PredictionService(
    adaptive_scoring_service=adaptive_scoring_service,
)

# Phase 13: 推奨エンジン
recommendation_service = RecommendationService()

source_registry_service = SourceRegistryService()
