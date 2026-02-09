"""データモデルパッケージ.

Market Trend Monitor の全データモデルを提供します。
- 基本モデル: Article, Trend, Report, Notification
- Evidence Ledger: Evidence, Claim
- Signal Scoring: Signal, SignalScore, SignalGrade
- Red Team: RedTeamAnalysis, CounterArgument, InvalidationCondition
- Prediction Review: Prediction, PredictionAccuracy
"""

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
from apps.market_trend_monitor.backend.models.evidence import (
    Claim,
    ClaimLevel,
    ClaimSchema,
    Evidence,
    EvidenceSchema,
)
from apps.market_trend_monitor.backend.models.prediction import (
    Prediction,
    PredictionAccuracy,
    PredictionAccuracySchema,
    PredictionOutcome,
    PredictionReview,
    PredictionReviewInput,
    PredictionReviewSchema,
    PredictionSchema,
    PredictionStatus,
)
from apps.market_trend_monitor.backend.models.red_team import (
    Challenge,
    ChallengeResult,
    ChallengeResultSchema,
    ChallengeSchema,
    ChallengeType,
    CounterArgument,
    CounterArgumentSchema,
    InvalidationCondition,
    InvalidationConditionSchema,
    RedTeamAnalysis,
    RedTeamAnalysisSchema,
)
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
from apps.market_trend_monitor.backend.models.signal import (
    Signal,
    SignalGrade,
    SignalSchema,
    SignalScore,
    SignalScoreSchema,
)
from apps.market_trend_monitor.backend.models.source_registry import (
    SourceRegistryEntry,
    SourceRegistrySchema,
)


__all__ = [
    # ============================================================
    # 基本 Pydantic スキーマ（Agent I/O）
    # ============================================================
    "AnalyzerInput",
    "AnalyzerOutput",
    # ============================================================
    # 基本 dataclass モデル
    # ============================================================
    "Article",
    "ArticleSchema",
    # ============================================================
    # Red Team モデル
    # ============================================================
    "Challenge",
    "ChallengeResult",
    "ChallengeResultSchema",
    "ChallengeSchema",
    "ChallengeType",
    "Claim",
    "ClaimLevel",
    "ClaimSchema",
    "CollectorInput",
    "CollectorOutput",
    "CounterArgument",
    "CounterArgumentSchema",
    "DateRange",
    # ============================================================
    # Evidence Ledger モデル
    # ============================================================
    "Evidence",
    "EvidenceSchema",
    "InvalidationCondition",
    "InvalidationConditionSchema",
    "Notification",
    "NotificationPriority",
    "NotificationSchema",
    "NotifierInput",
    "NotifierOutput",
    # ============================================================
    # Prediction Review モデル
    # ============================================================
    "Prediction",
    "PredictionAccuracy",
    "PredictionAccuracySchema",
    "PredictionOutcome",
    "PredictionReview",
    "PredictionReviewInput",
    "PredictionReviewSchema",
    "PredictionSchema",
    "PredictionStatus",
    "RedTeamAnalysis",
    "RedTeamAnalysisSchema",
    "Report",
    "ReportSchema",
    "ReportSection",
    "ReportSectionSchema",
    "ReporterInput",
    "ReporterOutput",
    "SentimentType",
    # ============================================================
    # Signal Scoring モデル
    # ============================================================
    "Signal",
    "SignalGrade",
    "SignalSchema",
    "SignalScore",
    "SignalScoreSchema",
    # ============================================================
    # 情報源台帳
    # ============================================================
    "SourceRegistryEntry",
    "SourceRegistrySchema",
    "SourceType",
    "Trend",
    "TrendSchema",
]
