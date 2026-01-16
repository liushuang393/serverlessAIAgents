# -*- coding: utf-8 -*-
"""データモデルパッケージ.

Market Trend Monitor の全データモデルを提供します。
- 基本モデル: Article, Trend, Report, Notification
- Evidence Ledger: Evidence, Claim
- Signal Scoring: Signal, SignalScore, SignalGrade
- Red Team: RedTeamAnalysis, CounterArgument, InvalidationCondition
- Prediction Review: Prediction, PredictionAccuracy
"""

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
from apps.market_trend_monitor.backend.models.evidence import (
    Claim,
    ClaimLevel,
    ClaimSchema,
    Evidence,
    EvidenceSchema,
)
from apps.market_trend_monitor.backend.models.signal import (
    Signal,
    SignalGrade,
    SignalSchema,
    SignalScore,
    SignalScoreSchema,
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

__all__ = [
    # ============================================================
    # 基本 dataclass モデル
    # ============================================================
    "Article",
    "Notification",
    "NotificationPriority",
    "Report",
    "ReportSection",
    "SentimentType",
    "SourceType",
    "Trend",
    # ============================================================
    # 基本 Pydantic スキーマ（Agent I/O）
    # ============================================================
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
    # ============================================================
    # Evidence Ledger モデル
    # ============================================================
    "Evidence",
    "EvidenceSchema",
    "Claim",
    "ClaimLevel",
    "ClaimSchema",
    # ============================================================
    # Signal Scoring モデル
    # ============================================================
    "Signal",
    "SignalGrade",
    "SignalSchema",
    "SignalScore",
    "SignalScoreSchema",
    # ============================================================
    # Red Team モデル
    # ============================================================
    "Challenge",
    "ChallengeResult",
    "ChallengeResultSchema",
    "ChallengeSchema",
    "ChallengeType",
    "CounterArgument",
    "CounterArgumentSchema",
    "InvalidationCondition",
    "InvalidationConditionSchema",
    "RedTeamAnalysis",
    "RedTeamAnalysisSchema",
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
]

