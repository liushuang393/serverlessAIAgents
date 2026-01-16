# -*- coding: utf-8 -*-
"""エージェントパッケージ.

Market Trend Monitor の全エージェントを提供します。
- 基本エージェント: Collector, Analyzer, Reporter, Notifier
- 核心機能エージェント: EvidenceLedger, SignalScorer, RedTeam, PredictionReview
"""

from apps.market_trend_monitor.backend.agents.analyzer_agent import AnalyzerAgent
from apps.market_trend_monitor.backend.agents.collector_agent import CollectorAgent
from apps.market_trend_monitor.backend.agents.evidence_ledger_agent import (
    EvidenceLedgerAgent,
    EvidenceLedgerInput,
    EvidenceLedgerOutput,
)
from apps.market_trend_monitor.backend.agents.notifier_agent import NotifierAgent
from apps.market_trend_monitor.backend.agents.prediction_review_agent import (
    PredictionReviewAgent,
    PredictionReviewInput,
    PredictionReviewOutput,
)
from apps.market_trend_monitor.backend.agents.redteam_agent import (
    RedTeamAgent,
    RedTeamInput,
    RedTeamOutput,
)
from apps.market_trend_monitor.backend.agents.reporter_agent import ReporterAgent
from apps.market_trend_monitor.backend.agents.signal_scorer_agent import (
    SignalScorerAgent,
    SignalScorerInput,
    SignalScorerOutput,
)

__all__ = [
    # 基本エージェント
    "AnalyzerAgent",
    "CollectorAgent",
    "NotifierAgent",
    "ReporterAgent",
    # 核心機能エージェント
    "EvidenceLedgerAgent",
    "EvidenceLedgerInput",
    "EvidenceLedgerOutput",
    "PredictionReviewAgent",
    "PredictionReviewInput",
    "PredictionReviewOutput",
    "RedTeamAgent",
    "RedTeamInput",
    "RedTeamOutput",
    "SignalScorerAgent",
    "SignalScorerInput",
    "SignalScorerOutput",
]

