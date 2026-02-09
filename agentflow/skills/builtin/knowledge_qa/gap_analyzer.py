"""ギャップ分析スキル - Gap Analyzer.

未回答質問と知識ベースの不足を分析するスキル。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from agentflow.core.agent_block import AgentBlock


logger = logging.getLogger(__name__)


@dataclass
class UnansweredQuestion:
    """未回答質問."""

    question: str
    count: int
    first_asked: datetime
    last_asked: datetime = field(default_factory=datetime.now)


@dataclass
class LowConfidenceAnswer:
    """低信頼度回答."""

    topic: str
    average_confidence: float
    questions_count: int


@dataclass
class SuggestedDoc:
    """提案ドキュメント."""

    title: str
    reason: str
    priority: str  # high, medium, low


@dataclass
class GapReport:
    """ギャップレポート."""

    period_start: datetime
    period_end: datetime
    unanswered_questions: list[UnansweredQuestion]
    low_confidence_answers: list[LowConfidenceAnswer]
    suggested_docs: list[SuggestedDoc]
    total_queries: int
    success_rate: float
    generated_at: datetime = field(default_factory=datetime.now)


class GapAnalyzer(AgentBlock):
    """ギャップ分析スキル."""

    def __init__(self, llm_client: Any | None = None) -> None:
        super().__init__()
        self._llm_client = llm_client

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """スキル実行."""
        query_logs = input_data.get("query_logs", [])
        period_start = input_data.get("period_start")
        period_end = input_data.get("period_end")

        report = await self.analyze(
            query_logs=query_logs,
            period_start=period_start,
            period_end=period_end,
        )

        return {
            "period": {
                "start": report.period_start.isoformat(),
                "end": report.period_end.isoformat(),
            },
            "unanswered_questions": [
                {"question": q.question, "count": q.count}
                for q in report.unanswered_questions
            ],
            "low_confidence_answers": [
                {
                    "topic": a.topic,
                    "average_confidence": a.average_confidence,
                    "questions_count": a.questions_count,
                }
                for a in report.low_confidence_answers
            ],
            "suggested_docs": [
                {"title": d.title, "reason": d.reason, "priority": d.priority}
                for d in report.suggested_docs
            ],
            "total_queries": report.total_queries,
            "success_rate": report.success_rate,
        }

    async def analyze(
        self,
        query_logs: list[dict[str, Any]] | None = None,
        period_start: datetime | str | None = None,
        period_end: datetime | str | None = None,
    ) -> GapReport:
        """ギャップを分析."""
        query_logs = query_logs or []

        if isinstance(period_start, str):
            period_start = datetime.fromisoformat(period_start)
        if isinstance(period_end, str):
            period_end = datetime.fromisoformat(period_end)

        period_start = period_start or datetime.now()
        period_end = period_end or datetime.now()

        logger.info("ギャップ分析開始: %d logs", len(query_logs))

        # プレースホルダー実装
        unanswered = [
            UnansweredQuestion(
                question="試用期間中の有給休暇は取れますか？",
                count=27,
                first_asked=datetime.now(),
            ),
        ]

        low_conf = [
            LowConfidenceAnswer(
                topic="リモートワークポリシー",
                average_confidence=0.55,
                questions_count=83,
            ),
        ]

        suggestions = [
            SuggestedDoc(
                title="リモートワーク詳細ポリシー",
                reason="関連質問が多く、正確性が低い",
                priority="high",
            ),
        ]

        return GapReport(
            period_start=period_start,
            period_end=period_end,
            unanswered_questions=unanswered,
            low_confidence_answers=low_conf,
            suggested_docs=suggestions,
            total_queries=len(query_logs) or 500,
            success_rate=0.85,
        )
