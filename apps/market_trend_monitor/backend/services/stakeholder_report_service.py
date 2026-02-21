"""ステークホルダーレポートサービス.

経営層向け/技術チーム向けレポートを生成します。
LLMによる洞察の自然言語化を提供します。
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any

from agentflow import get_llm


if TYPE_CHECKING:
    from apps.market_trend_monitor.backend.models import Trend
    from apps.market_trend_monitor.backend.services.signal_service import SignalService


@dataclass
class StakeholderReport:
    """ステークホルダーレポートデータモデル."""

    id: str
    report_type: str  # "executive", "technical", "weekly_digest"
    title: str
    executive_summary: str
    key_findings: list[str]
    recommendations: list[str]
    risk_factors: list[str]
    generated_at: datetime = field(default_factory=datetime.now)
    data_sources: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "report_type": self.report_type,
            "title": self.title,
            "executive_summary": self.executive_summary,
            "key_findings": self.key_findings,
            "recommendations": self.recommendations,
            "risk_factors": self.risk_factors,
            "generated_at": self.generated_at.isoformat(),
            "data_sources": self.data_sources,
            "metadata": self.metadata,
        }


class StakeholderReportService:
    """ステークホルダーレポートサービス.

    - 経営層向けサマリーレポート生成
    - 技術チーム向け詳細レポート生成
    - 週次ダイジェスト生成
    """

    def __init__(
        self,
        *,
        llm: Any | None = None,
        signal_service: SignalService | None = None,
    ) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._llm = llm
        self._signal_service = signal_service
        self._reports: dict[str, StakeholderReport] = {}

    def _get_llm(self) -> Any:
        """LLMインスタンスを取得."""
        if self._llm is None:
            self._llm = get_llm(temperature=0.5)
        return self._llm

    async def generate_executive_report(
        self,
        trends: list[Trend],
        period_days: int = 7,
    ) -> StakeholderReport:
        """経営層向けレポートを生成.

        Args:
            trends: トレンドリスト
            period_days: 対象期間日数

        Returns:
            経営層向けレポート
        """
        top_trends = sorted(trends, key=lambda t: t.score, reverse=True)[:5]
        trend_summaries = [f"- {t.topic}: score={t.score:.2f}, growth={t.growth_rate:.1%}" for t in top_trends]

        dashboard_stats = {}
        if self._signal_service:
            dashboard_stats = self._signal_service.get_dashboard_stats()

        try:
            llm = self._get_llm()
            prompt = (
                "Generate an executive summary for COBOL to Java migration market trends.\n\n"
                f"Period: last {period_days} days\n"
                f"Top trends:\n" + "\n".join(trend_summaries) + "\n"
                f"Signal stats: {dashboard_stats}\n\n"
                "Provide:\n"
                "1. Executive summary (2-3 sentences)\n"
                "2. Key findings (3-5 bullets)\n"
                "3. Recommendations (2-3 bullets)\n"
                "4. Risk factors (2-3 bullets)\n\n"
                "Return JSON: {"
                '"summary": "...", '
                '"findings": ["..."], '
                '"recommendations": ["..."], '
                '"risks": ["..."]'
                "}\n\nJSON:"
            )
            response = await llm.chat([{"role": "user", "content": prompt}])
            raw = response if isinstance(response, str) else str(response)
            analysis = self._parse_report_response(raw)

            report = StakeholderReport(
                id=str(uuid.uuid4()),
                report_type="executive",
                title=f"Executive Market Brief ({period_days}-day review)",
                executive_summary=analysis.get("summary", self._fallback_summary(trends)),
                key_findings=analysis.get("findings", []),
                recommendations=analysis.get("recommendations", []),
                risk_factors=analysis.get("risks", []),
                data_sources={
                    "trend_count": len(trends),
                    "period_days": period_days,
                    "signal_stats": dashboard_stats,
                },
            )

        except Exception as e:
            self._logger.warning("経営層レポート生成失敗: %s", e)
            report = StakeholderReport(
                id=str(uuid.uuid4()),
                report_type="executive",
                title=f"Executive Market Brief ({period_days}-day review)",
                executive_summary=self._fallback_summary(trends),
                key_findings=[f"{t.topic}: score {t.score:.2f}" for t in top_trends],
                recommendations=["Continue monitoring key trends"],
                risk_factors=["LLM analysis unavailable for detailed insights"],
                data_sources={"trend_count": len(trends), "period_days": period_days},
            )

        self._reports[report.id] = report
        return report

    async def generate_technical_report(
        self,
        topic: str,
        trends: list[Trend],
    ) -> StakeholderReport:
        """技術チーム向けレポートを生成.

        Args:
            topic: 対象トピック
            trends: トレンドリスト

        Returns:
            技術レポート
        """
        related_trends = [t for t in trends if topic.lower() in t.topic.lower()]
        if not related_trends:
            related_trends = trends[:5]

        try:
            llm = self._get_llm()
            trend_details = "\n".join(
                f"- {t.topic}: score={t.score:.2f}, growth={t.growth_rate:.1%}, sentiment={t.sentiment.value}"
                for t in related_trends
            )
            prompt = (
                f"Generate a technical analysis report on '{topic}' "
                f"for the engineering team.\n\n"
                f"Related trends:\n{trend_details}\n\n"
                "Provide:\n"
                "1. Technical summary\n"
                "2. Key findings with technical depth\n"
                "3. Technical recommendations\n"
                "4. Technical risks\n\n"
                "Return JSON: {"
                '"summary": "...", '
                '"findings": ["..."], '
                '"recommendations": ["..."], '
                '"risks": ["..."]'
                "}\n\nJSON:"
            )
            response = await llm.chat([{"role": "user", "content": prompt}])
            raw = response if isinstance(response, str) else str(response)
            analysis = self._parse_report_response(raw)

            report = StakeholderReport(
                id=str(uuid.uuid4()),
                report_type="technical",
                title=f"Technical Analysis: {topic}",
                executive_summary=analysis.get("summary", f"Technical analysis of {topic}"),
                key_findings=analysis.get("findings", []),
                recommendations=analysis.get("recommendations", []),
                risk_factors=analysis.get("risks", []),
                data_sources={"topic": topic, "related_trend_count": len(related_trends)},
            )

        except Exception as e:
            self._logger.warning("技術レポート生成失敗: %s", e)
            report = StakeholderReport(
                id=str(uuid.uuid4()),
                report_type="technical",
                title=f"Technical Analysis: {topic}",
                executive_summary=f"Technical analysis of {topic}",
                key_findings=[t.topic for t in related_trends],
                recommendations=[],
                risk_factors=[],
                data_sources={"topic": topic, "related_trend_count": len(related_trends)},
            )

        self._reports[report.id] = report
        return report

    async def generate_weekly_digest(
        self,
        trends: list[Trend],
    ) -> StakeholderReport:
        """週次ダイジェストを生成.

        Args:
            trends: 今週のトレンドリスト

        Returns:
            週次ダイジェストレポート
        """
        growing = [t for t in trends if t.growth_rate > 0.05]
        declining = [t for t in trends if t.growth_rate < -0.05]
        new_topics = [t for t in trends if t.metadata.get("growth_state") == "new"]

        findings = []
        if growing:
            findings.append(f"{len(growing)} trends showing growth")
        if declining:
            findings.append(f"{len(declining)} trends declining")
        if new_topics:
            findings.append(f"{len(new_topics)} new topics detected")

        summary = (
            f"Weekly digest: {len(trends)} trends tracked. "
            f"{len(growing)} growing, {len(declining)} declining, "
            f"{len(new_topics)} new."
        )

        report = StakeholderReport(
            id=str(uuid.uuid4()),
            report_type="weekly_digest",
            title="Weekly Market Digest",
            executive_summary=summary,
            key_findings=findings,
            recommendations=[],
            risk_factors=[],
            data_sources={
                "total_trends": len(trends),
                "growing": len(growing),
                "declining": len(declining),
                "new_topics": len(new_topics),
            },
        )

        self._reports[report.id] = report
        return report

    def get_report(self, report_id: str) -> StakeholderReport | None:
        """レポートを取得."""
        return self._reports.get(report_id)

    def list_reports(
        self,
        report_type: str | None = None,
    ) -> list[StakeholderReport]:
        """レポート一覧を取得."""
        reports = list(self._reports.values())
        if report_type:
            reports = [r for r in reports if r.report_type == report_type]
        return sorted(reports, key=lambda r: r.generated_at, reverse=True)

    @staticmethod
    def _fallback_summary(trends: list[Trend]) -> str:
        """フォールバックサマリーを生成."""
        if not trends:
            return "No trends detected in the analysis period."
        top = sorted(trends, key=lambda t: t.score, reverse=True)[:3]
        topics = ", ".join(t.topic for t in top)
        return f"Key market trends: {topics}. Total {len(trends)} trends tracked."

    @staticmethod
    def _parse_report_response(raw: str) -> dict[str, Any]:
        """LLMレスポンスをパース."""
        try:
            import json

            start = raw.find("{")
            end = raw.rfind("}")
            if start != -1 and end != -1 and end > start:
                return json.loads(raw[start : end + 1])
        except (json.JSONDecodeError, ValueError):
            pass
        return {}
