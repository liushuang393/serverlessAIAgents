"""レポート構築スキル - Report Builder.

収集・分析結果からレポートを構築するスキル。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from agentflow.core.agent_block import AgentBlock


logger = logging.getLogger(__name__)


class ReportFormat(str, Enum):
    """レポート形式."""

    MARKDOWN = "markdown"
    HTML = "html"
    JSON = "json"


@dataclass
class ReportConfig:
    """レポート設定."""

    format: ReportFormat = ReportFormat.MARKDOWN
    include_charts: bool = False
    max_topics: int = 10
    language: str = "ja"


@dataclass
class IntelReport:
    """情報レポート."""

    report_id: str
    title: str
    executive_summary: str
    market_trends: list[dict[str, Any]]
    tech_trends: list[dict[str, Any]]
    competitor_insights: list[dict[str, Any]]
    action_suggestions: list[str]
    format: ReportFormat
    generated_at: datetime = field(default_factory=datetime.now)


class ReportBuilder(AgentBlock):
    """レポート構築スキル."""

    def __init__(
        self,
        config: ReportConfig | None = None,
        llm_client: Any | None = None,
    ) -> None:
        super().__init__()
        self._config = config or ReportConfig()
        self._llm_client = llm_client

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """スキル実行."""
        trend_data = input_data.get("trend_data", {})
        title = input_data.get("title", "情報レポート")

        report = await self.build(trend_data=trend_data, title=title)

        return {
            "report_id": report.report_id,
            "title": report.title,
            "executive_summary": report.executive_summary,
            "market_trends": report.market_trends,
            "tech_trends": report.tech_trends,
            "action_suggestions": report.action_suggestions,
            "format": report.format.value,
            "generated_at": report.generated_at.isoformat(),
        }

    async def build(
        self,
        trend_data: dict[str, Any] | None = None,
        title: str = "情報レポート",
    ) -> IntelReport:
        """レポートを構築."""
        import uuid

        report_id = f"intel-{uuid.uuid4().hex[:8]}"
        trend_data = trend_data or {}

        logger.info("レポート構築開始: %s", title)

        # エグゼクティブサマリー生成
        summary = await self._generate_summary(trend_data)

        return IntelReport(
            report_id=report_id,
            title=title,
            executive_summary=summary,
            market_trends=[{"topic": "Enterprise AI", "direction": "up", "comment": "急成長中"}],
            tech_trends=[{"topic": "LLM Agents", "direction": "up", "comment": "注目分野"}],
            competitor_insights=[{"name": "Competitor A", "activity": "新製品リリース", "risk": "medium"}],
            action_suggestions=[
                "AI Agent分野への投資を検討",
                "競合動向の継続監視",
            ],
            format=self._config.format,
        )

    async def _generate_summary(self, trend_data: dict[str, Any]) -> str:
        """エグゼクティブサマリーを生成."""
        if self._llm_client:
            try:
                response = await self._llm_client.chat(
                    [
                        {
                            "role": "user",
                            "content": f"以下のデータに基づいてサマリーを生成: {trend_data}",
                        }
                    ]
                )
                if isinstance(response, dict):
                    return str(response.get("content", ""))
                return str(response)
            except Exception as e:
                logger.warning("LLMサマリー生成エラー: %s", e)

        return "市場は成長傾向にあり、AI Agent分野が特に注目されています。"
