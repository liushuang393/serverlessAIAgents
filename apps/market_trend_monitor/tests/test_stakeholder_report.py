"""ステークホルダーレポートユニットテスト.

StakeholderReportService のテスト。
Mock LLM を使用（外部依存なし）。
"""

from __future__ import annotations

import json
from unittest.mock import AsyncMock, MagicMock

from apps.market_trend_monitor.backend.models import SentimentType, Trend
from apps.market_trend_monitor.backend.services.stakeholder_report_service import (
    StakeholderReport,
    StakeholderReportService,
)


# ============================================================
# Helpers
# ============================================================


def _make_trend(
    trend_id: str = "t-1",
    topic: str = "COBOL Migration",
    score: float = 0.8,
    growth_rate: float = 0.2,
    sentiment: SentimentType = SentimentType.POSITIVE,
    metadata: dict | None = None,
) -> Trend:
    """テスト用 Trend を生成."""
    return Trend(
        id=trend_id,
        topic=topic,
        score=score,
        articles_count=5,
        keywords=["COBOL", "Java"],
        sentiment=sentiment,
        growth_rate=growth_rate,
        metadata=metadata or {},
    )


def _make_mock_llm():
    """テスト用 Mock LLM を生成."""
    mock = AsyncMock()
    mock.chat = AsyncMock(
        return_value=json.dumps(
            {
                "summary": "Market shows strong growth in AI-powered migration tools.",
                "findings": ["AI migration adoption increasing", "COBOL skills declining"],
                "recommendations": ["Invest in AI tooling", "Build Java expertise"],
                "risks": ["Skills shortage", "Migration complexity"],
            }
        )
    )
    return mock


# ============================================================
# Model Tests
# ============================================================


class TestStakeholderReport:
    """StakeholderReport データモデルのテスト."""

    def test_report_creation(self) -> None:
        """レポート生成テスト."""
        report = StakeholderReport(
            id="r-1",
            report_type="executive",
            title="Test Report",
            executive_summary="Summary",
            key_findings=["finding1"],
            recommendations=["rec1"],
            risk_factors=["risk1"],
        )
        assert report.id == "r-1"
        assert report.report_type == "executive"

    def test_report_to_dict(self) -> None:
        """to_dict 変換テスト."""
        report = StakeholderReport(
            id="r-1",
            report_type="technical",
            title="Test",
            executive_summary="Summary",
            key_findings=[],
            recommendations=[],
            risk_factors=[],
        )
        d = report.to_dict()
        assert d["id"] == "r-1"
        assert d["report_type"] == "technical"
        assert "generated_at" in d

    def test_report_defaults(self) -> None:
        """デフォルト値テスト."""
        report = StakeholderReport(
            id="r-1",
            report_type="executive",
            title="Test",
            executive_summary="",
            key_findings=[],
            recommendations=[],
            risk_factors=[],
        )
        assert report.data_sources == {}
        assert report.metadata == {}


# ============================================================
# Service Tests
# ============================================================


class TestStakeholderReportService:
    """StakeholderReportService のテスト."""

    async def test_generate_executive_report(self) -> None:
        """経営層レポート生成テスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        trends = [
            _make_trend("t-1", "COBOL Migration", 0.9),
            _make_trend("t-2", "AI Tools", 0.7),
        ]

        report = await service.generate_executive_report(trends, period_days=7)
        assert report.report_type == "executive"
        assert "7-day" in report.title
        assert len(report.key_findings) > 0
        assert len(report.recommendations) > 0

    async def test_generate_executive_report_llm_failure(self) -> None:
        """経営層レポート: LLM失敗時フォールバック."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(side_effect=Exception("LLM error"))
        service = StakeholderReportService(llm=mock_llm)
        trends = [_make_trend()]

        report = await service.generate_executive_report(trends)
        assert report.report_type == "executive"
        # フォールバックサマリー
        assert "COBOL Migration" in report.executive_summary

    async def test_generate_executive_report_empty_trends(self) -> None:
        """空トレンドの経営層レポートテスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        report = await service.generate_executive_report([], period_days=7)
        assert report is not None
        assert report.report_type == "executive"

    async def test_generate_executive_report_with_signal_service(self) -> None:
        """SignalService統合の経営層レポートテスト."""
        mock_signal = MagicMock()
        mock_signal.get_dashboard_stats = MagicMock(
            return_value={
                "total_signals": 10,
                "average_score": 3.5,
            }
        )
        service = StakeholderReportService(
            llm=_make_mock_llm(),
            signal_service=mock_signal,
        )
        trends = [_make_trend()]
        report = await service.generate_executive_report(trends)
        assert report.data_sources.get("signal_stats") is not None

    async def test_generate_technical_report(self) -> None:
        """技術レポート生成テスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        trends = [
            _make_trend("t-1", "COBOL Migration"),
            _make_trend("t-2", "AI Tools"),
        ]

        report = await service.generate_technical_report("COBOL", trends)
        assert report.report_type == "technical"
        assert "COBOL" in report.title

    async def test_generate_technical_report_no_match(self) -> None:
        """マッチなしの技術レポートテスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        trends = [_make_trend("t-1", "AI Tools")]

        report = await service.generate_technical_report("Quantum", trends)
        assert report.report_type == "technical"

    async def test_generate_technical_report_llm_failure(self) -> None:
        """技術レポート: LLM失敗時フォールバック."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(side_effect=Exception("LLM error"))
        service = StakeholderReportService(llm=mock_llm)

        report = await service.generate_technical_report("COBOL", [_make_trend()])
        assert report.report_type == "technical"

    async def test_generate_weekly_digest(self) -> None:
        """週次ダイジェスト生成テスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        trends = [
            _make_trend("t-1", growth_rate=0.3),  # 成長
            _make_trend("t-2", growth_rate=-0.2),  # 減少
            _make_trend("t-3", growth_rate=0.0, metadata={"growth_state": "new"}),
        ]

        report = await service.generate_weekly_digest(trends)
        assert report.report_type == "weekly_digest"
        assert "Weekly" in report.title
        assert report.data_sources["total_trends"] == 3
        assert report.data_sources["growing"] == 1
        assert report.data_sources["declining"] == 1
        assert report.data_sources["new_topics"] == 1

    async def test_generate_weekly_digest_empty(self) -> None:
        """空の週次ダイジェストテスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        report = await service.generate_weekly_digest([])
        assert report.data_sources["total_trends"] == 0

    def test_get_report(self) -> None:
        """レポート取得テスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        report = StakeholderReport(
            id="r-1",
            report_type="executive",
            title="Test",
            executive_summary="",
            key_findings=[],
            recommendations=[],
            risk_factors=[],
        )
        service._reports["r-1"] = report
        assert service.get_report("r-1") is report
        assert service.get_report("unknown") is None

    def test_list_reports(self) -> None:
        """レポート一覧テスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        for i in range(3):
            report = StakeholderReport(
                id=f"r-{i}",
                report_type="executive" if i < 2 else "technical",
                title=f"Test {i}",
                executive_summary="",
                key_findings=[],
                recommendations=[],
                risk_factors=[],
            )
            service._reports[report.id] = report

        all_reports = service.list_reports()
        assert len(all_reports) == 3

        exec_reports = service.list_reports(report_type="executive")
        assert len(exec_reports) == 2

    def test_fallback_summary_with_trends(self) -> None:
        """フォールバックサマリー: トレンドありテスト."""
        trends = [_make_trend("t-1", "Topic A"), _make_trend("t-2", "Topic B")]
        summary = StakeholderReportService._fallback_summary(trends)
        assert "Topic A" in summary or "Topic B" in summary
        assert "2" in summary

    def test_fallback_summary_empty(self) -> None:
        """フォールバックサマリー: トレンドなしテスト."""
        summary = StakeholderReportService._fallback_summary([])
        assert "No trends" in summary

    def test_parse_report_response_valid(self) -> None:
        """正常なJSONパーステスト."""
        raw = '{"summary": "test", "findings": ["f1"]}'
        result = StakeholderReportService._parse_report_response(raw)
        assert result["summary"] == "test"

    def test_parse_report_response_invalid(self) -> None:
        """不正JSONパーステスト."""
        result = StakeholderReportService._parse_report_response("not json")
        assert result == {}

    async def test_report_stored_after_generation(self) -> None:
        """生成後にレポートが保存されるテスト."""
        service = StakeholderReportService(llm=_make_mock_llm())
        report = await service.generate_executive_report([_make_trend()])
        assert service.get_report(report.id) is not None
