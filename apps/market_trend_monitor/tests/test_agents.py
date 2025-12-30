"""エージェントのユニットテスト."""

import pytest

from apps.market_trend_monitor.backend.agents import (
    AnalyzerAgent,
    CollectorAgent,
    NotifierAgent,
    ReporterAgent,
)


class TestCollectorAgent:
    """CollectorAgent のテスト."""

    @pytest.mark.asyncio
    async def test_collector_initialization(self) -> None:
        """初期化テスト."""
        agent = CollectorAgent()
        await agent.initialize()
        assert agent is not None
        await agent.cleanup()

    @pytest.mark.asyncio
    async def test_collector_run(self) -> None:
        """実行テスト."""
        agent = CollectorAgent()
        await agent.initialize()

        input_data = {
            "keywords": ["COBOL", "Java"],
            "sources": ["news"],
        }

        result = await agent.run(input_data)

        assert "articles" in result
        assert "total_count" in result
        assert "sources_stats" in result
        assert result["total_count"] > 0

        await agent.cleanup()


class TestAnalyzerAgent:
    """AnalyzerAgent のテスト."""

    @pytest.mark.asyncio
    async def test_analyzer_initialization(self) -> None:
        """初期化テスト."""
        agent = AnalyzerAgent()
        await agent.initialize()
        assert agent is not None
        await agent.cleanup()

    @pytest.mark.asyncio
    async def test_analyzer_run(self) -> None:
        """実行テスト."""
        # まず CollectorAgent でデータ収集
        collector = CollectorAgent()
        await collector.initialize()
        collector_result = await collector.run(
            {"keywords": ["COBOL", "Java"], "sources": ["news"]}
        )
        await collector.cleanup()

        # AnalyzerAgent で分析
        agent = AnalyzerAgent()
        await agent.initialize()

        result = await agent.run({"articles": collector_result["articles"]})

        assert "trends" in result
        assert "summary" in result
        assert "keywords_stats" in result
        assert len(result["trends"]) > 0

        await agent.cleanup()


class TestReporterAgent:
    """ReporterAgent のテスト."""

    @pytest.mark.asyncio
    async def test_reporter_initialization(self) -> None:
        """初期化テスト."""
        agent = ReporterAgent()
        await agent.initialize()
        assert agent is not None
        await agent.cleanup()

    @pytest.mark.asyncio
    async def test_reporter_run(self) -> None:
        """実行テスト."""
        # データ準備
        collector = CollectorAgent()
        await collector.initialize()
        collector_result = await collector.run(
            {"keywords": ["COBOL", "Java"], "sources": ["news"]}
        )
        await collector.cleanup()

        analyzer = AnalyzerAgent()
        await analyzer.initialize()
        analyzer_result = await analyzer.run({"articles": collector_result["articles"]})
        await analyzer.cleanup()

        # ReporterAgent でレポート生成
        agent = ReporterAgent()
        await agent.initialize()

        result = await agent.run(
            {
                "trends": analyzer_result["trends"],
                "summary": analyzer_result["summary"],
                "period": "2025-W03",
            }
        )

        assert "report" in result
        assert "formats" in result
        assert result["report"]["title"] is not None

        await agent.cleanup()


class TestNotifierAgent:
    """NotifierAgent のテスト."""

    @pytest.mark.asyncio
    async def test_notifier_initialization(self) -> None:
        """初期化テスト."""
        agent = NotifierAgent()
        await agent.initialize()
        assert agent is not None
        await agent.cleanup()

    @pytest.mark.asyncio
    async def test_notifier_run(self) -> None:
        """実行テスト."""
        # データ準備
        collector = CollectorAgent()
        await collector.initialize()
        collector_result = await collector.run(
            {"keywords": ["COBOL", "Java"], "sources": ["news"]}
        )
        await collector.cleanup()

        analyzer = AnalyzerAgent()
        await analyzer.initialize()
        analyzer_result = await analyzer.run({"articles": collector_result["articles"]})
        await analyzer.cleanup()

        # NotifierAgent で通知
        agent = NotifierAgent()
        await agent.initialize()

        result = await agent.run(
            {"trends": analyzer_result["trends"], "alert_threshold": 0.3}
        )

        assert "notifications" in result
        assert "alerts_count" in result

        await agent.cleanup()

