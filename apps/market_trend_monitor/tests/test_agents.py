"""エージェントのユニットテスト."""

import pytest

from apps.market_trend_monitor.backend.agents import (
    AnalyzerAgent,
    CollectorAgent,
    EvidenceLedgerAgent,
    NotifierAgent,
    PredictionReviewAgent,
    RedTeamAgent,
    ReporterAgent,
    SignalScorerAgent,
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



class TestEvidenceLedgerAgent:
    """EvidenceLedgerAgent のテスト."""

    @pytest.mark.asyncio
    async def test_evidence_ledger_initialization(self) -> None:
        """初期化テスト."""
        agent = EvidenceLedgerAgent()
        await agent.initialize()
        assert agent is not None
        await agent.cleanup()

    @pytest.mark.asyncio
    async def test_evidence_ledger_run(self) -> None:
        """実行テスト."""
        agent = EvidenceLedgerAgent()
        await agent.initialize()

        input_data = {
            "articles": [
                {
                    "id": "test-1",
                    "title": "AI市場が急成長",
                    "url": "https://example.com/ai-growth",
                    "source": "news",
                    "published_at": "2025-01-15T10:00:00",
                    "content": "AI市場は2025年に大幅な成長が予測されています。",
                    "keywords": ["AI", "市場"],
                    "collected_at": "2025-01-15T12:00:00",
                    "metadata": {},
                }
            ],
            "claim_text": "AI市場は2025年に成長する",
        }

        result = await agent.run(input_data)

        assert "evidences" in result
        assert "evidence_ids" in result
        assert result["total_registered"] >= 1

        await agent.cleanup()


class TestSignalScorerAgent:
    """SignalScorerAgent のテスト."""

    @pytest.mark.asyncio
    async def test_signal_scorer_initialization(self) -> None:
        """初期化テスト."""
        agent = SignalScorerAgent()
        await agent.initialize()
        assert agent is not None
        await agent.cleanup()

    @pytest.mark.asyncio
    async def test_signal_scorer_run(self) -> None:
        """実行テスト."""
        agent = SignalScorerAgent()
        await agent.initialize()

        input_data = {
            "signals": [
                {
                    "id": "sig-1",
                    "name": "AI市場成長シグナル",
                    "description": "AI関連企業の株価上昇",
                    "source": "market_data",
                    "detected_at": "2025-01-15T10:00:00",
                    "raw_value": 0.75,
                    "metadata": {},
                }
            ],
        }

        result = await agent.run(input_data)

        assert "signals" in result
        assert "total_evaluated" in result
        assert "dashboard_stats" in result

        await agent.cleanup()


class TestRedTeamAgent:
    """RedTeamAgent のテスト."""

    @pytest.mark.asyncio
    async def test_redteam_initialization(self) -> None:
        """初期化テスト."""
        agent = RedTeamAgent()
        await agent.initialize()
        assert agent is not None
        await agent.cleanup()

    @pytest.mark.asyncio
    async def test_redteam_run(self) -> None:
        """実行テスト."""
        agent = RedTeamAgent()
        await agent.initialize()

        input_data = {
            "claim": {
                "id": "claim-1",
                "text": "AI市場は2025年に50%成長する",
                "level": "hypothesis",
                "confidence": 0.7,
                "created_at": "2025-01-15T10:00:00",
                "evidence_ids": [],
                "metadata": {},
            },
            "evidences": [
                {
                    "id": "ev-1",
                    "source_id": "article-1",
                    "source_type": "news",
                    "content": "AI市場の成長予測",
                    "reliability": 0.8,
                    "collected_at": "2025-01-15T10:00:00",
                    "metadata": {},
                }
            ],
        }

        result = await agent.run(input_data)

        assert "challenges" in result
        assert "results" in result
        assert "stats" in result

        await agent.cleanup()


class TestPredictionReviewAgent:
    """PredictionReviewAgent のテスト."""

    @pytest.mark.asyncio
    async def test_prediction_review_initialization(self) -> None:
        """初期化テスト."""
        agent = PredictionReviewAgent()
        await agent.initialize()
        assert agent is not None
        await agent.cleanup()

    @pytest.mark.asyncio
    async def test_prediction_review_run(self) -> None:
        """実行テスト."""
        agent = PredictionReviewAgent()
        await agent.initialize()

        input_data = {
            "predictions": [
                {
                    "id": "pred-1",
                    "statement": "AI市場は2025年Q1に成長する",
                    "target_date": "2025-01-15",
                    "confidence": 0.8,
                    "claim_id": "claim-1",
                    "created_at": "2025-01-01T10:00:00",
                    "status": "pending",
                    "metadata": {},
                }
            ],
            "actual_outcomes": {
                "pred-1": "AI市場は2025年Q1に15%成長した",
            },
        }

        result = await agent.run(input_data)

        assert "reviews" in result
        assert "total_reviewed" in result
        assert "accuracy_stats" in result
        assert result["total_reviewed"] == 1

        await agent.cleanup()