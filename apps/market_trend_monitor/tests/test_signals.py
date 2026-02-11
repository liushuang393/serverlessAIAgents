"""Signal Scoring ユニットテスト.

SignalScore/Signal モデルおよび SignalService のテスト。
"""

from __future__ import annotations

import pytest

from apps.market_trend_monitor.backend.models import (
    SentimentType,
    Signal,
    SignalGrade,
    SignalScore,
    Trend,
)
from apps.market_trend_monitor.backend.services.signal_service import SignalService


# ============================================================
# Model Tests
# ============================================================


class TestSignalScore:
    """SignalScore データモデルのテスト."""

    def test_total_calculation(self) -> None:
        """合計スコア計算テスト."""
        score = SignalScore(
            reliability=0.8,
            leading=0.7,
            relevance=0.9,
            actionability=0.6,
            convergence=0.5,
        )
        assert score.total == pytest.approx(3.5)

    def test_grade_a(self) -> None:
        """グレードA判定テスト (>= 4.0)."""
        score = SignalScore(
            reliability=0.9,
            leading=0.8,
            relevance=0.9,
            actionability=0.8,
            convergence=0.8,
        )
        assert score.grade == SignalGrade.A

    def test_grade_b(self) -> None:
        """グレードB判定テスト (3.0-3.9)."""
        score = SignalScore(
            reliability=0.7,
            leading=0.6,
            relevance=0.7,
            actionability=0.5,
            convergence=0.6,
        )
        assert score.grade == SignalGrade.B

    def test_grade_c(self) -> None:
        """グレードC判定テスト (2.0-2.9)."""
        score = SignalScore(
            reliability=0.5,
            leading=0.5,
            relevance=0.5,
            actionability=0.3,
            convergence=0.2,
        )
        assert score.grade == SignalGrade.C

    def test_grade_d(self) -> None:
        """グレードD判定テスト (< 2.0)."""
        score = SignalScore(
            reliability=0.2,
            leading=0.1,
            relevance=0.3,
            actionability=0.1,
            convergence=0.1,
        )
        assert score.grade == SignalGrade.D

    def test_grade_boundary_a(self) -> None:
        """境界値テスト: 合計 4.0 でグレードA."""
        score = SignalScore(
            reliability=0.8,
            leading=0.8,
            relevance=0.8,
            actionability=0.8,
            convergence=0.8,
        )
        assert score.total == pytest.approx(4.0)
        assert score.grade == SignalGrade.A

    def test_grade_boundary_b(self) -> None:
        """境界値テスト: 合計 3.0 でグレードB."""
        score = SignalScore(
            reliability=0.6,
            leading=0.6,
            relevance=0.6,
            actionability=0.6,
            convergence=0.6,
        )
        assert score.total == pytest.approx(3.0)
        assert score.grade == SignalGrade.B

    def test_all_zeros(self) -> None:
        """全軸0のテスト."""
        score = SignalScore()
        assert score.total == 0.0
        assert score.grade == SignalGrade.D

    def test_to_dict(self) -> None:
        """to_dict 変換テスト."""
        score = SignalScore(reliability=0.5, leading=0.5)
        d = score.to_dict()
        assert "total" in d
        assert "grade" in d
        assert d["reliability"] == 0.5


class TestSignalModel:
    """Signal データモデルのテスト."""

    def test_signal_creation(self) -> None:
        """信号の生成テスト."""
        score = SignalScore(
            reliability=0.9, leading=0.8, relevance=0.9,
            actionability=0.8, convergence=0.8,
        )
        signal = Signal(id="sig-1", trend_id="t-1", score=score)
        assert signal.id == "sig-1"
        assert signal.grade == SignalGrade.A

    def test_signal_grade_delegates_to_score(self) -> None:
        """Signal.grade が score.grade に委譲されるテスト."""
        score = SignalScore(reliability=0.3, leading=0.3)
        signal = Signal(id="sig-1", trend_id="t-1", score=score)
        assert signal.grade == score.grade

    def test_signal_to_dict(self) -> None:
        """to_dict 変換テスト."""
        signal = Signal(
            id="sig-1",
            trend_id="t-1",
            score=SignalScore(reliability=0.7),
        )
        d = signal.to_dict()
        assert d["id"] == "sig-1"
        assert d["trend_id"] == "t-1"
        assert "score" in d
        assert "grade" in d


class TestSignalGrade:
    """SignalGrade Enum のテスト."""

    def test_grade_values(self) -> None:
        """全グレードの値テスト."""
        assert SignalGrade.A.value == "A"
        assert SignalGrade.B.value == "B"
        assert SignalGrade.C.value == "C"
        assert SignalGrade.D.value == "D"


# ============================================================
# Service Tests
# ============================================================


def _make_trend(
    trend_id: str = "t-1",
    topic: str = "COBOL to Java Migration",
    score: float = 0.85,
    growth_rate: float = 0.3,
    sentiment: SentimentType = SentimentType.POSITIVE,
    keywords: list[str] | None = None,
) -> Trend:
    """テスト用 Trend を生成."""
    return Trend(
        id=trend_id,
        topic=topic,
        score=score,
        articles_count=10,
        keywords=keywords or ["COBOL", "Java", "migration"],
        sentiment=sentiment,
        growth_rate=growth_rate,
    )


class TestSignalService:
    """SignalService のテスト."""

    async def test_evaluate_trend(self) -> None:
        """トレンド評価テスト."""
        service = SignalService()
        trend = _make_trend()

        signal = await service.evaluate_trend(
            trend=trend,
            evidence_count=3,
            source_types=["arxiv", "news"],
        )

        assert signal.id is not None
        assert signal.trend_id == "t-1"
        assert signal.score.total > 0.0

    async def test_evaluate_trend_no_sources(self) -> None:
        """情報源なしの評価テスト."""
        service = SignalService()
        trend = _make_trend()

        signal = await service.evaluate_trend(trend=trend, evidence_count=0)

        # Phase 10: raw scores are weighted. reliability raw=0.3 * 5 * weight
        assert signal.score.convergence == pytest.approx(0.0)

    async def test_get_signal(self) -> None:
        """信号取得テスト."""
        service = SignalService()
        trend = _make_trend()
        signal = await service.evaluate_trend(trend=trend, evidence_count=1)

        retrieved = service.get_signal(signal.id)
        assert retrieved is not None
        assert retrieved.id == signal.id

    def test_get_signal_not_found(self) -> None:
        """存在しない信号取得テスト."""
        service = SignalService()
        assert service.get_signal("nonexistent") is None

    async def test_list_signals(self) -> None:
        """信号一覧テスト."""
        service = SignalService()
        t1 = _make_trend(trend_id="t-1", score=0.9, growth_rate=0.5)
        t2 = _make_trend(trend_id="t-2", score=0.3, growth_rate=-0.2)

        await service.evaluate_trend(t1, evidence_count=5, source_types=["arxiv"])
        await service.evaluate_trend(t2, evidence_count=1, source_types=["rss"])

        signals = service.list_signals()
        assert len(signals) == 2
        # スコア降順
        assert signals[0].score.total >= signals[1].score.total

    async def test_list_signals_min_grade_filter(self) -> None:
        """最小グレードフィルタテスト."""
        service = SignalService()
        # 高スコアトレンド
        t_high = _make_trend(
            trend_id="t-high", score=0.95, growth_rate=0.5,
            keywords=["COBOL", "Java", "migration", "AI", "LLM", "modernization"],
        )
        # 低スコアトレンド
        t_low = _make_trend(
            trend_id="t-low", score=0.1, growth_rate=-0.3,
            keywords=["unrelated"],
            sentiment=SentimentType.NEGATIVE,
        )

        await service.evaluate_trend(t_high, evidence_count=5, source_types=["arxiv", "github"])
        await service.evaluate_trend(t_low, evidence_count=0, source_types=[])

        a_signals = service.list_signals(min_grade=SignalGrade.A)
        all_signals = service.list_signals()
        assert len(a_signals) <= len(all_signals)

    def test_dashboard_stats_empty(self) -> None:
        """空のダッシュボード統計テスト."""
        service = SignalService()
        stats = service.get_dashboard_stats()

        assert stats["total_signals"] == 0
        assert stats["average_score"] == 0.0
        assert stats["strong_signals_count"] == 0

    async def test_dashboard_stats(self) -> None:
        """ダッシュボード統計テスト."""
        service = SignalService()
        t1 = _make_trend(trend_id="t-1")
        t2 = _make_trend(trend_id="t-2")
        await service.evaluate_trend(t1, evidence_count=3, source_types=["arxiv"])
        await service.evaluate_trend(t2, evidence_count=2, source_types=["news"])

        stats = service.get_dashboard_stats()
        assert stats["total_signals"] == 2
        assert stats["average_score"] > 0.0
        assert "grade_distribution" in stats

    def test_reliability_calculation(self) -> None:
        """信頼性スコア計算テスト."""
        service = SignalService()
        assert service._calculate_reliability(["arxiv"]) == pytest.approx(0.95)
        assert service._calculate_reliability(["github"]) == pytest.approx(0.85)
        assert service._calculate_reliability(["news"]) == pytest.approx(0.65)
        assert service._calculate_reliability(["rss"]) == pytest.approx(0.50)
        assert service._calculate_reliability([]) == pytest.approx(0.3)

    def test_reliability_mixed_sources(self) -> None:
        """複合ソースの信頼性スコア計算テスト."""
        service = SignalService()
        score = service._calculate_reliability(["arxiv", "rss"])
        expected = (0.95 + 0.50) / 2
        assert score == pytest.approx(expected)

    def test_leading_calculation(self) -> None:
        """先行性スコア計算テスト."""
        service = SignalService()
        # 高成長率 = 高先行性
        assert service._calculate_leading(0.5) == pytest.approx(1.0)
        # 成長率0 = 中程度
        assert service._calculate_leading(0.0) == pytest.approx(0.5)
        # 負の成長率 = 低先行性
        assert service._calculate_leading(-0.5) == pytest.approx(0.0)

    def test_relevance_calculation(self) -> None:
        """関連性スコア計算テスト.

        分母はトレンド側のキーワード数。マッチ率で算出。
        """
        service = SignalService()
        # 全キーワードがターゲットにマッチ → 3/3 = 1.0
        score_full = service._calculate_relevance(["COBOL", "Java", "migration"])
        assert score_full == pytest.approx(1.0)
        # 部分マッチ → 1/2 = 0.5
        score_partial = service._calculate_relevance(["COBOL", "unrelated"])
        assert score_partial == pytest.approx(0.5)
        # 無関係キーワード → max(0/2, 0.1) = 0.1
        score_none = service._calculate_relevance(["unrelated", "xyz"])
        assert score_none == pytest.approx(0.1)
        # 空キーワード → フォールバック 0.2
        assert service._calculate_relevance([]) == pytest.approx(0.2)
        # 単一マッチキーワード → 1/1 = 1.0（実運用の典型パターン）
        score_single = service._calculate_relevance(["COBOL"])
        assert score_single == pytest.approx(1.0)

    def test_actionability_calculation(self) -> None:
        """実行可能性スコア計算テスト."""
        service = SignalService()
        assert service._calculate_actionability("positive") == pytest.approx(0.8)
        assert service._calculate_actionability("neutral") == pytest.approx(0.5)
        assert service._calculate_actionability("negative") == pytest.approx(0.3)

    def test_convergence_calculation(self) -> None:
        """収束性スコア計算テスト."""
        service = SignalService()
        assert service._calculate_convergence(0) == pytest.approx(0.0)
        assert service._calculate_convergence(3) == pytest.approx(0.6)
        assert service._calculate_convergence(5) == pytest.approx(1.0)
        # 5件以上は1.0で飽和
        assert service._calculate_convergence(10) == pytest.approx(1.0)
