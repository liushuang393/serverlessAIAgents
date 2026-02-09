"""Signal Scoring サービス.

信号評価の管理を行うビジネスロジック層。
5軸評価体系に基づく統一的な信号強度評価を提供します。
"""

import logging
import uuid
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.models import (
    Signal,
    SignalGrade,
    SignalScore,
    Trend,
)


class SignalService:
    """信号評価サービス.

    5軸評価体系に基づく統一的な信号強度評価を提供します。

    評価軸:
    - 信頼性 (Reliability): 情報源の信頼度
    - 先行性 (Leading): 市場先行度
    - 関連性 (Relevance): ドメイン関連度
    - 実行可能性 (Actionability): 行動可能性
    - 収束性 (Convergence): 多ソース一致度
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        # インメモリストレージ（将来的にDB化）
        self._signals: dict[str, Signal] = {}
        # 対象キーワード（関連性評価用）
        self._target_keywords: list[str] = [
            "COBOL",
            "Java",
            "migration",
            "AI",
            "LLM",
            "modernization",
            "legacy",
        ]

    def evaluate_trend(
        self,
        trend: Trend,
        evidence_count: int = 0,
        source_types: list[str] | None = None,
    ) -> Signal:
        """トレンドに対する信号評価を実行.

        Args:
            trend: 評価対象トレンド
            evidence_count: 関連証拠数
            source_types: 情報源タイプリスト

        Returns:
            評価済み信号
        """
        source_types = source_types or []

        # 5軸スコアを計算
        score = SignalScore(
            reliability=self._calculate_reliability(source_types),
            leading=self._calculate_leading(trend.growth_rate),
            relevance=self._calculate_relevance(trend.keywords),
            actionability=self._calculate_actionability(trend.sentiment.value),
            convergence=self._calculate_convergence(evidence_count),
        )

        signal = Signal(
            id=str(uuid.uuid4()),
            trend_id=trend.id,
            score=score,
            evaluated_at=datetime.now(),
            metadata={
                "trend_topic": trend.topic,
                "evidence_count": evidence_count,
            },
        )

        self._signals[signal.id] = signal
        self._logger.info(
            f"Signal evaluated: {signal.id}, "
            f"grade={signal.grade.value}, total={score.total:.2f}"
        )
        return signal

    def get_signal(self, signal_id: str) -> Signal | None:
        """信号を取得."""
        return self._signals.get(signal_id)

    def list_signals(
        self,
        min_grade: SignalGrade | None = None,
    ) -> list[Signal]:
        """信号一覧を取得.

        Args:
            min_grade: 最小グレード

        Returns:
            信号リスト
        """
        signals = list(self._signals.values())

        if min_grade:
            grade_order = {"A": 4, "B": 3, "C": 2, "D": 1}
            min_order = grade_order.get(min_grade.value, 0)
            signals = [
                s for s in signals
                if grade_order.get(s.grade.value, 0) >= min_order
            ]

        return sorted(
            signals, key=lambda s: s.score.total, reverse=True
        )

    def get_dashboard_stats(self) -> dict[str, Any]:
        """ダッシュボード用統計を取得."""
        signals = list(self._signals.values())

        grade_counts = {"A": 0, "B": 0, "C": 0, "D": 0}
        for signal in signals:
            grade_counts[signal.grade.value] += 1

        avg_score = 0.0
        if signals:
            avg_score = sum(s.score.total for s in signals) / len(signals)

        return {
            "total_signals": len(signals),
            "grade_distribution": grade_counts,
            "average_score": avg_score,
            "strong_signals_count": grade_counts["A"] + grade_counts["B"],
        }

    def _calculate_reliability(self, source_types: list[str]) -> float:
        """情報源に基づく信頼性スコアを計算."""
        if not source_types:
            return 0.3

        source_scores = {
            "arxiv": 0.95,
            "github": 0.85,
            "news": 0.65,
            "rss": 0.50,
        }

        scores = [source_scores.get(s.lower(), 0.5) for s in source_types]
        return sum(scores) / len(scores)

    def _calculate_leading(self, growth_rate: float) -> float:
        """成長率に基づく先行性スコアを計算.

        高い成長率 = 新興トレンド = 高い先行性
        """
        # 成長率を0-1に正規化（-0.5〜+0.5を想定）
        normalized = (growth_rate + 0.5) / 1.0
        return max(0.0, min(1.0, normalized))

    def _calculate_relevance(self, keywords: list[str]) -> float:
        """キーワードに基づく関連性スコアを計算."""
        if not keywords:
            return 0.2

        # ターゲットキーワードとの一致率
        matches = sum(
            1 for kw in keywords
            if any(
                target.lower() in kw.lower()
                for target in self._target_keywords
            )
        )

        return min(matches / max(len(self._target_keywords), 1), 1.0)

    def _calculate_actionability(self, sentiment: str) -> float:
        """センチメントに基づく実行可能性スコアを計算.

        positive = 機会 = 高い実行可能性
        """
        sentiment_scores = {
            "positive": 0.8,
            "neutral": 0.5,
            "negative": 0.3,
        }
        return sentiment_scores.get(sentiment.lower(), 0.5)

    def _calculate_convergence(self, evidence_count: int) -> float:
        """証拠数に基づく収束性スコアを計算.

        多くの証拠 = 多ソース裏付け = 高い収束性
        """
        # 5件以上で最大スコア
        return min(evidence_count / 5.0, 1.0)
