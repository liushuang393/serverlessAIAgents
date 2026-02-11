"""Signal Scoring サービス.

信号評価の管理を行うビジネスロジック層。
5軸評価体系に基づく統一的な信号強度評価を提供します。
"""

import asyncio
import logging
import uuid
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.db import init_db
from apps.market_trend_monitor.backend.db.models import SignalModel
from apps.market_trend_monitor.backend.db.session import async_session
from apps.market_trend_monitor.backend.models import (
    Signal,
    SignalGrade,
    SignalScore,
    Trend,
)
from sqlalchemy import select
from sqlalchemy.exc import IntegrityError


class SignalService:
    """信号評価サービス.

    5軸評価体系に基づく統一的な信号強度評価を提供します。

    評価軸:
    - 信頼性 (Reliability): 情報源の信頼度
    - 先行性 (Leading): 市場先行度
    - 関連性 (Relevance): ドメイン関連度
    - 実行可能性 (Actionability): 行動可能性
    - 収束性 (Convergence): 多ソース一致度

    Phase 9: AdaptiveScoringServiceによる重みの動的調整をサポート。
    Phase 12: BayesianConfidenceServiceとの統合。
    """

    # デフォルト重み（AdaptiveScoringService未初期化時のフォールバック）
    DEFAULT_WEIGHTS: dict[str, float] = {
        "reliability": 0.2,
        "leading": 0.2,
        "relevance": 0.2,
        "actionability": 0.2,
        "convergence": 0.2,
    }

    def __init__(
        self,
        *,
        adaptive_scoring_service: Any | None = None,
        bayesian_confidence_service: Any | None = None,
        session_factory: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            adaptive_scoring_service: 適応的スコアリングサービス（Phase 9）
            bayesian_confidence_service: ベイズ信頼度サービス（Phase 12）
            session_factory: DB セッションファクトリ
        """
        self._logger = logging.getLogger(self.__class__.__name__)
        # 起動中の高速参照用キャッシュ
        self._signals: dict[str, Signal] = {}
        # Phase 9: 適応的スコアリング
        self._adaptive_scoring = adaptive_scoring_service
        # Phase 12: ベイズ信頼度
        self._bayesian_confidence = bayesian_confidence_service
        self._session_factory = session_factory or async_session
        self._loaded_from_db = False
        self._persist_lock = asyncio.Lock()
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

    async def initialize(self) -> None:
        """永続層から信号データをロード."""
        if self._loaded_from_db:
            return
        await init_db()

        async with self._session_factory() as session:
            rows = (await session.execute(select(SignalModel))).scalars().all()

        signals: dict[str, Signal] = {}
        for row in rows:
            signals[row.id] = Signal(
                id=row.id,
                trend_id=row.trend_id,
                score=SignalScore(
                    reliability=float(row.reliability),
                    leading=float(row.leading),
                    relevance=float(row.relevance),
                    actionability=float(row.actionability),
                    convergence=float(row.convergence),
                ),
                evaluated_at=row.evaluated_at,
                metadata=dict(row.metadata_json or {}),
            )

        self._signals = signals
        self._loaded_from_db = True
        self._logger.info("SignalService 初期化: signals=%s", len(signals))

    async def _get_weights(self) -> dict[str, float]:
        """Phase 9: 現在の重みを取得.

        AdaptiveScoringServiceが利用可能な場合はそこから取得、
        そうでない場合はデフォルト重みを返す（後方互換性）。
        """
        if self._adaptive_scoring:
            try:
                scoring_weights = await self._adaptive_scoring.get_current_weights()
                return scoring_weights.as_weight_dict()
            except Exception as e:
                self._logger.warning("適応的重み取得失敗、デフォルトを使用: %s", e)
        return dict(self.DEFAULT_WEIGHTS)

    async def evaluate_trend(
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

        # Phase 10: 適応的重みを取得して適用
        weights = await self._get_weights()

        # 各軸の生スコア(0-1)を計算
        raw_scores = {
            "reliability": self._calculate_reliability(source_types),
            "leading": self._calculate_leading(trend.growth_rate),
            "relevance": self._calculate_relevance(trend.keywords),
            "actionability": self._calculate_actionability(trend.sentiment.value),
            "convergence": self._calculate_convergence(evidence_count),
        }

        # 重み適用: 各軸 = raw_score * 5 * weight → total範囲 0-5 維持
        score = SignalScore(
            reliability=raw_scores["reliability"] * 5 * weights["reliability"],
            leading=raw_scores["leading"] * 5 * weights["leading"],
            relevance=raw_scores["relevance"] * 5 * weights["relevance"],
            actionability=raw_scores["actionability"] * 5 * weights["actionability"],
            convergence=raw_scores["convergence"] * 5 * weights["convergence"],
        )

        # Phase 12: ベイズ信頼度情報をメタデータに含める
        bayesian_info = {}
        if self._bayesian_confidence:
            try:
                claim_id = trend.metadata.get("claim_id", f"trend-{trend.id}")
                mean, std = await self._bayesian_confidence.get_confidence_with_uncertainty(
                    claim_id,
                )
                bayesian_info = {
                    "bayesian_mean": round(mean, 4),
                    "bayesian_std": round(std, 4),
                }
            except Exception as e:
                self._logger.debug("ベイズ信頼度取得失敗: %s", e)

        signal = Signal(
            id=str(uuid.uuid4()),
            trend_id=trend.id,
            score=score,
            evaluated_at=datetime.now(),
            metadata={
                "trend_topic": trend.topic,
                "evidence_count": evidence_count,
                "raw_scores": raw_scores,
                "weights": weights,
                **bayesian_info,
            },
        )

        self._signals[signal.id] = signal
        self._logger.info(
            f"Signal evaluated: {signal.id}, "
            f"grade={signal.grade.value}, total={score.total:.2f}"
        )
        await self.persist_signal(signal)
        return signal

    async def persist_signal(self, signal: Signal) -> None:
        """信号を永続化（upsert）."""
        async with self._persist_lock:
            await init_db()
            async with self._session_factory() as session:
                try:
                    row = await session.get(SignalModel, signal.id)
                    if row is None:
                        row = SignalModel(id=signal.id)
                        session.add(row)

                    self._apply_signal_row(row, signal)
                    await session.commit()
                    return
                except IntegrityError as exc:
                    await session.rollback()
                    self._logger.warning(
                        "信号の同時保存競合を解消: signal_id=%s",
                        signal.id,
                    )
                    row = await session.get(SignalModel, signal.id)
                    if row is None:
                        raise exc

                    self._apply_signal_row(row, signal)
                    await session.commit()

    async def persist_signals_by_ids(self, signal_ids: list[str]) -> int:
        """ID一覧で信号を永続化."""
        persisted = 0
        for signal_id in signal_ids:
            signal = self._signals.get(signal_id)
            if not signal:
                continue
            await self.persist_signal(signal)
            persisted += 1
        return persisted

    @staticmethod
    def _apply_signal_row(row: SignalModel, signal: Signal) -> None:
        """SignalModel へ Signal の値を反映."""
        row.trend_id = signal.trend_id
        row.reliability = float(signal.score.reliability)
        row.leading = float(signal.score.leading)
        row.relevance = float(signal.score.relevance)
        row.actionability = float(signal.score.actionability)
        row.convergence = float(signal.score.convergence)
        row.total = float(signal.score.total)
        row.grade = signal.grade.value
        row.evaluated_at = signal.evaluated_at
        row.metadata_json = dict(signal.metadata or {})

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
        """キーワードに基づく関連性スコアを計算.

        トレンドのキーワードのうち、ターゲットキーワードにマッチする割合を算出。
        分母はトレンド側のキーワード数（ターゲット数ではない）。
        """
        if not keywords:
            return 0.2

        # トレンドキーワードのうちターゲットにマッチする数
        matches = sum(
            1 for kw in keywords
            if any(
                target.lower() in kw.lower()
                for target in self._target_keywords
            )
        )

        return max(matches / len(keywords), 0.1)

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
