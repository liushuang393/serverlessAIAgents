"""カバレッジダッシュボードサービス.

知識ベースのカバレッジと品質を可視化。

機能:
- 部門/トピック別カバレッジ
- 命中率統計
- ギャップ分析

使用例:
    >>> from apps.faq_system.backend.services import CoverageDashboard
    >>>
    >>> dashboard = CoverageDashboard()
    >>> report = await dashboard.get_coverage_report()
"""

from __future__ import annotations

import logging
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class CoverageLevel(str, Enum):
    """カバレッジレベル."""

    EXCELLENT = "excellent"  # >= 90%
    GOOD = "good"  # >= 70%
    FAIR = "fair"  # >= 50%
    POOR = "poor"  # < 50%


@dataclass
class QueryLog:
    """クエリログ.

    Attributes:
        query_id: クエリID
        query_text: クエリテキスト
        topic: トピック
        department: 部門
        kb_type: KBタイプ
        hit: 命中したか
        hit_count: 命中ドキュメント数
        top_score: 最高スコア
        created_at: 作成日時
    """

    query_id: str
    query_text: str
    topic: str = ""
    department: str = ""
    kb_type: str = ""
    hit: bool = False
    hit_count: int = 0
    top_score: float = 0.0
    response_time_ms: float = 0.0
    created_at: datetime = field(default_factory=datetime.now)
    user_id: str = ""


@dataclass
class CoverageStats:
    """カバレッジ統計.

    Attributes:
        total_queries: 総クエリ数
        hit_queries: 命中クエリ数
        miss_queries: ミスクエリ数
        hit_rate: 命中率
        avg_hit_count: 平均命中数
        avg_top_score: 平均トップスコア
        avg_response_time_ms: 平均応答時間
    """

    total_queries: int = 0
    hit_queries: int = 0
    miss_queries: int = 0
    hit_rate: float = 0.0
    avg_hit_count: float = 0.0
    avg_top_score: float = 0.0
    avg_response_time_ms: float = 0.0
    level: CoverageLevel = CoverageLevel.POOR


@dataclass
class TopicCoverage:
    """トピック別カバレッジ.

    Attributes:
        topic: トピック
        stats: 統計
        sample_misses: ミスサンプル
        recommendations: 推奨事項
    """

    topic: str
    stats: CoverageStats
    sample_misses: list[str] = field(default_factory=list)
    recommendations: list[str] = field(default_factory=list)
    trend: str = "stable"  # improving, stable, declining


@dataclass
class GapAnalysis:
    """ギャップ分析.

    Attributes:
        topic: トピック
        query_count: クエリ数
        hit_rate: 命中率
        severity: 深刻度
        sample_queries: サンプルクエリ
        suggested_content: 提案コンテンツ
    """

    topic: str
    query_count: int
    hit_rate: float
    severity: str  # low, medium, high, critical
    sample_queries: list[str] = field(default_factory=list)
    suggested_content: list[str] = field(default_factory=list)


@dataclass
class CoverageReport:
    """カバレッジレポート.

    Attributes:
        overall: 全体統計
        by_topic: トピック別
        by_department: 部門別
        by_kb_type: KBタイプ別
        gaps: ギャップリスト
        generated_at: 生成日時
    """

    overall: CoverageStats
    by_topic: list[TopicCoverage] = field(default_factory=list)
    by_department: dict[str, CoverageStats] = field(default_factory=dict)
    by_kb_type: dict[str, CoverageStats] = field(default_factory=dict)
    gaps: list[GapAnalysis] = field(default_factory=list)
    trends: dict[str, str] = field(default_factory=dict)
    generated_at: datetime = field(default_factory=datetime.now)
    period_start: datetime | None = None
    period_end: datetime | None = None


@dataclass
class CoverageDashboardConfig:
    """ダッシュボード設定."""

    # 閾値
    excellent_threshold: float = 0.9
    good_threshold: float = 0.7
    fair_threshold: float = 0.5

    # ギャップ分析
    min_queries_for_gap: int = 5
    critical_hit_rate_threshold: float = 0.3

    # トレンド分析
    trend_comparison_days: int = 7


class CoverageDashboard:
    """カバレッジダッシュボード.

    知識ベースのカバレッジを可視化。

    Example:
        >>> dashboard = CoverageDashboard()
        >>>
        >>> # クエリログを記録
        >>> await dashboard.log_query(
        ...     query_id="q-001",
        ...     query_text="年休の付与日数",
        ...     topic="人事",
        ...     hit=True,
        ...     hit_count=3,
        ... )
        >>>
        >>> # レポート生成
        >>> report = await dashboard.get_coverage_report()
    """

    def __init__(
        self,
        config: CoverageDashboardConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or CoverageDashboardConfig()
        self._query_logs: dict[str, QueryLog] = {}
        self._logger = logging.getLogger(__name__)

    async def log_query(
        self,
        query_id: str,
        query_text: str,
        topic: str = "",
        department: str = "",
        kb_type: str = "",
        hit: bool = False,
        hit_count: int = 0,
        top_score: float = 0.0,
        response_time_ms: float = 0.0,
        user_id: str = "",
    ) -> QueryLog:
        """クエリログを記録.

        Args:
            query_id: クエリID
            query_text: クエリテキスト
            topic: トピック
            department: 部門
            kb_type: KBタイプ
            hit: 命中したか
            hit_count: 命中数
            top_score: トップスコア
            response_time_ms: 応答時間
            user_id: ユーザーID

        Returns:
            クエリログ
        """
        log = QueryLog(
            query_id=query_id,
            query_text=query_text,
            topic=topic,
            department=department,
            kb_type=kb_type,
            hit=hit,
            hit_count=hit_count,
            top_score=top_score,
            response_time_ms=response_time_ms,
            user_id=user_id,
        )

        self._query_logs[query_id] = log

        return log

    async def get_coverage_report(
        self,
        since: datetime | None = None,
        until: datetime | None = None,
    ) -> CoverageReport:
        """カバレッジレポートを生成.

        Args:
            since: 開始日時
            until: 終了日時

        Returns:
            カバレッジレポート
        """
        logs = self._get_logs(since, until)

        report = CoverageReport(
            overall=self._calculate_stats(logs),
            period_start=since,
            period_end=until,
        )

        # トピック別
        by_topic = self._group_by_topic(logs)
        for topic, topic_logs in by_topic.items():
            stats = self._calculate_stats(topic_logs)
            sample_misses = [log.query_text for log in topic_logs if not log.hit][:5]

            topic_coverage = TopicCoverage(
                topic=topic,
                stats=stats,
                sample_misses=sample_misses,
                recommendations=self._generate_recommendations(stats),
                trend=self._calculate_trend(topic, logs),
            )
            report.by_topic.append(topic_coverage)

        # トピック別を命中率でソート
        report.by_topic.sort(key=lambda x: x.stats.hit_rate)

        # 部門別
        by_dept = self._group_by_department(logs)
        for dept, dept_logs in by_dept.items():
            report.by_department[dept] = self._calculate_stats(dept_logs)

        # KBタイプ別
        by_kb = self._group_by_kb_type(logs)
        for kb_type, kb_logs in by_kb.items():
            report.by_kb_type[kb_type] = self._calculate_stats(kb_logs)

        # ギャップ分析
        report.gaps = self._analyze_gaps(logs)

        return report

    def _get_logs(
        self,
        since: datetime | None = None,
        until: datetime | None = None,
    ) -> list[QueryLog]:
        """ログを取得."""
        logs = list(self._query_logs.values())

        if since:
            logs = [l for l in logs if l.created_at >= since]
        if until:
            logs = [l for l in logs if l.created_at <= until]

        return logs

    def _calculate_stats(self, logs: list[QueryLog]) -> CoverageStats:
        """統計を計算."""
        if not logs:
            return CoverageStats()

        total = len(logs)
        hit_logs = [l for l in logs if l.hit]
        miss_logs = [l for l in logs if not l.hit]

        stats = CoverageStats(
            total_queries=total,
            hit_queries=len(hit_logs),
            miss_queries=len(miss_logs),
            hit_rate=len(hit_logs) / total,
            avg_hit_count=(sum(l.hit_count for l in hit_logs) / len(hit_logs) if hit_logs else 0),
            avg_top_score=(sum(l.top_score for l in hit_logs) / len(hit_logs) if hit_logs else 0),
            avg_response_time_ms=(sum(l.response_time_ms for l in logs) / total),
        )

        # レベル判定
        if stats.hit_rate >= self._config.excellent_threshold:
            stats.level = CoverageLevel.EXCELLENT
        elif stats.hit_rate >= self._config.good_threshold:
            stats.level = CoverageLevel.GOOD
        elif stats.hit_rate >= self._config.fair_threshold:
            stats.level = CoverageLevel.FAIR
        else:
            stats.level = CoverageLevel.POOR

        return stats

    def _group_by_topic(self, logs: list[QueryLog]) -> dict[str, list[QueryLog]]:
        """トピック別にグループ化."""
        by_topic: dict[str, list[QueryLog]] = defaultdict(list)
        for log in logs:
            topic = log.topic or "未分類"
            by_topic[topic].append(log)
        return dict(by_topic)

    def _group_by_department(self, logs: list[QueryLog]) -> dict[str, list[QueryLog]]:
        """部門別にグループ化."""
        by_dept: dict[str, list[QueryLog]] = defaultdict(list)
        for log in logs:
            dept = log.department or "未分類"
            by_dept[dept].append(log)
        return dict(by_dept)

    def _group_by_kb_type(self, logs: list[QueryLog]) -> dict[str, list[QueryLog]]:
        """KBタイプ別にグループ化."""
        by_kb: dict[str, list[QueryLog]] = defaultdict(list)
        for log in logs:
            kb = log.kb_type or "未分類"
            by_kb[kb].append(log)
        return dict(by_kb)

    def _generate_recommendations(self, stats: CoverageStats) -> list[str]:
        """推奨事項を生成."""
        recommendations = []

        if stats.hit_rate < self._config.critical_hit_rate_threshold:
            recommendations.append("⚠️ 命中率が非常に低いです。コンテンツの追加が急務です。")
        elif stats.hit_rate < self._config.fair_threshold:
            recommendations.append("📝 命中率が低いです。関連ドキュメントの追加をご検討ください。")

        if stats.avg_top_score < 0.5:
            recommendations.append("🎯 検索精度が低いです。術語辞書の充実をご検討ください。")

        if stats.avg_response_time_ms > 2000:
            recommendations.append("⏱️ 応答時間が遅いです。インデックスの最適化をご検討ください。")

        return recommendations

    def _calculate_trend(self, topic: str, logs: list[QueryLog]) -> str:
        """トレンドを計算."""
        now = datetime.now()
        comparison_days = self._config.trend_comparison_days

        # 直近期間
        recent_start = now - timedelta(days=comparison_days)
        recent_logs = [l for l in logs if l.topic == topic and l.created_at >= recent_start]

        # 前期間
        previous_start = recent_start - timedelta(days=comparison_days)
        previous_logs = [
            l for l in logs if l.topic == topic and previous_start <= l.created_at < recent_start
        ]

        if not recent_logs or not previous_logs:
            return "stable"

        recent_rate = len([l for l in recent_logs if l.hit]) / len(recent_logs)
        previous_rate = len([l for l in previous_logs if l.hit]) / len(previous_logs)

        diff = recent_rate - previous_rate
        if diff > 0.05:
            return "improving"
        if diff < -0.05:
            return "declining"
        return "stable"

    def _analyze_gaps(self, logs: list[QueryLog]) -> list[GapAnalysis]:
        """ギャップ分析."""
        gaps = []
        by_topic = self._group_by_topic(logs)

        for topic, topic_logs in by_topic.items():
            if len(topic_logs) < self._config.min_queries_for_gap:
                continue

            hit_rate = len([l for l in topic_logs if l.hit]) / len(topic_logs)

            if hit_rate >= self._config.good_threshold:
                continue

            # 深刻度判定
            if hit_rate < self._config.critical_hit_rate_threshold:
                severity = "critical"
            elif hit_rate < self._config.fair_threshold:
                severity = "high"
            else:
                severity = "medium"

            # サンプルクエリ
            miss_logs = [l for l in topic_logs if not l.hit]
            sample_queries = [l.query_text for l in miss_logs[:5]]

            gap = GapAnalysis(
                topic=topic,
                query_count=len(topic_logs),
                hit_rate=hit_rate,
                severity=severity,
                sample_queries=sample_queries,
                suggested_content=[
                    f"「{topic}」に関するFAQドキュメント",
                    f"「{topic}」の用語辞書エントリー",
                ],
            )
            gaps.append(gap)

        # 深刻度でソート
        severity_order = {"critical": 0, "high": 1, "medium": 2, "low": 3}
        gaps.sort(key=lambda g: severity_order.get(g.severity, 99))

        return gaps

    def get_summary(self) -> dict[str, Any]:
        """サマリーを取得."""
        logs = self._get_logs()

        if not logs:
            return {"status": "no_data"}

        stats = self._calculate_stats(logs)

        return {
            "status": "ok",
            "overall_hit_rate": stats.hit_rate,
            "level": stats.level.value,
            "total_queries": stats.total_queries,
            "topics_count": len(self._group_by_topic(logs)),
            "gaps_count": len(self._analyze_gaps(logs)),
        }


__all__ = [
    "CoverageDashboard",
    "CoverageDashboardConfig",
    "CoverageLevel",
    "CoverageReport",
    "CoverageStats",
    "GapAnalysis",
    "QueryLog",
    "TopicCoverage",
]
