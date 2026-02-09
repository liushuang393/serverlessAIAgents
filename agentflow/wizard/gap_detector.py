"""CapabilityGapDetector - 能力缺口検出.

システムに不足している能力を検出し、改善提案を生成します。

検出ソース:
- SkillEngine misses（Skill が見つからなかったケース）
- Agent failures（Agent 実行失敗）
- User feedback（ユーザーフィードバック）

使用例:
    >>> detector = CapabilityGapDetector()
    >>> analysis = await detector.analyze()
    >>> for gap in analysis.priority_gaps:
    ...     print(f"{gap.description}: {gap.suggested_solution}")
"""

from __future__ import annotations

import logging
import uuid
from collections import Counter
from datetime import UTC, datetime, timedelta
from typing import Any

from agentflow.wizard.models import (
    CapabilityGap,
    GapAnalysis,
    GapType,
)


class CapabilityGapDetector:
    """能力缺口検出器.

    システムの能力缺口を検出し、優先度付きの改善提案を生成します。

    検出ソース:
    - SkillEngine misses（マッチしなかった Skill 検索）
    - Agent failures（Agent 実行失敗のログ）
    - User feedback（ユーザーからのフィードバック）
    """

    def __init__(
        self,
        min_frequency: int = 3,
        severity_threshold: float = 0.3,
        lookback_days: int = 30,
    ) -> None:
        """初期化.

        Args:
            min_frequency: 缺口として認識する最小発生頻度
            severity_threshold: 優先缺口の重大度閾値
            lookback_days: 分析対象期間（日数）
        """
        self._min_frequency = min_frequency
        self._severity_threshold = severity_threshold
        self._lookback_days = lookback_days
        self._logger = logging.getLogger(__name__)

        # 検出ログ（インメモリ）
        self._skill_misses: list[dict[str, Any]] = []
        self._agent_failures: list[dict[str, Any]] = []
        self._user_feedback: list[dict[str, Any]] = []

    def record_skill_miss(
        self,
        query: str,
        context: dict[str, Any] | None = None,
    ) -> None:
        """Skill ミスを記録.

        Args:
            query: 検索クエリ
            context: コンテキスト情報
        """
        self._skill_misses.append({
            "query": query,
            "context": context or {},
            "timestamp": datetime.now(UTC),
        })
        self._logger.debug(f"Recorded skill miss: {query}")

    def record_agent_failure(
        self,
        agent_name: str,
        error: str,
        input_data: dict[str, Any] | None = None,
        context: dict[str, Any] | None = None,
    ) -> None:
        """Agent 失敗を記録.

        Args:
            agent_name: Agent 名
            error: エラーメッセージ
            input_data: 入力データ
            context: コンテキスト情報
        """
        self._agent_failures.append({
            "agent_name": agent_name,
            "error": error,
            "input_data": input_data or {},
            "context": context or {},
            "timestamp": datetime.now(UTC),
        })
        self._logger.debug(f"Recorded agent failure: {agent_name} - {error}")

    def record_user_feedback(
        self,
        feedback_type: str,
        message: str,
        related_feature: str | None = None,
        severity: float = 0.5,
    ) -> None:
        """ユーザーフィードバックを記録.

        Args:
            feedback_type: フィードバックタイプ（bug, feature_request, complaint）
            message: フィードバックメッセージ
            related_feature: 関連機能
            severity: 重大度
        """
        self._user_feedback.append({
            "type": feedback_type,
            "message": message,
            "related_feature": related_feature,
            "severity": severity,
            "timestamp": datetime.now(UTC),
        })
        self._logger.debug(f"Recorded user feedback: {feedback_type}")

    async def analyze(self) -> GapAnalysis:
        """能力缺口を分析.

        Returns:
            缺口分析結果
        """
        gaps: list[CapabilityGap] = []
        cutoff_time = datetime.now(UTC) - timedelta(days=self._lookback_days)

        # 1. Skill ミスを分析
        skill_gaps = await self._analyze_skill_misses(cutoff_time)
        gaps.extend(skill_gaps)

        # 2. Agent 失敗を分析
        agent_gaps = await self._analyze_agent_failures(cutoff_time)
        gaps.extend(agent_gaps)

        # 3. ユーザーフィードバックを分析
        feedback_gaps = await self._analyze_user_feedback(cutoff_time)
        gaps.extend(feedback_gaps)

        # 重複を統合
        merged_gaps = self._merge_similar_gaps(gaps)

        # 優先度でソート
        sorted_gaps = sorted(
            merged_gaps,
            key=lambda g: (g.severity, g.frequency),
            reverse=True,
        )

        # 優先缺口を抽出
        priority_gaps = [
            g for g in sorted_gaps
            if g.severity >= self._severity_threshold
        ]

        # 推奨アクションを生成
        recommendations = self._generate_recommendations(priority_gaps)

        return GapAnalysis(
            gaps=sorted_gaps,
            total_count=len(sorted_gaps),
            auto_fixable_count=sum(1 for g in sorted_gaps if g.auto_fixable),
            priority_gaps=priority_gaps[:10],
            recommendations=recommendations,
        )

    async def _analyze_skill_misses(
        self,
        cutoff_time: datetime,
    ) -> list[CapabilityGap]:
        """Skill ミスを分析.

        Args:
            cutoff_time: 分析対象の最古日時

        Returns:
            検出された缺口リスト
        """
        gaps = []

        # 期間内のミスをフィルタ
        recent_misses = [
            m for m in self._skill_misses
            if m["timestamp"] >= cutoff_time
        ]

        # クエリごとの頻度をカウント
        query_counts = Counter(m["query"] for m in recent_misses)

        for query, count in query_counts.items():
            if count >= self._min_frequency:
                gaps.append(CapabilityGap(
                    gap_id=f"skill_{uuid.uuid4().hex[:8]}",
                    gap_type=GapType.SKILL,
                    description=f"Skill not found for: {query}",
                    frequency=count,
                    severity=min(count / 10, 1.0),
                    suggested_solution=f"Create a new Skill for '{query}'",
                    auto_fixable=True,
                    source="skill_engine",
                ))

        return gaps

    async def _analyze_agent_failures(
        self,
        cutoff_time: datetime,
    ) -> list[CapabilityGap]:
        """Agent 失敗を分析.

        Args:
            cutoff_time: 分析対象の最古日時

        Returns:
            検出された缺口リスト
        """
        gaps = []

        # 期間内の失敗をフィルタ
        recent_failures = [
            f for f in self._agent_failures
            if f["timestamp"] >= cutoff_time
        ]

        # Agent ごとの失敗をグループ化
        agent_failures: dict[str, list[dict[str, Any]]] = {}
        for failure in recent_failures:
            agent_name = failure["agent_name"]
            if agent_name not in agent_failures:
                agent_failures[agent_name] = []
            agent_failures[agent_name].append(failure)

        for agent_name, failures in agent_failures.items():
            if len(failures) >= self._min_frequency:
                # エラーパターンを分析
                error_patterns = Counter(f["error"][:50] for f in failures)
                most_common_error = error_patterns.most_common(1)[0][0] if error_patterns else "Unknown"

                gaps.append(CapabilityGap(
                    gap_id=f"agent_{uuid.uuid4().hex[:8]}",
                    gap_type=GapType.AGENT,
                    description=f"Agent '{agent_name}' failures: {most_common_error}",
                    frequency=len(failures),
                    severity=min(len(failures) / 20, 1.0),
                    suggested_solution=f"Improve error handling in '{agent_name}' or add fallback logic",
                    auto_fixable=False,
                    source="agent_execution",
                    metadata={"error_patterns": dict(error_patterns)},
                ))

        return gaps

    async def _analyze_user_feedback(
        self,
        cutoff_time: datetime,
    ) -> list[CapabilityGap]:
        """ユーザーフィードバックを分析.

        Args:
            cutoff_time: 分析対象の最古日時

        Returns:
            検出された缺口リスト
        """
        gaps = []

        # 期間内のフィードバックをフィルタ
        recent_feedback = [
            f for f in self._user_feedback
            if f["timestamp"] >= cutoff_time
        ]

        # 機能ごとにグループ化
        feature_feedback: dict[str, list[dict[str, Any]]] = {}
        for feedback in recent_feedback:
            feature = feedback.get("related_feature", "general")
            if feature not in feature_feedback:
                feature_feedback[feature] = []
            feature_feedback[feature].append(feedback)

        for feature, feedbacks in feature_feedback.items():
            if len(feedbacks) >= self._min_frequency:
                # 平均重大度を計算
                avg_severity = sum(f["severity"] for f in feedbacks) / len(feedbacks)

                # フィードバックタイプを分析
                types = Counter(f["type"] for f in feedbacks)
                primary_type = types.most_common(1)[0][0] if types else "general"

                gap_type = {
                    "bug": GapType.AGENT,
                    "feature_request": GapType.SKILL,
                    "complaint": GapType.PATTERN,
                }.get(primary_type, GapType.KNOWLEDGE)

                gaps.append(CapabilityGap(
                    gap_id=f"feedback_{uuid.uuid4().hex[:8]}",
                    gap_type=gap_type,
                    description=f"User feedback for '{feature}': {primary_type}",
                    frequency=len(feedbacks),
                    severity=avg_severity,
                    suggested_solution=f"Review and address user feedback for '{feature}'",
                    auto_fixable=False,
                    source="user_feedback",
                    metadata={"feedback_types": dict(types)},
                ))

        return gaps

    def _merge_similar_gaps(
        self,
        gaps: list[CapabilityGap],
    ) -> list[CapabilityGap]:
        """類似した缺口を統合.

        Args:
            gaps: 缺口リスト

        Returns:
            統合された缺口リスト
        """
        if not gaps:
            return []

        # 簡易的な統合（同じ gap_type で description が類似）
        merged: list[CapabilityGap] = []
        seen_descriptions: set[str] = set()

        for gap in gaps:
            # 正規化した説明
            normalized = gap.description.lower()[:30]

            if normalized not in seen_descriptions:
                merged.append(gap)
                seen_descriptions.add(normalized)
            else:
                # 既存の缺口の頻度を増加
                for existing in merged:
                    if existing.description.lower()[:30] == normalized:
                        existing.frequency += gap.frequency
                        existing.severity = max(existing.severity, gap.severity)
                        break

        return merged

    def _generate_recommendations(
        self,
        priority_gaps: list[CapabilityGap],
    ) -> list[str]:
        """推奨アクションを生成.

        Args:
            priority_gaps: 優先缺口リスト

        Returns:
            推奨アクションリスト
        """
        recommendations = []

        # 自動修正可能な缺口
        auto_fixable = [g for g in priority_gaps if g.auto_fixable]
        if auto_fixable:
            recommendations.append(
                f"{len(auto_fixable)} gaps can be auto-fixed. "
                f"Run SystemSynthesizer to generate solutions."
            )

        # Skill 缺口
        skill_gaps = [g for g in priority_gaps if g.gap_type == GapType.SKILL]
        if skill_gaps:
            recommendations.append(
                f"{len(skill_gaps)} Skill gaps detected. "
                f"Consider using SkillForge to create new Skills."
            )

        # Agent 缺口
        agent_gaps = [g for g in priority_gaps if g.gap_type == GapType.AGENT]
        if agent_gaps:
            recommendations.append(
                f"{len(agent_gaps)} Agent issues detected. "
                f"Review Agent implementations and add error handling."
            )

        # 高頻度缺口
        high_freq = [g for g in priority_gaps if g.frequency >= 10]
        if high_freq:
            recommendations.append(
                f"{len(high_freq)} high-frequency gaps need immediate attention."
            )

        return recommendations

    def clear_logs(self) -> None:
        """ログをクリア."""
        self._skill_misses.clear()
        self._agent_failures.clear()
        self._user_feedback.clear()
        self._logger.info("Gap detector logs cleared")

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        return {
            "skill_misses": len(self._skill_misses),
            "agent_failures": len(self._agent_failures),
            "user_feedback": len(self._user_feedback),
            "lookback_days": self._lookback_days,
            "min_frequency": self._min_frequency,
        }


__all__ = ["CapabilityGapDetector"]
