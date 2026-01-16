# -*- coding: utf-8 -*-
"""フィードバックサービス.

ユーザーからのフィードバック収集と分析。

機能:
- 有用/無用フィードバック収集
- 待補充点の自動生成
- フィードバック分析

使用例:
    >>> from apps.faq_system.backend.services import FeedbackService
    >>>
    >>> service = FeedbackService()
    >>> await service.submit_feedback(
    ...     query_id="q-001",
    ...     helpful=True,
    ...     comment="わかりやすかった",
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any
from collections import defaultdict

logger = logging.getLogger(__name__)


class FeedbackType(str, Enum):
    """フィードバックタイプ."""

    HELPFUL = "helpful"  # 有用
    NOT_HELPFUL = "not_helpful"  # 無用
    PARTIAL = "partial"  # 部分的に有用
    INCORRECT = "incorrect"  # 不正確
    OUTDATED = "outdated"  # 古い情報
    MISSING = "missing"  # 情報不足


class FeedbackStatus(str, Enum):
    """フィードバックステータス."""

    NEW = "new"
    REVIEWED = "reviewed"
    ACTIONED = "actioned"
    DISMISSED = "dismissed"


@dataclass
class Feedback:
    """フィードバック.

    Attributes:
        feedback_id: フィードバックID
        query_id: クエリID
        user_id: ユーザーID
        feedback_type: フィードバックタイプ
        helpful: 有用か
        comment: コメント
        tags: タグ
        status: ステータス
        created_at: 作成日時
    """

    feedback_id: str
    query_id: str
    user_id: str = ""
    feedback_type: FeedbackType = FeedbackType.HELPFUL
    helpful: bool = True
    comment: str = ""
    tags: list[str] = field(default_factory=list)
    status: FeedbackStatus = FeedbackStatus.NEW
    created_at: datetime = field(default_factory=datetime.now)
    query_text: str = ""
    answer_text: str = ""
    kb_type: str = ""
    topic: str = ""

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "feedback_id": self.feedback_id,
            "query_id": self.query_id,
            "user_id": self.user_id,
            "feedback_type": self.feedback_type.value,
            "helpful": self.helpful,
            "comment": self.comment,
            "tags": self.tags,
            "status": self.status.value,
            "created_at": self.created_at.isoformat(),
            "topic": self.topic,
        }


@dataclass
class ImprovementSuggestion:
    """改善提案.

    Attributes:
        suggestion_id: 提案ID
        topic: トピック
        suggestion_type: 提案タイプ
        description: 説明
        priority: 優先度
        related_feedbacks: 関連フィードバック
    """

    suggestion_id: str
    topic: str
    suggestion_type: str  # add_content, update_content, improve_accuracy
    description: str
    priority: str = "medium"  # low, medium, high
    related_feedbacks: list[str] = field(default_factory=list)
    sample_queries: list[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)


@dataclass
class FeedbackStats:
    """フィードバック統計.

    Attributes:
        total: 総数
        helpful_count: 有用数
        not_helpful_count: 無用数
        helpful_rate: 有用率
        by_topic: トピック別
        by_kb_type: KBタイプ別
    """

    total: int = 0
    helpful_count: int = 0
    not_helpful_count: int = 0
    helpful_rate: float = 0.0
    by_topic: dict[str, dict[str, int]] = field(default_factory=dict)
    by_kb_type: dict[str, dict[str, int]] = field(default_factory=dict)
    period_start: datetime | None = None
    period_end: datetime | None = None


@dataclass
class FeedbackServiceConfig:
    """フィードバックサービス設定."""

    # 自動分析
    auto_suggest_threshold: int = 5  # 提案生成の最小フィードバック数
    low_helpful_rate_threshold: float = 0.5  # 低有用率の閾値

    # 保持期間
    retention_days: int = 365


class FeedbackService:
    """フィードバックサービス.

    ユーザーフィードバックの収集と分析。

    Example:
        >>> service = FeedbackService()
        >>>
        >>> # フィードバック送信
        >>> await service.submit_feedback(
        ...     query_id="q-001",
        ...     helpful=True,
        ... )
        >>>
        >>> # 統計取得
        >>> stats = service.get_stats()
    """

    def __init__(
        self,
        config: FeedbackServiceConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or FeedbackServiceConfig()
        self._feedbacks: dict[str, Feedback] = {}
        self._suggestions: dict[str, ImprovementSuggestion] = {}
        self._counter = 0
        self._logger = logging.getLogger(__name__)

    async def submit_feedback(
        self,
        query_id: str,
        helpful: bool,
        user_id: str = "",
        feedback_type: FeedbackType | None = None,
        comment: str = "",
        tags: list[str] | None = None,
        query_text: str = "",
        answer_text: str = "",
        kb_type: str = "",
        topic: str = "",
    ) -> Feedback:
        """フィードバックを送信.

        Args:
            query_id: クエリID
            helpful: 有用か
            user_id: ユーザーID
            feedback_type: フィードバックタイプ
            comment: コメント
            tags: タグ
            query_text: クエリテキスト
            answer_text: 回答テキスト
            kb_type: KBタイプ
            topic: トピック

        Returns:
            フィードバック
        """
        self._counter += 1
        feedback_id = f"fb-{self._counter:06d}"

        if feedback_type is None:
            feedback_type = FeedbackType.HELPFUL if helpful else FeedbackType.NOT_HELPFUL

        feedback = Feedback(
            feedback_id=feedback_id,
            query_id=query_id,
            user_id=user_id,
            feedback_type=feedback_type,
            helpful=helpful,
            comment=comment,
            tags=tags or [],
            query_text=query_text,
            answer_text=answer_text,
            kb_type=kb_type,
            topic=topic,
        )

        self._feedbacks[feedback_id] = feedback

        self._logger.info(
            "Feedback submitted: %s, helpful=%s, topic=%s",
            feedback_id, helpful, topic,
        )

        # 自動提案チェック
        await self._check_auto_suggestions(topic)

        return feedback

    async def _check_auto_suggestions(self, topic: str) -> None:
        """自動提案をチェック.

        Args:
            topic: トピック
        """
        if not topic:
            return

        # トピック別のフィードバックを集計
        topic_feedbacks = [
            f for f in self._feedbacks.values() if f.topic == topic
        ]

        if len(topic_feedbacks) < self._config.auto_suggest_threshold:
            return

        # 有用率を計算
        helpful_count = len([f for f in topic_feedbacks if f.helpful])
        helpful_rate = helpful_count / len(topic_feedbacks)

        # 低有用率の場合は提案を生成
        if helpful_rate < self._config.low_helpful_rate_threshold:
            await self._generate_suggestion(topic, topic_feedbacks, helpful_rate)

    async def _generate_suggestion(
        self,
        topic: str,
        feedbacks: list[Feedback],
        helpful_rate: float,
    ) -> None:
        """改善提案を生成.

        Args:
            topic: トピック
            feedbacks: フィードバックリスト
            helpful_rate: 有用率
        """
        # 既存の提案がないか確認
        existing = [
            s for s in self._suggestions.values()
            if s.topic == topic and s.created_at > datetime.now() - timedelta(days=7)
        ]
        if existing:
            return

        suggestion_id = f"sug-{len(self._suggestions) + 1:04d}"

        # 無用フィードバックのサンプルクエリ
        not_helpful = [f for f in feedbacks if not f.helpful]
        sample_queries = [f.query_text for f in not_helpful[:5] if f.query_text]

        # コメントから問題を分析
        comments = [f.comment for f in not_helpful if f.comment]
        suggestion_type = self._analyze_issue_type(comments)

        suggestion = ImprovementSuggestion(
            suggestion_id=suggestion_id,
            topic=topic,
            suggestion_type=suggestion_type,
            description=f"トピック「{topic}」の有用率が{helpful_rate:.0%}と低いです。"
            f"関連コンテンツの追加・更新をご検討ください。",
            priority="high" if helpful_rate < 0.3 else "medium",
            related_feedbacks=[f.feedback_id for f in not_helpful],
            sample_queries=sample_queries,
        )

        self._suggestions[suggestion_id] = suggestion

        self._logger.info(
            "Generated improvement suggestion: %s for topic %s",
            suggestion_id, topic,
        )

    def _analyze_issue_type(self, comments: list[str]) -> str:
        """問題タイプを分析.

        Args:
            comments: コメントリスト

        Returns:
            問題タイプ
        """
        all_comments = " ".join(comments).lower()

        if "古い" in all_comments or "更新" in all_comments:
            return "update_content"
        if "足りない" in all_comments or "不足" in all_comments:
            return "add_content"
        if "間違い" in all_comments or "不正確" in all_comments:
            return "improve_accuracy"

        return "add_content"

    def get_feedback(self, feedback_id: str) -> Feedback | None:
        """フィードバックを取得.

        Args:
            feedback_id: フィードバックID

        Returns:
            フィードバック、または None
        """
        return self._feedbacks.get(feedback_id)

    def get_feedbacks(
        self,
        query_id: str | None = None,
        user_id: str | None = None,
        helpful: bool | None = None,
        topic: str | None = None,
        since: datetime | None = None,
        until: datetime | None = None,
        limit: int = 100,
    ) -> list[Feedback]:
        """フィードバックを取得.

        Args:
            query_id: クエリID
            user_id: ユーザーID
            helpful: 有用か
            topic: トピック
            since: 開始日時
            until: 終了日時
            limit: 取得件数

        Returns:
            フィードバックリスト
        """
        results = list(self._feedbacks.values())

        if query_id:
            results = [f for f in results if f.query_id == query_id]
        if user_id:
            results = [f for f in results if f.user_id == user_id]
        if helpful is not None:
            results = [f for f in results if f.helpful == helpful]
        if topic:
            results = [f for f in results if f.topic == topic]
        if since:
            results = [f for f in results if f.created_at >= since]
        if until:
            results = [f for f in results if f.created_at <= until]

        # 日時降順
        results.sort(key=lambda f: f.created_at, reverse=True)

        return results[:limit]

    def get_stats(
        self,
        since: datetime | None = None,
        until: datetime | None = None,
    ) -> FeedbackStats:
        """統計を取得.

        Args:
            since: 開始日時
            until: 終了日時

        Returns:
            統計
        """
        feedbacks = self.get_feedbacks(
            since=since, until=until, limit=10000
        )

        stats = FeedbackStats(
            total=len(feedbacks),
            helpful_count=len([f for f in feedbacks if f.helpful]),
            not_helpful_count=len([f for f in feedbacks if not f.helpful]),
            period_start=since,
            period_end=until,
        )

        if stats.total > 0:
            stats.helpful_rate = stats.helpful_count / stats.total

        # トピック別
        by_topic: dict[str, dict[str, int]] = defaultdict(lambda: {"helpful": 0, "not_helpful": 0})
        for f in feedbacks:
            if f.topic:
                if f.helpful:
                    by_topic[f.topic]["helpful"] += 1
                else:
                    by_topic[f.topic]["not_helpful"] += 1
        stats.by_topic = dict(by_topic)

        # KBタイプ別
        by_kb: dict[str, dict[str, int]] = defaultdict(lambda: {"helpful": 0, "not_helpful": 0})
        for f in feedbacks:
            if f.kb_type:
                if f.helpful:
                    by_kb[f.kb_type]["helpful"] += 1
                else:
                    by_kb[f.kb_type]["not_helpful"] += 1
        stats.by_kb_type = dict(by_kb)

        return stats

    def get_suggestions(
        self,
        topic: str | None = None,
        priority: str | None = None,
    ) -> list[ImprovementSuggestion]:
        """改善提案を取得.

        Args:
            topic: トピック
            priority: 優先度

        Returns:
            提案リスト
        """
        results = list(self._suggestions.values())

        if topic:
            results = [s for s in results if s.topic == topic]
        if priority:
            results = [s for s in results if s.priority == priority]

        return results

    def get_low_performing_topics(
        self,
        threshold: float = 0.5,
        min_feedbacks: int = 5,
    ) -> list[dict[str, Any]]:
        """低パフォーマンストピックを取得.

        Args:
            threshold: 有用率閾値
            min_feedbacks: 最小フィードバック数

        Returns:
            トピックリスト
        """
        stats = self.get_stats()
        results = []

        for topic, counts in stats.by_topic.items():
            total = counts["helpful"] + counts["not_helpful"]
            if total < min_feedbacks:
                continue

            helpful_rate = counts["helpful"] / total
            if helpful_rate < threshold:
                results.append({
                    "topic": topic,
                    "helpful_rate": helpful_rate,
                    "total_feedbacks": total,
                    "helpful_count": counts["helpful"],
                    "not_helpful_count": counts["not_helpful"],
                })

        # 有用率昇順
        results.sort(key=lambda x: x["helpful_rate"])

        return results


__all__ = [
    "FeedbackService",
    "FeedbackServiceConfig",
    "Feedback",
    "FeedbackType",
    "FeedbackStatus",
    "FeedbackStats",
    "ImprovementSuggestion",
]
