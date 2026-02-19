"""通知エージェント.

重要な変化を検知して通知します。
"""

import logging
import uuid
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.models import Notification, NotificationPriority, Trend

from agentflow.core.agent_block import AgentBlock


class NotifierAgent(AgentBlock):
    """通知エージェント.

    役割:
    - 閾値ベースのアラート検知
    - 通知チャネル管理（WebSocket、メール、Slack）
    - 通知履歴管理

    入力:
        {
            "trends": [Trend, ...],
            "alert_threshold": 0.3
        }

    出力:
        {
            "notifications": [Notification, ...],
            "alerts_count": 3
        }
    """

    def __init__(self) -> None:
        """初期化."""
        super().__init__()
        self._logger = logging.getLogger(__name__)
        self._notification_history: list[Notification] = []

    async def initialize(self) -> None:
        """エージェント初期化."""
        await super().initialize()
        self._logger.info("NotifierAgent initialized")

        # TODO: 通知チャネル初期化
        # self.websocket_manager = WebSocketManager()
        # self.email_client = EmailClient()
        # self.slack_client = SlackClient()

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """通知処理を実行.

        Args:
            input_data: 入力データ
                - trends: トレンドリスト
                - alert_threshold: アラート閾値
                - shared_context: 共有コンテキスト（記憶システム付き）

        Returns:
            通知結果
                - notifications: 通知リスト
                - alerts_count: アラート数
        """
        trends_data = input_data.get("trends", [])
        alert_threshold = input_data.get("alert_threshold", 0.3)
        shared_context = input_data.get("shared_context")

        self._logger.info(f"Starting notification check: {len(trends_data)} trends")

        # Trend オブジェクトに変換
        trends = [self._dict_to_trend(t) for t in trends_data]

        # 過去の通知履歴を検索（記憶システムから）
        past_notifications = (
            await self._recall_past_notifications(shared_context) if shared_context else []
        )

        # アラート検知（重複チェック）
        notifications = await self._detect_alerts(trends, alert_threshold, past_notifications)

        # 通知送信
        await self._send_notifications(notifications)

        # 通知を記憶システムに保存
        if shared_context:
            await self._save_notifications_to_memory(notifications, shared_context)

        self._logger.info(f"Notifications sent: {len(notifications)} alerts")

        return {
            "notifications": [n.to_dict() for n in notifications],
            "alerts_count": len(notifications),
        }

    def _dict_to_trend(self, data: dict[str, Any]) -> Trend:
        """辞書からTrendオブジェクトを生成."""
        from apps.market_trend_monitor.backend.models import SentimentType

        from agentflow.utils.type_helpers import convert_enum

        return Trend(
            id=data["id"],
            topic=data["topic"],
            score=data["score"],
            articles_count=data["articles_count"],
            keywords=data["keywords"],
            sentiment=convert_enum(data["sentiment"], SentimentType),
            growth_rate=data["growth_rate"],
            created_at=datetime.fromisoformat(data["created_at"]),
            metadata=data.get("metadata", {}),
        )

    async def _detect_alerts(
        self,
        trends: list[Trend],
        threshold: float,
        past_notifications: list[dict[str, Any]] | None = None,
    ) -> list[Notification]:
        """アラート検知.

        Args:
            trends: トレンドリスト
            threshold: 成長率閾値
            past_notifications: 過去の通知履歴（記憶システムから取得）

        Returns:
            通知リスト
        """
        notifications: list[Notification] = []
        past_notifications = past_notifications or []

        # 過去の通知トピックを抽出（重複チェック用）
        past_topics = {n.get("topic", "") for n in past_notifications}

        for trend in trends:
            # 重複チェック: 過去24時間以内に同じトピックで通知済みの場合はスキップ
            if trend.topic in past_topics:
                self._logger.debug(f"Skipping duplicate notification for topic: {trend.topic}")
                continue

            # 高成長率のトレンドをアラート
            if trend.growth_rate >= threshold:
                priority = self._calculate_priority(trend.growth_rate)
                notification = Notification(
                    id=str(uuid.uuid4()),
                    type="alert",
                    priority=priority,
                    message=f"「{trend.topic}」が急成長中（成長率: {trend.growth_rate:.1%}）",
                    timestamp=datetime.now(),
                    metadata={
                        "trend_id": trend.id,
                        "topic": trend.topic,
                        "growth_rate": trend.growth_rate,
                        "articles_count": trend.articles_count,
                    },
                )
                notifications.append(notification)

            # 高スコアのトレンドを通知
            elif trend.score >= 0.8:
                notification = Notification(
                    id=str(uuid.uuid4()),
                    type="info",
                    priority=NotificationPriority.MEDIUM,
                    message=f"「{trend.topic}」が注目されています（スコア: {trend.score:.2f}）",
                    timestamp=datetime.now(),
                    metadata={"trend_id": trend.id, "topic": trend.topic, "score": trend.score},
                )
                notifications.append(notification)

        return notifications

    def _calculate_priority(self, growth_rate: float) -> NotificationPriority:
        """優先度計算.

        Args:
            growth_rate: 成長率

        Returns:
            通知優先度
        """
        if growth_rate >= 0.5:
            return NotificationPriority.CRITICAL
        if growth_rate >= 0.4:
            return NotificationPriority.HIGH
        if growth_rate >= 0.3:
            return NotificationPriority.MEDIUM
        return NotificationPriority.LOW

    async def _send_notifications(self, notifications: list[Notification]) -> None:
        """通知送信.

        Args:
            notifications: 通知リスト
        """
        for notification in notifications:
            self._logger.info(
                f"Sending notification: {notification.priority.value} - {notification.message}"
            )

            # 履歴に追加
            self._notification_history.append(notification)

            # TODO: 実際の通知送信
            # await self.websocket_manager.broadcast(notification.to_dict())
            # if notification.priority in [NotificationPriority.HIGH, NotificationPriority.CRITICAL]:
            #     await self.email_client.send(notification)
            #     await self.slack_client.send(notification)

    async def _recall_past_notifications(self, shared_context: Any) -> list[dict[str, Any]]:
        """過去の通知履歴を記憶システムから検索.

        Args:
            shared_context: 共有コンテキスト

        Returns:
            過去の通知リスト
        """
        try:
            # 過去24時間以内の通知を検索
            memories = await shared_context.recall(topic="sent_notifications", limit=50)
            past_notifications = []
            for memory in memories:
                # メタデータから通知情報を抽出
                metadata = memory.metadata
                if "notification_id" in metadata:
                    past_notifications.append(metadata)

            self._logger.info(f"Recalled {len(past_notifications)} past notifications from memory")
            return past_notifications
        except Exception as e:
            self._logger.warning(f"Failed to recall past notifications: {e}")
            return []

    async def _save_notifications_to_memory(
        self, notifications: list[Notification], shared_context: Any
    ) -> None:
        """通知を記憶システムに保存.

        Args:
            notifications: 通知リスト
            shared_context: 共有コンテキスト
        """
        try:
            for notification in notifications:
                # 通知情報を記憶
                memory_text = f"Notification: {notification.message}\nType: {notification.type}\nPriority: {notification.priority.value}\nTimestamp: {notification.timestamp.isoformat()}"

                await shared_context.remember(
                    memory_text,
                    topic="sent_notifications",
                    metadata={
                        "notification_id": notification.id,
                        "type": notification.type,
                        "priority": notification.priority.value,
                        "topic": notification.metadata.get("topic", ""),
                        "message": notification.message,
                        "timestamp": notification.timestamp.isoformat(),
                    },
                )

            self._logger.info(f"Saved {len(notifications)} notifications to memory system")
        except Exception as e:
            self._logger.warning(f"Failed to save notifications to memory: {e}")

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        self._logger.info("NotifierAgent cleanup")
        await super().cleanup()
