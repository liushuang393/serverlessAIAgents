"""通知エージェント.

重要な変化を検知して通知します。
"""

import asyncio
import logging
import os
import smtplib
import uuid
from datetime import datetime
from email.message import EmailMessage
from typing import Any

from apps.market_trend_monitor.backend.models import Notification, NotificationPriority, Trend

from agentflow.core.agent_block import AgentBlock

try:
    import httpx
except ImportError:  # pragma: no cover - optional dependency
    httpx = None


class _NotificationChannel:
    """通知チャネル基底."""

    def __init__(self, channel_name: str) -> None:
        self.channel_name = channel_name

    async def send(self, notification: Notification) -> bool:
        raise NotImplementedError


class _WebhookNotificationChannel(_NotificationChannel):
    """Webhook 通知チャネル."""

    def __init__(self, webhook_url: str, timeout_seconds: float = 10.0) -> None:
        super().__init__("webhook")
        self._webhook_url = webhook_url
        self._timeout_seconds = timeout_seconds

    async def send(self, notification: Notification) -> bool:
        if httpx is None:
            return False
        payload = notification.to_dict()
        async with httpx.AsyncClient(timeout=self._timeout_seconds) as client:
            response = await client.post(self._webhook_url, json=payload)
            return response.status_code < 400


class _EmailNotificationChannel(_NotificationChannel):
    """メール通知チャネル."""

    def __init__(
        self,
        *,
        smtp_host: str,
        smtp_port: int,
        from_email: str,
        to_email: str,
        username: str | None = None,
        password: str | None = None,
        use_tls: bool = True,
    ) -> None:
        super().__init__("email")
        self._smtp_host = smtp_host
        self._smtp_port = smtp_port
        self._from_email = from_email
        self._to_email = to_email
        self._username = username
        self._password = password
        self._use_tls = use_tls

    async def send(self, notification: Notification) -> bool:
        return await asyncio.to_thread(self._send_sync, notification)

    def _send_sync(self, notification: Notification) -> bool:
        message = EmailMessage()
        message["Subject"] = f"[MarketTrend][{notification.priority.value.upper()}] {notification.type}"
        message["From"] = self._from_email
        message["To"] = self._to_email
        message.set_content(notification.message)

        with smtplib.SMTP(self._smtp_host, self._smtp_port, timeout=10) as smtp:
            if self._use_tls:
                smtp.starttls()
            if self._username and self._password:
                smtp.login(self._username, self._password)
            smtp.send_message(message)
        return True


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
        self._runtime_mode = os.getenv("APP_RUNTIME_MODE", "dev").strip().lower()
        self._channels: list[_NotificationChannel] = []

    async def initialize(self) -> None:
        """エージェント初期化."""
        await super().initialize()
        self._logger.info("NotifierAgent initialized")
        self._channels = self._build_channels()
        self._logger.info("Notifier channels initialized: %d", len(self._channels))

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
        past_notifications = await self._recall_past_notifications(shared_context) if shared_context else []

        # アラート検知（重複チェック）
        notifications = await self._detect_alerts(trends, alert_threshold, past_notifications)

        # 通知送信
        delivery_results = await self._send_notifications(notifications)

        # 通知を記憶システムに保存
        if shared_context:
            await self._save_notifications_to_memory(notifications, shared_context)

        self._logger.info(f"Notifications sent: {len(notifications)} alerts")

        return {
            "notifications": [n.to_dict() for n in notifications],
            "alerts_count": len(notifications),
            "delivery_status": delivery_results,
            "fallback_used": any(bool(item.get("fallback_used")) for item in delivery_results),
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

    async def _send_notifications(self, notifications: list[Notification]) -> list[dict[str, Any]]:
        """通知送信.

        Args:
            notifications: 通知リスト
        """
        delivery_results: list[dict[str, Any]] = []
        for notification in notifications:
            self._logger.info(f"Sending notification: {notification.priority.value} - {notification.message}")

            if not self._channels:
                fallback_used = self._runtime_mode == "dev"
                delivery_status = "sent" if fallback_used else "failed"
                result = {
                    "notification_id": notification.id,
                    "delivery_status": delivery_status,
                    "fallback_used": fallback_used,
                    "channels": [],
                }
                notification.metadata["delivery_status"] = delivery_status
                notification.metadata["fallback_used"] = fallback_used
                self._notification_history.append(notification)
                delivery_results.append(result)
                if not fallback_used:
                    self._logger.warning("通知チャネル未設定のため送信失敗: %s", notification.id)
                continue

            channel_results: list[dict[str, Any]] = []
            any_success = False
            for channel in self._channels:
                success = await self._send_with_retry(channel, notification)
                channel_results.append({"channel": channel.channel_name, "success": success})
                if success:
                    any_success = True

            delivery_status = "sent" if any_success else "failed"
            notification.metadata["delivery_status"] = delivery_status
            notification.metadata["fallback_used"] = False
            notification.metadata["channels"] = channel_results
            self._notification_history.append(notification)

            delivery_results.append(
                {
                    "notification_id": notification.id,
                    "delivery_status": delivery_status,
                    "fallback_used": False,
                    "channels": channel_results,
                }
            )
        return delivery_results

    def _build_channels(self) -> list[_NotificationChannel]:
        """環境変数から通知チャネルを構築する."""
        channels: list[_NotificationChannel] = []

        webhook_url = os.getenv("MARKET_NOTIFICATION_WEBHOOK_URL", "").strip()
        if webhook_url:
            channels.append(_WebhookNotificationChannel(webhook_url=webhook_url))

        smtp_host = os.getenv("MARKET_NOTIFICATION_SMTP_HOST", "").strip()
        to_email = os.getenv("MARKET_NOTIFICATION_EMAIL_TO", "").strip()
        from_email = os.getenv("MARKET_NOTIFICATION_EMAIL_FROM", "").strip() or to_email
        if smtp_host and to_email and from_email:
            smtp_port_raw = os.getenv("MARKET_NOTIFICATION_SMTP_PORT", "587").strip()
            smtp_port = int(smtp_port_raw) if smtp_port_raw.isdigit() else 587
            username = os.getenv("MARKET_NOTIFICATION_SMTP_USER", "").strip() or None
            password = os.getenv("MARKET_NOTIFICATION_SMTP_PASS", "").strip() or None
            use_tls = os.getenv("MARKET_NOTIFICATION_SMTP_TLS", "true").strip().lower() != "false"
            channels.append(
                _EmailNotificationChannel(
                    smtp_host=smtp_host,
                    smtp_port=smtp_port,
                    from_email=from_email,
                    to_email=to_email,
                    username=username,
                    password=password,
                    use_tls=use_tls,
                )
            )

        return channels

    async def _send_with_retry(self, channel: _NotificationChannel, notification: Notification) -> bool:
        """通知送信を軽量リトライする."""
        for attempt in range(3):
            try:
                ok = await channel.send(notification)
                if ok:
                    return True
            except Exception as exc:
                self._logger.warning(
                    "通知送信失敗: channel=%s attempt=%d error=%s",
                    channel.channel_name,
                    attempt + 1,
                    exc,
                )
            if attempt < 2:
                await asyncio.sleep(0.2 * (2**attempt))
        return False

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

    async def _save_notifications_to_memory(self, notifications: list[Notification], shared_context: Any) -> None:
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
                        "delivery_status": notification.metadata.get("delivery_status", ""),
                        "fallback_used": bool(notification.metadata.get("fallback_used", False)),
                    },
                )

            self._logger.info(f"Saved {len(notifications)} notifications to memory system")
        except Exception as e:
            self._logger.warning(f"Failed to save notifications to memory: {e}")

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        self._logger.info("NotifierAgent cleanup")
        await super().cleanup()
