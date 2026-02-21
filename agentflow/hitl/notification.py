"""通知システムモジュール.

承認リクエストの通知を複数チャンネル（Email, Slack, Webhook）で送信。
プラグイン可能なアーキテクチャで拡張性を確保。
"""

from __future__ import annotations

import asyncio
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Any

import httpx


if TYPE_CHECKING:
    from collections.abc import Awaitable, Callable

    from agentflow.hitl.types import ApprovalRequest


logger = logging.getLogger(__name__)


class NotificationChannel(str, Enum):
    """通知チャンネルタイプ."""

    EMAIL = "email"
    SLACK = "slack"
    WEBHOOK = "webhook"
    CONSOLE = "console"  # 開発/テスト用


@dataclass
class NotificationConfig:
    """通知設定."""

    channel: NotificationChannel
    enabled: bool = True
    config: dict[str, Any] | None = None


class NotificationProvider(ABC):
    """通知プロバイダー基底クラス.

    各チャンネルは このクラスを継承して実装。
    """

    @property
    @abstractmethod
    def channel(self) -> NotificationChannel:
        """チャンネルタイプ."""
        ...

    @abstractmethod
    async def send(
        self,
        request: ApprovalRequest,
        message: str,
        **kwargs: Any,
    ) -> bool:
        """通知を送信.

        Args:
            request: 承認リクエスト
            message: 通知メッセージ
            **kwargs: 追加パラメータ

        Returns:
            送信成功した場合 True
        """
        ...


class ConsoleNotificationProvider(NotificationProvider):
    """コンソール通知プロバイダー（開発/テスト用）."""

    @property
    def channel(self) -> NotificationChannel:
        """チャンネルタイプ."""
        return NotificationChannel.CONSOLE

    async def send(
        self,
        request: ApprovalRequest,
        message: str,
        **kwargs: Any,
    ) -> bool:
        """コンソールに通知を出力."""
        logger.info(f"[NOTIFICATION] action={request.action}, priority={request.priority}, message={message}")
        return True


class SlackNotificationProvider(NotificationProvider):
    """Slack通知プロバイダー."""

    def __init__(self, webhook_url: str, channel: str | None = None) -> None:
        """初期化.

        Args:
            webhook_url: Slack Webhook URL
            channel: 送信先チャンネル（オプション）
        """
        self._webhook_url = webhook_url
        self._channel = channel

    @property
    def channel(self) -> NotificationChannel:
        """チャンネルタイプ."""
        return NotificationChannel.SLACK

    async def send(
        self,
        request: ApprovalRequest,
        message: str,
        **kwargs: Any,
    ) -> bool:
        """Slackに通知を送信."""
        payload = self._build_payload(request, message)
        try:
            async with httpx.AsyncClient() as client:
                response = await client.post(
                    self._webhook_url,
                    json=payload,
                    timeout=10.0,
                )
                if response.status_code == 200:
                    logger.info(f"Slack通知送信成功: {request.id}")
                    return True
                logger.error(f"Slack通知送信失敗: {response.status_code}")
                return False
        except Exception as e:
            logger.error(f"Slack通知エラー: {e}", exc_info=True)
            return False

    def _build_payload(
        self,
        request: ApprovalRequest,
        message: str,
    ) -> dict[str, Any]:
        """Slackペイロードを構築."""
        color = self._priority_to_color(request.priority)
        return {
            "attachments": [
                {
                    "color": color,
                    "title": f"承認リクエスト: {request.action}",
                    "text": message,
                    "fields": [
                        {"title": "理由", "value": request.reason, "short": False},
                        {"title": "優先度", "value": request.priority, "short": True},
                        {"title": "リクエストID", "value": request.id, "short": True},
                    ],
                }
            ]
        }

    def _priority_to_color(self, priority: str) -> str:
        """優先度を色に変換."""
        colors = {
            "low": "#36a64f",  # 緑
            "normal": "#2196F3",  # 青
            "high": "#ff9800",  # オレンジ
            "critical": "#f44336",  # 赤
        }
        return colors.get(priority, "#2196F3")


class WebhookNotificationProvider(NotificationProvider):
    """Webhook通知プロバイダー."""

    def __init__(
        self,
        url: str,
        headers: dict[str, str] | None = None,
        method: str = "POST",
    ) -> None:
        """初期化.

        Args:
            url: Webhook URL
            headers: カスタムヘッダー
            method: HTTPメソッド
        """
        self._url = url
        self._headers = headers or {}
        self._method = method

    @property
    def channel(self) -> NotificationChannel:
        """チャンネルタイプ."""
        return NotificationChannel.WEBHOOK

    async def send(
        self,
        request: ApprovalRequest,
        message: str,
        **kwargs: Any,
    ) -> bool:
        """Webhookに通知を送信."""
        payload = {
            "request_id": request.id,
            "action": request.action,
            "reason": request.reason,
            "priority": request.priority,
            "message": message,
            "context": request.context,
            "created_at": request.created_at.isoformat(),
        }
        try:
            async with httpx.AsyncClient() as client:
                response = await client.request(
                    method=self._method,
                    url=self._url,
                    json=payload,
                    headers=self._headers,
                    timeout=10.0,
                )
                if response.is_success:
                    logger.info(f"Webhook通知送信成功: {request.id}")
                    return True
                logger.error(f"Webhook通知送信失敗: {response.status_code}")
                return False
        except Exception as e:
            logger.error(f"Webhook通知エラー: {e}", exc_info=True)
            return False


class NotificationManager:
    """通知マネージャー.

    複数のプロバイダーを管理し、承認リクエストの通知を送信。

    使用例:
        >>> manager = NotificationManager()
        >>> manager.register_provider(SlackNotificationProvider(webhook_url="..."))
        >>> await manager.notify(request, "承認が必要です")
    """

    def __init__(self) -> None:
        """初期化."""
        self._providers: dict[NotificationChannel, NotificationProvider] = {}
        self._default_message_template = "承認リクエスト: {action} - {reason}"

    def register_provider(self, provider: NotificationProvider) -> None:
        """プロバイダーを登録.

        Args:
            provider: 通知プロバイダー
        """
        self._providers[provider.channel] = provider
        logger.info(f"通知プロバイダー登録: {provider.channel.value}")

    def unregister_provider(self, channel: NotificationChannel) -> None:
        """プロバイダーを登録解除.

        Args:
            channel: チャンネルタイプ
        """
        if channel in self._providers:
            del self._providers[channel]
            logger.info(f"通知プロバイダー解除: {channel.value}")

    async def notify(
        self,
        request: ApprovalRequest,
        message: str | None = None,
        channels: list[NotificationChannel] | None = None,
    ) -> dict[NotificationChannel, bool]:
        """通知を送信.

        Args:
            request: 承認リクエスト
            message: カスタムメッセージ（オプション）
            channels: 送信先チャンネル（省略時は全て）

        Returns:
            チャンネルごとの送信結果
        """
        effective_message = message or self._default_message_template.format(
            action=request.action,
            reason=request.reason,
        )
        target_channels = channels or list(self._providers.keys())
        results: dict[NotificationChannel, bool] = {}

        # 並行送信
        tasks = []
        for channel in target_channels:
            provider = self._providers.get(channel)
            if provider:
                tasks.append(self._send_with_channel(provider, request, effective_message, channel))

        if tasks:
            completed = await asyncio.gather(*tasks, return_exceptions=True)
            for channel, result in zip(target_channels, completed, strict=False):
                if isinstance(result, BaseException):
                    logger.error(f"通知エラー ({channel.value}): {result}")
                    results[channel] = False
                else:
                    results[channel] = result

        return results

    async def _send_with_channel(
        self,
        provider: NotificationProvider,
        request: ApprovalRequest,
        message: str,
        channel: NotificationChannel,
    ) -> bool:
        """個別チャンネルに送信."""
        return await provider.send(request, message)

    def get_callback(self) -> Callable[[ApprovalRequest], Awaitable[None]]:
        """ApprovalFlow用のコールバックを取得.

        Returns:
            通知送信コールバック
        """

        async def callback(request: ApprovalRequest) -> None:
            await self.notify(request)

        return callback
