"""機票監視通知サービス."""

from __future__ import annotations

import os
import smtplib
import uuid
from datetime import UTC, datetime
from email.message import EmailMessage
from typing import TYPE_CHECKING, Any

from apps.messaging_hub.flight.models import (
    FlightOffer,
    FlightWatchSubscription,
    NotificationChannel,
    NotificationTarget,
)


if TYPE_CHECKING:
    from apps.messaging_hub.storage.sqlite_store import SQLiteMessagingHubStore
    from kernel.runtime.websocket import WebSocketHub


class FlightNotificationService:
    """機票監視通知サービス."""

    def __init__(
        self,
        *,
        store: SQLiteMessagingHubStore,
        websocket_hub: WebSocketHub,
    ) -> None:
        """初期化."""
        self._store = store
        self._hub = websocket_hub

    async def notify_price_drop(
        self,
        *,
        subscription: FlightWatchSubscription,
        offers: list[FlightOffer],
        reason: str,
    ) -> list[dict[str, Any]]:
        """通知を送信する."""
        deliveries: list[dict[str, Any]] = []
        targets = subscription.notification_targets or [NotificationTarget(channel=NotificationChannel.IN_APP)]
        recommended = offers[0] if offers else None
        payload = {
            "subscription_id": subscription.subscription_id,
            "reason": reason,
            "lowest_price": recommended.price if recommended is not None else None,
            "offer": recommended.model_dump() if recommended is not None else None,
        }
        for target in targets:
            if target.channel == NotificationChannel.IN_APP:
                deliveries.append(await self._send_in_app(subscription, payload))
                continue
            if target.channel == NotificationChannel.EMAIL:
                deliveries.append(await self._send_email(target, payload))
        return deliveries

    async def _send_in_app(
        self,
        subscription: FlightWatchSubscription,
        payload: dict[str, Any],
    ) -> dict[str, Any]:
        """站内通知を送信する."""
        rooms: list[str] = []
        if subscription.conversation_id:
            rooms.append(subscription.conversation_id)
        rooms.append(f"user:{subscription.user_id}")
        message = {"type": "notification", "data": payload}
        delivered_total = 0
        for room in rooms:
            delivered_total += await self._hub.broadcast_room(room, message)
        if delivered_total == 0:
            delivered_total = await self._hub.broadcast(message)
        delivery = {
            "delivery_id": f"delivery_{uuid.uuid4().hex}",
            "subscription_id": subscription.subscription_id,
            "channel": NotificationChannel.IN_APP.value,
            "destination": subscription.conversation_id or subscription.user_id,
            "status": "sent",
            "payload": payload,
            "created_at": datetime.now(UTC).isoformat(),
        }
        await self._store.add_notification_delivery(delivery)
        return delivery

    async def _send_email(
        self,
        target: NotificationTarget,
        payload: dict[str, Any],
    ) -> dict[str, Any]:
        """メール通知を送信する."""
        destination = target.destination
        if not destination:
            delivery = {
                "delivery_id": f"delivery_{uuid.uuid4().hex}",
                "subscription_id": payload.get("subscription_id"),
                "channel": NotificationChannel.EMAIL.value,
                "destination": None,
                "status": "failed",
                "payload": payload,
                "error": "destination_missing",
                "created_at": datetime.now(UTC).isoformat(),
            }
            await self._store.add_notification_delivery(delivery)
            return delivery

        try:
            import asyncio

            await asyncio.to_thread(_send_email_sync, destination, payload)
            status = "sent"
            error: str | None = None
        except Exception as exc:  # pragma: no cover - SMTP 環境依存
            status = "failed"
            error = str(exc)

        delivery = {
            "delivery_id": f"delivery_{uuid.uuid4().hex}",
            "subscription_id": payload.get("subscription_id"),
            "channel": NotificationChannel.EMAIL.value,
            "destination": destination,
            "status": status,
            "payload": payload,
            "error": error,
            "created_at": datetime.now(UTC).isoformat(),
        }
        await self._store.add_notification_delivery(delivery)
        return delivery


def _send_email_sync(destination: str, payload: dict[str, Any]) -> None:
    """同期でメール送信する."""
    smtp_host = os.getenv("MESSAGING_HUB_SMTP_HOST", "").strip()
    from_email = os.getenv("MESSAGING_HUB_EMAIL_FROM", "").strip()
    username = os.getenv("MESSAGING_HUB_SMTP_USERNAME", "").strip()
    password = os.getenv("MESSAGING_HUB_SMTP_PASSWORD", "").strip()
    smtp_port = int(os.getenv("MESSAGING_HUB_SMTP_PORT", "587"))
    if not smtp_host or not from_email:
        msg = "smtp_not_configured"
        raise RuntimeError(msg)

    message = EmailMessage()
    message["Subject"] = "[MessagingHub][FlightWatch] Price drop detected"
    message["From"] = from_email
    message["To"] = destination
    lowest_price = payload.get("lowest_price")
    reason = payload.get("reason", "")
    message.set_content(f"Price drop detected: {reason}\nLowest price: {lowest_price}")

    with smtplib.SMTP(smtp_host, smtp_port, timeout=10) as smtp:
        smtp.starttls()
        if username and password:
            smtp.login(username, password)
        smtp.send_message(message)
