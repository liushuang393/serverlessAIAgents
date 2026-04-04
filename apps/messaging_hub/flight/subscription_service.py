"""サブスクリプション管理 + 監視ループ."""

from __future__ import annotations

import uuid
from datetime import UTC, datetime, timedelta
from typing import TYPE_CHECKING, Any

from apps.messaging_hub.flight.models import (
    FlightSearchRequest,
    FlightWatchSubscription,
    NotificationChannel,
    NotificationTarget,
    SubscriptionStatus,
)


if TYPE_CHECKING:
    from apps.messaging_hub.flight.notification_service import FlightNotificationService
    from apps.messaging_hub.flight.search_service import FlightSearchService
    from apps.messaging_hub.storage.sqlite_store import SQLiteMessagingHubStore


class FlightSubscriptionService:
    """機票監視購読の CRUD + 定期チェック."""

    def __init__(
        self,
        *,
        store: SQLiteMessagingHubStore,
        search_service: FlightSearchService,
        notification_service: FlightNotificationService,
    ) -> None:
        """初期化."""
        self._store = store
        self._search = search_service
        self._notification = notification_service

    async def create(
        self,
        *,
        request: FlightSearchRequest,
        user_id: str,
        conversation_id: str | None,
    ) -> FlightWatchSubscription:
        """監視購読を作成する."""
        search_result = await self._search.search(request)
        lowest_price = search_result.recommended_offer.price if search_result.recommended_offer is not None else None
        now = datetime.now(UTC)
        subscription = FlightWatchSubscription(
            subscription_id=f"fws_{uuid.uuid4().hex}",
            user_id=user_id,
            conversation_id=conversation_id,
            status=SubscriptionStatus.ACTIVE,
            provider=search_result.provider_used,
            request=request,
            ranking_weights=request.ranking_weights,
            notification_targets=request.notification_targets
            or [NotificationTarget(channel=NotificationChannel.IN_APP)],
            baseline_price=lowest_price,
            target_price=request.target_price,
            next_check_at=(now + timedelta(hours=request.poll_interval_hours)).isoformat(),
            created_at=now.isoformat(),
            updated_at=now.isoformat(),
        )
        await self._store.upsert_flight_watch_subscription(subscription.to_store_dict())
        await self._store.add_flight_offer_snapshot(
            subscription_id=subscription.subscription_id,
            provider=search_result.provider_used,
            lowest_price=lowest_price,
            offers=[offer.model_dump() for offer in search_result.offers],
            checked_at=now.isoformat(),
        )
        return subscription

    async def update_status(
        self,
        subscription_id: str,
        status: SubscriptionStatus,
    ) -> dict[str, Any] | None:
        """購読状態を更新する."""
        subscription = await self._store.get_flight_watch_subscription(subscription_id)
        if subscription is None:
            return None
        subscription["status"] = status.value
        subscription["updated_at"] = datetime.now(UTC).isoformat()
        await self._store.upsert_flight_watch_subscription(subscription)
        return subscription

    async def check_due(self) -> dict[str, Any]:
        """期限到来した購読をチェックし、必要に応じて通知する."""
        now_iso = datetime.now(UTC).isoformat()
        due = await self._store.list_flight_watch_subscriptions(
            status=SubscriptionStatus.ACTIVE.value,
            due_before=now_iso,
        )
        checked = 0
        notified = 0
        for raw_subscription in due:
            subscription = FlightWatchSubscription.model_validate(raw_subscription)
            result = await self._search.search(subscription.request)
            lowest_price = result.recommended_offer.price if result.recommended_offer is not None else None
            await self._store.add_flight_offer_snapshot(
                subscription_id=subscription.subscription_id,
                provider=result.provider_used,
                lowest_price=lowest_price,
                offers=[offer.model_dump() for offer in result.offers],
            )
            checked += 1
            should_notify, reason = _should_notify(subscription, lowest_price)
            subscription.last_checked_at = now_iso
            subscription.next_check_at = (
                datetime.now(UTC) + timedelta(hours=subscription.request.poll_interval_hours)
            ).isoformat()
            if should_notify and result.offers:
                await self._notification.notify_price_drop(
                    subscription=subscription,
                    offers=result.offers,
                    reason=reason,
                )
                subscription.last_notified_price = lowest_price
                subscription.baseline_price = lowest_price
                notified += 1
            elif lowest_price is not None and subscription.baseline_price is None:
                subscription.baseline_price = lowest_price
            subscription.updated_at = datetime.now(UTC).isoformat()
            await self._store.upsert_flight_watch_subscription(subscription.to_store_dict())
        return {"checked": checked, "notified": notified}


def _should_notify(
    subscription: FlightWatchSubscription,
    lowest_price: float | None,
) -> tuple[bool, str]:
    """通知要否を判定する."""
    if lowest_price is None:
        return False, "no_offer"
    if subscription.target_price is not None and lowest_price <= subscription.target_price:
        if subscription.last_notified_price is None or lowest_price < subscription.last_notified_price:
            return True, "target_price_reached"
    if subscription.baseline_price is None:
        return False, "baseline_initialized"
    if lowest_price < subscription.baseline_price:
        if subscription.last_notified_price is None or lowest_price < subscription.last_notified_price:
            return True, "new_lowest_price"
    return False, "unchanged"
