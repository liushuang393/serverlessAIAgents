"""Flight watch service and provider implementations."""

from __future__ import annotations

import hashlib
import logging
import re
import smtplib
import uuid
from datetime import UTC, datetime, timedelta
from email.message import EmailMessage
from enum import StrEnum
from typing import TYPE_CHECKING, Any, Protocol

from pydantic import BaseModel, Field, model_validator

from apps.messaging_hub.flight_offer_extractor import DiscoveredFlightOfferExtractor
from apps.messaging_hub.task_harness import ProviderCandidate, TaskProviderDiscoveryService


if TYPE_CHECKING:
    from apps.messaging_hub.storage.sqlite_store import SQLiteMessagingHubStore
    from kernel.runtime.websocket import WebSocketHub
    from kernel.skills.gateway import SkillGateway


_LOGGER = logging.getLogger(__name__)
_DATE_RANGE_PATTERN = re.compile(
    r"(?P<label>depart|departure|return|出发|去程|回来|返程|帰り|往路|復路)"
    r"[:：\s]*(?P<start>\d{4}-\d{2}-\d{2})(?:\s*(?:to|~|～|-)\s*(?P<end>\d{4}-\d{2}-\d{2}))?",
    re.IGNORECASE,
)
_ORIGIN_PATTERN = re.compile(
    r"(?:origin|from|出发地|出発地|从)[:：\s]*(?P<origin>[A-Za-z]{3,4}|[\u4e00-\u9fff]{2,16})",
    re.IGNORECASE,
)
_DESTINATION_PATTERN = re.compile(
    r"(?:destination|to|目的地|到着地|到)[:：\s]*(?P<destination>[A-Za-z]{3,4}|[\u4e00-\u9fff]{2,16})",
    re.IGNORECASE,
)
_BUDGET_PATTERN = re.compile(r"(?:budget|予算|预算)[:：\s]*([0-9]+(?:\.[0-9]+)?)", re.IGNORECASE)
_MAX_STOPS_PATTERN = re.compile(r"(?:max[_\s-]*stops|转机|乗継)[:：\s]*([0-9]+)", re.IGNORECASE)
_WATCH_PATTERN = re.compile(r"(watch|monitor|降价|値下がり|price drop|监视)", re.IGNORECASE)


class NotificationChannel(StrEnum):
    """通知チャネル."""

    IN_APP = "in_app"
    EMAIL = "email"


class SubscriptionStatus(StrEnum):
    """監視ステータス."""

    ACTIVE = "active"
    PAUSED = "paused"
    CANCELLED = "cancelled"


class DateWindow(BaseModel):
    """日付ウィンドウ."""

    start_date: str = Field(..., min_length=10)
    end_date: str = Field(..., min_length=10)


class RankingWeights(BaseModel):
    """ランキング重み."""

    price: float = Field(default=0.5, ge=0.0)
    duration: float = Field(default=0.3, ge=0.0)
    convenience: float = Field(default=0.2, ge=0.0)

    @model_validator(mode="after")
    def normalize(self) -> RankingWeights:
        """重み合計を 1.0 に正規化する."""
        total = self.price + self.duration + self.convenience
        if total <= 0:
            self.price = 0.5
            self.duration = 0.3
            self.convenience = 0.2
            return self
        self.price = round(self.price / total, 4)
        self.duration = round(self.duration / total, 4)
        self.convenience = round(self.convenience / total, 4)
        return self


class NotificationTarget(BaseModel):
    """通知先."""

    channel: NotificationChannel = Field(default=NotificationChannel.IN_APP)
    destination: str | None = Field(default=None)


class FlightSearchRequest(BaseModel):
    """機票検索リクエスト."""

    origin: str = Field(..., min_length=1)
    destination: str = Field(..., min_length=1)
    depart_window: DateWindow
    return_window: DateWindow
    budget: float | None = Field(default=None, gt=0.0)
    max_stops: int | None = Field(default=1, ge=0, le=4)
    preferred_departure_time: str | None = Field(default=None)
    preferred_arrival_time: str | None = Field(default=None)
    carrier_blacklist: list[str] = Field(default_factory=list)
    ranking_weights: RankingWeights = Field(default_factory=RankingWeights)
    provider: str = Field(default="auto")
    target_price: float | None = Field(default=None, gt=0.0)
    poll_interval_hours: int = Field(default=6, ge=1, le=24)
    create_watch: bool = Field(default=False)
    notification_targets: list[NotificationTarget] = Field(default_factory=list)


class FlightOffer(BaseModel):
    """フライト提案."""

    offer_id: str
    provider: str
    origin: str
    destination: str
    depart_date: str
    return_date: str
    price: float = Field(..., gt=0.0)
    currency: str = Field(default="USD")
    total_duration_minutes: int = Field(..., ge=1)
    stops: int = Field(default=0, ge=0)
    carrier: str = Field(default="")
    red_eye: bool = Field(default=False)
    airport_change: bool = Field(default=False)
    layover_minutes: list[int] = Field(default_factory=list)
    metadata: dict[str, Any] = Field(default_factory=dict)
    score: float = Field(default=0.0)
    score_breakdown: dict[str, float] = Field(default_factory=dict)


class FlightSearchResult(BaseModel):
    """機票検索結果."""

    offers: list[FlightOffer] = Field(default_factory=list)
    ranking_weights: RankingWeights = Field(default_factory=RankingWeights)
    provider_used: str = Field(default="fake")
    recommended_offer: FlightOffer | None = Field(default=None)
    metadata: dict[str, Any] = Field(default_factory=dict)


class FlightWatchSubscription(BaseModel):
    """機票監視購読."""

    subscription_id: str
    user_id: str
    conversation_id: str | None = None
    status: SubscriptionStatus = Field(default=SubscriptionStatus.ACTIVE)
    provider: str = Field(default="auto")
    request: FlightSearchRequest
    ranking_weights: RankingWeights = Field(default_factory=RankingWeights)
    notification_targets: list[NotificationTarget] = Field(default_factory=list)
    baseline_price: float | None = None
    target_price: float | None = None
    last_notified_price: float | None = None
    last_checked_at: str | None = None
    next_check_at: str
    created_at: str
    updated_at: str

    def to_store_dict(self) -> dict[str, Any]:
        """ストア保存用辞書."""
        payload = self.model_dump()
        payload["status"] = self.status.value
        payload["request"] = self.request.model_dump()
        payload["ranking_weights"] = self.ranking_weights.model_dump()
        payload["notification_targets"] = [item.model_dump() for item in self.notification_targets]
        return payload


class FlightSearchProvider(Protocol):
    """検索 provider interface."""

    async def search(
        self,
        request: FlightSearchRequest,
        provider_candidates: list[ProviderCandidate] | None = None,
    ) -> list[FlightOffer]:
        """検索結果一覧を返す."""


class FakeFlightProvider:
    """テスト向け deterministic provider."""

    name = "fake"

    async def search(
        self,
        request: FlightSearchRequest,
        provider_candidates: list[ProviderCandidate] | None = None,
    ) -> list[FlightOffer]:
        """疑似フライト提案を返す."""
        seed = hashlib.sha1(
            f"{request.origin}|{request.destination}|{request.depart_window.start_date}|{request.return_window.start_date}".encode()
        ).hexdigest()
        base_price = 220 + (int(seed[:4], 16) % 300)
        base_duration = 660 + (int(seed[4:8], 16) % 260)
        offers: list[FlightOffer] = []
        for index in range(4):
            offer = FlightOffer(
                offer_id=f"fake_{index}_{seed[:8]}",
                provider=self.name,
                origin=request.origin.upper(),
                destination=request.destination.upper(),
                depart_date=request.depart_window.start_date,
                return_date=request.return_window.start_date,
                price=float(base_price + (index * 37) - (index % 2) * 18),
                currency="USD",
                total_duration_minutes=base_duration + (index * 55),
                stops=min(index, 2),
                carrier=["SkyJet", "Vista Air", "Blue Orbit", "Northline"][index],
                red_eye=index == 2,
                airport_change=index == 3,
                layover_minutes=[95 + (index * 15)] if index > 0 else [],
                metadata={"source": "fake_provider"},
            )
            offers.append(offer)
        return offers


class WebAggregatorFlightProvider:
    """web/browser skill を利用する provider."""

    name = "web"

    def __init__(self, skill_gateway: SkillGateway | None = None) -> None:
        """初期化."""
        self._gateway = skill_gateway
        self._extractor = DiscoveredFlightOfferExtractor(skill_gateway)

    async def search(
        self,
        request: FlightSearchRequest,
        provider_candidates: list[ProviderCandidate] | None = None,
    ) -> list[FlightOffer]:
        """web_search を試行し、価格抽出できた場合のみ返す."""
        if self._gateway is None:
            return []

        discovered_offers = await self._extractor.extract(
            request=request,
            provider_candidates=provider_candidates or [],
        )
        if discovered_offers:
            return [offer for offer in discovered_offers if isinstance(offer, FlightOffer)]

        queries = self._build_queries(request, provider_candidates or [])
        items: list[dict[str, Any]] = []
        for query in queries:
            try:
                result = await self._gateway.call("web_search", {"query": query})
            except Exception:
                continue
            if not result.success:
                continue
            payload = result.result
            current_items = (
                payload
                if isinstance(payload, list)
                else payload.get("results", [])
                if isinstance(payload, dict)
                else []
            )
            if isinstance(current_items, list):
                items.extend(item for item in current_items if isinstance(item, dict))
            if items:
                break

        offers: list[FlightOffer] = []
        for index, item in enumerate(items[:5]):
            text = " ".join(str(item.get(key, "")) for key in ("title", "snippet", "content"))
            price_match = re.search(r"(?:USD|\$)\s*([0-9]+(?:\.[0-9]+)?)", text)
            if price_match is None:
                continue
            price = float(price_match.group(1))
            offers.append(
                FlightOffer(
                    offer_id=f"web_{index}_{uuid.uuid4().hex[:8]}",
                    provider=self.name,
                    origin=request.origin.upper(),
                    destination=request.destination.upper(),
                    depart_date=request.depart_window.start_date,
                    return_date=request.return_window.start_date,
                    price=price,
                    currency="USD",
                    total_duration_minutes=720 + (index * 40),
                    stops=min(index, 2),
                    carrier=str(item.get("source", "aggregator")),
                    red_eye=False,
                    airport_change=False,
                    layover_minutes=[90 + (index * 10)] if index > 0 else [],
                    metadata={"snippet": text[:300]},
                )
            )
        return offers

    @staticmethod
    def _build_queries(
        request: FlightSearchRequest,
        provider_candidates: list[ProviderCandidate],
    ) -> list[str]:
        """候補 website を優先した検索クエリを構築する."""
        base_query = (
            f"round trip flight {request.origin} to {request.destination} "
            f"{request.depart_window.start_date} {request.return_window.start_date}"
        )
        queries: list[str] = []
        for candidate in provider_candidates[:3]:
            if candidate.domain:
                queries.append(f"site:{candidate.domain} {base_query}")
        queries.append(base_query)
        return queries


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
        rooms = []
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
            self._send_email_sync(destination, payload)
            status = "sent"
            error: str | None = None
        except Exception as exc:  # pragma: no cover - SMTP env dependent
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

    @staticmethod
    def _send_email_sync(destination: str, payload: dict[str, Any]) -> None:
        """同期でメール送信する."""
        smtp_host = ""
        smtp_port = 587
        from_email = ""
        username = ""
        password = ""
        import os

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


class FlightWatchService:
    """機票検索・監視・通知を担当するサービス."""

    def __init__(
        self,
        *,
        store: SQLiteMessagingHubStore,
        websocket_hub: WebSocketHub,
        skill_gateway: SkillGateway | None = None,
    ) -> None:
        """初期化."""
        self._store = store
        self._providers: dict[str, FlightSearchProvider] = {
            "fake": FakeFlightProvider(),
            "web": WebAggregatorFlightProvider(skill_gateway=skill_gateway),
        }
        self._provider_discovery = TaskProviderDiscoveryService(skill_gateway=skill_gateway)
        self._notification_service = FlightNotificationService(store=store, websocket_hub=websocket_hub)

    async def search(self, request: FlightSearchRequest) -> FlightSearchResult:
        """検索して ranked result を返す."""
        provider_names = self._resolve_provider_order(request.provider)
        provider_candidates = await self.discover_sources(request)
        offers: list[FlightOffer] = []
        provider_used = provider_names[0]
        for provider_name in provider_names:
            provider_used = provider_name
            provider = self._providers[provider_name]
            offers = await provider.search(request, provider_candidates=provider_candidates)
            if offers:
                break
        ranked = self._rank_offers(offers, request.ranking_weights, request.max_stops, request.budget)
        recommended = ranked[0] if ranked else None
        return FlightSearchResult(
            offers=ranked,
            ranking_weights=request.ranking_weights,
            provider_used=provider_used,
            recommended_offer=recommended,
            metadata={
                "requested_provider": request.provider,
                "provider_candidates": [candidate.model_dump(mode="json") for candidate in provider_candidates],
            },
        )

    async def discover_sources(self, request: FlightSearchRequest) -> list[ProviderCandidate]:
        """検索に使う候補 website を先に発見する."""
        keywords = [
            request.origin.lower(),
            request.destination.lower(),
            "flight",
            "airfare",
            "travel",
        ]
        return await self._provider_discovery.discover(
            query=(f"best websites to compare round trip flight prices {request.origin} {request.destination}"),
            task_kind="structured_monitoring",
            keywords=keywords,
            candidate_limit=5,
        )

    async def create_subscription(
        self,
        *,
        request: FlightSearchRequest,
        user_id: str,
        conversation_id: str | None,
    ) -> FlightWatchSubscription:
        """監視購読を作成する."""
        search_result = await self.search(request)
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

    async def update_subscription_status(
        self, subscription_id: str, status: SubscriptionStatus
    ) -> dict[str, Any] | None:
        """購読状態を更新する."""
        subscription = await self._store.get_flight_watch_subscription(subscription_id)
        if subscription is None:
            return None
        subscription["status"] = status.value
        subscription["updated_at"] = datetime.now(UTC).isoformat()
        await self._store.upsert_flight_watch_subscription(subscription)
        return subscription

    async def check_due_subscriptions(self) -> dict[str, Any]:
        """期限到来した購読をチェックする."""
        now_iso = datetime.now(UTC).isoformat()
        due = await self._store.list_flight_watch_subscriptions(
            status=SubscriptionStatus.ACTIVE.value, due_before=now_iso
        )
        checked = 0
        notified = 0
        for raw_subscription in due:
            subscription = FlightWatchSubscription.model_validate(raw_subscription)
            result = await self.search(subscription.request)
            lowest_price = result.recommended_offer.price if result.recommended_offer is not None else None
            await self._store.add_flight_offer_snapshot(
                subscription_id=subscription.subscription_id,
                provider=result.provider_used,
                lowest_price=lowest_price,
                offers=[offer.model_dump() for offer in result.offers],
            )
            checked += 1
            should_notify, reason = self._should_notify(subscription, lowest_price)
            subscription.last_checked_at = now_iso
            subscription.next_check_at = (
                datetime.now(UTC) + timedelta(hours=subscription.request.poll_interval_hours)
            ).isoformat()
            if should_notify and result.offers:
                await self._notification_service.notify_price_drop(
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

    @staticmethod
    def extract_request_from_message(message: str) -> tuple[dict[str, Any], list[str]]:
        """自然言語メッセージから部分的な検索条件を抽出する."""
        partial: dict[str, Any] = {}
        origin_match = _ORIGIN_PATTERN.search(message)
        if origin_match:
            partial["origin"] = origin_match.group("origin").strip().upper()
        destination_match = _DESTINATION_PATTERN.search(message)
        if destination_match:
            partial["destination"] = destination_match.group("destination").strip().upper()

        for match in _DATE_RANGE_PATTERN.finditer(message):
            label = match.group("label").lower()
            start_date = match.group("start")
            end_date = match.group("end") or start_date
            window = {"start_date": start_date, "end_date": end_date}
            if label in {"depart", "departure", "出发", "去程", "往路"}:
                partial["depart_window"] = window
            else:
                partial["return_window"] = window

        budget_match = _BUDGET_PATTERN.search(message)
        if budget_match:
            partial["budget"] = float(budget_match.group(1))
        stops_match = _MAX_STOPS_PATTERN.search(message)
        if stops_match:
            partial["max_stops"] = int(stops_match.group(1))
        if _WATCH_PATTERN.search(message):
            partial["create_watch"] = True

        missing_fields = [
            field_name
            for field_name in ("origin", "destination", "depart_window", "return_window")
            if field_name not in partial
        ]
        return partial, missing_fields

    @staticmethod
    def build_clarification_questions(partial: dict[str, Any], missing_fields: list[str]) -> list[dict[str, Any]]:
        """不足項目に対応する補足質問を構築する."""
        questions: list[dict[str, Any]] = []
        if "origin" in missing_fields:
            questions.append({"id": "origin", "text": "出発地を入力してください", "type": "text", "required": True})
        if "destination" in missing_fields:
            questions.append(
                {"id": "destination", "text": "目的地を入力してください", "type": "text", "required": True}
            )
        if "depart_window" in missing_fields:
            questions.append(
                {
                    "id": "depart_window",
                    "text": "往路の日付範囲を YYYY-MM-DD..YYYY-MM-DD 形式で入力してください",
                    "type": "text",
                    "required": True,
                }
            )
        if "return_window" in missing_fields:
            questions.append(
                {
                    "id": "return_window",
                    "text": "復路の日付範囲を YYYY-MM-DD..YYYY-MM-DD 形式で入力してください",
                    "type": "text",
                    "required": True,
                }
            )
        if "create_watch" not in partial:
            questions.append(
                {
                    "id": "create_watch",
                    "text": "値下がり監視を有効にしますか？",
                    "type": "select",
                    "required": True,
                    "options": ["yes", "no"],
                }
            )
        return questions

    @staticmethod
    def merge_clarification_answers(
        partial_request: dict[str, Any],
        answers: dict[str, Any],
    ) -> dict[str, Any]:
        """clarification 回答を検索条件へ反映する."""
        merged = dict(partial_request)
        for key, value in answers.items():
            if key in {"depart_window", "return_window"} and isinstance(value, str) and ".." in value:
                start_date, end_date = value.split("..", 1)
                merged[key] = {"start_date": start_date.strip(), "end_date": end_date.strip()}
                continue
            if key == "create_watch":
                merged[key] = str(value).strip().lower() in {"true", "1", "yes", "y"}
                continue
            merged[key] = value
        return merged

    @staticmethod
    def _resolve_provider_order(provider: str) -> list[str]:
        """provider 試行順を返す."""
        if provider == "web":
            return ["web", "fake"]
        if provider == "fake":
            return ["fake"]
        return ["web", "fake"]

    @staticmethod
    def _rank_offers(
        offers: list[FlightOffer],
        weights: RankingWeights,
        max_stops: int | None,
        budget: float | None,
    ) -> list[FlightOffer]:
        """重み付きスコアで並び替える."""
        filtered = []
        for offer in offers:
            if max_stops is not None and offer.stops > max_stops:
                continue
            if budget is not None and offer.price > budget:
                continue
            filtered.append(offer)
        if not filtered:
            return []

        prices = [offer.price for offer in filtered]
        durations = [offer.total_duration_minutes for offer in filtered]
        min_price = min(prices)
        max_price = max(prices)
        min_duration = min(durations)
        max_duration = max(durations)

        ranked: list[FlightOffer] = []
        for offer in filtered:
            price_score = FlightWatchService._normalize_inverse(offer.price, min_price, max_price)
            duration_score = FlightWatchService._normalize_inverse(
                float(offer.total_duration_minutes),
                float(min_duration),
                float(max_duration),
            )
            convenience_score = FlightWatchService._calculate_convenience_score(offer)
            total_score = (
                price_score * weights.price
                + duration_score * weights.duration
                + convenience_score * weights.convenience
            )
            offer.score = round(total_score, 4)
            offer.score_breakdown = {
                "price": round(price_score, 4),
                "duration": round(duration_score, 4),
                "convenience": round(convenience_score, 4),
            }
            ranked.append(offer)

        ranked.sort(key=lambda item: (-item.score, item.price, item.total_duration_minutes))
        return ranked

    @staticmethod
    def _normalize_inverse(value: float, minimum: float, maximum: float) -> float:
        """低いほど高得点の正規化."""
        if maximum <= minimum:
            return 1.0
        return max(0.0, 1.0 - ((value - minimum) / (maximum - minimum)))

    @staticmethod
    def _calculate_convenience_score(offer: FlightOffer) -> float:
        """利便性スコアを計算する."""
        score = 1.0
        score -= min(offer.stops * 0.2, 0.6)
        if offer.red_eye:
            score -= 0.12
        if offer.airport_change:
            score -= 0.18
        if offer.layover_minutes:
            spread = max(offer.layover_minutes) - min(offer.layover_minutes)
            score -= min(spread / 600.0, 0.1)
        return max(0.0, round(score, 4))

    @staticmethod
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
