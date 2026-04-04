"""機票検索・監視のドメインモデル定義."""

from __future__ import annotations

import re
from enum import StrEnum
from typing import Any

from pydantic import BaseModel, Field, model_validator


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


# === NLP パーサー（自然言語→構造化データ抽出）===

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


def build_clarification_questions(
    partial: dict[str, Any],
    missing_fields: list[str],
) -> list[dict[str, Any]]:
    """不足項目に対応する補足質問を構築する."""
    questions: list[dict[str, Any]] = []
    if "origin" in missing_fields:
        questions.append({"id": "origin", "text": "出発地を入力してください", "type": "text", "required": True})
    if "destination" in missing_fields:
        questions.append({"id": "destination", "text": "目的地を入力してください", "type": "text", "required": True})
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
