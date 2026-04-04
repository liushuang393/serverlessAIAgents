"""フライト提案のランキングエンジン."""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from apps.messaging_hub.flight.models import FlightOffer, RankingWeights


def rank_offers(
    offers: list[FlightOffer],
    weights: RankingWeights,
    max_stops: int | None = None,
    budget: float | None = None,
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
    min_price, max_price = min(prices), max(prices)
    min_duration, max_duration = min(durations), max(durations)

    ranked: list[FlightOffer] = []
    for offer in filtered:
        price_score = _normalize_inverse(offer.price, min_price, max_price)
        duration_score = _normalize_inverse(
            float(offer.total_duration_minutes),
            float(min_duration),
            float(max_duration),
        )
        convenience_score = _calculate_convenience_score(offer)
        total_score = (
            price_score * weights.price + duration_score * weights.duration + convenience_score * weights.convenience
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


def _normalize_inverse(value: float, minimum: float, maximum: float) -> float:
    """低いほど高得点の正規化."""
    if maximum <= minimum:
        return 1.0
    return max(0.0, 1.0 - ((value - minimum) / (maximum - minimum)))


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
