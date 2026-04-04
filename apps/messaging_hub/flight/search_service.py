"""フライト検索サービス — 検索のみの単一責務."""

from __future__ import annotations

from typing import TYPE_CHECKING

from apps.messaging_hub.flight.models import FlightOffer, FlightSearchRequest, FlightSearchResult
from apps.messaging_hub.flight.providers.fake import FakeFlightProvider
from apps.messaging_hub.flight.providers.web import WebAggregatorFlightProvider
from apps.messaging_hub.flight.ranking import rank_offers
from apps.messaging_hub.task_harness import ProviderCandidate, TaskProviderDiscoveryService


if TYPE_CHECKING:
    from apps.messaging_hub.flight.providers.base import FlightSearchProvider
    from kernel.skills.gateway import SkillGateway


class FlightSearchService:
    """機票検索サービス（検索 + ランキングのみ）."""

    def __init__(
        self,
        *,
        skill_gateway: SkillGateway | None = None,
    ) -> None:
        """初期化."""
        self._providers: dict[str, FlightSearchProvider] = {
            "fake": FakeFlightProvider(),
            "web": WebAggregatorFlightProvider(skill_gateway=skill_gateway),
        }
        self._provider_discovery = TaskProviderDiscoveryService(skill_gateway=skill_gateway)

    async def search(self, request: FlightSearchRequest) -> FlightSearchResult:
        """検索して ranked result を返す."""
        provider_names = _resolve_provider_order(request.provider)
        provider_candidates = await self.discover_sources(request)
        offers: list[FlightOffer] = []
        provider_used = provider_names[0]
        for provider_name in provider_names:
            provider_used = provider_name
            provider = self._providers[provider_name]
            offers = await provider.search(request, provider_candidates=provider_candidates)
            if offers:
                break
        ranked = rank_offers(offers, request.ranking_weights, request.max_stops, request.budget)
        recommended = ranked[0] if ranked else None
        return FlightSearchResult(
            offers=ranked,
            ranking_weights=request.ranking_weights,
            provider_used=provider_used,
            recommended_offer=recommended,
            metadata={
                "requested_provider": request.provider,
                "provider_candidates": [c.model_dump(mode="json") for c in provider_candidates],
            },
        )

    async def discover_sources(self, request: FlightSearchRequest) -> list[ProviderCandidate]:
        """検索に使う候補 website を先に発見する."""
        keywords = [request.origin.lower(), request.destination.lower(), "flight", "airfare", "travel"]
        return await self._provider_discovery.discover(
            query=f"best websites to compare round trip flight prices {request.origin} {request.destination}",
            task_kind="structured_monitoring",
            keywords=keywords,
            candidate_limit=5,
        )


def _resolve_provider_order(provider: str) -> list[str]:
    """provider 試行順を返す."""
    if provider == "web":
        return ["web", "fake"]
    if provider == "fake":
        return ["fake"]
    return ["web", "fake"]
