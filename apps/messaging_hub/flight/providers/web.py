"""Web/Browser skill を利用する検索プロバイダ."""

from __future__ import annotations

import logging
import re
import uuid
from typing import TYPE_CHECKING, Any


_logger = logging.getLogger(__name__)

from apps.messaging_hub.flight.models import FlightOffer, FlightSearchRequest


if TYPE_CHECKING:
    from apps.messaging_hub.flight_offer_extractor import DiscoveredFlightOfferExtractor
    from apps.messaging_hub.task_harness import ProviderCandidate
    from kernel.skills.gateway import SkillGateway


class WebAggregatorFlightProvider:
    """web/browser skill を利用する provider."""

    name = "web"

    def __init__(self, skill_gateway: SkillGateway | None = None) -> None:
        """初期化."""
        self._gateway = skill_gateway
        # 遅延インポートで循環依存を回避
        self._extractor: DiscoveredFlightOfferExtractor | None = None
        if skill_gateway is not None:
            from apps.messaging_hub.flight_offer_extractor import DiscoveredFlightOfferExtractor

            self._extractor = DiscoveredFlightOfferExtractor(skill_gateway)

    async def search(
        self,
        request: FlightSearchRequest,
        provider_candidates: list[ProviderCandidate] | None = None,
    ) -> list[FlightOffer]:
        """web_search を試行し、価格抽出できた場合のみ返す."""
        if self._gateway is None:
            return []

        if self._extractor is not None:
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
                _logger.warning("web_search 呼び出し失敗: query=%s", query, exc_info=True)
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
