"""テスト向け deterministic フライト検索プロバイダ."""

from __future__ import annotations

import hashlib
from typing import TYPE_CHECKING

from apps.messaging_hub.flight.models import FlightOffer, FlightSearchRequest


if TYPE_CHECKING:
    from apps.messaging_hub.task_harness import ProviderCandidate


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
