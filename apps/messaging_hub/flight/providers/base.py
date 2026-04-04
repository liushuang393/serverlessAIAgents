"""検索プロバイダ Protocol."""

from __future__ import annotations

from typing import TYPE_CHECKING, Protocol


if TYPE_CHECKING:
    from apps.messaging_hub.flight.models import FlightOffer, FlightSearchRequest
    from apps.messaging_hub.task_harness import ProviderCandidate


class FlightSearchProvider(Protocol):
    """検索 provider interface."""

    async def search(
        self,
        request: FlightSearchRequest,
        provider_candidates: list[ProviderCandidate] | None = None,
    ) -> list[FlightOffer]:
        """検索結果一覧を返す."""
