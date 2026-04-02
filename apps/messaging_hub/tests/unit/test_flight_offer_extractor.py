"""Flight offer extractor tests."""

from __future__ import annotations

from typing import Any

import pytest

from apps.messaging_hub.flight_offer_extractor import DiscoveredFlightOfferExtractor
from apps.messaging_hub.flight_watch import DateWindow, FlightSearchRequest
from apps.messaging_hub.task_harness import ProviderCandidate
from kernel.skills.gateway import RiskLevel, SkillCategory, SkillDefinition, SkillResult


class _ExtractorGateway:
    """Extractor 用 SkillGateway 代替."""

    def __init__(self) -> None:
        self._skills = [
            SkillDefinition(
                name="http_request",
                description="http",
                category=SkillCategory.NETWORK,
                risk_level=RiskLevel.LOW,
                handler=self._noop,
            ),
        ]

    async def _noop(self, *_args: Any, **_kwargs: Any) -> dict[str, Any]:
        return {}

    def list_available_skills(self) -> list[SkillDefinition]:
        return list(self._skills)

    async def call(self, skill_name: str, params: dict[str, Any]) -> SkillResult:
        assert skill_name == "http_request"
        assert params["url"] == "https://travel.test/flights"
        return SkillResult(
            success=True,
            skill_name=skill_name,
            result={
                "status_code": 200,
                "body": (
                    "<html><body>"
                    "Round trip fare USD 432 nonstop 11h 20m "
                    "Alternative fare $518 1 stop 13h 05m"
                    "</body></html>"
                ),
            },
        )


@pytest.mark.asyncio
async def test_extractor_parses_prices_and_durations_from_discovered_site() -> None:
    """候補サイト本文から価格・所要時間・乗継を抽出できること."""
    extractor = DiscoveredFlightOfferExtractor(skill_gateway=_ExtractorGateway())
    request = FlightSearchRequest(
        origin="HND",
        destination="LAX",
        depart_window=DateWindow(start_date="2026-05-01", end_date="2026-05-03"),
        return_window=DateWindow(start_date="2026-05-10", end_date="2026-05-12"),
    )
    offers = await extractor.extract(
        request=request,
        provider_candidates=[
            ProviderCandidate(
                domain="travel.test",
                url="https://travel.test/flights",
                title="Travel Test Flights",
                snippet="cheap airfare",
            )
        ],
    )

    assert len(offers) == 2
    assert offers[0].price == 432.0
    assert offers[0].total_duration_minutes == 680
    assert offers[0].stops == 0
    assert offers[1].stops == 1
