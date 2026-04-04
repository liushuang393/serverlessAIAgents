"""検索プロバイダ."""

from apps.messaging_hub.flight.providers.base import FlightSearchProvider
from apps.messaging_hub.flight.providers.fake import FakeFlightProvider
from apps.messaging_hub.flight.providers.web import WebAggregatorFlightProvider


__all__ = ["FakeFlightProvider", "FlightSearchProvider", "WebAggregatorFlightProvider"]
