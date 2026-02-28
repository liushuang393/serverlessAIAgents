from __future__ import annotations

from typing import TYPE_CHECKING, Any

import httpx
import pytest

from agentflow.services import weather_service as weather_module
from agentflow.services.weather_service import WeatherService


if TYPE_CHECKING:
    from collections.abc import Callable


def _mock_client_factory(
    transport: httpx.MockTransport,
    original_cls: type[httpx.AsyncClient],
) -> Callable[..., httpx.AsyncClient]:
    def _factory(*args: Any, **kwargs: Any) -> httpx.AsyncClient:
        kwargs["transport"] = transport
        return original_cls(*args, **kwargs)

    return _factory


@pytest.mark.asyncio
async def test_weather_service_forecast_success(monkeypatch: pytest.MonkeyPatch) -> None:
    def handler(request: httpx.Request) -> httpx.Response:
        host = request.url.host or ""
        if "geocoding-api.open-meteo.com" in host:
            return httpx.Response(
                200,
                json={
                    "results": [
                        {
                            "name": "Tokyo",
                            "country": "Japan",
                            "latitude": 35.6762,
                            "longitude": 139.6503,
                        }
                    ]
                },
            )
        if "api.open-meteo.com" in host:
            return httpx.Response(
                200,
                json={
                    "timezone": "Asia/Tokyo",
                    "current": {
                        "time": "2026-02-28T09:00",
                        "temperature_2m": 12.4,
                        "weather_code": 1,
                        "wind_speed_10m": 8.5,
                    },
                    "daily": {
                        "time": ["2026-02-28", "2026-03-01", "2026-03-02"],
                        "weather_code": [1, 3, 61],
                        "temperature_2m_max": [14.0, 12.0, 10.0],
                        "temperature_2m_min": [7.0, 6.0, 5.0],
                        "precipitation_probability_max": [10, 20, 80],
                    },
                },
            )
        return httpx.Response(404, json={})

    transport = httpx.MockTransport(handler)
    original_cls = httpx.AsyncClient
    monkeypatch.setattr(
        weather_module.httpx,
        "AsyncClient",
        _mock_client_factory(transport, original_cls),
    )

    service = WeatherService()
    result = await service.execute(action="forecast", city="Tokyo", days=3)

    assert result.success is True
    assert result.data["city"] == "Tokyo"
    assert result.data["country"] == "Japan"
    assert result.data["current"]["weather_text"] != ""
    assert len(result.data["daily"]) == 3


@pytest.mark.asyncio
async def test_weather_service_city_not_found(monkeypatch: pytest.MonkeyPatch) -> None:
    def handler(request: httpx.Request) -> httpx.Response:
        _ = request
        return httpx.Response(200, json={"results": []})

    transport = httpx.MockTransport(handler)
    original_cls = httpx.AsyncClient
    monkeypatch.setattr(
        weather_module.httpx,
        "AsyncClient",
        _mock_client_factory(transport, original_cls),
    )

    service = WeatherService()
    result = await service.execute(action="forecast", city="NoSuchCity", days=3)

    assert result.success is False
    assert result.error_code == "city_not_found"


@pytest.mark.asyncio
async def test_weather_service_api_unavailable(monkeypatch: pytest.MonkeyPatch) -> None:
    def handler(request: httpx.Request) -> httpx.Response:
        host = request.url.host or ""
        if "geocoding-api.open-meteo.com" in host:
            return httpx.Response(
                200,
                json={
                    "results": [
                        {
                            "name": "Beijing",
                            "country": "China",
                            "latitude": 39.9042,
                            "longitude": 116.4074,
                        }
                    ]
                },
            )
        return httpx.Response(503, json={"error": "service unavailable"})

    transport = httpx.MockTransport(handler)
    original_cls = httpx.AsyncClient
    monkeypatch.setattr(
        weather_module.httpx,
        "AsyncClient",
        _mock_client_factory(transport, original_cls),
    )

    service = WeatherService()
    result = await service.execute(action="forecast", city="Beijing", days=3)

    assert result.success is False
    assert result.error_code == "weather_api_unavailable"
