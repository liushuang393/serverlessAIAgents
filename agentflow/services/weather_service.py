"""Weather Service - Open-Meteo 天気取得サービス.

都市名を入力として Open-Meteo API から現在天気と予報を取得する。
API キー不要で利用できるため、FAQ 系アプリの軽量な外部情報取得に適用する。
"""

from __future__ import annotations

import time
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import httpx

from agentflow.services.base import ServiceBase, ServiceEvent


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


_GEOCODING_URL = "https://geocoding-api.open-meteo.com/v1/search"
_FORECAST_URL = "https://api.open-meteo.com/v1/forecast"


_WEATHER_CODE_TEXT: dict[int, str] = {
    0: "晴朗",
    1: "大致晴朗",
    2: "局部多云",
    3: "阴天",
    45: "有雾",
    48: "雾凇",
    51: "小毛毛雨",
    53: "毛毛雨",
    55: "强毛毛雨",
    56: "冻毛毛雨",
    57: "强冻毛毛雨",
    61: "小雨",
    63: "中雨",
    65: "大雨",
    66: "冻雨",
    67: "强冻雨",
    71: "小雪",
    73: "中雪",
    75: "大雪",
    77: "雪粒",
    80: "阵雨",
    81: "较强阵雨",
    82: "强阵雨",
    85: "阵雪",
    86: "强阵雪",
    95: "雷暴",
    96: "雷暴伴小冰雹",
    99: "雷暴伴大冰雹",
}


@dataclass
class WeatherConfig:
    """WeatherService 設定."""

    timeout_seconds: float = 8.0
    default_days: int = 3
    max_days: int = 7
    language: str = "zh"


class WeatherService(ServiceBase[dict[str, Any]]):
    """Open-Meteo を使って天気予報を返すサービス."""

    def __init__(self, config: WeatherConfig | None = None) -> None:
        super().__init__()
        self._config = config or WeatherConfig()

    async def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        action = str(kwargs.get("action", "forecast")).strip().lower()
        if action != "forecast":
            yield self._emit_error(
                execution_id,
                "invalid_action",
                f"Unknown action: {action}",
            )
            return

        city = str(kwargs.get("city", "")).strip()
        if not city:
            yield self._emit_error(
                execution_id,
                "city_required",
                "city is required",
            )
            return

        days = self._normalize_days(kwargs.get("days"))
        started_at = time.time()

        yield self._emit_progress(
            execution_id,
            10,
            "都市名を解決中...",
            phase="geocoding",
        )

        try:
            location = await self._geocode_city(city)
        except Exception:
            yield self._emit_error(
                execution_id,
                "weather_api_unavailable",
                "天气服务暂时不可用，请稍后再试。",
            )
            return

        if location is None:
            yield self._emit_error(
                execution_id,
                "city_not_found",
                f"未找到城市：{city}",
            )
            return

        yield self._emit_progress(
            execution_id,
            50,
            "获取天气预报中...",
            phase="forecast",
        )

        try:
            forecast = await self._fetch_forecast(
                latitude=float(location["latitude"]),
                longitude=float(location["longitude"]),
                days=days,
            )
            payload = self._build_payload(location=location, forecast=forecast, days=days)
        except ValueError:
            yield self._emit_error(
                execution_id,
                "weather_parse_error",
                "天气数据解析失败，请稍后再试。",
            )
            return
        except Exception:
            yield self._emit_error(
                execution_id,
                "weather_api_unavailable",
                "天气服务暂时不可用，请稍后再试。",
            )
            return

        yield self._emit_progress(
            execution_id,
            100,
            "完了",
            phase="complete",
        )

        yield self._emit_result(
            execution_id,
            payload,
            duration_ms=(time.time() - started_at) * 1000,
        )

    def _normalize_days(self, value: Any) -> int:
        try:
            parsed = int(value)
        except (TypeError, ValueError):
            parsed = self._config.default_days
        return max(1, min(self._config.max_days, parsed))

    async def _geocode_city(self, city: str) -> dict[str, Any] | None:
        params = {
            "name": city,
            "count": 1,
            "language": self._config.language,
            "format": "json",
        }
        async with httpx.AsyncClient(timeout=self._config.timeout_seconds) as client:
            response = await client.get(_GEOCODING_URL, params=params)
            response.raise_for_status()
            payload = response.json()

        results = payload.get("results")
        if not isinstance(results, list) or not results:
            return None

        first = results[0]
        if not isinstance(first, dict):
            return None

        latitude = first.get("latitude")
        longitude = first.get("longitude")
        if not isinstance(latitude, (int, float)) or not isinstance(longitude, (int, float)):
            raise ValueError("invalid geocoding coordinates")

        return first

    async def _fetch_forecast(
        self,
        *,
        latitude: float,
        longitude: float,
        days: int,
    ) -> dict[str, Any]:
        params = {
            "latitude": latitude,
            "longitude": longitude,
            "current": "temperature_2m,weather_code,wind_speed_10m",
            "daily": (
                "weather_code,temperature_2m_max,temperature_2m_min,"
                "precipitation_probability_max"
            ),
            "timezone": "auto",
            "forecast_days": days,
        }
        async with httpx.AsyncClient(timeout=self._config.timeout_seconds) as client:
            response = await client.get(_FORECAST_URL, params=params)
            response.raise_for_status()
            payload: dict[str, Any] = response.json()
        return payload

    def _build_payload(
        self,
        *,
        location: dict[str, Any],
        forecast: dict[str, Any],
        days: int,
    ) -> dict[str, Any]:
        current = forecast.get("current")
        daily = forecast.get("daily")
        if not isinstance(current, dict) or not isinstance(daily, dict):
            raise ValueError("invalid forecast payload")

        weather_code = self._safe_int(current.get("weather_code"))
        if weather_code is None:
            raise ValueError("invalid weather_code")

        current_payload = {
            "time": str(current.get("time", "")),
            "temperature_c": self._safe_float(current.get("temperature_2m")),
            "wind_speed_kmh": self._safe_float(current.get("wind_speed_10m")),
            "weather_code": weather_code,
            "weather_text": self._weather_text(weather_code),
        }

        daily_payload = self._build_daily_payload(daily, days)

        city_name = str(location.get("name", "")).strip()
        country_name = str(location.get("country", "")).strip()
        admin1 = str(location.get("admin1", "")).strip()

        return {
            "city": city_name,
            "country": country_name,
            "region": admin1,
            "latitude": self._safe_float(location.get("latitude")),
            "longitude": self._safe_float(location.get("longitude")),
            "timezone": str(forecast.get("timezone", "")),
            "current": current_payload,
            "daily": daily_payload,
            "days": days,
        }

    def _build_daily_payload(self, daily: dict[str, Any], days: int) -> list[dict[str, Any]]:
        times = daily.get("time")
        codes = daily.get("weather_code")
        temp_max = daily.get("temperature_2m_max")
        temp_min = daily.get("temperature_2m_min")
        rain_prob = daily.get("precipitation_probability_max")

        if not all(isinstance(item, list) for item in [times, codes, temp_max, temp_min, rain_prob]):
            raise ValueError("invalid daily payload")

        forecast_days: list[dict[str, Any]] = []
        upper_bound = min(days, len(times), len(codes), len(temp_max), len(temp_min), len(rain_prob))
        for idx in range(upper_bound):
            code = self._safe_int(codes[idx])
            if code is None:
                raise ValueError("invalid daily weather code")
            forecast_days.append(
                {
                    "date": str(times[idx]),
                    "weather_code": code,
                    "weather_text": self._weather_text(code),
                    "temperature_max_c": self._safe_float(temp_max[idx]),
                    "temperature_min_c": self._safe_float(temp_min[idx]),
                    "precipitation_probability_max": self._safe_float(rain_prob[idx]),
                }
            )
        return forecast_days

    @staticmethod
    def _safe_int(value: Any) -> int | None:
        try:
            return int(value)
        except (TypeError, ValueError):
            return None

    @staticmethod
    def _safe_float(value: Any) -> float | None:
        try:
            return float(value)
        except (TypeError, ValueError):
            return None

    def _weather_text(self, code: int) -> str:
        return _WEATHER_CODE_TEXT.get(code, f"未知天气({code})")


__all__ = ["WeatherConfig", "WeatherService"]
