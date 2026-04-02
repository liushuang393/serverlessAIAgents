"""Discovered-site flight offer extractor."""

from __future__ import annotations

import re
import uuid
from typing import TYPE_CHECKING, Any

from apps.messaging_hub.task_harness import ProviderCandidate


if TYPE_CHECKING:
    from kernel.skills.gateway import SkillGateway


_HTML_TAG_PATTERN = re.compile(r"<[^>]+>")
_PRICE_PATTERN = re.compile(
    r"(?:USD|\$)\s*(?P<p1>[0-9]{2,5}(?:\.[0-9]+)?)|(?P<p2>[0-9]{2,5}(?:\.[0-9]+)?)\s*(?:USD|usd)",
    re.IGNORECASE,
)
_DURATION_PATTERN = re.compile(
    r"(?:(?P<hours>[0-9]{1,2})\s*h(?:ours?)?\s*(?P<minutes>[0-9]{1,2})?\s*m?)|(?P<total>[0-9]{2,4})\s*min",
    re.IGNORECASE,
)
_STOPS_PATTERN = re.compile(r"(?P<count>[0-3])\s*stop|non[-\s]?stop", re.IGNORECASE)


class DiscoveredFlightOfferExtractor:
    """Discovery-first で見つけたサイトから構造化 offer を抽出する."""

    def __init__(self, skill_gateway: SkillGateway | None = None) -> None:
        """初期化."""
        self._gateway = skill_gateway
        self._skill_names = (
            {skill.name for skill in skill_gateway.list_available_skills()}
            if skill_gateway is not None
            else set()
        )

    async def extract(
        self,
        *,
        request: Any,
        provider_candidates: list[ProviderCandidate],
    ) -> list[Any]:
        """候補サイトから offer を抽出する."""
        offers: list[Any] = []
        for candidate in provider_candidates[:3]:
            page_text = await self._fetch_candidate_text(candidate)
            if not page_text:
                continue
            offers.extend(
                self._offers_from_text(
                    text=page_text,
                    candidate=candidate,
                    request=request,
                )
            )
            if offers:
                break
        return offers

    async def _fetch_candidate_text(self, candidate: ProviderCandidate) -> str:
        """候補サイトの本文を取得する."""
        if self._gateway is None:
            return ""
        if "http_request" in self._skill_names:
            text = await self._fetch_via_http(candidate.url)
            if text:
                return text
        if {"browser_navigate", "browser_get_text"}.issubset(self._skill_names):
            return await self._fetch_via_browser(candidate.url)
        return candidate.snippet

    async def _fetch_via_http(self, url: str) -> str:
        """HTTP で本文を取得する."""
        try:
            result = await self._gateway.call(
                "http_request",
                {
                    "method": "GET",
                    "url": url,
                    "headers": {"accept": "text/html,application/xhtml+xml"},
                },
            )
        except Exception:
            return ""
        if not result.success or not isinstance(result.result, dict):
            return ""
        if int(result.result.get("status_code", 0)) >= 400:
            return ""
        body = str(result.result.get("body", "")).strip()
        if not body:
            return ""
        return self._normalize_text(body)

    async def _fetch_via_browser(self, url: str) -> str:
        """ブラウザで本文を取得する."""
        try:
            nav_result = await self._gateway.call("browser_navigate", {"url": url})
        except Exception:
            return ""
        if not nav_result.success:
            return ""
        try:
            text_result = await self._gateway.call("browser_get_text", {"selector": "body"})
        except Exception:
            return ""
        if not text_result.success or not isinstance(text_result.result, dict):
            return ""
        return self._normalize_text(str(text_result.result.get("text", "")))

    def _offers_from_text(
        self,
        *,
        text: str,
        candidate: ProviderCandidate,
        request: Any,
    ) -> list[Any]:
        """本文から offer 一覧を抽出する."""
        from apps.messaging_hub.flight_watch import FlightOffer

        prices = self._extract_prices(text)
        if not prices:
            return []
        durations = self._extract_durations(text)
        stops = self._extract_stops(text)
        carrier = self._carrier_name(candidate)

        offers: list[Any] = []
        for index, price in enumerate(prices[:3]):
            duration = durations[index] if index < len(durations) else 720 + (index * 30)
            stop_count = stops[index] if index < len(stops) else 0
            offers.append(
                FlightOffer(
                    offer_id=f"web_discovered_{uuid.uuid4().hex[:8]}",
                    provider="web",
                    origin=str(getattr(request, "origin", "")).upper(),
                    destination=str(getattr(request, "destination", "")).upper(),
                    depart_date=str(getattr(request.depart_window, "start_date", "")),
                    return_date=str(getattr(request.return_window, "start_date", "")),
                    price=price,
                    currency="USD",
                    total_duration_minutes=duration,
                    stops=stop_count,
                    carrier=carrier,
                    red_eye=False,
                    airport_change=False,
                    layover_minutes=[90] if stop_count > 0 else [],
                    metadata={
                        "source_url": candidate.url,
                        "source_domain": candidate.domain,
                        "rationale": candidate.rationale,
                    },
                )
            )
        return offers

    @staticmethod
    def _normalize_text(raw_text: str) -> str:
        """HTML/空白を粗く正規化する."""
        stripped = _HTML_TAG_PATTERN.sub(" ", raw_text)
        return " ".join(stripped.split())[:12000]

    @staticmethod
    def _extract_prices(text: str) -> list[float]:
        """価格一覧を抽出する."""
        prices: list[float] = []
        for match in _PRICE_PATTERN.finditer(text):
            value = match.group("p1") or match.group("p2")
            if value is None:
                continue
            parsed = float(value)
            if parsed < 50.0:
                continue
            if parsed not in prices:
                prices.append(parsed)
        return prices

    @staticmethod
    def _extract_durations(text: str) -> list[int]:
        """所要時間一覧を抽出する."""
        durations: list[int] = []
        for match in _DURATION_PATTERN.finditer(text):
            total = match.group("total")
            if total:
                duration = int(total)
            else:
                hours = int(match.group("hours") or "0")
                minutes = int(match.group("minutes") or "0")
                duration = (hours * 60) + minutes
            if duration <= 0:
                continue
            durations.append(duration)
        return durations

    @staticmethod
    def _extract_stops(text: str) -> list[int]:
        """乗継数一覧を抽出する."""
        stops: list[int] = []
        for match in _STOPS_PATTERN.finditer(text):
            matched_text = match.group(0).lower()
            if "non" in matched_text:
                stops.append(0)
                continue
            stops.append(int(match.group("count") or "0"))
        return stops

    @staticmethod
    def _carrier_name(candidate: ProviderCandidate) -> str:
        """候補サイトから provider 表示名を作る."""
        title = candidate.title.strip()
        if title:
            return title[:48]
        domain = candidate.domain.replace("www.", "")
        return domain.split(".")[0].replace("-", " ").title()
