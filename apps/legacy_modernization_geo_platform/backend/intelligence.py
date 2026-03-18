"""External intelligence integration for GEO demand discovery."""

from __future__ import annotations

import os
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone

from apps.legacy_modernization_geo_platform.backend.schemas import GeoExecuteRequest
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from apps.decision_governance_engine.services.intelligence_service import (
        IntelligenceConfig,
        IntelligenceResult,
        IntelligenceService,
    )


@dataclass(slots=True)
class NormalizedEvidenceSource:
    """Normalized live-search result used across the GEO pipeline."""

    url: str
    title: str
    publisher: str
    summary: str
    snippet: str
    reliability: str
    published_at: datetime | None
    retrieved_at: datetime
    provider: str
    tags: list[str]

    @property
    def is_fresh(self) -> bool:
        """Return whether the source is still considered fresh."""
        anchor = _ensure_utc(self.published_at or self.retrieved_at)
        return datetime.now(timezone.utc) - anchor <= timedelta(days=30)

    @property
    def citation_ready(self) -> bool:
        """Return whether this source has enough metadata to quote safely."""
        return bool(self.url and self.summary and (self.snippet or self.title))

    def to_evidence_dict(self) -> dict[str, str | bool]:
        """Convert the source into a JSON-serializable evidence payload."""
        return {
            "url": self.url,
            "title": self.title,
            "publisher": self.publisher,
            "summary": self.summary,
            "snippet": self.snippet,
            "reliability": self.reliability,
            "citation_ready": self.citation_ready,
            "fresh": self.is_fresh,
            "provider": self.provider,
        }


@dataclass(slots=True)
class IntelligenceSnapshot:
    """Aggregated result from the external search providers."""

    sources: list[NormalizedEvidenceSource]
    warnings: list[str]
    primary_provider: str


class GeoIntelligenceAdapter:
    """Wrap the shared intelligence service with GEO-specific defaults."""

    def __init__(self, service: "IntelligenceService | None" = None) -> None:
        """Initialize the live intelligence adapter."""
        if service is None:
            from apps.decision_governance_engine.services.intelligence_service import (
                IntelligenceConfig,
                IntelligenceService,
            )

            service = IntelligenceService(
                IntelligenceConfig(
                    provider_preference=["serpapi", "bing", "duckduckgo_search"],
                    mode="STANDARD",
                ),
            )
        self._service = service

    async def gather_market_intelligence(self, request: GeoExecuteRequest) -> IntelligenceSnapshot:
        """Collect normalized intelligence for the request."""
        if os.getenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE") == "1":
            return self._sample_snapshot(request)
        queries = self._build_queries(request)
        sources: list[NormalizedEvidenceSource] = []
        warnings: list[str] = []
        primary_provider = "none"

        for query in queries:
            result = await self._service.gather(query, topics=self._build_topics(request))
            if primary_provider == "none" and result.provider:
                primary_provider = result.provider
            warnings.extend(result.warnings)
            sources.extend(self._normalize_result(result))

        deduplicated = self._deduplicate_sources(sources)
        return IntelligenceSnapshot(
            sources=deduplicated[:8],
            warnings=warnings,
            primary_provider=primary_provider,
        )

    def _build_queries(self, request: GeoExecuteRequest) -> list[str]:
        """Build search queries from the execution request."""
        accounts = list(request.inputs.target_accounts)
        industries = list(request.targets.industries)
        stacks = list(request.targets.legacy_stacks)
        regions = request.inputs.regions or request.targets.regions or ["Japan"]
        region_text = " ".join(regions[:2])
        stack_text = " ".join(stacks[:2]) if stacks else "legacy modernization"

        queries: list[str] = []
        for account in accounts[:2]:
            queries.append(f"{account} {stack_text} modernization {region_text}")
        for industry in industries[:2]:
            queries.append(f"{industry} {stack_text} migration demand {region_text}")
        if not queries:
            queries.append(f"legacy modernization demand {region_text}")
        return queries

    def _build_topics(self, request: GeoExecuteRequest) -> list[str]:
        """Build related topics for the intelligence service."""
        topics = list(request.targets.legacy_stacks)
        topics.extend(request.inputs.target_services)
        topics.extend(request.targets.industries)
        return [item for item in topics if item][:5]

    def _normalize_result(self, result: IntelligenceResult) -> list[NormalizedEvidenceSource]:
        """Normalize the shared service response."""
        normalized: list[NormalizedEvidenceSource] = []
        for item in result.evidence:
            normalized.append(
                NormalizedEvidenceSource(
                    url=item.url,
                    title=item.title,
                    publisher=item.publisher or result.provider,
                    summary=item.summary or item.snippet,
                    snippet=item.snippet,
                    reliability=item.reliability.value,
                    published_at=item.published_at,
                    retrieved_at=item.retrieved_at,
                    provider=result.provider,
                    tags=list(item.tags),
                ),
            )
        return normalized

    def _deduplicate_sources(
        self,
        sources: list[NormalizedEvidenceSource],
    ) -> list[NormalizedEvidenceSource]:
        """Deduplicate results by URL while preserving order."""
        seen: set[str] = set()
        deduplicated: list[NormalizedEvidenceSource] = []
        for source in sources:
            if source.url in seen:
                continue
            seen.add(source.url)
            deduplicated.append(source)
        return deduplicated

    def _sample_snapshot(self, request: GeoExecuteRequest) -> IntelligenceSnapshot:
        """Return deterministic intelligence for automated tests."""
        stack = (request.targets.legacy_stacks or ["COBOL"])[0]
        industry = (request.targets.industries or ["manufacturing"])[0]
        now = datetime.now(timezone.utc)
        sources = [
            NormalizedEvidenceSource(
                url=f"https://example.test/{industry}/{stack.lower()}-modernization",
                title=f"{industry} {stack} modernization demand signal",
                publisher="sample-intel",
                summary=f"{industry} organizations are evaluating incremental {stack} modernization programs.",
                snippet="Sample evidence for automated tests.",
                reliability="HIGH",
                published_at=now,
                retrieved_at=now,
                provider="sample",
                tags=["sample", industry, stack],
            ),
            NormalizedEvidenceSource(
                url=f"https://example.test/{industry}/migration-roadmap",
                title=f"{industry} migration roadmap",
                publisher="sample-intel",
                summary="Roadmap evidence indicating phased migration is preferred over big bang rewrites.",
                snippet="Phased migration reduces delivery risk.",
                reliability="MEDIUM",
                published_at=now,
                retrieved_at=now,
                provider="sample",
                tags=["sample", "roadmap"],
            ),
            NormalizedEvidenceSource(
                url=f"https://example.test/{industry}/skills-gap",
                title="Legacy skills gap report",
                publisher="sample-intel",
                summary="The maintenance talent gap is driving modernization urgency.",
                snippet="Skills shortages increase modernization urgency.",
                reliability="HIGH",
                published_at=now,
                retrieved_at=now,
                provider="sample",
                tags=["sample", "skills-gap"],
            ),
        ]
        return IntelligenceSnapshot(sources=sources, warnings=[], primary_provider="sample")


def _ensure_utc(value: datetime) -> datetime:
    """Normalize datetimes to timezone-aware UTC."""
    if value.tzinfo is None:
        return value.replace(tzinfo=timezone.utc)
    return value.astimezone(timezone.utc)
