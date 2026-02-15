# -*- coding: utf-8 -*-
"""Capability registry and ontology mapping."""

from __future__ import annotations

import re
from collections import OrderedDict
from collections.abc import Iterable
from dataclasses import dataclass
from typing import Any

from apps.platform.schemas.capability_schemas import (
    CAPABILITY_DOMAINS,
    CanonicalCapability,
    CapabilityAggregate,
)


_LEGACY_ALIAS_MAP: dict[str, str] = {
    # knowledge
    "rag": "knowledge.retrieval.rag",
    "retrieval": "knowledge.retrieval.search",
    "search": "knowledge.retrieval.search",
    "semantic_search": "knowledge.retrieval.semantic_search",
    "faq": "knowledge.faq.answering",
    "knowledge": "knowledge.management.general",
    "indexing": "knowledge.indexing.general",
    "document": "knowledge.document.processing",
    # reasoning
    "analysis": "reasoning.analysis.general",
    "trend_detection": "reasoning.analysis.trend_detection",
    "prediction": "reasoning.forecast.prediction",
    "signal_scoring": "reasoning.scoring.signal",
    "ranking": "reasoning.scoring.ranking",
    "benchmarking": "reasoning.analysis.benchmarking",
    "categorization": "reasoning.classification.categorization",
    # interaction
    "chat": "interaction.conversation.chat",
    "chatbot": "interaction.conversation.chatbot",
    "conversation": "interaction.conversation.general",
    "summary": "interaction.reporting.summary",
    "summarization": "interaction.reporting.summarization",
    "notification": "interaction.notification.dispatch",
    "alerting": "interaction.notification.alerting",
    "meeting": "interaction.collaboration.meeting",
    "notes": "interaction.collaboration.notes",
    # integration
    "sql": "integration.data.sql",
    "calendar": "integration.calendar.management",
    "web_scraping": "integration.ingest.web_scraping",
    "data_collection": "integration.ingest.collection",
    "intent_routing": "integration.routing.intent",
    # operations
    "coordination": "operations.orchestration.coordination",
    "orchestration": "operations.orchestration.general",
    "migration": "operations.migration.code",
    "code_transform": "operations.transformation.code",
    "test_generation": "operations.quality.test_generation",
    "auto_fix": "operations.quality.auto_fix",
    "file_organization": "operations.management.file_organization",
    # governance
    "validation": "governance.validation.general",
    "review": "governance.review.general",
    "audit": "governance.audit.general",
    "evidence_tracking": "governance.audit.evidence_tracking",
    "gate": "governance.gate.general",
    "cognitive_check": "governance.gate.cognitive_check",
    # media
    "chart": "media.visualization.chart",
    "image_generation": "media.generation.image",
    "design_skills": "media.design.skills",
    "vision": "media.perception.vision",
    "voice": "media.perception.voice",
}

_DOMAIN_KEYWORDS: dict[str, tuple[str, ...]] = {
    "knowledge": (
        "rag",
        "search",
        "retriev",
        "faq",
        "knowledge",
        "document",
        "index",
        "embedding",
        "chunk",
    ),
    "reasoning": (
        "analysis",
        "reason",
        "predict",
        "score",
        "rank",
        "classif",
        "trend",
        "diagnosis",
    ),
    "interaction": (
        "chat",
        "conversation",
        "summary",
        "report",
        "meeting",
        "notes",
        "notification",
        "assist",
        "sales",
    ),
    "integration": (
        "sql",
        "calendar",
        "api",
        "gateway",
        "webhook",
        "slack",
        "telegram",
        "discord",
        "routing",
    ),
    "operations": (
        "deploy",
        "migration",
        "transform",
        "orchestration",
        "coordination",
        "test",
        "fix",
        "build",
        "scaffold",
        "manage",
    ),
    "governance": (
        "gate",
        "validation",
        "review",
        "audit",
        "evidence",
        "compliance",
        "policy",
        "rule",
    ),
    "media": (
        "image",
        "design",
        "vision",
        "voice",
        "chart",
        "video",
        "audio",
        "creative",
    ),
}

_SEGMENT_RE = re.compile(r"[^a-z0-9]+")


@dataclass(slots=True)
class _ParsedCapability:
    id: str
    domain: str
    task: str
    qualifier: str | None


class CapabilityRegistry:
    """Capability ontology mapper."""

    def canonicalize(self, raw: str) -> CanonicalCapability:
        """Convert legacy capability/tag string to canonical object."""
        normalized = self._normalize(raw)
        if not normalized:
            normalized = "unknown"

        canonical_id = _LEGACY_ALIAS_MAP.get(normalized)
        if canonical_id is None:
            domain = self._infer_domain(normalized)
            canonical_id = self._build_id(domain, normalized)

        parsed = self._parse_id(canonical_id)
        return CanonicalCapability(
            id=parsed.id,
            domain=parsed.domain,
            task=parsed.task,
            qualifier=parsed.qualifier,
            label=self._build_label(parsed),
            aliases=[raw],
        )

    def canonicalize_many(self, raw_values: Iterable[str]) -> list[CanonicalCapability]:
        """Canonicalize and deduplicate capabilities preserving order."""
        dedup: OrderedDict[str, CanonicalCapability] = OrderedDict()
        for raw in raw_values:
            cap = self.canonicalize(raw)
            existed = dedup.get(cap.id)
            if existed is None:
                dedup[cap.id] = cap
            else:
                aliases = set(existed.aliases)
                aliases.update(cap.aliases)
                existed.aliases = sorted(aliases)
        return list(dedup.values())

    def aggregate(
        self,
        items: Iterable[tuple[str, list[CanonicalCapability]]],
    ) -> list[dict[str, Any]]:
        """Aggregate canonical capabilities by app."""
        stats: dict[str, dict[str, Any]] = {}
        for app_name, capabilities in items:
            for cap in capabilities:
                bucket = stats.setdefault(
                    cap.id,
                    {
                        "capability": cap,
                        "count": 0,
                        "apps": set(),
                        "aliases": set(),
                    },
                )
                bucket["count"] += 1
                bucket["apps"].add(app_name)
                bucket["aliases"].update(cap.aliases)

        aggregates: list[CapabilityAggregate] = []
        for item in sorted(stats.values(), key=lambda i: (-i["count"], i["capability"].id)):
            cap = item["capability"]
            aggregates.append(
                CapabilityAggregate(
                    id=cap.id,
                    domain=cap.domain,
                    task=cap.task,
                    qualifier=cap.qualifier,
                    label=cap.label,
                    aliases=sorted(item["aliases"]),
                    count=item["count"],
                    apps=sorted(item["apps"]),
                ),
            )
        return [a.model_dump() for a in aggregates]

    @staticmethod
    def _normalize(raw: str) -> str:
        value = raw.strip().lower()
        value = _SEGMENT_RE.sub("_", value)
        value = re.sub(r"_+", "_", value).strip("_")
        return value

    @staticmethod
    def _build_id(domain: str, normalized: str) -> str:
        segments = [seg for seg in normalized.split("_") if seg]
        task = segments[0] if segments else "unknown"
        if len(segments) == 1:
            return f"{domain}.{task}"
        qualifier = ".".join(segments[1:])
        return f"{domain}.{task}.{qualifier}"

    @staticmethod
    def _build_label(parsed: _ParsedCapability) -> str:
        tail = parsed.task if parsed.qualifier is None else f"{parsed.task}.{parsed.qualifier}"
        label = tail.replace(".", " ").replace("_", " ").strip()
        return label.title()

    @staticmethod
    def _parse_id(capability_id: str) -> _ParsedCapability:
        parts = [p for p in capability_id.split(".") if p]
        if len(parts) < 2:
            return _ParsedCapability(
                id=f"custom.{capability_id}" if capability_id else "custom.unknown",
                domain="custom",
                task=parts[0] if parts else "unknown",
                qualifier=None,
            )

        domain = parts[0]
        task = parts[1]
        qualifier = ".".join(parts[2:]) if len(parts) > 2 else None
        return _ParsedCapability(
            id=capability_id,
            domain=domain,
            task=task,
            qualifier=qualifier,
        )

    def _infer_domain(self, normalized: str) -> str:
        for domain in CAPABILITY_DOMAINS:
            keywords = _DOMAIN_KEYWORDS.get(domain, ())
            if any(keyword in normalized for keyword in keywords):
                return domain
        return "custom"

