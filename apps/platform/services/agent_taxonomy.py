# -*- coding: utf-8 -*-
"""Agent taxonomy inference service.

Platform で使用する業務基盤分類（business base）と
Agent pattern 分類の推定ロジックを提供する。
"""

from __future__ import annotations

from collections import Counter
from collections.abc import Iterable
import re

from apps.platform.services.capability_registry import CapabilityRegistry


BUSINESS_BASE_TYPES: tuple[str, ...] = (
    "platform",
    "knowledge",
    "reasoning",
    "interaction",
    "integration",
    "operations",
    "governance",
    "media",
    "custom",
)

ENGINE_PATTERN_TYPES: tuple[str, ...] = (
    "simple",
    "flow",
    "pipeline",
    "coordinator",
    "deep_agent",
    "custom",
)

AGENT_PATTERN_TYPES: tuple[str, ...] = (
    "specialist",
    "coordinator",
    "pipeline_stage",
    "gatekeeper",
    "reviewer",
    "analyzer",
    "executor",
    "router",
    "reporter",
    "custom",
)

_NORMALIZE_RE = re.compile(r"[^a-z0-9]+")

_BUSINESS_ALIAS_MAP: dict[str, str] = {
    "management": "platform",
    "manager": "platform",
    "platform": "platform",
    "knowledge": "knowledge",
    "rag": "knowledge",
    "reason": "reasoning",
    "analytics": "reasoning",
    "interaction": "interaction",
    "chat": "interaction",
    "messaging": "interaction",
    "integration": "integration",
    "connector": "integration",
    "operations": "operations",
    "migration": "operations",
    "governance": "governance",
    "compliance": "governance",
    "media": "media",
    "design": "media",
}

_ENGINE_ALIAS_MAP: dict[str, str] = {
    "simple": "simple",
    "flow": "flow",
    "pipeline": "pipeline",
    "coordinator": "coordinator",
    "deep_agent": "deep_agent",
    "deepagent": "deep_agent",
    "base_engine": "pipeline",
    "custom_engine": "custom",
}

_AGENT_PATTERN_ALIAS_MAP: dict[str, str] = {
    "specialist": "specialist",
    "coordinator": "coordinator",
    "orchestrator": "coordinator",
    "pipeline": "pipeline_stage",
    "pipeline_stage": "pipeline_stage",
    "gate": "gatekeeper",
    "gatekeeper": "gatekeeper",
    "reviewer": "reviewer",
    "validator": "reviewer",
    "checker": "reviewer",
    "verifier": "reviewer",
    "analyzer": "analyzer",
    "executor": "executor",
    "worker": "executor",
    "router": "router",
    "dispatcher": "router",
    "reporter": "reporter",
}

_APP_TAG_HINTS: dict[str, tuple[str, ...]] = {
    "platform": ("platform", "dashboard", "management"),
    "knowledge": ("rag", "faq", "knowledge", "retrieval", "search"),
    "reasoning": ("analysis", "trend", "prediction", "scoring"),
    "interaction": ("chat", "messaging", "meeting", "notification"),
    "integration": ("sql", "gateway", "webhook", "mcp"),
    "operations": ("migration", "deploy", "publish", "workflow", "pipeline"),
    "governance": ("governance", "decision", "audit", "compliance", "gate"),
    "media": ("design", "image", "video", "voice"),
}

_PATTERN_KEYWORDS: dict[str, tuple[str, ...]] = {
    "coordinator": ("coordinator", "orchestrator", "hub", "manager"),
    "gatekeeper": ("gatekeeper", "gate", "policy", "guard"),
    "reviewer": ("review", "checker", "verify", "validation", "quality", "compliance"),
    "analyzer": ("analy", "diagnos", "score", "trend", "insight"),
    "reporter": ("report", "summary"),
    "router": ("router", "route", "dispatch", "intent"),
    "executor": ("executor", "collector", "transform", "synthesis", "fixer", "worker"),
}


class AgentTaxonomyService:
    """業務基盤/Agent pattern の推定サービス."""

    def __init__(self, capability_registry: CapabilityRegistry | None = None) -> None:
        self._capability_registry = capability_registry or CapabilityRegistry()

    @staticmethod
    def normalize_business_base(value: str | None) -> str | None:
        """business base 値を正規化."""
        if value is None:
            return None
        normalized = AgentTaxonomyService._normalize_token(value)
        if not normalized:
            return None
        if normalized in BUSINESS_BASE_TYPES:
            return normalized
        return _BUSINESS_ALIAS_MAP.get(normalized, "custom")

    @staticmethod
    def normalize_engine_pattern(value: str | None) -> str | None:
        """engine pattern 値を正規化."""
        if value is None:
            return None
        normalized = AgentTaxonomyService._normalize_token(value)
        if not normalized:
            return None
        if normalized in ENGINE_PATTERN_TYPES:
            return normalized
        return _ENGINE_ALIAS_MAP.get(normalized, "custom")

    @staticmethod
    def normalize_agent_pattern(value: str | None) -> str | None:
        """Agent pattern 値を正規化."""
        if value is None:
            return None
        normalized = AgentTaxonomyService._normalize_token(value)
        if not normalized:
            return None
        if normalized in AGENT_PATTERN_TYPES:
            return normalized
        return _AGENT_PATTERN_ALIAS_MAP.get(normalized, "custom")

    def infer_app_business_base(
        self,
        *,
        app_name: str,
        tags: Iterable[str],
        contracts_rag_enabled: bool,
        agent_capabilities: Iterable[Iterable[str]],
    ) -> str:
        """App の business base を推定."""
        app_key = self._normalize_token(app_name)
        tags_norm = [self._normalize_token(tag) for tag in tags]
        tags_norm = [tag for tag in tags_norm if tag]

        if app_key == "platform" or "platform" in tags_norm:
            return "platform"

        domain_counter: Counter[str] = Counter()
        for capabilities in agent_capabilities:
            for capability in capabilities:
                canonical = self._capability_registry.canonicalize(str(capability))
                domain = canonical.domain
                if domain in BUSINESS_BASE_TYPES:
                    domain_counter[domain] += 1

        if domain_counter:
            return sorted(domain_counter.items(), key=lambda item: (-item[1], item[0]))[0][0]

        if contracts_rag_enabled or "rag" in tags_norm:
            return "knowledge"

        hint_counter: Counter[str] = Counter()
        for base, keywords in _APP_TAG_HINTS.items():
            for tag in tags_norm:
                if any(keyword in tag for keyword in keywords):
                    hint_counter[base] += 1
                    break
        if hint_counter:
            return sorted(hint_counter.items(), key=lambda item: (-item[1], item[0]))[0][0]

        return "custom"

    def infer_agent_business_base(
        self,
        *,
        raw_business_base: str | None,
        capabilities: Iterable[str],
        fallback_app_base: str,
    ) -> str:
        """Agent の business base を推定."""
        normalized = self.normalize_business_base(raw_business_base)
        if normalized is not None:
            return normalized

        domain_counter: Counter[str] = Counter()
        for capability in capabilities:
            canonical = self._capability_registry.canonicalize(str(capability))
            domain = canonical.domain
            if domain in BUSINESS_BASE_TYPES:
                domain_counter[domain] += 1

        if domain_counter:
            return sorted(domain_counter.items(), key=lambda item: (-item[1], item[0]))[0][0]

        fallback = self.normalize_business_base(fallback_app_base)
        return fallback or "custom"

    def infer_agent_pattern(
        self,
        *,
        raw_pattern: str | None,
        name: str,
        module: str | None,
        engine_pattern: str | None,
    ) -> str:
        """Agent pattern を推定."""
        normalized = self.normalize_agent_pattern(raw_pattern)
        if normalized is not None:
            return normalized

        token = " ".join([name, module or ""]).lower()
        for pattern, keywords in _PATTERN_KEYWORDS.items():
            if any(keyword in token for keyword in keywords):
                return pattern

        normalized_engine = self.normalize_engine_pattern(engine_pattern)
        if normalized_engine == "coordinator":
            return "coordinator"
        if normalized_engine == "pipeline":
            return "pipeline_stage"
        if normalized_engine == "deep_agent":
            return "coordinator"

        return "specialist"

    @staticmethod
    def _normalize_token(value: str) -> str:
        normalized = value.strip().lower()
        normalized = _NORMALIZE_RE.sub("_", normalized)
        normalized = re.sub(r"_+", "_", normalized).strip("_")
        return normalized
