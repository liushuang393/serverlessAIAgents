"""Agent taxonomy inference service.

Platform で使用する業務基盤分類（business base）、
Agent type/pattern、App template の推定ロジックを提供する。
"""

from __future__ import annotations

import re
from collections import Counter
from typing import TYPE_CHECKING

from apps.platform.services.capability_registry import CapabilityRegistry


if TYPE_CHECKING:
    from collections.abc import Iterable


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

AGENT_TYPE_TYPES: tuple[str, ...] = (
    "specialist",
    "planner",
    "reactor",
    "executor",
    "reviewer",
    "gatekeeper",
    "router",
    "reporter",
    "custom",
)

APP_TEMPLATE_TYPES: tuple[str, ...] = (
    "faq_knowledge_service",
    "intelligence_monitoring",
    "decision_governance",
    "workflow_orchestrator",
    "multichannel_assistant",
    "ops_automation_runner",
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

_AGENT_TYPE_ALIAS_MAP: dict[str, str] = {
    "specialist": "specialist",
    "planner": "planner",
    "coordinator": "planner",
    "orchestrator": "planner",
    "reactor": "reactor",
    "analyzer": "reactor",
    "executor": "executor",
    "worker": "executor",
    "reviewer": "reviewer",
    "validator": "reviewer",
    "gatekeeper": "gatekeeper",
    "gate": "gatekeeper",
    "router": "router",
    "dispatcher": "router",
    "reporter": "reporter",
}

_AGENT_PATTERN_TO_TYPE: dict[str, str] = {
    "specialist": "specialist",
    "coordinator": "planner",
    "analyzer": "reactor",
    "pipeline_stage": "executor",
    "executor": "executor",
    "reviewer": "reviewer",
    "gatekeeper": "gatekeeper",
    "router": "router",
    "reporter": "reporter",
    "custom": "custom",
}

_AGENT_TYPE_TO_PATTERN: dict[str, str] = {
    "specialist": "specialist",
    "planner": "coordinator",
    "reactor": "analyzer",
    "executor": "executor",
    "reviewer": "reviewer",
    "gatekeeper": "gatekeeper",
    "router": "router",
    "reporter": "reporter",
    "custom": "custom",
}

_APP_TEMPLATE_ALIAS_MAP: dict[str, str] = {
    "faq": "faq_knowledge_service",
    "faq_knowledge": "faq_knowledge_service",
    "knowledge": "faq_knowledge_service",
    "intelligence": "intelligence_monitoring",
    "monitoring": "intelligence_monitoring",
    "governance": "decision_governance",
    "workflow": "workflow_orchestrator",
    "orchestrator": "workflow_orchestrator",
    "assistant": "multichannel_assistant",
    "multichannel": "multichannel_assistant",
    "ops": "ops_automation_runner",
    "automation": "ops_automation_runner",
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

_APP_TEMPLATE_HINTS: dict[str, tuple[str, ...]] = {
    "faq_knowledge_service": ("faq", "knowledge", "rag", "retrieval", "search"),
    "intelligence_monitoring": ("monitor", "intelligence", "trend", "signal", "analysis"),
    "decision_governance": ("governance", "decision", "audit", "compliance", "policy"),
    "workflow_orchestrator": ("workflow", "orchestration", "pipeline", "coordination"),
    "multichannel_assistant": ("assistant", "chat", "channel", "messaging", "copilot"),
    "ops_automation_runner": ("ops", "automation", "runner", "migration", "deploy", "batch"),
}

_AGENT_TYPE_CATALOG: dict[str, dict[str, object]] = {
    "specialist": {
        "label": "Specialist",
        "summary": "単一責務の専門実行",
        "behaviors": {
            "observe": True,
            "reflect": False,
            "experiment": False,
            "intent_recognition": False,
            "decompose": False,
            "delegate": False,
        },
    },
    "planner": {
        "label": "Planner",
        "summary": "意図認識・分解・委譲の中枢",
        "behaviors": {
            "observe": True,
            "reflect": True,
            "experiment": False,
            "intent_recognition": True,
            "decompose": True,
            "delegate": True,
        },
    },
    "reactor": {
        "label": "Reactor",
        "summary": "観察・反省・再試行ループ",
        "behaviors": {
            "observe": True,
            "reflect": True,
            "experiment": True,
            "intent_recognition": False,
            "decompose": False,
            "delegate": False,
        },
    },
    "executor": {
        "label": "Executor",
        "summary": "ツール実行と副作用処理",
        "behaviors": {
            "observe": True,
            "reflect": False,
            "experiment": False,
            "intent_recognition": False,
            "decompose": False,
            "delegate": False,
        },
    },
    "reviewer": {
        "label": "Reviewer",
        "summary": "検証と再実行判定",
        "behaviors": {
            "observe": True,
            "reflect": True,
            "experiment": False,
            "intent_recognition": False,
            "decompose": False,
            "delegate": False,
        },
    },
    "gatekeeper": {
        "label": "Gatekeeper",
        "summary": "ポリシー判定・承認ゲート",
        "behaviors": {
            "observe": True,
            "reflect": False,
            "experiment": False,
            "intent_recognition": True,
            "decompose": False,
            "delegate": False,
        },
    },
    "router": {
        "label": "Router",
        "summary": "意図判定と経路振り分け",
        "behaviors": {
            "observe": True,
            "reflect": False,
            "experiment": False,
            "intent_recognition": True,
            "decompose": False,
            "delegate": True,
        },
    },
    "reporter": {
        "label": "Reporter",
        "summary": "集約・要約・報告出力",
        "behaviors": {
            "observe": True,
            "reflect": False,
            "experiment": False,
            "intent_recognition": False,
            "decompose": False,
            "delegate": False,
        },
    },
    "custom": {
        "label": "Custom",
        "summary": "プロジェクト固有挙動",
        "behaviors": {
            "observe": True,
            "reflect": False,
            "experiment": False,
            "intent_recognition": False,
            "decompose": False,
            "delegate": False,
        },
    },
}

_APP_TEMPLATE_CATALOG: dict[str, dict[str, str]] = {
    "faq_knowledge_service": {
        "label": "FAQ Knowledge Service",
        "description": "FAQ/RAG 中心の知識提供アプリ",
    },
    "intelligence_monitoring": {
        "label": "Intelligence Monitoring",
        "description": "継続観測・分析・シグナル監視",
    },
    "decision_governance": {
        "label": "Decision Governance",
        "description": "意思決定・監査・承認統制",
    },
    "workflow_orchestrator": {
        "label": "Workflow Orchestrator",
        "description": "ワークフロー分解と連携実行",
    },
    "multichannel_assistant": {
        "label": "Multichannel Assistant",
        "description": "複数チャネル対応アシスタント",
    },
    "ops_automation_runner": {
        "label": "Ops Automation Runner",
        "description": "運用自動化・定期ジョブ実行",
    },
}


class AgentTaxonomyService:
    """業務基盤/Agent 分類/App template の推定サービス."""

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

    @staticmethod
    def normalize_agent_type(value: str | None) -> str | None:
        """Agent type 値を正規化."""
        if value is None:
            return None
        normalized = AgentTaxonomyService._normalize_token(value)
        if not normalized:
            return None
        if normalized in AGENT_TYPE_TYPES:
            return normalized
        return _AGENT_TYPE_ALIAS_MAP.get(normalized, "custom")

    @staticmethod
    def normalize_app_template(value: str | None) -> str | None:
        """App template 値を正規化."""
        if value is None:
            return None
        normalized = AgentTaxonomyService._normalize_token(value)
        if not normalized:
            return None
        if normalized in APP_TEMPLATE_TYPES:
            return normalized
        return _APP_TEMPLATE_ALIAS_MAP.get(normalized)

    @staticmethod
    def pattern_to_agent_type(pattern: str | None) -> str | None:
        """legacy pattern から agent_type を解決."""
        normalized = AgentTaxonomyService.normalize_agent_pattern(pattern)
        if normalized is None:
            return None
        return _AGENT_PATTERN_TO_TYPE.get(normalized, "custom")

    @staticmethod
    def agent_type_to_pattern(agent_type: str | None) -> str | None:
        """agent_type から legacy pattern を解決."""
        normalized = AgentTaxonomyService.normalize_agent_type(agent_type)
        if normalized is None:
            return None
        return _AGENT_TYPE_TO_PATTERN.get(normalized, "custom")

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
        raw_agent_type: str | None = None,
        name: str,
        module: str | None,
        engine_pattern: str | None,
    ) -> str:
        """Agent pattern を推定."""
        normalized = self.normalize_agent_pattern(raw_pattern)
        if normalized is not None:
            return normalized

        from_type = self.agent_type_to_pattern(raw_agent_type)
        if from_type is not None:
            return from_type

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

    def infer_agent_type(
        self,
        *,
        raw_agent_type: str | None,
        raw_pattern: str | None,
        name: str,
        module: str | None,
        engine_pattern: str | None,
    ) -> str:
        """Agent type を推定."""
        normalized = self.normalize_agent_type(raw_agent_type)
        if normalized is not None:
            return normalized

        from_pattern = self.pattern_to_agent_type(raw_pattern)
        if from_pattern is not None:
            return from_pattern

        inferred_pattern = self.infer_agent_pattern(
            raw_pattern=None,
            raw_agent_type=None,
            name=name,
            module=module,
            engine_pattern=engine_pattern,
        )
        return self.pattern_to_agent_type(inferred_pattern) or "specialist"

    def infer_app_template(
        self,
        *,
        product_line: str | None,
        engine_pattern: str | None,
        tags: Iterable[str],
    ) -> str:
        """App template を推定."""
        normalized_product_line = self._normalize_token(product_line or "")
        normalized_engine = self.normalize_engine_pattern(engine_pattern)
        tag_tokens = [self._normalize_token(tag) for tag in tags]
        tag_tokens = [tag for tag in tag_tokens if tag]
        tag_blob = " ".join(tag_tokens)

        if normalized_product_line == "faq":
            return "faq_knowledge_service"
        if normalized_product_line == "assistant":
            return "multichannel_assistant"
        if normalized_product_line == "migration":
            return "ops_automation_runner"

        for template_id, hints in _APP_TEMPLATE_HINTS.items():
            if any(hint in tag_blob for hint in hints):
                return template_id

        if normalized_engine in {"flow", "pipeline", "coordinator", "deep_agent"}:
            return "workflow_orchestrator"

        return "workflow_orchestrator"

    @staticmethod
    def list_agent_types() -> list[dict[str, object]]:
        """Agent type 定義一覧を返す."""
        rows: list[dict[str, object]] = []
        for agent_type in AGENT_TYPE_TYPES:
            metadata = _AGENT_TYPE_CATALOG[agent_type]
            rows.append(
                {
                    "agent_type": agent_type,
                    "label": metadata["label"],
                    "summary": metadata["summary"],
                    "behaviors": metadata["behaviors"],
                }
            )
        return rows

    @staticmethod
    def list_app_templates() -> list[dict[str, str]]:
        """App template 定義一覧を返す."""
        rows: list[dict[str, str]] = []
        for template_id in APP_TEMPLATE_TYPES:
            metadata = _APP_TEMPLATE_CATALOG[template_id]
            rows.append(
                {
                    "app_template": template_id,
                    "label": metadata["label"],
                    "description": metadata["description"],
                }
            )
        return rows

    @staticmethod
    def _normalize_token(value: str) -> str:
        normalized = value.strip().lower()
        normalized = _NORMALIZE_RE.sub("_", normalized)
        return re.sub(r"_+", "_", normalized).strip("_")
