"""Retrieval Gate V2 for on-demand strategy/context retrieval."""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from agentflow.evolution.types import (
    RetrievalDecisionInput,
    RetrievalMode,
    StalenessRisk,
)


_logger = logging.getLogger(__name__)


class RetrievalReason(str, Enum):
    """Primary reason label for retrieval decision."""

    EXPLICIT_REQUEST = "explicit_request"
    RECENT_FAILURES = "recent_failures"
    HIGH_COMPLEXITY = "high_complexity"
    LOW_CONFIDENCE = "low_confidence"
    HIGH_NOVELTY = "high_novelty"
    SAFE_SKIP = "safe_skip"
    CASUAL_CHAT = "casual_chat"
    META_QUESTION = "meta_question"
    SIMPLE_TASK = "simple_task"
    CONTEXT_SUFFICIENT = "context_sufficient"
    DEFAULT_LIGHT = "default_light"


@dataclass
class RetrievalDecision:
    """Retrieval decision result."""

    should_retrieve: bool
    mode: RetrievalMode
    confidence: float
    reason: RetrievalReason
    reason_codes: list[str] = field(default_factory=list)
    suggested_query: str | None = None
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class GateConfig:
    """Gate thresholds and keyword configuration."""

    enable_query_rewrite: bool = True
    domain_keywords: list[str] = field(default_factory=list)
    confidence_threshold: float = 0.5
    high_confidence_skip_threshold: float = 0.82
    low_complexity_skip_threshold: float = 0.35
    low_novelty_skip_threshold: float = 0.40
    low_confidence_threshold: float = 0.55
    high_complexity_threshold: float = 0.70
    high_novelty_threshold: float = 0.65
    recent_failure_threshold: int = 2


class RetrievalGate:
    """Rule-based Retrieval Gate V2."""

    _EXPLICIT_KEYWORDS = {
        "文书",
        "文档",
        "资料",
        "参考",
        "检索",
        "検索",
        "参照",
        "仕様",
        "manual",
        "documentation",
        "reference",
        "search",
        "source",
    }
    _CASUAL_KEYWORDS = {
        "hello",
        "hi",
        "thanks",
        "thank you",
        "你好",
        "谢谢",
        "こんにちは",
        "ありがとう",
    }
    _META_KEYWORDS = {
        "你是",
        "你能",
        "who are you",
        "what can you",
        "あなたは",
        "何ができる",
    }
    _SIMPLE_TASK_KEYWORDS = {
        "translate",
        "calculate",
        "format",
        "翻译",
        "计算",
        "格式化",
        "翻訳",
        "計算",
        "整形",
    }

    def __init__(self, config: GateConfig | None = None) -> None:
        self._config = config or GateConfig()
        self._logger = logging.getLogger(__name__)

    async def should_retrieve(
        self,
        input_data: RetrievalDecisionInput | str | None = None,
        context: dict[str, Any] | None = None,
        *,
        query: str | None = None,
    ) -> RetrievalDecision:
        """Return retrieval decision.

        Preferred call style:
            should_retrieve(RetrievalDecisionInput(...))

        Compatibility call style:
            should_retrieve(query="...", context={...})
        """
        if input_data is None and query is not None:
            input_data = query

        if input_data is None:
            msg = "input_data or query is required"
            raise ValueError(msg)

        if isinstance(input_data, RetrievalDecisionInput):
            payload = input_data
        else:
            payload = self._from_legacy(query=input_data, context=context or {})

        query_lower = payload.query.lower()
        reason_codes: list[str] = []

        explicit_request = payload.explicit_request or self._check_explicit_request(query_lower)

        if not explicit_request and self._is_casual_chat(query_lower):
            return self._build_skip(
                reason=RetrievalReason.CASUAL_CHAT,
                confidence=0.85,
                reason_codes=[RetrievalReason.CASUAL_CHAT.value],
            )

        if not explicit_request and self._is_meta_question(query_lower):
            return self._build_skip(
                reason=RetrievalReason.META_QUESTION,
                confidence=0.85,
                reason_codes=[RetrievalReason.META_QUESTION.value],
            )

        if not explicit_request and self._is_simple_task(query_lower):
            return self._build_skip(
                reason=RetrievalReason.SIMPLE_TASK,
                confidence=0.8,
                reason_codes=[RetrievalReason.SIMPLE_TASK.value],
            )

        if explicit_request:
            reason_codes.append(RetrievalReason.EXPLICIT_REQUEST.value)

        if payload.recent_failures >= self._config.recent_failure_threshold:
            reason_codes.append(RetrievalReason.RECENT_FAILURES.value)
            return RetrievalDecision(
                should_retrieve=True,
                mode=RetrievalMode.DEEP,
                confidence=0.92,
                reason=RetrievalReason.RECENT_FAILURES,
                reason_codes=reason_codes,
                suggested_query=self._rewrite_query(payload.query),
                metadata=self._decision_metadata(payload),
            )

        if explicit_request:
            return RetrievalDecision(
                should_retrieve=True,
                mode=RetrievalMode.DEEP,
                confidence=0.9,
                reason=RetrievalReason.EXPLICIT_REQUEST,
                reason_codes=reason_codes,
                suggested_query=self._rewrite_query(payload.query),
                metadata=self._decision_metadata(payload),
            )

        if self._context_sufficient(payload.query, payload.context):
            reason_codes.append(RetrievalReason.CONTEXT_SUFFICIENT.value)

        # Fixed rules from plan:
        # 1) high confidence + low complexity + low novelty + low staleness => skip
        if (
            payload.self_confidence >= self._config.high_confidence_skip_threshold
            and payload.complexity <= self._config.low_complexity_skip_threshold
            and payload.novelty <= self._config.low_novelty_skip_threshold
            and payload.staleness_risk == StalenessRisk.LOW
            and RetrievalReason.CONTEXT_SUFFICIENT.value in reason_codes
        ):
            reason_codes.append(RetrievalReason.SAFE_SKIP.value)
            return self._build_skip(
                reason=RetrievalReason.SAFE_SKIP,
                confidence=0.88,
                reason_codes=reason_codes,
                metadata=self._decision_metadata(payload),
            )

        # 2) low confidence OR high complexity OR high novelty => deep
        if payload.self_confidence < self._config.low_confidence_threshold:
            reason_codes.append(RetrievalReason.LOW_CONFIDENCE.value)
        if payload.complexity >= self._config.high_complexity_threshold:
            reason_codes.append(RetrievalReason.HIGH_COMPLEXITY.value)
        if payload.novelty >= self._config.high_novelty_threshold:
            reason_codes.append(RetrievalReason.HIGH_NOVELTY.value)

        if reason_codes and any(
            code
            in {
                RetrievalReason.LOW_CONFIDENCE.value,
                RetrievalReason.HIGH_COMPLEXITY.value,
                RetrievalReason.HIGH_NOVELTY.value,
            }
            for code in reason_codes
        ):
            return RetrievalDecision(
                should_retrieve=True,
                mode=RetrievalMode.DEEP,
                confidence=0.83,
                reason=RetrievalReason.HIGH_COMPLEXITY,
                reason_codes=reason_codes,
                suggested_query=self._rewrite_query(payload.query),
                metadata=self._decision_metadata(payload),
            )

        # 3) default => light
        reason_codes.append(RetrievalReason.DEFAULT_LIGHT.value)
        return RetrievalDecision(
            should_retrieve=True,
            mode=RetrievalMode.LIGHT,
            confidence=0.7,
            reason=RetrievalReason.DEFAULT_LIGHT,
            reason_codes=reason_codes,
            suggested_query=self._rewrite_query(payload.query),
            metadata=self._decision_metadata(payload),
        )

    def add_domain_keywords(self, keywords: list[str]) -> None:
        self._config.domain_keywords.extend(keywords)

    def set_confidence_threshold(self, threshold: float) -> None:
        self._config.confidence_threshold = threshold

    def _from_legacy(self, query: str, context: dict[str, Any]) -> RetrievalDecisionInput:
        explicit = self._check_explicit_request(query.lower())
        return RetrievalDecisionInput(
            query=query,
            context=context,
            complexity=self._to_float(context.get("task_complexity"), 0.5),
            self_confidence=self._to_float(context.get("self_confidence"), 0.5),
            novelty=self._to_float(context.get("task_novelty"), 0.5),
            recent_failures=self._to_int(context.get("recent_failures"), 0),
            explicit_request=explicit,
            staleness_risk=self._staleness_from_context(context),
            tenant_id=self._text(context.get("tenant_id")),
            app_id=self._text(context.get("app_id")),
            product_line=self._text(context.get("product_line")),
            task_signature=self._text(context.get("task_signature")),
        )

    def _staleness_from_context(self, context: dict[str, Any]) -> StalenessRisk:
        raw = str(context.get("staleness_risk", "medium")).strip().lower()
        if raw in {"low", "medium", "high"}:
            return StalenessRisk(raw)
        return StalenessRisk.MEDIUM

    def _check_explicit_request(self, query: str) -> bool:
        if any(self._contains_keyword(query, keyword) for keyword in self._EXPLICIT_KEYWORDS):
            return True
        return any(self._contains_keyword(query, keyword.lower()) for keyword in self._config.domain_keywords)

    def _is_casual_chat(self, query: str) -> bool:
        return any(self._contains_keyword(query, keyword) for keyword in self._CASUAL_KEYWORDS)

    def _is_meta_question(self, query: str) -> bool:
        return any(self._contains_keyword(query, keyword) for keyword in self._META_KEYWORDS)

    def _is_simple_task(self, query: str) -> bool:
        return any(self._contains_keyword(query, keyword) for keyword in self._SIMPLE_TASK_KEYWORDS)

    def _context_sufficient(self, query: str, context: dict[str, Any]) -> bool:
        if context.get("rag_results"):
            return True

        existing = str(context.get("existing_info", "")).lower()
        if not existing:
            return False

        tokens = {token for token in re.findall(r"\w+", query.lower()) if len(token) > 2}
        if not tokens:
            return False

        overlap = sum(1 for token in tokens if token in existing)
        return overlap >= 2

    def _decision_metadata(self, payload: RetrievalDecisionInput) -> dict[str, Any]:
        return {
            "complexity": payload.complexity,
            "self_confidence": payload.self_confidence,
            "novelty": payload.novelty,
            "recent_failures": payload.recent_failures,
            "staleness_risk": payload.staleness_risk.value,
        }

    def _rewrite_query(self, query: str) -> str:
        if not self._config.enable_query_rewrite:
            return query

        result = query
        removals = [
            r"(please|could you|can you|would you)",
            r"(请问|麻烦|请)",
            r"(してください|お願いします)",
        ]
        for pattern in removals:
            result = re.sub(pattern, "", result, flags=re.IGNORECASE)
        result = re.sub(r"\s+", " ", result).strip()
        return result or query

    def _build_skip(
        self,
        *,
        reason: RetrievalReason,
        confidence: float,
        reason_codes: list[str],
        metadata: dict[str, Any] | None = None,
    ) -> RetrievalDecision:
        return RetrievalDecision(
            should_retrieve=False,
            mode=RetrievalMode.SKIP,
            confidence=confidence,
            reason=reason,
            reason_codes=reason_codes,
            metadata=metadata or {},
        )

    def _text(self, value: Any) -> str | None:
        if value is None:
            return None
        text = str(value).strip()
        return text or None

    def _to_float(self, value: Any, default: float) -> float:
        try:
            return float(value)
        except (TypeError, ValueError):
            return default

    def _to_int(self, value: Any, default: int) -> int:
        try:
            return int(value)
        except (TypeError, ValueError):
            return default

    def _contains_keyword(self, query: str, keyword: str) -> bool:
        if not keyword:
            return False
        normalized_keyword = keyword.strip().lower()
        if not normalized_keyword:
            return False

        if re.search(r"[a-z0-9]", normalized_keyword, re.IGNORECASE):
            pattern = rf"\b{re.escape(normalized_keyword)}\b"
            return re.search(pattern, query, re.IGNORECASE) is not None

        return normalized_keyword in query
