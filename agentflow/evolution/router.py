"""Strategy router for orchestrator integration."""

from __future__ import annotations

from typing import Any

from agentflow.evolution.types import (
    RetrievalDecisionInput,
    RetrievalMode,
    StalenessRisk,
    StrategyDecision,
    StrategySearchRequest,
)


class StrategyRouter:
    """Routes whether and how to reuse strategy capsules."""

    def __init__(
        self,
        *,
        retrieval_gate: Any,
        registry: Any,
    ) -> None:
        self._retrieval_gate = retrieval_gate
        self._registry = registry

    async def route(
        self,
        *,
        task: str,
        plan: Any,
        context: dict[str, Any],
    ) -> StrategyDecision:
        decision_input = RetrievalDecisionInput(
            query=task,
            context=context,
            complexity=self._safe_float(context.get("task_complexity"), 0.5),
            self_confidence=self._safe_float(context.get("self_confidence"), 0.5),
            novelty=self._safe_float(context.get("task_novelty"), 0.5),
            recent_failures=self._safe_int(context.get("recent_failures"), 0),
            explicit_request=bool(context.get("force_retrieval", False)),
            staleness_risk=self._staleness(context.get("staleness_risk")),
            tenant_id=self._text(context.get("tenant_id")),
            app_id=self._text(context.get("app_id")),
            product_line=self._text(context.get("product_line")),
            task_signature=self._text(context.get("task_signature")),
        )

        retrieval_decision = await self._retrieval_gate.should_retrieve(decision_input)
        if retrieval_decision.mode == RetrievalMode.SKIP:
            return StrategyDecision(
                use_strategy=False,
                mode=retrieval_decision.mode,
                reasons=list(retrieval_decision.reason_codes),
                metadata={"retrieval_decision": retrieval_decision.model_dump()},
            )

        search_request = StrategySearchRequest(
            tenant_id=decision_input.tenant_id,
            app_id=decision_input.app_id,
            product_line=decision_input.product_line,
            intent=task,
            task_signature=decision_input.task_signature,
            decision_mode=retrieval_decision.mode,
            top_k=3 if retrieval_decision.mode == RetrievalMode.LIGHT else 8,
        )
        search_response = await self._registry.search(search_request)

        if not search_response.strategies:
            return StrategyDecision(
                use_strategy=False,
                mode=retrieval_decision.mode,
                reasons=list(retrieval_decision.reason_codes) + search_response.reasons,
                search_response=search_response,
                metadata={"retrieval_decision": retrieval_decision.model_dump()},
            )

        top = search_response.strategies[0]
        return StrategyDecision(
            use_strategy=True,
            mode=retrieval_decision.mode,
            reasons=list(retrieval_decision.reason_codes) + top.reasons,
            selected_strategy_id=top.strategy_id,
            selected_capsule=top.capsule,
            search_response=search_response,
            metadata={"retrieval_decision": retrieval_decision.model_dump()},
        )

    def _staleness(self, raw: Any) -> StalenessRisk:
        text = self._text(raw)
        if text in {StalenessRisk.LOW.value, StalenessRisk.MEDIUM.value, StalenessRisk.HIGH.value}:
            return StalenessRisk(text)
        return StalenessRisk.MEDIUM

    def _text(self, value: Any) -> str | None:
        if value is None:
            return None
        text = str(value).strip()
        return text or None

    def _safe_float(self, value: Any, default: float) -> float:
        try:
            return float(value)
        except (TypeError, ValueError):
            return default

    def _safe_int(self, value: Any, default: int) -> int:
        try:
            return int(value)
        except (TypeError, ValueError):
            return default
