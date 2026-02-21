"""Orchestration helper for end-to-end evolution loop."""

from __future__ import annotations

import hashlib
from typing import Any

from agentflow.evolution.extractor import StrategyExtractor
from agentflow.evolution.types import OutcomeRecord, ScoreUpdateRequest, StrategyStatus


class EvolutionEngine:
    """Coordinates recorder -> extractor -> registry -> validator -> scoring."""

    def __init__(
        self,
        *,
        recorder: Any,
        registry: Any,
        extractor: StrategyExtractor | None = None,
        validator: Any | None = None,
    ) -> None:
        self._recorder = recorder
        self._registry = registry
        self._extractor = extractor or StrategyExtractor()
        self._validator = validator

    async def finalize_run(
        self,
        *,
        run_id: str,
        task: str,
        context: dict[str, Any],
        success: bool,
        strategy_id: str | None,
        latency_ms: float,
        failure_reason: str | None = None,
    ) -> dict[str, Any]:
        """Finalize a run and update evolution state."""
        task_signature = str(context.get("task_signature") or task)
        task_signature_hash = hashlib.md5(
            task_signature.encode("utf-8"),
            usedforsecurity=False,
        ).hexdigest()

        outcome = OutcomeRecord(
            run_id=run_id,
            strategy_id=strategy_id,
            task_signature_hash=task_signature_hash,
            difficulty=float(context.get("task_complexity", 0.5)),
            self_confidence=float(context.get("self_confidence", 0.5)),
            used_retrieval=bool(context.get("used_retrieval", False)),
            success=success,
            failure_reason=failure_reason,
            latency_ms=latency_ms,
            token_in=int(context.get("token_in", 0)),
            token_out=int(context.get("token_out", 0)),
            cost_usd=float(context.get("cost_usd", 0.0)),
        )

        events = await self._recorder.list_events(run_id)
        result: dict[str, Any] = {"events": len(events), "registered_strategy_id": None}

        if success:
            capsule = await self._extractor.extract(
                task=task,
                context=context,
                events=events,
                outcome=outcome,
            )
            if capsule is not None:
                record = await self._registry.register(capsule, status=StrategyStatus.CANDIDATE)
                result["registered_strategy_id"] = record.id

                if self._validator is not None:
                    job_id = await self._validator.submit_candidate(
                        strategy_id=record.id,
                        capsule=capsule,
                        metadata={"run_id": run_id, "task": task},
                    )
                    result["validation_job_id"] = job_id

        if strategy_id:
            update = await self._registry.score_update(
                ScoreUpdateRequest(
                    strategy_id=strategy_id,
                    success=success,
                    latency_ms=latency_ms,
                    cost_efficiency=float(context.get("cost_efficiency", 0.5)),
                    condition_match=float(context.get("condition_match", 1.0)),
                    failure_streak=int(context.get("failure_streak", 0)),
                    environment_mismatch=bool(context.get("environment_mismatch", False)),
                )
            )
            result["updated_strategy"] = update.id if update else None

        return result
