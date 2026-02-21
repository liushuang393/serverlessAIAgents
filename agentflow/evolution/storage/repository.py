"""SQL repository helpers for Evolution V2."""

from __future__ import annotations

from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any

from sqlalchemy import delete, select

from agentflow.evolution.storage.models import (
    EvolutionExecutionEvent,
    EvolutionOutcome,
    EvolutionStrategy,
    EvolutionStrategyKeyword,
    EvolutionStrategyScore,
    EvolutionValidationResult,
)


if TYPE_CHECKING:
    from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker


class EvolutionRepository:
    """Minimal SQL repository for strategy persistence."""

    def __init__(
        self,
        session_factory: async_sessionmaker[AsyncSession],
    ) -> None:
        self._session_factory = session_factory

    async def insert_strategy(
        self,
        *,
        strategy_id: str,
        tenant_id: str | None,
        app_id: str | None,
        product_line: str | None,
        scope_level: str,
        intent_hash: str,
        capsule_json: dict[str, Any],
        status: str,
        version: int,
        parent_strategy_id: str | None = None,
        valid_from: datetime | None = None,
        valid_to: datetime | None = None,
        last_verified_at: datetime | None = None,
    ) -> None:
        async with self._session_factory() as session:
            strategy = EvolutionStrategy(
                id=strategy_id,
                tenant_id=tenant_id,
                app_id=app_id,
                product_line=product_line,
                scope_level=scope_level,
                intent_hash=intent_hash,
                capsule_json=capsule_json,
                status=status,
                version=version,
                parent_strategy_id=parent_strategy_id,
                valid_from=valid_from,
                valid_to=valid_to,
                last_verified_at=last_verified_at,
                created_at=datetime.now(UTC),
                updated_at=datetime.now(UTC),
            )
            session.add(strategy)
            await session.flush()
            session.add(
                EvolutionStrategyScore(
                    strategy_id=strategy_id,
                    updated_at=datetime.now(UTC),
                )
            )
            await session.commit()

    async def replace_keywords(self, strategy_id: str, keywords: list[str]) -> None:
        async with self._session_factory() as session:
            await session.execute(
                delete(EvolutionStrategyKeyword).where(EvolutionStrategyKeyword.strategy_id == strategy_id)
            )
            for keyword in keywords:
                session.add(
                    EvolutionStrategyKeyword(
                        strategy_id=strategy_id,
                        keyword=keyword,
                        weight=1.0,
                    )
                )
            await session.commit()

    async def list_execution_events(self, run_id: str) -> list[EvolutionExecutionEvent]:
        async with self._session_factory() as session:
            stmt = (
                select(EvolutionExecutionEvent)
                .where(EvolutionExecutionEvent.run_id == run_id)
                .order_by(EvolutionExecutionEvent.created_at.asc())
            )
            result = await session.execute(stmt)
            return list(result.scalars().all())

    async def append_execution_event(
        self,
        *,
        run_id: str,
        step_id: str | None,
        event_type: str,
        payload_json: dict[str, Any],
    ) -> None:
        async with self._session_factory() as session:
            session.add(
                EvolutionExecutionEvent(
                    run_id=run_id,
                    step_id=step_id,
                    event_type=event_type,
                    payload_json=payload_json,
                    created_at=datetime.now(UTC),
                )
            )
            await session.commit()

    async def insert_outcome(
        self,
        *,
        run_id: str,
        strategy_id: str | None,
        task_signature_hash: str | None,
        difficulty: float,
        self_confidence: float,
        used_retrieval: bool,
        success: bool,
        failure_reason: str | None,
        latency_ms: float,
        token_in: int,
        token_out: int,
        cost_usd: float,
    ) -> None:
        async with self._session_factory() as session:
            session.add(
                EvolutionOutcome(
                    run_id=run_id,
                    strategy_id=strategy_id,
                    task_signature_hash=task_signature_hash,
                    difficulty=difficulty,
                    self_confidence=self_confidence,
                    used_retrieval=used_retrieval,
                    success=success,
                    failure_reason=failure_reason,
                    latency_ms=latency_ms,
                    token_in=token_in,
                    token_out=token_out,
                    cost_usd=cost_usd,
                    created_at=datetime.now(UTC),
                )
            )
            await session.commit()

    async def insert_validation_result(
        self,
        *,
        strategy_id: str,
        job_id: str,
        status: str,
        checks_json: dict[str, Any],
        score_delta: float,
    ) -> None:
        async with self._session_factory() as session:
            session.add(
                EvolutionValidationResult(
                    strategy_id=strategy_id,
                    job_id=job_id,
                    status=status,
                    checks_json=checks_json,
                    score_delta=score_delta,
                    created_at=datetime.now(UTC),
                )
            )
            await session.commit()
