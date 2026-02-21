"""StrategyRegistry tests."""

from __future__ import annotations

from datetime import UTC, datetime, timedelta

import pytest

from agentflow.evolution.registry import StrategyRegistry
from agentflow.evolution.types import (
    RetrievalMode,
    ScopeLevel,
    ScoreUpdateRequest,
    StrategyCapsule,
    StrategyScope,
    StrategySearchRequest,
    StrategyStatus,
    StrategyValidity,
)


@pytest.mark.asyncio
async def test_scope_priority_tenant_app_first() -> None:
    registry = StrategyRegistry()

    app_capsule = StrategyCapsule(
        intent="analyze migration risk",
        tool_sequence=["tool://risk"],
        keywords=["analyze", "migration", "risk"],
        scope=StrategyScope(
            scope_level=ScopeLevel.TENANT_APP,
            tenant_id="t1",
            app_id="a1",
            product_line="migration",
        ),
    )
    pl_capsule = StrategyCapsule(
        intent="analyze migration risk",
        tool_sequence=["tool://pl"],
        keywords=["analyze", "migration", "risk"],
        scope=StrategyScope(
            scope_level=ScopeLevel.TENANT_PRODUCT_LINE,
            tenant_id="t1",
            app_id=None,
            product_line="migration",
        ),
    )
    global_capsule = StrategyCapsule(
        intent="analyze migration risk",
        tool_sequence=["tool://global"],
        keywords=["analyze", "migration", "risk"],
        scope=StrategyScope(scope_level=ScopeLevel.GLOBAL_VERIFIED),
    )

    app_record = await registry.register(app_capsule, status=StrategyStatus.VERIFIED)
    await registry.score_update(
        ScoreUpdateRequest(
            strategy_id=app_record.id,
            success=True,
            latency_ms=120,
        )
    )

    pl_record = await registry.register(pl_capsule, status=StrategyStatus.VERIFIED)
    await registry.score_update(
        ScoreUpdateRequest(
            strategy_id=pl_record.id,
            success=True,
            latency_ms=130,
        )
    )

    global_record = await registry.register(global_capsule, status=StrategyStatus.VERIFIED)
    await registry.score_update(
        ScoreUpdateRequest(
            strategy_id=global_record.id,
            success=True,
            latency_ms=150,
        )
    )

    response = await registry.search(
        StrategySearchRequest(
            tenant_id="t1",
            app_id="a1",
            product_line="migration",
            intent="analyze migration risk",
            task_signature="migration risk",
            decision_mode=RetrievalMode.DEEP,
            top_k=3,
        )
    )

    assert response.strategies
    assert response.strategies[0].strategy_id == app_record.id


@pytest.mark.asyncio
async def test_stale_strategy_penalized() -> None:
    registry = StrategyRegistry()

    stale_capsule = StrategyCapsule(
        intent="legacy conversion",
        tool_sequence=["tool://old"],
        keywords=["legacy", "conversion"],
        scope=StrategyScope(scope_level=ScopeLevel.GLOBAL_VERIFIED),
        validity=StrategyValidity(max_age_days=30),
    )
    fresh_capsule = StrategyCapsule(
        intent="legacy conversion",
        tool_sequence=["tool://new"],
        keywords=["legacy", "conversion"],
        scope=StrategyScope(scope_level=ScopeLevel.GLOBAL_VERIFIED),
        validity=StrategyValidity(max_age_days=30),
    )

    stale = await registry.register(stale_capsule, status=StrategyStatus.VERIFIED)
    stale.last_verified_at = datetime.now(UTC) - timedelta(days=45)
    stale.updated_at = stale.last_verified_at

    fresh = await registry.register(fresh_capsule, status=StrategyStatus.VERIFIED)

    await registry.score_update(ScoreUpdateRequest(strategy_id=stale.id, success=True))
    await registry.score_update(ScoreUpdateRequest(strategy_id=fresh.id, success=True))

    response = await registry.search(
        StrategySearchRequest(
            tenant_id="t1",
            app_id="a1",
            product_line="migration",
            intent="legacy conversion",
            task_signature="legacy conversion",
            decision_mode=RetrievalMode.DEEP,
            top_k=2,
        )
    )

    assert response.strategies
    assert response.strategies[0].strategy_id == fresh.id


@pytest.mark.asyncio
async def test_suspect_trigger_by_failure_streak() -> None:
    registry = StrategyRegistry()
    capsule = StrategyCapsule(
        intent="check rollout",
        keywords=["check", "rollout"],
        scope=StrategyScope(scope_level=ScopeLevel.TENANT_APP, tenant_id="t1", app_id="a1"),
    )
    record = await registry.register(capsule, status=StrategyStatus.VERIFIED)

    updated = await registry.score_update(
        ScoreUpdateRequest(
            strategy_id=record.id,
            success=False,
            failure_streak=2,
            environment_mismatch=False,
        )
    )

    assert updated is not None
    assert updated.status == StrategyStatus.SUSPECT
