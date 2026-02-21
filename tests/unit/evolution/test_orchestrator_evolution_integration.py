"""Orchestrator + Evolution integration tests."""

from __future__ import annotations

import pytest

from agentflow.evolution.recorder import InMemoryExecutionRecorder
from agentflow.evolution.types import (
    RetrievalMode,
    StrategyCapsule,
    StrategyDecision,
    StrategyScope,
)
from agentflow.orchestration.orchestrator import Orchestrator
from agentflow.orchestration.planner import ExecutionPlan, PlanStep, StepType


class _DummyLLM:
    async def generate(self, prompt: str) -> str:
        return f"ok:{prompt[:20]}"


class _DummyRouter:
    async def route(self, *, task: str, plan: ExecutionPlan, context: dict) -> StrategyDecision:
        capsule = StrategyCapsule(
            intent=task,
            tool_sequence=["tool://synthetic"],
            guard_conditions=["product_line:framework"],
            keywords=["task", "framework"],
            scope=StrategyScope(scope_level="tenant_app", tenant_id="t1", app_id="a1"),
        )
        return StrategyDecision(
            use_strategy=True,
            mode=RetrievalMode.DEEP,
            reasons=["test"],
            selected_strategy_id="strat-test",
            selected_capsule=capsule,
        )


class _DummyEvolutionEngine:
    def __init__(self) -> None:
        self.calls: list[dict] = []

    async def finalize_run(self, **kwargs):  # type: ignore[no-untyped-def]
        self.calls.append(kwargs)
        return {"finalized": True, "run_id": kwargs["run_id"]}


@pytest.mark.asyncio
async def test_orchestrator_routes_strategy_and_records_events() -> None:
    recorder = InMemoryExecutionRecorder()
    evolution_engine = _DummyEvolutionEngine()
    orchestrator = Orchestrator(
        llm_client=_DummyLLM(),
        tool_provider=None,
        strategy_router=_DummyRouter(),
        execution_recorder=recorder,
        evolution_engine=evolution_engine,
    )

    async def _custom_plan(*args, **kwargs):  # type: ignore[no-untyped-def]
        return ExecutionPlan(
            name="test-plan",
            goal="test",
            steps=[
                PlanStep(
                    name="llm-step",
                    step_type=StepType.LLM_GENERATION,
                    params={"prompt": "hello"},
                    order=0,
                )
            ],
            context={},
        )

    orchestrator._planner.create_plan = _custom_plan  # type: ignore[assignment]

    result = await orchestrator.execute(
        "test task",
        context={"tenant_id": "t1", "app_id": "a1", "product_line": "framework"},
    )

    assert result.success is True
    assert result.metadata["strategy_decision"]["selected_strategy_id"] == "strat-test"
    assert evolution_engine.calls

    events = await recorder.list_events(result.execution_id)
    event_types = {event.event_type for event in events}
    assert "step_start" in event_types
    assert "llm_result" in event_types
    assert "step_success" in event_types
