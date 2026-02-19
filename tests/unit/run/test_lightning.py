"""Lightning 着想ランタイム拡張のテスト."""

from __future__ import annotations

from typing import Any

import pytest

from agentflow.engines.base import BaseEngine, EngineConfig
from agentflow.run import (
    LightningTracer,
    LightningTrainingRequest,
    MemoryLightningStore,
    TrajectoryAdapter,
)


class _DummyEngine(BaseEngine):
    async def _initialize(self) -> None:
        return None

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        return {"success": True, "answer": f"echo:{inputs.get('question', '')}"}

    async def _execute_stream(self, inputs: dict[str, Any]):
        yield {"type": "node_start", "node_id": "step1", "node_name": "step1"}
        yield {
            "type": "node_complete",
            "node_id": "step1",
            "node_name": "step1",
            "data": {
                "prompt": inputs.get("question", ""),
                "response": "ok",
            },
        }
        yield {"type": "result", "data": {"success": True, "answer": "ok"}}


@pytest.mark.asyncio
async def test_memory_store_and_tracer_records_event_and_reward() -> None:
    store = MemoryLightningStore()
    tracer = LightningTracer(store)

    await tracer.record_event(
        run_id="run-1",
        flow_id="flow-1",
        event={
            "type": "node_complete",
            "node_id": "analysis",
            "node_name": "analysis",
            "data": {"result": "done"},
        },
    )
    await tracer.record_reward(
        run_id="run-1",
        flow_id="flow-1",
        value=0.8,
        metadata={"node_id": "analysis"},
    )

    events = await store.list_events("run-1")
    rewards = await store.list_rewards("run-1")

    assert len(events) == 1
    assert events[0].event_type == "node_complete"
    assert events[0].node_id == "analysis"
    assert len(rewards) == 1
    assert rewards[0].value == pytest.approx(0.8)


@pytest.mark.asyncio
async def test_trajectory_adapter_applies_terminal_reward() -> None:
    store = MemoryLightningStore()
    tracer = LightningTracer(store)

    await tracer.record_event(
        run_id="run-2",
        flow_id="flow-2",
        event={
            "event_type": "node.complete",
            "node_id": "s1",
            "data": {"prompt": "p1", "response": "r1"},
        },
    )
    await tracer.record_event(
        run_id="run-2",
        flow_id="flow-2",
        event={
            "event_type": "node.complete",
            "node_id": "s2",
            "data": {"prompt": "p2", "response": "r2"},
        },
    )
    await tracer.record_reward(run_id="run-2", flow_id="flow-2", value=1.2)

    events = await store.list_events("run-2")
    rewards = await store.list_rewards("run-2")
    transitions = TrajectoryAdapter.to_transition_samples(events=events, rewards=rewards)

    assert len(transitions) == 2
    assert transitions[0].reward == pytest.approx(0.0)
    assert transitions[1].reward == pytest.approx(1.2)


@pytest.mark.asyncio
async def test_base_engine_run_and_stream_record_lightning_data() -> None:
    store = MemoryLightningStore()
    config = EngineConfig(
        name="dummy",
        lightning_store=store,
        reward_evaluator=lambda result: 1.0 if result.get("success") else -1.0,
    )
    engine = _DummyEngine(config=config)

    # run()
    result = await engine.run({"question": "q1"})
    assert result["success"] is True

    run_ids = await store.list_run_ids()
    assert len(run_ids) == 1
    first_run_events = await store.list_events(run_ids[0])
    first_run_rewards = await store.list_rewards(run_ids[0])
    assert any(event.event_type == "engine.run.start" for event in first_run_events)
    assert any(event.event_type == "engine.run.result" for event in first_run_events)
    assert len(first_run_rewards) == 1

    # run_stream()
    streamed = []
    async for event in engine.run_stream({"question": "q2"}):
        streamed.append(event)

    assert any((e.get("type") or e.get("event_type")) == "result" for e in streamed)

    run_ids = await store.list_run_ids()
    assert len(run_ids) == 2
    second_run_events = await store.list_events(run_ids[1])
    second_run_rewards = await store.list_rewards(run_ids[1])
    transitions = TrajectoryAdapter.to_transition_samples(
        events=second_run_events,
        rewards=second_run_rewards,
    )
    assert len(second_run_events) >= 4
    assert len(second_run_rewards) == 1
    assert len(transitions) == 1
    assert transitions[0].reward == pytest.approx(1.0)


@pytest.mark.asyncio
async def test_base_engine_train_lightning_applies_profile() -> None:
    store = MemoryLightningStore()
    config = EngineConfig(
        name="dummy-train",
        lightning_store=store,
        reward_evaluator=lambda result: 1.0 if result.get("success") else -1.0,
    )
    config.lightning.enable_training = True
    config.lightning.enable_api_optimization = True
    config.lightning.backend = "builtin"

    engine = _DummyEngine(config=config)
    async for _event in engine.run_stream({"question": "q1"}):
        pass

    result = await engine.train_lightning(LightningTrainingRequest(apply_optimized_profile=True))
    assert result.success is True
    assert result.backend == "builtin"
    assert result.optimized is True
    assert "optimized_profile" in engine.config.llm_config
