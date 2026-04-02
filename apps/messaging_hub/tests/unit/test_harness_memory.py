"""Harness memory tests."""

from __future__ import annotations

import pytest

from apps.messaging_hub.harness_memory import HarnessMemoryService
from apps.messaging_hub.task_harness import TaskHarnessPlanner
from kernel.memory.memory_manager import MemoryManager


@pytest.mark.asyncio
async def test_harness_memory_persists_shared_and_private_layers() -> None:
    """main/shared/private の境界で記憶を保存・再取得できること."""
    planner = TaskHarnessPlanner()
    plan = await planner.build_plan(
        message="Monitor flight prices from HND to LAX",
        required_capability="flight_watch",
        input_data={
            "request": {
                "origin": "HND",
                "destination": "LAX",
                "depart_window": {"start_date": "2026-05-01", "end_date": "2026-05-03"},
                "return_window": {"start_date": "2026-05-10", "end_date": "2026-05-12"},
                "create_watch": True,
            }
        },
        partial_request={
            "origin": "HND",
            "destination": "LAX",
            "depart_window": {"start_date": "2026-05-01", "end_date": "2026-05-03"},
            "return_window": {"start_date": "2026-05-10", "end_date": "2026-05-12"},
        },
    )
    service = HarnessMemoryService(
        manager=MemoryManager(
            token_threshold=1000,
            enable_auto_distill=False,
            enable_auto_forget=False,
        )
    )
    await service.start()
    try:
        await service.remember_from_plan(
            harness_plan=plan,
            task_id="task_memory_seed",
            user_id="u-memory",
            conversation_id="chat:memory",
            values={
                "raw_user_request": "Monitor flight prices from HND to LAX",
                "task_plan": plan.execution_plan,
                "user_preferences": {"origin": "HND", "budget": 500},
                "provider_candidates": [{"domain": "example.com"}],
                "task_episode": {"status": "completed"},
                "monitor_baseline": {"baseline_price": 420.0},
            },
        )
        await service.remember_private(
            task_id="task_memory_seed",
            user_id="u-memory",
            conversation_id="chat:memory",
            agent_name="FlightWatchAgent",
            key="specialist_scratchpad",
            content={"offers_found": 3},
            description="specialist output",
        )

        snapshot = await service.snapshot_for_plan(
            harness_plan=plan,
            task_id="task_memory_seed",
            user_id="u-memory",
            conversation_id="chat:memory",
            agent_names=["FlightWatchAgent"],
        )
    finally:
        await service.stop()

    assert "user_preferences" in snapshot.main_agent
    assert "provider_candidates" in snapshot.shared_task
    assert "FlightWatchAgent" in snapshot.subagent_private
    assert "specialist_scratchpad" in snapshot.subagent_private["FlightWatchAgent"]
