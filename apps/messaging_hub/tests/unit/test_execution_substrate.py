"""Execution substrate unit tests."""

from __future__ import annotations

from typing import TYPE_CHECKING

import pytest

from apps.messaging_hub.execution_substrate import (
    ActionLogEntry,
    ActionLogStatus,
    ExecutionCheckpointRecord,
    ExecutionDecisionRecord,
    ExecutionFeedbackRecord,
    ExecutionFeedbackSource,
    ExecutionProfile,
    ExecutionSessionRecord,
    ExecutionSubstrateService,
    SQLiteExecutionSubstrateStore,
)
from contracts.policy import EvalResult
from kernel.state.models import Decision, DecisionType


if TYPE_CHECKING:
    from pathlib import Path


@pytest.mark.asyncio
async def test_execution_substrate_persists_and_inspects_records(tmp_path: Path) -> None:
    """session/decision/checkpoint/feedback/action log が inspection へ集約されること."""
    store = SQLiteExecutionSubstrateStore(db_path=tmp_path / "execution_substrate.db")
    service = ExecutionSubstrateService(store)

    session = await service.start_session(
        task_id="task_1",
        user_id="u1",
        conversation_id="chat:test",
        execution_profile=ExecutionProfile.CAPABILITY_ROUTE,
        context_snapshot={"message": "route this task"},
        status="running",
    )
    await service.record_decision(
        task_id="task_1",
        step="agent_route",
        decision_type=DecisionType.ACTION,
        choice="BusinessAdvisorAgent",
        reason="capability router selected the best candidate",
    )
    await service.record_checkpoint(
        task_id="task_1",
        stage="plan_created",
        snapshot={"harness_plan": {"blueprint_id": "general.autonomous_assistant"}},
    )
    await service.record_feedback(
        task_id="task_1",
        source=ExecutionFeedbackSource.VERIFIER,
        title="route_verification",
        feedback="all constraints passed",
        passed=True,
        score=1.0,
        eval_result=EvalResult(
            evaluator="route_verifier",
            passed=True,
            score=1.0,
            reason="result_payload exists",
        ),
    )
    await service.record_action(
        task_id="task_1",
        stage="capability_execution",
        action_type="invoke_agent",
        summary="BusinessAdvisorAgent executed",
        status=ActionLogStatus.COMPLETED,
    )

    inspection = await service.inspect_task("task_1")

    assert inspection is not None
    assert inspection.execution_session["session_id"] == session.session_id
    assert inspection.latest_checkpoint is not None
    assert inspection.latest_checkpoint["stage"] == "plan_created"
    assert inspection.execution_summary["decision_count"] == 1
    assert inspection.execution_summary["checkpoint_count"] == 1
    assert inspection.execution_summary["feedback_count"] == 1
    assert inspection.execution_summary["action_count"] == 1
    assert inspection.feedback_summary["by_source"]["verifier"] == 1
    assert inspection.action_log_summary["by_type"]["invoke_agent"] == 1


@pytest.mark.asyncio
async def test_execution_substrate_replay_merges_records_and_events(tmp_path: Path) -> None:
    """replay が substrate record と AG-UI event を時系列統合すること."""
    store = SQLiteExecutionSubstrateStore(db_path=tmp_path / "execution_replay.db")
    await store.initialize()
    service = ExecutionSubstrateService(store)

    session = ExecutionSessionRecord(
        session_id="exec_test",
        task_id="task_2",
        user_id="u1",
        conversation_id="chat:flight",
        status="monitoring",
        execution_profile=ExecutionProfile.MONITORED_READ_THEN_WRITE,
        context_snapshot={"message": "watch flights"},
        created_at="2026-01-01T00:00:00+00:00",
        updated_at="2026-01-01T00:00:00+00:00",
    )
    await store.upsert_execution_session(session.model_dump(mode="json"))
    await store.add_action_log(
        ActionLogEntry(
            id="act_1",
            session_id=session.session_id,
            task_id=session.task_id,
            stage="provider_discovery",
            action_type="discover_provider_candidates",
            summary="provider discovery",
            status=ActionLogStatus.COMPLETED,
            created_at="2026-01-01T00:00:01+00:00",
        ).model_dump(mode="json")
    )
    await store.add_execution_decision(
        ExecutionDecisionRecord(
            id="dec_1",
            session_id=session.session_id,
            task_id=session.task_id,
            decision=Decision(
                step="provider_routing",
                decision_type=DecisionType.ACTION,
                choice="web_provider",
                reason="best provider selected",
            ),
            created_at="2026-01-01T00:00:02+00:00",
        ).model_dump(mode="json")
    )
    await store.add_execution_feedback(
        ExecutionFeedbackRecord(
            id="fb_1",
            session_id=session.session_id,
            task_id=session.task_id,
            source=ExecutionFeedbackSource.VERIFIER,
            title="flight_task_verification",
            feedback="verification passed",
            passed=True,
            score=0.9,
            eval_result=EvalResult(
                evaluator="flight_task_verification",
                passed=True,
                score=0.9,
                reason="search_result exists",
            ),
            created_at="2026-01-01T00:00:03+00:00",
        ).model_dump(mode="json")
    )
    await store.add_execution_checkpoint(
        ExecutionCheckpointRecord(
            id="chk_1",
            session_id=session.session_id,
            task_id=session.task_id,
            stage="pre_mutation_subscription",
            snapshot={"subscription": "pending"},
            created_at="2026-01-01T00:00:04+00:00",
        ).model_dump(mode="json")
    )

    replay = await service.replay_task(
        task_id="task_2",
        execution_events=[
            {
                "id": "exec_evt_1",
                "run_id": "task_2",
                "skill_name": "web_search",
                "status": "success",
                "started_at": "2026-01-01T00:00:02.500000+00:00",
                "completed_at": "2026-01-01T00:00:02.900000+00:00",
                "params": {"query": "flight deals"},
                "result": {"hits": 3},
                "metadata": {"step_id": "step_1"},
            }
        ],
        agui_events=[
            {
                "event_type": "flow.complete",
                "payload": {"event_type": "flow.complete"},
                "created_at": "2026-01-01T00:00:05+00:00",
            }
        ],
    )

    assert replay is not None
    timeline_kinds = [item["kind"] for item in replay.timeline]
    assert timeline_kinds == ["action_log", "decision", "execution_event", "feedback", "checkpoint", "agui_event"]
    assert replay.execution_summary["latest_checkpoint_stage"] == "pre_mutation_subscription"


@pytest.mark.asyncio
async def test_execution_substrate_builds_monitoring_summary(tmp_path: Path) -> None:
    """monitoring summary が aggregate 指標を返すこと."""
    store = SQLiteExecutionSubstrateStore(db_path=tmp_path / "execution_monitoring.db")
    service = ExecutionSubstrateService(store)

    await service.start_session(
        task_id="task_monitoring",
        user_id="u1",
        conversation_id="chat:test",
        execution_profile=ExecutionProfile.GAP_FILL_GOVERNED,
        context_snapshot={"message": "create artifact"},
        status="completed",
    )
    await service.record_action(
        task_id="task_monitoring",
        stage="gap_fill",
        action_type="create_runtime_artifact",
        summary="artifact created",
        status=ActionLogStatus.FAILED,
        artifact_refs=["gart_1"],
    )
    await service.record_feedback(
        task_id="task_monitoring",
        source=ExecutionFeedbackSource.VERIFIER,
        title="artifact_verification",
        feedback="verification failed",
        passed=False,
        score=0.2,
        eval_result=EvalResult(
            evaluator="artifact_verification",
            passed=False,
            score=0.2,
            reason="artifact missing checks",
        ),
    )
    await service.record_feedback(
        task_id="task_monitoring",
        source=ExecutionFeedbackSource.SCORER,
        title="assistant_quality_score",
        feedback="quality score",
        passed=True,
        score=0.75,
        eval_result=EvalResult(
            evaluator="assistant_quality_score",
            passed=True,
            score=0.75,
            reason="good",
        ),
    )

    summary = await service.monitoring_summary(
        approvals=[
            {
                "status": "approved",
                "created_at": "2026-01-01T00:00:00",
                "decided_at": "2026-01-01T00:00:10",
            },
            {
                "status": "pending",
                "created_at": "2026-01-01T00:01:00",
                "decided_at": None,
            },
        ]
    )

    assert summary.total_sessions == 1
    assert summary.by_profile["gap_fill_governed"] == 1
    assert summary.runtime_artifact_actions == 1
    assert summary.unsafe_action_rate == 1.0
    assert summary.verification_failure_rate == 1.0
    assert summary.avg_quality_score == 0.75
    assert summary.approval_pending == 1
    assert summary.avg_approval_latency_seconds == 10.0
