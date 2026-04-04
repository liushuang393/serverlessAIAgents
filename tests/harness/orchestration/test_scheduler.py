"""ScheduledPipeline のユニットテスト."""

from __future__ import annotations

from datetime import UTC, datetime, timedelta
from typing import Any

import pytest

from harness.orchestration.models import ExecutionPlan, PlanStep
from harness.orchestration.scheduler import (
    ScheduledPipeline,
    ScheduleStatus,
)


def _make_plan(goal: str = "テスト計画") -> ExecutionPlan:
    return ExecutionPlan(
        goal=goal,
        steps=[PlanStep(step_id="s1", agent_id="a1", description="テスト")],
    )


async def _mock_execute(plan: ExecutionPlan, context: dict[str, Any]) -> dict[str, Any]:
    """テスト用実行関数."""
    return {"status": "ok", "goal": plan.goal, "context_keys": list(context.keys())}


async def _failing_execute(plan: ExecutionPlan, context: dict[str, Any]) -> dict[str, Any]:
    """テスト用失敗関数."""
    msg = "テスト実行エラー"
    raise RuntimeError(msg)


class TestSchedule:
    """スケジュール登録テスト."""

    def test_schedule_creates_entry(self) -> None:
        scheduler = ScheduledPipeline()
        plan = _make_plan()
        entry = scheduler.schedule(plan, interval_hours=6)

        assert entry.schedule_id.startswith("sched-")
        assert entry.status == ScheduleStatus.ACTIVE
        assert entry.interval_hours == 6
        assert entry.run_count == 0
        assert entry.plan.goal == "テスト計画"

    def test_schedule_with_context(self) -> None:
        scheduler = ScheduledPipeline()
        entry = scheduler.schedule(
            _make_plan(),
            interval_hours=12,
            context={"user_id": "u1"},
        )
        assert entry.context == {"user_id": "u1"}

    def test_schedule_with_until(self) -> None:
        scheduler = ScheduledPipeline()
        until = datetime.now(UTC) + timedelta(days=30)
        entry = scheduler.schedule(_make_plan(), interval_hours=6, until=until)
        assert entry.until == until


class TestCheckDue:
    """check_due テスト."""

    @pytest.mark.asyncio
    async def test_not_due_yet(self) -> None:
        """次回実行時刻が未来の場合は実行しない."""
        scheduler = ScheduledPipeline()
        scheduler.schedule(_make_plan(), interval_hours=6)

        result = await scheduler.check_due(_mock_execute)

        assert result.checked == 0
        assert result.executed == 0

    @pytest.mark.asyncio
    async def test_due_entry_executes(self) -> None:
        """期限到来したエントリは実行される."""
        scheduler = ScheduledPipeline()
        entry = scheduler.schedule(_make_plan(), interval_hours=1)
        # 強制的に過去に設定
        entry.next_run_at = datetime.now(UTC) - timedelta(minutes=1)

        result = await scheduler.check_due(_mock_execute)

        assert result.checked == 1
        assert result.executed == 1
        assert entry.run_count == 1
        assert entry.last_result is not None
        assert entry.last_result["status"] == "ok"
        # 次回実行時刻が更新されていること
        assert entry.next_run_at > datetime.now(UTC)

    @pytest.mark.asyncio
    async def test_execution_error_recorded(self) -> None:
        """実行エラーが記録される."""
        scheduler = ScheduledPipeline()
        entry = scheduler.schedule(_make_plan(), interval_hours=1)
        entry.next_run_at = datetime.now(UTC) - timedelta(minutes=1)

        result = await scheduler.check_due(_failing_execute)

        assert result.checked == 1
        assert result.executed == 0
        assert len(result.errors) == 1
        assert "テスト実行エラー" in result.errors[0]["error"]

    @pytest.mark.asyncio
    async def test_expired_entry_completed(self) -> None:
        """有効期限切れのエントリは COMPLETED になる."""
        scheduler = ScheduledPipeline()
        entry = scheduler.schedule(
            _make_plan(),
            interval_hours=1,
            until=datetime.now(UTC) - timedelta(hours=1),
        )
        entry.next_run_at = datetime.now(UTC) - timedelta(minutes=1)

        await scheduler.check_due(_mock_execute)

        assert entry.status == ScheduleStatus.COMPLETED

    @pytest.mark.asyncio
    async def test_multiple_due_entries(self) -> None:
        """複数の期限到来エントリが実行される."""
        scheduler = ScheduledPipeline()
        for i in range(3):
            entry = scheduler.schedule(_make_plan(f"計画{i}"), interval_hours=1)
            entry.next_run_at = datetime.now(UTC) - timedelta(minutes=1)

        result = await scheduler.check_due(_mock_execute)

        assert result.checked == 3
        assert result.executed == 3


class TestCancelPauseResume:
    """キャンセル・一時停止・再開テスト."""

    def test_cancel(self) -> None:
        scheduler = ScheduledPipeline()
        entry = scheduler.schedule(_make_plan(), interval_hours=6)
        assert scheduler.cancel(entry.schedule_id) is True
        assert entry.status == ScheduleStatus.CANCELLED

    def test_cancel_nonexistent(self) -> None:
        scheduler = ScheduledPipeline()
        assert scheduler.cancel("nonexistent") is False

    def test_pause_and_resume(self) -> None:
        scheduler = ScheduledPipeline()
        entry = scheduler.schedule(_make_plan(), interval_hours=6)

        assert scheduler.pause(entry.schedule_id) is True
        assert entry.status == ScheduleStatus.PAUSED

        assert scheduler.resume(entry.schedule_id) is True
        assert entry.status == ScheduleStatus.ACTIVE

    @pytest.mark.asyncio
    async def test_paused_not_executed(self) -> None:
        """一時停止中のエントリは実行されない."""
        scheduler = ScheduledPipeline()
        entry = scheduler.schedule(_make_plan(), interval_hours=1)
        entry.next_run_at = datetime.now(UTC) - timedelta(minutes=1)
        scheduler.pause(entry.schedule_id)

        result = await scheduler.check_due(_mock_execute)

        assert result.checked == 0
        assert result.executed == 0


class TestGetAndList:
    """取得・一覧テスト."""

    def test_get(self) -> None:
        scheduler = ScheduledPipeline()
        entry = scheduler.schedule(_make_plan(), interval_hours=6)
        assert scheduler.get(entry.schedule_id) is entry

    def test_get_nonexistent(self) -> None:
        scheduler = ScheduledPipeline()
        assert scheduler.get("nonexistent") is None

    def test_list_active(self) -> None:
        scheduler = ScheduledPipeline()
        scheduler.schedule(_make_plan("a"), interval_hours=6)
        e2 = scheduler.schedule(_make_plan("b"), interval_hours=6)
        scheduler.cancel(e2.schedule_id)
        scheduler.schedule(_make_plan("c"), interval_hours=6)

        active = scheduler.list_active()
        assert len(active) == 2
