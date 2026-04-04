"""orchestration モデルのユニットテスト."""

from __future__ import annotations

from datetime import UTC, datetime

import pytest

from harness.orchestration.models import (
    ExecutionPlan,
    PlannerInput,
    PlannerOutput,
    PlanStep,
    ReplanRequest,
    StepStatus,
)
from harness.risk.service import RiskLevel


class TestPlanStep:
    """PlanStep のバリデーションテスト."""

    def test_default_values(self) -> None:
        step = PlanStep(step_id="s1", agent_id="agent1", description="テスト")
        assert step.risk_level == RiskLevel.LOW
        assert step.status == StepStatus.PENDING
        assert step.timeout_seconds == 300
        assert step.max_retries == 2
        assert step.dependencies == []
        assert step.input_spec == {}
        assert step.metadata == {}

    def test_custom_values(self) -> None:
        step = PlanStep(
            step_id="s2",
            agent_id="agent2",
            description="削除処理",
            risk_level=RiskLevel.HIGH,
            dependencies=["s1"],
            timeout_seconds=60,
            max_retries=0,
        )
        assert step.risk_level == RiskLevel.HIGH
        assert step.dependencies == ["s1"]
        assert step.timeout_seconds == 60

    def test_invalid_timeout(self) -> None:
        with pytest.raises(Exception):
            PlanStep(step_id="s1", agent_id="a", description="t", timeout_seconds=0)


class TestExecutionPlan:
    """ExecutionPlan のテスト."""

    def test_auto_plan_id(self) -> None:
        plan = ExecutionPlan(goal="テスト目標")
        assert plan.plan_id.startswith("plan-")
        assert len(plan.plan_id) == 13  # "plan-" + 8 hex chars

    def test_created_at_auto(self) -> None:
        plan = ExecutionPlan(goal="テスト")
        assert isinstance(plan.created_at, datetime)

    def test_get_step_found(self) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="step1"),
            PlanStep(step_id="s2", agent_id="a2", description="step2"),
        ]
        plan = ExecutionPlan(goal="テスト", steps=steps)
        assert plan.get_step("s1") is not None
        assert plan.get_step("s1").agent_id == "a1"  # type: ignore[union-attr]

    def test_get_step_not_found(self) -> None:
        plan = ExecutionPlan(goal="テスト")
        assert plan.get_step("nonexistent") is None

    def test_compute_overall_risk_empty(self) -> None:
        plan = ExecutionPlan(goal="テスト")
        assert plan.compute_overall_risk() == RiskLevel.LOW

    def test_compute_overall_risk_highest(self) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1", risk_level=RiskLevel.LOW),
            PlanStep(step_id="s2", agent_id="a2", description="d2", risk_level=RiskLevel.HIGH),
            PlanStep(step_id="s3", agent_id="a3", description="d3", risk_level=RiskLevel.MEDIUM),
        ]
        plan = ExecutionPlan(goal="テスト", steps=steps)
        assert plan.compute_overall_risk() == RiskLevel.HIGH

    def test_serialization_roundtrip(self) -> None:
        step = PlanStep(step_id="s1", agent_id="a1", description="d1")
        plan = ExecutionPlan(goal="テスト", steps=[step])
        data = plan.model_dump()
        restored = ExecutionPlan.model_validate(data)
        assert restored.goal == plan.goal
        assert len(restored.steps) == 1
        assert restored.steps[0].step_id == "s1"


class TestPlannerInput:
    """PlannerInput のテスト."""

    def test_minimal(self) -> None:
        inp = PlannerInput(user_request="データ分析して")
        assert inp.context == {}
        assert inp.constraints == []
        assert inp.available_agents == []


class TestPlannerOutput:
    """PlannerOutput のテスト."""

    def test_with_plan(self) -> None:
        plan = ExecutionPlan(goal="テスト")
        output = PlannerOutput(plan=plan, reasoning="理由", warnings=["注意"])
        assert output.reasoning == "理由"
        assert len(output.warnings) == 1


class TestReplanRequest:
    """ReplanRequest のテスト."""

    def test_basic(self) -> None:
        plan = ExecutionPlan(goal="テスト")
        req = ReplanRequest(
            original_plan=plan,
            failed_step_id="s2",
            failure_reason="タイムアウト",
        )
        assert req.failed_step_id == "s2"
        assert req.completed_results == {}
