"""Replanner のテスト."""

import pytest

from agentflow.patterns.planner import DynamicPlanner, Plan, Step
from agentflow.patterns.replanner import Replanner
from agentflow.patterns.validator import ValidationResult


class MockLLM:
    """テスト用のモック LLM."""

    async def generate(self, prompt: str) -> str:
        """モックレスポンスを返す."""
        return """Plan: Try a different approach
E1: calculate("2+2") - Calculate the sum
Plan: Verify the result
E2: llm("Verify #E1") - Verify the calculation
"""


@pytest.mark.asyncio
class TestReplanner:
    """Replanner クラスのテスト."""

    @pytest.fixture
    def planner(self) -> DynamicPlanner:
        """テスト用の DynamicPlanner を作成."""
        llm = MockLLM()
        return DynamicPlanner(
            llm=llm,
            available_tools=["search", "llm", "calculate"],
        )

    @pytest.fixture
    def replanner(self, planner: DynamicPlanner) -> Replanner:
        """テスト用の Replanner を作成."""
        return Replanner(planner=planner, max_replans=3)

    @pytest.fixture
    def original_plan(self) -> Plan:
        """元の計画を作成."""
        return Plan(
            task="Calculate 2+2",
            steps=[
                Step(
                    step_id="E1",
                    description="Search for calculator",
                    tool="search",
                    parameters={"query": "calculator"},
                    dependencies=[],
                ),
                Step(
                    step_id="E2",
                    description="Calculate",
                    tool="calculate",
                    parameters={"expression": "2+2"},
                    dependencies=["E1"],
                ),
            ],
            created_at="2024-01-01T00:00:00Z",
        )

    @pytest.fixture
    def failed_step(self) -> Step:
        """失敗したステップを作成."""
        return Step(
            step_id="E1",
            description="Search for calculator",
            tool="search",
            parameters={"query": "calculator"},
            dependencies=[],
        )

    @pytest.fixture
    def validation_result(self) -> ValidationResult:
        """検証結果を作成."""
        return ValidationResult(
            is_valid=False,
            confidence=0.8,
            error="Tool not found",
            suggestions=["Try a different tool", "Check tool availability"],
        )

    async def test_replan(
        self,
        replanner: Replanner,
        original_plan: Plan,
        failed_step: Step,
        validation_result: ValidationResult,
    ) -> None:
        """再計画の生成をテスト."""
        execution_results = {}

        new_plan = await replanner.replan(
            original_plan=original_plan,
            failed_step=failed_step,
            validation_result=validation_result,
            execution_results=execution_results,
        )

        assert isinstance(new_plan, Plan)
        assert len(new_plan.steps) > 0
        assert new_plan.steps[0].step_id == "E1"

    async def test_build_replan_context(
        self,
        replanner: Replanner,
        original_plan: Plan,
        failed_step: Step,
        validation_result: ValidationResult,
    ) -> None:
        """再計画コンテキストの構築をテスト."""
        execution_results = {
            "E0": {
                "success": True,
                "result": "Previous result",
            }
        }

        context = replanner._build_replan_context(
            original_plan=original_plan,
            failed_step=failed_step,
            validation_result=validation_result,
            execution_results=execution_results,
        )

        assert "original_task" in context
        assert "failed_step_id" in context
        assert "error" in context
        assert "suggestions" in context
        assert "successful_steps" in context
        assert context["original_task"] == original_plan.task
        assert context["failed_step_id"] == failed_step.step_id
        assert context["error"] == validation_result.error

    async def test_generate_replan_task(
        self,
        replanner: Replanner,
        original_plan: Plan,
        failed_step: Step,
        validation_result: ValidationResult,
    ) -> None:
        """再計画タスクの生成をテスト."""
        task = replanner._generate_replan_task(
            original_plan=original_plan,
            failed_step=failed_step,
            validation_result=validation_result,
        )

        assert isinstance(task, str)
        assert original_plan.task in task
        assert failed_step.description in task
        assert validation_result.error in task

