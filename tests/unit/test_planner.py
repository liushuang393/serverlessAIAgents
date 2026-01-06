# -*- coding: utf-8 -*-
"""PlannerAgent パターンのユニットテスト."""

import pytest

from agentflow.patterns.planner import (
    Plan,
    PlanExecutor,
    PlannerAgent,
    PlanResult,
    PlanStatus,
    Step,
    StepStatus,
)


class TestStepAndPlan:
    """Step と Plan のデータモデルテスト."""

    def test_step_creation(self) -> None:
        """Step の作成テスト."""
        step = Step(
            id="step-1",
            name="分析",
            description="データを分析する",
            agent="AnalysisAgent",
        )

        assert step.id == "step-1"
        assert step.name == "分析"
        assert step.status == StepStatus.PENDING
        assert step.agent == "AnalysisAgent"

    def test_plan_creation(self) -> None:
        """Plan の作成テスト."""
        steps = [
            Step(id="s1", name="Step 1"),
            Step(id="s2", name="Step 2", dependencies=["s1"]),
        ]
        plan = Plan(goal="テスト目標", steps=steps)

        assert plan.goal == "テスト目標"
        assert len(plan.steps) == 2
        assert plan.status == PlanStatus.DRAFT

    def test_plan_get_next_step(self) -> None:
        """次のステップ取得テスト."""
        steps = [
            Step(id="s1", name="Step 1"),
            Step(id="s2", name="Step 2", dependencies=["s1"]),
            Step(id="s3", name="Step 3", dependencies=["s2"]),
        ]
        plan = Plan(goal="test", steps=steps)

        # 最初は s1 が次のステップ
        next_step = plan.get_next_step()
        assert next_step is not None
        assert next_step.id == "s1"

        # s1 を完了
        steps[0].status = StepStatus.COMPLETED

        # s1 完了後は s2 が次
        next_step = plan.get_next_step()
        assert next_step is not None
        assert next_step.id == "s2"

    def test_plan_progress(self) -> None:
        """進捗率テスト."""
        steps = [
            Step(id="s1", name="Step 1", status=StepStatus.COMPLETED),
            Step(id="s2", name="Step 2", status=StepStatus.COMPLETED),
            Step(id="s3", name="Step 3", status=StepStatus.PENDING),
            Step(id="s4", name="Step 4", status=StepStatus.PENDING),
        ]
        plan = Plan(goal="test", steps=steps)

        assert plan.get_progress() == 0.5

    def test_plan_is_complete(self) -> None:
        """完了判定テスト."""
        steps = [
            Step(id="s1", name="Step 1", status=StepStatus.COMPLETED),
            Step(id="s2", name="Step 2", status=StepStatus.SKIPPED),
        ]
        plan = Plan(goal="test", steps=steps)

        assert plan.is_complete() is True

    def test_plan_has_failed(self) -> None:
        """失敗判定テスト."""
        steps = [
            Step(id="s1", name="Step 1", status=StepStatus.COMPLETED),
            Step(id="s2", name="Step 2", status=StepStatus.FAILED),
        ]
        plan = Plan(goal="test", steps=steps)

        assert plan.has_failed() is True


class TestPlannerAgent:
    """PlannerAgent のテスト."""

    @pytest.fixture
    def planner(self) -> PlannerAgent:
        """テスト用 PlannerAgent."""
        return PlannerAgent(llm=None, max_steps=5)

    @pytest.mark.asyncio
    async def test_create_simple_plan(self, planner: PlannerAgent) -> None:
        """シンプルな計画生成テスト（LLM なし）."""
        plan = await planner.create_plan(
            goal="Webアプリをデプロイする",
            context={"stack": "FastAPI"},
        )

        assert plan.goal == "Webアプリをデプロイする"
        assert len(plan.steps) == 4  # 分析、設計、実行、検証
        assert plan.status == PlanStatus.DRAFT

    @pytest.mark.asyncio
    async def test_replan(self, planner: PlannerAgent) -> None:
        """再計画テスト."""
        original = Plan(
            goal="test",
            steps=[
                Step(id="s1", name="Step 1", status=StepStatus.COMPLETED),
                Step(id="s2", name="Step 2", status=StepStatus.FAILED, error="Error"),
                Step(id="s3", name="Step 3"),
            ],
        )

        new_plan = await planner.replan(original, original.steps[1], "テストエラー")

        assert new_plan.status == PlanStatus.REPLANNING
        # s1 は完了のまま
        assert new_plan.steps[0].status == StepStatus.COMPLETED
        # s2 は PENDING にリセット
        assert new_plan.steps[1].status == StepStatus.PENDING


class TestPlanExecutor:
    """PlanExecutor のテスト."""

    @pytest.fixture
    def mock_agent(self) -> object:
        """モック Agent."""
        class MockAgent:
            async def run(self, inputs: dict) -> dict:
                return {"status": "ok", "processed": inputs}
        return MockAgent()

    @pytest.mark.asyncio
    async def test_execute_plan(self, mock_agent: object) -> None:
        """計画実行テスト."""
        plan = Plan(
            goal="テスト",
            steps=[
                Step(id="s1", name="Step 1", agent="mock"),
                Step(id="s2", name="Step 2", agent="mock", dependencies=["s1"]),
            ],
        )

        executor = PlanExecutor(agents={"mock": mock_agent})
        result = await executor.execute(plan)

        assert result.success is True
        assert result.plan.status == PlanStatus.COMPLETED
        assert "s1" in result.outputs
        assert "s2" in result.outputs

