"""DynamicPlanner のテスト."""

import pytest

from agentflow.patterns.planner import DynamicPlanner, Plan, Step


class MockLLM:
    """テスト用のモック LLM."""

    async def generate(self, prompt: str) -> str:
        """モックレスポンスを返す."""
        return """Plan: I need to search for information
E1: search("AI agents") - Search for AI agent information
Plan: I need to analyze the results
E2: llm("Summarize #E1") - Summarize the search results
"""


@pytest.mark.asyncio
class TestStep:
    """Step クラスのテスト."""

    def test_step_creation(self) -> None:
        """Step の作成をテスト."""
        step = Step(
            step_id="E1",
            description="Search for information",
            tool="search",
            parameters={"query": "AI agents"},
            dependencies=[],
        )

        assert step.step_id == "E1"
        assert step.description == "Search for information"
        assert step.tool == "search"
        assert step.parameters == {"query": "AI agents"}
        assert step.dependencies == []

    def test_step_with_dependencies(self) -> None:
        """依存関係を持つ Step をテスト."""
        step = Step(
            step_id="E2",
            description="Analyze results",
            tool="llm",
            parameters={"text": "#E1"},
            dependencies=["E1"],
        )

        assert step.step_id == "E2"
        assert step.dependencies == ["E1"]
        assert "#E1" in step.parameters["text"]


@pytest.mark.asyncio
class TestPlan:
    """Plan クラスのテスト."""

    def test_plan_creation(self) -> None:
        """Plan の作成をテスト."""
        step1 = Step(
            step_id="E1",
            description="Search",
            tool="search",
            parameters={"query": "test"},
            dependencies=[],
        )
        step2 = Step(
            step_id="E2",
            description="Analyze",
            tool="llm",
            parameters={"text": "#E1"},
            dependencies=["E1"],
        )

        plan = Plan(
            task="Find and analyze information",
            steps=[step1, step2],
            created_at="2024-01-01T00:00:00Z",
            reasoning="Need to search first, then analyze",
        )

        assert plan.task == "Find and analyze information"
        assert len(plan.steps) == 2
        assert plan.steps[0].step_id == "E1"
        assert plan.steps[1].step_id == "E2"
        assert plan.reasoning == "Need to search first, then analyze"


@pytest.mark.asyncio
class TestDynamicPlanner:
    """DynamicPlanner クラスのテスト."""

    @pytest.fixture
    def planner(self) -> DynamicPlanner:
        """テスト用の DynamicPlanner を作成."""
        llm = MockLLM()
        return DynamicPlanner(
            llm=llm,
            available_tools=["search", "llm", "calculate"],
            max_steps=10,
        )

    async def test_create_plan(self, planner: DynamicPlanner) -> None:
        """計画の作成をテスト."""
        plan = await planner.create_plan("Find information about AI agents")

        assert isinstance(plan, Plan)
        assert plan.task == "Find information about AI agents"
        assert len(plan.steps) > 0
        assert plan.steps[0].step_id == "E1"

    async def test_create_plan_with_context(self, planner: DynamicPlanner) -> None:
        """コンテキスト付きの計画作成をテスト."""
        context = {"language": "en", "max_results": 5}
        plan = await planner.create_plan(
            "Search for AI frameworks",
            context=context,
        )

        assert isinstance(plan, Plan)
        assert len(plan.steps) > 0

    async def test_parse_plan(self, planner: DynamicPlanner) -> None:
        """計画のパースをテスト."""
        response = """Plan: Search for information
E1: search("AI agents") - Find AI agent information
Plan: Analyze results
E2: llm("Summarize #E1") - Summarize the results
"""
        plan = planner._parse_plan(response, "Test task")

        assert len(plan.steps) == 2
        assert plan.steps[0].step_id == "E1"
        assert plan.steps[0].tool == "search"
        assert plan.steps[1].step_id == "E2"
        assert plan.steps[1].tool == "llm"
        assert "E1" in plan.steps[1].dependencies

    async def test_parse_step_line(self, planner: DynamicPlanner) -> None:
        """ステップ行のパースをテスト."""
        line = 'E1: search("AI agents") - Find information about AI agents'
        step = planner._parse_step_line(line)

        assert step.step_id == "E1"
        assert step.tool == "search"
        assert step.description == "Find information about AI agents"
        assert "query" in step.parameters

    async def test_fallback_plan(self, planner: DynamicPlanner) -> None:
        """フォールバック計画をテスト."""
        # 空のレスポンスでフォールバック計画が生成されることを確認
        plan = planner._parse_plan("", "Test task")

        assert len(plan.steps) == 1
        assert plan.steps[0].step_id == "E1"

