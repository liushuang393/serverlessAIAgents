"""PlanExecutor のテスト."""

import pytest

from agentflow.patterns.executor import ExecutionError, PlanExecutor
from agentflow.patterns.planner import Plan, Step


class MockToolCaller:
    """テスト用のモックツール呼び出し."""

    def __init__(self) -> None:
        """初期化."""
        self.call_count = 0
        self.calls: list[dict] = []

    async def call_tool(
        self,
        tool_uri: str,
        arguments: dict,
        user_id: str,
    ) -> dict:
        """モックツール呼び出し."""
        self.call_count += 1
        self.calls.append({
            "tool_uri": tool_uri,
            "arguments": arguments,
            "user_id": user_id,
        })

        # ツールに応じた結果を返す
        if "search" in tool_uri:
            return {
                "success": True,
                "result": "AI agents are software programs",
            }
        if "llm" in tool_uri:
            return {
                "success": True,
                "result": "Summary: AI agents are intelligent software",
            }
        return {
            "success": True,
            "result": "42",
        }


@pytest.mark.asyncio
class TestPlanExecutor:
    """PlanExecutor クラスのテスト."""

    @pytest.fixture
    def tool_caller(self) -> MockToolCaller:
        """テスト用のツール呼び出しを作成."""
        return MockToolCaller()

    @pytest.fixture
    def executor(self, tool_caller: MockToolCaller) -> PlanExecutor:
        """テスト用の PlanExecutor を作成."""
        return PlanExecutor(tool_caller=tool_caller)

    @pytest.fixture
    def simple_plan(self) -> Plan:
        """シンプルな計画を作成."""
        return Plan(
            task="Test task",
            steps=[
                Step(
                    step_id="E1",
                    description="Search",
                    tool="search",
                    parameters={"query": "AI agents"},
                    dependencies=[],
                )
            ],
            created_at="2024-01-01T00:00:00Z",
        )

    @pytest.fixture
    def multi_step_plan(self) -> Plan:
        """複数ステップの計画を作成."""
        return Plan(
            task="Test task",
            steps=[
                Step(
                    step_id="E1",
                    description="Search",
                    tool="search",
                    parameters={"query": "AI agents"},
                    dependencies=[],
                ),
                Step(
                    step_id="E2",
                    description="Summarize",
                    tool="llm",
                    parameters={"text": "#E1"},
                    dependencies=["E1"],
                ),
            ],
            created_at="2024-01-01T00:00:00Z",
        )

    async def test_execute_simple_plan(
        self,
        executor: PlanExecutor,
        simple_plan: Plan,
        tool_caller: MockToolCaller,
    ) -> None:
        """シンプルな計画の実行をテスト."""
        results = await executor.execute_plan(simple_plan)

        assert "E1" in results
        assert results["E1"]["success"] is True
        assert "AI agents" in results["E1"]["result"]
        assert tool_caller.call_count == 1

    async def test_execute_multi_step_plan(
        self,
        executor: PlanExecutor,
        multi_step_plan: Plan,
        tool_caller: MockToolCaller,
    ) -> None:
        """複数ステップの計画実行をテスト."""
        results = await executor.execute_plan(multi_step_plan)

        assert "E1" in results
        assert "E2" in results
        assert results["E1"]["success"] is True
        assert results["E2"]["success"] is True
        assert tool_caller.call_count == 2

    async def test_variable_substitution(
        self,
        executor: PlanExecutor,
        multi_step_plan: Plan,
    ) -> None:
        """変数置換をテスト."""
        results = await executor.execute_plan(multi_step_plan)

        # E2 のパラメータに E1 の結果が置換されていることを確認
        e2_call = [c for c in executor._tool_caller.calls if "llm" in c["tool_uri"]][0]
        assert "AI agents are software programs" in e2_call["arguments"]["text"]

    async def test_resolve_variables(self, executor: PlanExecutor) -> None:
        """変数解決をテスト."""
        parameters = {"text": "Summary of #E1 and #E2"}
        results = {
            "E1": {"result": "First result"},
            "E2": {"result": "Second result"},
        }

        resolved = executor._resolve_variables(parameters, results)

        assert "First result" in resolved["text"]
        assert "Second result" in resolved["text"]

    async def test_substitute_references(self, executor: PlanExecutor) -> None:
        """参照置換をテスト."""
        text = "Analyze #E1 and compare with #E2"
        results = {
            "E1": {"result": "Data A"},
            "E2": {"result": "Data B"},
        }

        substituted = executor._substitute_references(text, results)

        assert "Data A" in substituted
        assert "Data B" in substituted
        assert "#E1" not in substituted
        assert "#E2" not in substituted

