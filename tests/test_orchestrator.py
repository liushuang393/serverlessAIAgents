"""Orchestrator のテスト."""

import pytest

from agentflow.orchestration.executor import (
    ExecutorAgent,
    StepResult,
)
from agentflow.orchestration.monitor import (
    AlertSeverity,
    MonitorAgent,
    MonitorEvent,
    MonitorEventType,
)
from agentflow.orchestration.orchestrator import (
    ExecutionContext,
    ExecutionStatus,
    Orchestrator,
    OrchestratorConfig,
)
from agentflow.orchestration.planner import (
    ExecutionPlan,
    PlannerAgent,
    PlanStep,
    StepStatus,
    StepType,
)


class TestPlanStep:
    """PlanStep のテスト."""

    def test_create_step(self) -> None:
        """ステップ作成."""
        step = PlanStep(
            name="テストステップ",
            description="テスト用のステップです",
            step_type=StepType.TOOL_CALL,
            tool_uri="builtin://calculator",
        )

        assert step.name == "テストステップ"
        assert step.step_type == StepType.TOOL_CALL
        assert step.status == StepStatus.PENDING

    def test_step_with_dependencies(self) -> None:
        """依存関係付きステップ."""
        step1 = PlanStep(name="ステップ1")
        step2 = PlanStep(name="ステップ2", dependencies=[step1.id])

        assert step1.id in step2.dependencies


class TestExecutionPlan:
    """ExecutionPlan のテスト."""

    def test_create_plan(self) -> None:
        """計画作成."""
        plan = ExecutionPlan(
            name="テスト計画",
            goal="テストを実行する",
        )

        assert plan.name == "テスト計画"
        assert plan.goal == "テストを実行する"
        assert plan.total_steps == 0

    def test_get_progress_empty(self) -> None:
        """空の計画の進捗."""
        plan = ExecutionPlan()

        assert plan.get_progress() == 0.0

    def test_get_progress_with_steps(self) -> None:
        """ステップ付き計画の進捗."""
        step1 = PlanStep(name="ステップ1", status=StepStatus.COMPLETED)
        step2 = PlanStep(name="ステップ2", status=StepStatus.PENDING)

        plan = ExecutionPlan(steps=[step1, step2])

        assert plan.get_progress() == 0.5

    def test_get_ready_steps(self) -> None:
        """実行準備完了ステップ取得."""
        step1 = PlanStep(name="ステップ1", status=StepStatus.COMPLETED)
        step2 = PlanStep(name="ステップ2", dependencies=[step1.id])
        step3 = PlanStep(name="ステップ3", dependencies=[step2.id])

        plan = ExecutionPlan(steps=[step1, step2, step3])

        ready = plan.get_ready_steps()
        assert len(ready) == 1
        assert ready[0].id == step2.id


class TestPlannerAgent:
    """PlannerAgent のテスト."""

    @pytest.fixture
    def planner(self) -> PlannerAgent:
        """計画Agentを作成."""
        return PlannerAgent()

    @pytest.mark.asyncio
    async def test_create_simple_plan(self, planner: PlannerAgent) -> None:
        """シンプルな計画作成（LLMなし）."""
        plan = await planner.create_plan(
            goal="テストを実行する",
            context={"test": True},
        )

        assert plan.goal == "テストを実行する"
        assert len(plan.steps) > 0
        assert plan.total_steps > 0


class TestStepResult:
    """StepResult のテスト."""

    def test_create_success_result(self) -> None:
        """成功結果作成."""
        result = StepResult(
            step_id="step-123",
            success=True,
            output={"data": "test"},
            duration_ms=100.0,
        )

        assert result.success is True
        assert result.output == {"data": "test"}

    def test_create_failure_result(self) -> None:
        """失敗結果作成."""
        result = StepResult(
            step_id="step-123",
            success=False,
            error="エラーが発生しました",
        )

        assert result.success is False
        assert result.error == "エラーが発生しました"

    def test_to_dict(self) -> None:
        """辞書変換."""
        result = StepResult(
            step_id="step-123",
            success=True,
            output="test",
        )

        data = result.to_dict()

        assert data["step_id"] == "step-123"
        assert data["success"] is True


class TestExecutorAgent:
    """ExecutorAgent のテスト."""

    @pytest.fixture
    def executor(self) -> ExecutorAgent:
        """実行Agentを作成."""
        return ExecutorAgent()

    @pytest.mark.asyncio
    async def test_execute_llm_step_without_llm(self, executor: ExecutorAgent) -> None:
        """LLMなしでLLMステップ実行."""
        step = PlanStep(
            name="LLMステップ",
            step_type=StepType.LLM_GENERATION,
        )

        result = await executor.execute_step(step)

        assert result.success is False
        assert "LLMクライアント" in result.error

    def test_get_stats(self, executor: ExecutorAgent) -> None:
        """統計情報取得."""
        stats = executor.get_stats()

        assert "total_steps" in stats
        assert "success_count" in stats
        assert stats["has_llm"] is False


class TestMonitorEvent:
    """MonitorEvent のテスト."""

    def test_create_event(self) -> None:
        """イベント作成."""
        event = MonitorEvent(
            event_type=MonitorEventType.PROGRESS,
            execution_id="exec-123",
            message="進捗50%",
            data={"progress": 0.5},
        )

        assert event.event_type == MonitorEventType.PROGRESS
        assert event.execution_id == "exec-123"

    def test_to_json(self) -> None:
        """JSON変換."""
        event = MonitorEvent(
            event_type=MonitorEventType.ALERT,
            execution_id="exec-123",
            severity=AlertSeverity.WARNING,
        )

        json_str = event.to_json()

        assert "alert" in json_str
        assert "exec-123" in json_str


class TestMonitorAgent:
    """MonitorAgent のテスト。

    start_monitoring は asyncio.create_task を使うため、各テストを async にする。
    """

    @pytest.fixture
    def monitor(self) -> MonitorAgent:
        """監視Agentを作成."""
        return MonitorAgent()

    @pytest.mark.asyncio
    async def test_start_monitoring(self, monitor: MonitorAgent) -> None:
        """監視開始."""
        monitor.start_monitoring("exec-123")

        state = monitor.get_execution_state("exec-123")
        assert state is not None
        assert state.execution_id == "exec-123"

    @pytest.mark.asyncio
    async def test_stop_monitoring(self, monitor: MonitorAgent) -> None:
        """監視停止."""
        monitor.start_monitoring("exec-123")
        monitor.stop_monitoring("exec-123")

        state = monitor.get_execution_state("exec-123")
        assert state is None

    @pytest.mark.asyncio
    async def test_report_progress(self, monitor: MonitorAgent) -> None:
        """進捗報告."""
        monitor.start_monitoring("exec-123")
        monitor.report_progress("exec-123", 0.5, "処理中")

        state = monitor.get_execution_state("exec-123")
        assert state.progress == 0.5

    @pytest.mark.asyncio
    async def test_report_step_completed(self, monitor: MonitorAgent) -> None:
        """ステップ完了報告."""
        monitor.start_monitoring("exec-123")
        monitor.report_step_completed("exec-123", "step-1", "テストステップ", 100.0)

        state = monitor.get_execution_state("exec-123")
        assert state.completed_steps == 1
        assert state.consecutive_failures == 0

    @pytest.mark.asyncio
    async def test_report_step_failed(self, monitor: MonitorAgent) -> None:
        """ステップ失敗報告."""
        monitor.start_monitoring("exec-123")
        monitor.report_step_failed("exec-123", "step-1", "テストステップ", "エラー")

        state = monitor.get_execution_state("exec-123")
        assert state.failed_steps == 1
        assert state.consecutive_failures == 1


class TestExecutionContext:
    """ExecutionContext のテスト."""

    def test_create_context(self) -> None:
        """コンテキスト作成."""
        ctx = ExecutionContext(
            execution_id="exec-123",
            task="テストタスク",
        )

        assert ctx.execution_id == "exec-123"
        assert ctx.task == "テストタスク"
        assert ctx.status == ExecutionStatus.PENDING

    def test_to_dict(self) -> None:
        """辞書変換."""
        ctx = ExecutionContext(
            execution_id="exec-123",
            task="テスト",
            progress=0.5,
        )

        data = ctx.to_dict()

        assert data["execution_id"] == "exec-123"
        assert data["progress"] == 0.5


class TestOrchestrator:
    """Orchestrator のテスト."""

    @pytest.fixture
    def orchestrator(self) -> Orchestrator:
        """オーケストレーターを作成."""
        return Orchestrator(
            config=OrchestratorConfig(enable_monitoring=False),
        )

    def test_create_orchestrator(self, orchestrator: Orchestrator) -> None:
        """オーケストレーター作成."""
        assert orchestrator is not None
        assert orchestrator._initialized is False

    @pytest.mark.asyncio
    async def test_initialize(self, orchestrator: Orchestrator) -> None:
        """初期化."""
        await orchestrator.initialize()

        assert orchestrator._initialized is True

    def test_get_stats(self, orchestrator: Orchestrator) -> None:
        """統計情報取得."""
        stats = orchestrator.get_stats()

        assert "initialized" in stats
        assert "total_executions" in stats
        assert "config" in stats

    @pytest.mark.asyncio
    async def test_close(self, orchestrator: Orchestrator) -> None:
        """終了処理."""
        await orchestrator.initialize()
        await orchestrator.close()

        assert orchestrator._initialized is False
