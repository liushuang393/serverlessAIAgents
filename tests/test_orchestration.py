"""Orchestration モジュール単体テスト."""

import pytest

from agentflow.orchestration.executor import (
    ExecutorAgent,
    ExecutorConfig,
    StepResult,
)
from agentflow.orchestration.monitor import (
    AlertSeverity,
    MonitorAgent,
    MonitorEvent,
    MonitorEventType,
    MonitorThresholds,
)
from agentflow.orchestration.orchestrator import (
    ExecutionContext,
    ExecutionResult,
    ExecutionStatus,
    Orchestrator,
    OrchestratorConfig,
)
from agentflow.orchestration.planner import (
    ExecutionPlan,
    PlannerAgent,
    PlannerConfig,
    PlanStep,
    StepStatus,
    StepType,
)


class TestPlanStep:
    """PlanStep テストクラス."""

    def test_create_step(self) -> None:
        """ステップを作成できること."""
        step = PlanStep(
            name="分析",
            description="データを分析する",
            step_type=StepType.TOOL_CALL,
            tool_uri="builtin://analyzer",
        )

        assert step.name == "分析"
        assert step.step_type == StepType.TOOL_CALL
        assert step.status == StepStatus.PENDING

    def test_step_with_dependencies(self) -> None:
        """依存関係付きステップを作成できること."""
        step1 = PlanStep(name="ステップ1")
        step2 = PlanStep(name="ステップ2", dependencies=[step1.id])

        assert step2.dependencies == [step1.id]


class TestExecutionPlan:
    """ExecutionPlan テストクラス."""

    def test_create_plan(self) -> None:
        """計画を作成できること."""
        plan = ExecutionPlan(
            name="テスト計画",
            goal="テストを実行する",
            steps=[
                PlanStep(name="ステップ1"),
                PlanStep(name="ステップ2"),
            ],
        )

        assert plan.name == "テスト計画"
        assert len(plan.steps) == 2

    def test_get_ready_steps(self) -> None:
        """準備完了ステップを取得できること."""
        step1 = PlanStep(name="ステップ1")
        step2 = PlanStep(name="ステップ2", dependencies=[step1.id])

        plan = ExecutionPlan(steps=[step1, step2])

        # 最初はステップ1のみ準備完了
        ready = plan.get_ready_steps()
        assert len(ready) == 1
        assert ready[0].id == step1.id

        # ステップ1を完了
        step1.status = StepStatus.COMPLETED

        # ステップ2が準備完了になる
        ready = plan.get_ready_steps()
        assert len(ready) == 1
        assert ready[0].id == step2.id

    def test_get_progress(self) -> None:
        """進捗率を計算できること."""
        step1 = PlanStep(name="ステップ1")
        step2 = PlanStep(name="ステップ2")
        plan = ExecutionPlan(steps=[step1, step2])

        assert plan.get_progress() == 0.0

        step1.status = StepStatus.COMPLETED
        assert plan.get_progress() == 0.5

        step2.status = StepStatus.COMPLETED
        assert plan.get_progress() == 1.0


class TestPlannerAgent:
    """PlannerAgent テストクラス."""

    @pytest.fixture
    def planner(self) -> PlannerAgent:
        """Plannerを作成."""
        return PlannerAgent(config=PlannerConfig(max_steps=10))

    @pytest.mark.asyncio
    async def test_create_simple_plan(self, planner: PlannerAgent) -> None:
        """シンプルな計画を作成できること."""
        plan = await planner.create_plan(
            goal="テストタスクを実行",
            context={"key": "value"},
        )

        assert plan is not None
        assert plan.goal == "テストタスクを実行"
        assert len(plan.steps) > 0

    @pytest.mark.asyncio
    async def test_plan_has_dependencies(self, planner: PlannerAgent) -> None:
        """計画に依存関係が設定されること."""
        plan = await planner.create_plan(goal="複雑なタスク")

        # 少なくとも1つのステップに依存関係がある
        has_deps = any(len(step.dependencies) > 0 for step in plan.steps)
        assert has_deps or len(plan.steps) == 1


class TestStepResult:
    """StepResult テストクラス."""

    def test_create_success_result(self) -> None:
        """成功結果を作成できること."""
        result = StepResult(
            step_id="step-001",
            success=True,
            output={"data": "value"},
            duration_ms=150.0,
        )

        assert result.success is True
        assert result.output == {"data": "value"}

    def test_create_failure_result(self) -> None:
        """失敗結果を作成できること."""
        result = StepResult(
            step_id="step-002",
            success=False,
            error="実行エラー",
        )

        assert result.success is False
        assert result.error == "実行エラー"

    def test_to_dict(self) -> None:
        """辞書変換が正しいこと."""
        result = StepResult(
            step_id="step-003",
            success=True,
            duration_ms=200.0,
        )

        data = result.to_dict()
        assert data["step_id"] == "step-003"
        assert data["success"] is True


class TestExecutorAgent:
    """ExecutorAgent テストクラス."""

    @pytest.fixture
    def executor(self) -> ExecutorAgent:
        """Executorを作成."""
        return ExecutorAgent(config=ExecutorConfig(max_retries=2))

    @pytest.mark.asyncio
    async def test_execute_llm_step_without_llm(self, executor: ExecutorAgent) -> None:
        """LLMなしでLLMステップを実行するとエラーになること."""
        step = PlanStep(
            name="LLMステップ",
            step_type=StepType.LLM_GENERATION,
        )

        result = await executor.execute_step(step)
        assert result.success is False

    def test_get_stats(self, executor: ExecutorAgent) -> None:
        """統計情報を取得できること."""
        stats = executor.get_stats()

        assert "total_steps" in stats
        assert "success_count" in stats
        assert "has_llm" in stats


class TestMonitorEvent:
    """MonitorEvent テストクラス."""

    def test_create_event(self) -> None:
        """イベントを作成できること."""
        event = MonitorEvent(
            event_type=MonitorEventType.PROGRESS,
            execution_id="exec-001",
            message="進捗: 50%",
            data={"progress": 0.5},
        )

        assert event.event_type == MonitorEventType.PROGRESS
        assert event.execution_id == "exec-001"

    def test_to_dict(self) -> None:
        """辞書変換が正しいこと."""
        event = MonitorEvent(
            event_type=MonitorEventType.ALERT,
            execution_id="exec-002",
            severity=AlertSeverity.WARNING,
        )

        data = event.to_dict()
        assert data["event_type"] == "alert"
        assert data["severity"] == "warning"


class TestMonitorAgent:
    """MonitorAgent テストクラス."""

    @pytest.fixture
    def monitor(self) -> MonitorAgent:
        """Monitorを作成."""
        return MonitorAgent(thresholds=MonitorThresholds())

    async def test_start_monitoring(self, monitor: MonitorAgent) -> None:
        """監視を開始できること."""
        monitor.start_monitoring("exec-001")

        state = monitor.get_execution_state("exec-001")
        assert state is not None
        assert state.execution_id == "exec-001"
        assert state.is_active is True

        # クリーンアップ
        monitor.stop_monitoring("exec-001")

    async def test_stop_monitoring(self, monitor: MonitorAgent) -> None:
        """監視を停止できること."""
        monitor.start_monitoring("exec-002")
        monitor.stop_monitoring("exec-002")

        state = monitor.get_execution_state("exec-002")
        assert state is None

    async def test_report_progress(self, monitor: MonitorAgent) -> None:
        """進捗を報告できること."""
        monitor.start_monitoring("exec-003")
        monitor.report_progress("exec-003", 0.5, "処理中...")

        state = monitor.get_execution_state("exec-003")
        assert state is not None
        assert state.progress == 0.5

        # クリーンアップ
        monitor.stop_monitoring("exec-003")

    async def test_report_step_completed(self, monitor: MonitorAgent) -> None:
        """ステップ完了を報告できること."""
        monitor.start_monitoring("exec-004")
        monitor.report_step_completed("exec-004", "step-001", "分析", 100.0)

        state = monitor.get_execution_state("exec-004")
        assert state is not None
        assert state.completed_steps == 1
        assert state.consecutive_failures == 0

        # クリーンアップ
        monitor.stop_monitoring("exec-004")

    async def test_report_step_failed(self, monitor: MonitorAgent) -> None:
        """ステップ失敗を報告できること."""
        monitor.start_monitoring("exec-005")
        monitor.report_step_failed("exec-005", "step-001", "分析", "エラー")

        state = monitor.get_execution_state("exec-005")
        assert state is not None
        assert state.failed_steps == 1
        assert state.consecutive_failures == 1

        # クリーンアップ
        monitor.stop_monitoring("exec-005")


class TestExecutionContext:
    """ExecutionContext テストクラス."""

    def test_create_context(self) -> None:
        """コンテキストを作成できること."""
        ctx = ExecutionContext(
            execution_id="exec-001",
            task="テストタスク",
        )

        assert ctx.execution_id == "exec-001"
        assert ctx.task == "テストタスク"
        assert ctx.status == ExecutionStatus.PENDING

    def test_to_dict(self) -> None:
        """辞書変換が正しいこと."""
        ctx = ExecutionContext(
            execution_id="exec-002",
            task="タスク",
            progress=0.5,
        )

        data = ctx.to_dict()
        assert data["execution_id"] == "exec-002"
        assert data["progress"] == 0.5


class TestExecutionResult:
    """ExecutionResult テストクラス."""

    def test_create_success_result(self) -> None:
        """成功結果を作成できること."""
        result = ExecutionResult(
            execution_id="exec-001",
            success=True,
            output={"result": "完了"},
            duration_ms=1000.0,
        )

        assert result.success is True
        assert result.output == {"result": "完了"}

    def test_create_failure_result(self) -> None:
        """失敗結果を作成できること."""
        result = ExecutionResult(
            execution_id="exec-002",
            success=False,
            error="実行エラー",
        )

        assert result.success is False
        assert result.error == "実行エラー"


class TestOrchestrator:
    """Orchestrator テストクラス."""

    @pytest.fixture
    def orchestrator(self) -> Orchestrator:
        """Orchestratorを作成."""
        return Orchestrator(
            config=OrchestratorConfig(
                max_steps=5,
                enable_monitoring=False,  # テスト用に無効化
            ),
        )

    def test_create_orchestrator(self, orchestrator: Orchestrator) -> None:
        """Orchestratorを作成できること."""
        assert orchestrator is not None

    def test_get_stats(self, orchestrator: Orchestrator) -> None:
        """統計情報を取得できること."""
        stats = orchestrator.get_stats()

        assert "initialized" in stats
        assert "total_executions" in stats
        assert "config" in stats
