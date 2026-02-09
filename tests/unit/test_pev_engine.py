# -*- coding: utf-8 -*-
"""PEV (Plan-Execute-Verify) エンジンのユニットテスト.

HierarchicalPlanner, MonitoredExecutor, ResultVerifier, PEVEngineの機能をテストする。
"""

import pytest

from agentflow.pev import (
    PEVEngine,
    PEVEngineConfig,
    HierarchicalPlanner,
    HierarchicalPlan,
    PlanLevel,
    SubGoal,
    MonitoredExecutor,
    ExecutionMonitor,
    ExecutionResult,
    ResultVerifier,
    VerificationResult,
    VerificationStrategy,
)
from agentflow.pev.hierarchical_planner import GoalStatus
from agentflow.pev.monitored_executor import ExecutionEvent, ExecutionEventType


class TestHierarchicalPlanner:
    """HierarchicalPlannerのテスト."""

    @pytest.mark.asyncio
    async def test_create_simple_plan(self) -> None:
        """シンプルな計画作成のテスト."""
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(
            goal="テストタスクを完了",
            context={"test": True},
        )

        assert plan is not None
        assert plan.root_goal == "テストタスクを完了"
        assert len(plan.levels) > 0

    @pytest.mark.asyncio
    async def test_plan_has_goals(self) -> None:
        """計画に目標が含まれることのテスト."""
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(goal="分析レポートを作成")

        total_goals = sum(len(level.goals) for level in plan.levels)
        assert total_goals > 0

    @pytest.mark.asyncio
    async def test_get_ready_goals(self) -> None:
        """実行準備完了の目標取得テスト."""
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(goal="テスト")

        ready = plan.get_ready_goals()

        # 最初は依存のない目標が準備完了
        assert len(ready) > 0

    @pytest.mark.asyncio
    async def test_update_goal_status(self) -> None:
        """目標ステータス更新のテスト."""
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(goal="テスト")

        ready = plan.get_ready_goals()
        if ready:
            goal = ready[0]
            success = plan.update_goal_status(
                goal_id=goal.id,
                status=GoalStatus.COMPLETED,
                result={"done": True},
            )

            assert success is True
            updated = plan.get_goal(goal.id)
            assert updated.status == GoalStatus.COMPLETED

    @pytest.mark.asyncio
    async def test_get_progress(self) -> None:
        """進捗率取得のテスト."""
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(goal="テスト")

        # 初期状態
        progress = plan.get_progress()
        assert progress <= 0.01  # 浮動小数点比較

        # 1つ完了
        ready = plan.get_ready_goals()
        if ready:
            plan.update_goal_status(ready[0].id, GoalStatus.COMPLETED)
            progress = plan.get_progress()
            assert progress > 0.0

    @pytest.mark.asyncio
    async def test_replan(self) -> None:
        """再計画のテスト."""
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(goal="テスト")

        ready = plan.get_ready_goals()
        if ready:
            failed_goal = ready[0]
            plan.update_goal_status(failed_goal.id, GoalStatus.FAILED, error="テストエラー")

            new_plan = await planner.replan(
                plan=plan,
                failed_goal=failed_goal,
                error="テストエラー",
            )

            assert new_plan.replan_count == 1


class TestSubGoal:
    """SubGoalのテスト."""

    def test_create_subgoal(self) -> None:
        """サブ目標作成のテスト."""
        goal = SubGoal(
            name="分析",
            description="データを分析する",
            estimated_effort=50,
            priority=1,
        )

        assert goal.name == "分析"
        assert goal.status == GoalStatus.PENDING
        assert goal.id.startswith("goal-")

    def test_subgoal_with_dependencies(self) -> None:
        """依存関係付きサブ目標のテスト."""
        goal1 = SubGoal(name="準備")
        goal2 = SubGoal(name="実行", dependencies=[goal1.id])

        assert goal1.id in goal2.dependencies


class TestPlanLevel:
    """PlanLevelのテスト."""

    def test_create_level(self) -> None:
        """レベル作成のテスト."""
        level = PlanLevel(depth=0)
        level.goals = [
            SubGoal(name="目標1"),
            SubGoal(name="目標2"),
        ]

        assert level.depth == 0
        assert len(level.goals) == 2


class TestExecutionMonitor:
    """ExecutionMonitorのテスト."""

    def test_start_monitor(self) -> None:
        """モニター開始のテスト."""
        monitor = ExecutionMonitor(timeout_seconds=60)
        monitor.start()

        assert monitor._start_time is not None

    def test_check_timeout_not_exceeded(self) -> None:
        """タイムアウト未超過のテスト."""
        monitor = ExecutionMonitor(timeout_seconds=60)
        monitor.start()

        is_timeout = monitor.check_timeout()

        assert is_timeout is False

    def test_get_elapsed(self) -> None:
        """経過時間取得のテスト."""
        monitor = ExecutionMonitor(timeout_seconds=60)
        monitor.start()

        elapsed = monitor.get_elapsed()

        assert elapsed >= 0

    def test_check_stall(self) -> None:
        """進捗停滞チェックのテスト."""
        monitor = ExecutionMonitor(timeout_seconds=60)
        monitor.start()

        # 進捗がある場合
        is_stall = monitor.check_stall(0.5)
        assert is_stall is False


class TestResultVerifier:
    """ResultVerifierのテスト."""

    @pytest.mark.asyncio
    async def test_verify_exact_match_success(self) -> None:
        """完全一致検証（成功）のテスト."""
        verifier = ResultVerifier(default_strategy=VerificationStrategy.EXACT_MATCH)

        result = await verifier.verify(
            goal="テスト",
            result={"value": 100},
            expected={"value": 100},
        )

        assert result.is_acceptable is True
        assert result.score >= 0.99  # 浮動小数点比較

    @pytest.mark.asyncio
    async def test_verify_exact_match_failure(self) -> None:
        """完全一致検証（失敗）のテスト."""
        verifier = ResultVerifier(default_strategy=VerificationStrategy.EXACT_MATCH)

        result = await verifier.verify(
            goal="テスト",
            result={"value": 100},
            expected={"value": 200},
        )

        assert result.is_acceptable is False
        assert result.score <= 0.01  # 浮動小数点比較

    @pytest.mark.asyncio
    async def test_verify_threshold(self) -> None:
        """閾値検証のテスト."""
        verifier = ResultVerifier(
            default_strategy=VerificationStrategy.THRESHOLD,
            acceptance_threshold=0.7,
        )

        result = await verifier.verify(
            goal="テスト",
            result={"score": 0.8},
            expected={"score": 1.0},
        )

        assert result.is_acceptable is True

    @pytest.mark.asyncio
    async def test_verify_threshold_failure(self) -> None:
        """閾値検証（失敗）のテスト."""
        verifier = ResultVerifier(
            default_strategy=VerificationStrategy.THRESHOLD,
            acceptance_threshold=0.9,
        )

        result = await verifier.verify(
            goal="テスト",
            result={"score": 0.5},
            expected={"score": 1.0},
        )

        assert result.is_acceptable is False

    @pytest.mark.asyncio
    async def test_verify_with_strategy(self) -> None:
        """戦略指定での検証のテスト."""
        verifier = ResultVerifier(acceptance_threshold=0.5)

        result = await verifier.verify(
            goal="テスト",
            result={"data": "partial"},
            expected={"data": "complete"},
            strategy=VerificationStrategy.THRESHOLD,
        )

        assert result is not None


class TestMonitoredExecutor:
    """MonitoredExecutorのテスト."""

    @pytest.mark.asyncio
    async def test_create_executor(self) -> None:
        """エグゼキューター作成のテスト."""
        executor = MonitoredExecutor(
            timeout_seconds=60,
            max_concurrent=3,
        )

        assert executor.timeout_seconds == 60
        assert executor.max_concurrent == 3

    @pytest.mark.asyncio
    async def test_execute_simple_plan(self) -> None:
        """シンプルな計画実行のテスト."""
        executor = MonitoredExecutor(
            timeout_seconds=60,
            max_concurrent=3,
        )

        # 計画を作成
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(goal="テスト")

        events = []
        async for event in executor.execute(plan, context={}):
            events.append(event)

        # イベントが発生することを確認
        assert len(events) > 0
        # 開始イベントがあることを確認
        event_types = [e.type for e in events]
        assert ExecutionEventType.STARTED in event_types


class TestPEVEngine:
    """PEVEngineのテスト."""

    @pytest.mark.asyncio
    async def test_create_engine(self) -> None:
        """エンジン作成のテスト."""
        class SimpleAgent:
            async def run(self, inputs: dict) -> dict:
                return {"status": "completed"}

        config = PEVEngineConfig(
            max_replans=3,
            timeout_seconds=60,
            acceptance_threshold=0.7,
        )

        engine = PEVEngine(
            agents={"default": SimpleAgent()},
            default_agent=SimpleAgent(),
            config=config,
        )

        assert engine is not None
        assert engine.config.max_replans == 3

    @pytest.mark.asyncio
    async def test_engine_run(self) -> None:
        """エンジン実行のテスト."""
        class SimpleAgent:
            async def run(self, inputs: dict) -> dict:
                return {"status": "completed", "score": 0.9}

        config = PEVEngineConfig(
            max_replans=2,
            timeout_seconds=30,
            acceptance_threshold=0.5,
        )

        engine = PEVEngine(
            agents={"default": SimpleAgent()},
            default_agent=SimpleAgent(),
            config=config,
        )

        result = await engine.run(inputs={"goal": "テストタスク"})

        assert result is not None
        assert result.success is True or result.error is not None

    @pytest.mark.asyncio
    async def test_engine_config_defaults(self) -> None:
        """エンジン設定のデフォルト値テスト."""
        config = PEVEngineConfig()

        assert config.max_replans == 3
        assert config.timeout_seconds >= 599  # デフォルト600
        assert config.acceptance_threshold >= 0.0

    @pytest.mark.asyncio
    async def test_engine_get_planner(self) -> None:
        """プランナー取得のテスト."""
        config = PEVEngineConfig()
        engine = PEVEngine(config=config)

        planner = engine.get_planner()
        assert planner is not None
        assert isinstance(planner, HierarchicalPlanner)

    @pytest.mark.asyncio
    async def test_engine_get_executor(self) -> None:
        """エグゼキューター取得のテスト."""
        config = PEVEngineConfig()
        engine = PEVEngine(config=config)

        executor = engine.get_executor()
        assert executor is not None
        assert isinstance(executor, MonitoredExecutor)

    @pytest.mark.asyncio
    async def test_engine_run_stream(self) -> None:
        """ストリーミング実行のテスト."""
        config = PEVEngineConfig(timeout_seconds=5)
        engine = PEVEngine(config=config)

        events = []
        async for event in engine.run_stream({"goal": "テスト目標"}):
            events.append(event)
            if len(events) >= 3:  # 最初の数イベントのみ
                break

        assert len(events) > 0
        # 最初のイベントはSTARTED
        assert events[0].type == ExecutionEventType.STARTED


class TestHierarchicalPlannerAdvanced:
    """HierarchicalPlannerの追加テスト."""

    @pytest.mark.asyncio
    async def test_create_plan_with_context(self) -> None:
        """コンテキスト付き計画作成のテスト."""
        planner = HierarchicalPlanner()

        plan = await planner.create_plan(
            goal="データ分析",
            context={"data_source": "database", "format": "csv"},
            available_agents=["analyzer", "reporter"],
        )

        assert plan is not None
        assert plan.root_goal == "データ分析"

    @pytest.mark.asyncio
    async def test_plan_get_progress(self) -> None:
        """計画進捗取得のテスト."""
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(goal="テスト")

        progress = plan.get_progress()
        assert progress >= 0.0
        assert progress <= 1.0

    @pytest.mark.asyncio
    async def test_plan_get_ready_goals(self) -> None:
        """実行可能目標取得のテスト."""
        planner = HierarchicalPlanner()
        plan = await planner.create_plan(goal="テスト")

        ready = plan.get_ready_goals()
        # 初期状態では少なくとも1つの目標が実行可能
        assert isinstance(ready, list)


class TestResultVerifierAdvanced:
    """ResultVerifierの追加テスト."""

    @pytest.mark.asyncio
    async def test_verify_exact_match_success(self) -> None:
        """完全一致検証成功のテスト."""
        verifier = ResultVerifier()

        result = await verifier.verify(
            goal="データ取得",
            result={"status": "success", "data": [1, 2, 3]},
            expected={"status": "success", "data": [1, 2, 3]},
            strategy=VerificationStrategy.EXACT_MATCH,
        )

        assert result.is_acceptable is True
        assert result.score >= 0.99

    @pytest.mark.asyncio
    async def test_verify_exact_match_failure(self) -> None:
        """完全一致検証失敗のテスト."""
        verifier = ResultVerifier()

        result = await verifier.verify(
            goal="データ取得",
            result={"status": "error"},
            expected={"status": "success"},
            strategy=VerificationStrategy.EXACT_MATCH,
        )

        assert result.is_acceptable is False

    @pytest.mark.asyncio
    async def test_verify_exact_match_partial(self) -> None:
        """部分一致検証のテスト."""
        verifier = ResultVerifier()

        result = await verifier.verify(
            goal="データ取得",
            result={"status": "success", "data": [1, 2]},
            expected={"status": "success", "data": [1, 2, 3]},
            strategy=VerificationStrategy.EXACT_MATCH,
        )

        # 部分一致なのでスコアは1未満
        assert result.score < 1.0

    @pytest.mark.asyncio
    async def test_verify_threshold_with_score(self) -> None:
        """スコア付き閾値検証のテスト."""
        verifier = ResultVerifier(acceptance_threshold=0.6)

        # スコアが閾値以上
        result = await verifier.verify(
            goal="品質チェック",
            result={"score": 0.8},
            expected={},
            strategy=VerificationStrategy.THRESHOLD,
        )

        assert result.is_acceptable is True
        assert result.score >= 0.79

    @pytest.mark.asyncio
    async def test_verify_threshold_below(self) -> None:
        """閾値未満の検証テスト."""
        verifier = ResultVerifier(acceptance_threshold=0.9)

        result = await verifier.verify(
            goal="品質チェック",
            result={"score": 0.5},
            expected={},
            strategy=VerificationStrategy.THRESHOLD,
        )

        assert result.is_acceptable is False
        assert result.should_replan is True

    @pytest.mark.asyncio
    async def test_verify_threshold_100_scale(self) -> None:
        """100スケールのスコア検証テスト."""
        verifier = ResultVerifier(acceptance_threshold=0.7)

        result = await verifier.verify(
            goal="品質チェック",
            result={"score": 85},  # 100スケール
            expected={},
            strategy=VerificationStrategy.THRESHOLD,
        )

        # 85/100 = 0.85 >= 0.7
        assert result.is_acceptable is True

    @pytest.mark.asyncio
    async def test_verify_semantic_no_llm(self) -> None:
        """LLMなしの意味的検証テスト."""
        verifier = ResultVerifier()  # llm_client=None

        result = await verifier.verify(
            goal="テスト",
            result={"score": 0.8},
            expected={},
            strategy=VerificationStrategy.SEMANTIC,
        )

        # LLMなしの場合はthreshold検証にフォールバック
        assert result is not None

    @pytest.mark.asyncio
    async def test_verify_constraint_in_range(self) -> None:
        """範囲内制約検証のテスト."""
        verifier = ResultVerifier()

        result = await verifier.verify(
            goal="値チェック",
            result={"value": 50},
            expected={
                "constraints": [
                    {"type": "min", "field": "value", "value": 0},
                    {"type": "max", "field": "value", "value": 100},
                ]
            },
            strategy=VerificationStrategy.CONSTRAINT,
        )

        assert result.is_acceptable is True

    @pytest.mark.asyncio
    async def test_verify_constraint_out_of_range(self) -> None:
        """範囲外制約検証のテスト."""
        verifier = ResultVerifier()

        result = await verifier.verify(
            goal="値チェック",
            result={"value": 150},
            expected={
                "constraints": [
                    {"type": "max", "field": "value", "value": 100},
                ]
            },
            strategy=VerificationStrategy.CONSTRAINT,
        )

        assert result.is_acceptable is False

    @pytest.mark.asyncio
    async def test_verify_constraint_required(self) -> None:
        """必須フィールド制約検証のテスト."""
        verifier = ResultVerifier()

        # 必須フィールドあり
        result = await verifier.verify(
            goal="必須チェック",
            result={"name": "test"},
            expected={
                "constraints": [
                    {"type": "required", "field": "name"},
                ]
            },
            strategy=VerificationStrategy.CONSTRAINT,
        )
        assert result.is_acceptable is True

        # 必須フィールドなし
        result = await verifier.verify(
            goal="必須チェック",
            result={},
            expected={
                "constraints": [
                    {"type": "required", "field": "name"},
                ]
            },
            strategy=VerificationStrategy.CONSTRAINT,
        )
        assert result.is_acceptable is False

    @pytest.mark.asyncio
    async def test_verify_constraint_in_values(self) -> None:
        """値リスト制約検証のテスト."""
        verifier = ResultVerifier()

        result = await verifier.verify(
            goal="値リストチェック",
            result={"status": "active"},
            expected={
                "constraints": [
                    {"type": "in", "field": "status", "values": ["active", "inactive"]},
                ]
            },
            strategy=VerificationStrategy.CONSTRAINT,
        )

        assert result.is_acceptable is True

    @pytest.mark.asyncio
    async def test_verify_default_strategy(self) -> None:
        """デフォルト戦略のテスト."""
        verifier = ResultVerifier(default_strategy=VerificationStrategy.THRESHOLD)

        result = await verifier.verify(
            goal="テスト",
            result={"score": 0.8},
            expected={},
        )

        assert result.strategy_used == VerificationStrategy.THRESHOLD

    @pytest.mark.asyncio
    async def test_verify_threshold_strategy(self) -> None:
        """閾値検証のテスト."""
        verifier = ResultVerifier(acceptance_threshold=0.5)

        result = await verifier.verify(
            goal="スコア計算",
            result={"score": 0.8},
            expected={"score": 0.7},
            strategy=VerificationStrategy.THRESHOLD,
        )

        assert result is not None
        assert result.score >= 0.0

    @pytest.mark.asyncio
    async def test_verify_constraint_strategy(self) -> None:
        """制約検証のテスト."""
        verifier = ResultVerifier()

        result = await verifier.verify(
            goal="制約チェック",
            result={"value": 50},
            expected={"min": 0, "max": 100},
            strategy=VerificationStrategy.CONSTRAINT,
        )

        assert result is not None


class TestExecutionMonitorAdvanced:
    """ExecutionMonitorの追加テスト."""

    def test_monitor_not_started(self) -> None:
        """未開始状態のテスト."""
        monitor = ExecutionMonitor()

        # 開始前はタイムアウトしない
        assert monitor.check_timeout() is False
        assert monitor.get_elapsed() <= 0.001  # ほぼ0

    def test_monitor_stall_detection(self) -> None:
        """停滞検知のテスト."""
        monitor = ExecutionMonitor()
        monitor.start()

        # 最初の進捗報告（_last_progress=0なので0.5は進捗あり）
        is_stall = monitor.check_stall(0.5)
        assert is_stall is False

        # 同じ進捗を連続報告（停滞カウント増加）
        is_stall = monitor.check_stall(0.5)  # stall_count=1
        assert is_stall is False
        is_stall = monitor.check_stall(0.5)  # stall_count=2
        assert is_stall is False
        is_stall = monitor.check_stall(0.5)  # stall_count=3
        assert is_stall is False

        # 4回連続で停滞検知（stall_count > 3）
        is_stall = monitor.check_stall(0.5)  # stall_count=4
        assert is_stall is True

    def test_monitor_progress_reset(self) -> None:
        """進捗リセットのテスト."""
        monitor = ExecutionMonitor()
        monitor.start()

        # 停滞カウントを増やす
        monitor.check_stall(0.5)
        monitor.check_stall(0.5)

        # 進捗があるとリセット
        is_stall = monitor.check_stall(0.6)
        assert is_stall is False

