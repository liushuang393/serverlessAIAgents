"""Coordinator パターンのテスト.

このテストは協調器の基類と Supervisor/Hierarchical パターンをテストします。
"""

from typing import Any
from unittest.mock import MagicMock

import pytest

from agentflow.core.agent_block import AgentBlock
from agentflow.patterns.coordinator import (
    CoordinationPattern,
    CoordinatorBase,
    CoordinatorRegistry,
)
from agentflow.patterns.hierarchical import HierarchicalCoordinator, SubTask
from agentflow.patterns.supervisor import SupervisorCoordinator, SupervisorDecision


class MockAgent(AgentBlock):
    """テスト用モック Agent.

    テスト用に run メソッドの戻り値を制御できる Agent。
    呼び出し時の入力データも記録します。
    """

    def __init__(
        self,
        return_value: dict[str, Any] | None = None,
        return_sequence: list[dict[str, Any]] | None = None,
    ) -> None:
        """初期化.

        Args:
            return_value: run() の固定戻り値
            return_sequence: run() の連続戻り値（呼び出しごとに順番に返す）
        """
        # AgentBlock は metadata_path を必要とするが、テストでは直接設定してスキップ
        self._metadata = MagicMock()
        self._metadata.name = "mock_agent"
        self._metadata.version = "1.0.0"
        self._engine = None
        self._return_value = return_value or {}
        self._return_sequence = return_sequence or []
        self._call_count = 0
        self._call_history: list[dict[str, Any]] = []

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """モック実行.

        Args:
            input_data: 入力データ

        Returns:
            設定された戻り値
        """
        self._call_history.append(input_data)
        if self._return_sequence:
            # シーケンスから順番に返す
            if self._call_count < len(self._return_sequence):
                result = self._return_sequence[self._call_count]
            else:
                result = self._return_sequence[-1]  # 最後の値を繰り返す
            self._call_count += 1
            return result
        return self._return_value


class MockCoordinator(CoordinatorBase):
    """テスト用モック協調器."""

    def __init__(
        self,
        agents: list[AgentBlock] | None = None,
        return_value: dict[str, Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            agents: Agent リスト
            return_value: execute() の戻り値
        """
        super().__init__(agents=agents)
        self._return_value = return_value or {}

    @property
    def pattern(self) -> CoordinationPattern:
        return CoordinationPattern.SEQUENTIAL

    async def execute(self, task: str, **kwargs: Any) -> dict[str, Any]:
        if self._return_value:
            return self._return_value
        return {"result": f"executed: {task}"}


class TestCoordinationPattern:
    """CoordinationPattern 列挙のテスト."""

    def test_pattern_values(self) -> None:
        """パターン値が正しいことをテスト."""
        assert CoordinationPattern.SEQUENTIAL.value == "sequential"
        assert CoordinationPattern.CONCURRENT.value == "concurrent"
        assert CoordinationPattern.SUPERVISOR.value == "supervisor"
        assert CoordinationPattern.HIERARCHICAL.value == "hierarchical"
        assert CoordinationPattern.HANDOFF.value == "handoff"


class TestCoordinatorBase:
    """CoordinatorBase のテスト."""

    def test_add_agent(self) -> None:
        """Agent を追加できることをテスト."""
        coordinator = MockCoordinator()
        agent = MockAgent()
        coordinator.add_agent(agent)
        assert coordinator.agent_count == 1

    def test_remove_agent(self) -> None:
        """Agent を削除できることをテスト."""
        coordinator = MockCoordinator()
        agent = MockAgent()
        coordinator.add_agent(agent)
        assert coordinator.remove_agent(agent) is True
        assert coordinator.agent_count == 0

    def test_remove_nonexistent_agent(self) -> None:
        """存在しない Agent の削除で False が返ることをテスト."""
        coordinator = MockCoordinator()
        agent = MockAgent()
        assert coordinator.remove_agent(agent) is False

    def test_agents_property(self) -> None:
        """agents プロパティで Agent リストを取得できることをテスト."""
        agent1 = MockAgent()
        agent2 = MockAgent()
        coordinator = MockCoordinator(agents=[agent1, agent2])
        agents = coordinator.agents
        assert len(agents) == 2
        assert agent1 in agents
        assert agent2 in agents
        # リストは複製されていることを確認
        agents.append(MockAgent())
        assert coordinator.agent_count == 2

    def test_init_with_none_agents(self) -> None:
        """agents が None の場合、空リストで初期化されることをテスト."""
        coordinator = MockCoordinator(agents=None)
        assert coordinator.agent_count == 0
        assert coordinator.agents == []


class TestCoordinatorRegistry:
    """CoordinatorRegistry のテスト."""

    @pytest.fixture(autouse=True)
    def reset_singleton(self) -> None:
        """各テスト前にシングルトンをリセット."""
        CoordinatorRegistry._instance = None

    def test_singleton(self) -> None:
        """シングルトンパターンのテスト."""
        registry1 = CoordinatorRegistry()
        registry2 = CoordinatorRegistry()
        assert registry1 is registry2

    def test_register_and_get(self) -> None:
        """協調器の登録と取得をテスト."""
        registry = CoordinatorRegistry()
        coordinator = MockCoordinator()
        registry.register("test_coordinator", coordinator)
        assert registry.get("test_coordinator") is coordinator

    def test_get_by_pattern(self) -> None:
        """パターン種別で協調器を取得できることをテスト."""
        registry = CoordinatorRegistry()
        coordinator = MockCoordinator()  # SEQUENTIAL パターン
        registry.register("seq", coordinator)

        result = registry.get_by_pattern(CoordinationPattern.SEQUENTIAL)
        assert result is coordinator

    def test_get_by_pattern_not_found(self) -> None:
        """存在しないパターンで None が返ることをテスト."""
        registry = CoordinatorRegistry()
        coordinator = MockCoordinator()  # SEQUENTIAL パターン
        registry.register("seq", coordinator)

        result = registry.get_by_pattern(CoordinationPattern.HIERARCHICAL)
        assert result is None


class TestSupervisorDecision:
    """SupervisorDecision のテスト."""

    def test_create_delegate_decision(self) -> None:
        """DELEGATE 決定を作成できることをテスト."""
        decision = SupervisorDecision(
            action="DELEGATE",
            worker_name="research",
            worker_input={"query": "test"},
            reason="Need research",
        )
        assert decision.action == "DELEGATE"
        assert decision.worker_name == "research"
        assert decision.worker_input == {"query": "test"}

    def test_create_finish_decision(self) -> None:
        """FINISH 決定を作成できることをテスト."""
        decision = SupervisorDecision(
            action="FINISH",
            result={"summary": "done"},
            reason="Task complete",
        )
        assert decision.action == "FINISH"
        assert decision.result == {"summary": "done"}


class TestSupervisorCoordinator:
    """SupervisorCoordinator のテスト."""

    def test_init_validation(self) -> None:
        """初期化時の検証をテスト."""
        supervisor = MockAgent()
        workers = {"w1": MockAgent()}

        # 正常な初期化
        coordinator = SupervisorCoordinator(supervisor, workers)
        assert coordinator.pattern == CoordinationPattern.SUPERVISOR

    def test_init_empty_workers_raises(self) -> None:
        """空の workers で ValueError が発生することをテスト."""
        supervisor = MockAgent()
        with pytest.raises(ValueError, match="cannot be empty"):
            SupervisorCoordinator(supervisor, {})

    def test_init_invalid_supervisor_raises(self) -> None:
        """無効な supervisor で TypeError が発生することをテスト."""
        with pytest.raises(TypeError, match="must be AgentBlock"):
            SupervisorCoordinator("not_an_agent", {"w1": MockAgent()})  # type: ignore[arg-type]

    def test_init_invalid_max_iterations(self) -> None:
        """無効な max_iterations で ValueError が発生することをテスト."""
        supervisor = MockAgent()
        workers = {"w1": MockAgent()}
        with pytest.raises(ValueError, match="must be positive"):
            SupervisorCoordinator(supervisor, workers, max_iterations=0)

    @pytest.mark.asyncio
    async def test_execute_finish_immediately(self) -> None:
        """監督者が即座に FINISH を返す場合のテスト."""
        # 監督者が FINISH アクションを返す
        supervisor = MockAgent(return_value={"action": "FINISH", "result": "done", "reason": "complete"})
        worker = MockAgent()
        workers = {"w1": worker}

        coordinator = SupervisorCoordinator(supervisor, workers, max_iterations=5)
        result = await coordinator.execute("test task")

        assert result["result"] == "done"
        assert result["iterations"] == 1
        assert "context" in result

    @pytest.mark.asyncio
    async def test_execute_delegate_then_finish(self) -> None:
        """監督者が DELEGATE してから FINISH する場合のテスト."""
        # 監督者：最初は DELEGATE、次に FINISH
        supervisor = MockAgent(
            return_sequence=[
                {"action": "DELEGATE", "worker_name": "w1", "worker_input": {"query": "test"}},
                {"action": "FINISH", "result": "complete", "reason": "done"},
            ]
        )
        worker = MockAgent(return_value={"worker_result": "data"})
        workers = {"w1": worker}

        coordinator = SupervisorCoordinator(supervisor, workers, max_iterations=5)
        result = await coordinator.execute("test task")

        assert result["result"] == "complete"
        assert result["iterations"] == 2
        # ワーカーが呼び出されたことを確認
        assert len(worker._call_history) == 1
        assert worker._call_history[0] == {"query": "test"}

    @pytest.mark.asyncio
    async def test_execute_unknown_worker(self) -> None:
        """存在しないワーカーを指定した場合のテスト."""
        supervisor = MockAgent(
            return_sequence=[
                {"action": "DELEGATE", "worker_name": "unknown", "worker_input": {}},
                {"action": "FINISH", "result": "done"},
            ]
        )
        worker = MockAgent()
        workers = {"w1": worker}

        coordinator = SupervisorCoordinator(supervisor, workers, max_iterations=5)
        await coordinator.execute("test task")

        # ワーカーが呼び出されないことを確認
        assert len(worker._call_history) == 0

    @pytest.mark.asyncio
    async def test_execute_max_iterations_reached(self) -> None:
        """最大イテレーション数に到達した場合のテスト."""
        # 常に DELEGATE を返す（終了しない）
        supervisor = MockAgent(return_value={"action": "DELEGATE", "worker_name": "w1"})
        worker = MockAgent()
        workers = {"w1": worker}

        coordinator = SupervisorCoordinator(supervisor, workers, max_iterations=3)
        result = await coordinator.execute("test task")

        assert result["iterations"] == 3
        assert result["status"] == "max_iterations_reached"

    @pytest.mark.asyncio
    async def test_execute_invalid_action(self) -> None:
        """無効なアクションが FINISH にフォールバックすることをテスト."""
        supervisor = MockAgent(return_value={"action": "INVALID_ACTION"})
        worker = MockAgent()
        workers = {"w1": worker}

        coordinator = SupervisorCoordinator(supervisor, workers, max_iterations=5)
        result = await coordinator.execute("test task")

        # 無効なアクションは FINISH にフォールバック
        assert result["iterations"] == 1


class TestSubTask:
    """SubTask のテスト."""

    def test_create_subtask(self) -> None:
        """SubTask を作成できることをテスト."""
        subtask = SubTask(
            id="task_1",
            task="Research market",
            coordinator_type="research",
            priority=1,
            dependencies=["task_0"],
        )
        assert subtask.id == "task_1"
        assert subtask.task == "Research market"
        assert subtask.coordinator_type == "research"
        assert subtask.priority == 1
        assert subtask.dependencies == ["task_0"]


class TestHierarchicalCoordinator:
    """HierarchicalCoordinator のテスト."""

    def test_init_validation(self) -> None:
        """初期化時の検証をテスト."""
        root = MockAgent()
        sub_coordinators = {"research": MockCoordinator()}
        coordinator = HierarchicalCoordinator(root, sub_coordinators)
        assert coordinator.pattern == CoordinationPattern.HIERARCHICAL

    def test_init_empty_sub_coordinators_raises(self) -> None:
        """空の sub_coordinators で ValueError が発生することをテスト."""
        root = MockAgent()
        with pytest.raises(ValueError, match="cannot be empty"):
            HierarchicalCoordinator(root, {})

    def test_init_invalid_root_raises(self) -> None:
        """無効な root で TypeError が発生することをテスト."""
        with pytest.raises(TypeError, match="must be AgentBlock"):
            HierarchicalCoordinator("not_an_agent", {"r": MockCoordinator()})  # type: ignore[arg-type]

    @pytest.mark.asyncio
    async def test_execute_parallel(self) -> None:
        """並行実行モードでのテスト."""
        # root Agent: 分解 → 集約
        root = MockAgent(
            return_sequence=[
                # decompose: サブタスク2つを返す
                {
                    "subtasks": [
                        {"id": "t1", "task": "research", "coordinator_type": "research"},
                        {"id": "t2", "task": "analysis", "coordinator_type": "analysis"},
                    ]
                },
                # aggregate: 結果を集約
                {"final_result": {"summary": "all done"}},
            ]
        )
        research_coord = MockCoordinator(return_value={"research": "data"})
        analysis_coord = MockCoordinator(return_value={"analysis": "result"})
        sub_coordinators = {"research": research_coord, "analysis": analysis_coord}

        coordinator = HierarchicalCoordinator(root, sub_coordinators, parallel_execution=True)
        result = await coordinator.execute("complex task")

        assert result["subtask_count"] == 2
        assert result["result"] == {"summary": "all done"}
        assert "t1" in result["subtask_results"]
        assert "t2" in result["subtask_results"]

    @pytest.mark.asyncio
    async def test_execute_sequential(self) -> None:
        """順次実行モードでのテスト."""
        root = MockAgent(
            return_sequence=[
                {"subtasks": [{"id": "t1", "task": "research", "coordinator_type": "research"}]},
                {"final_result": "done"},
            ]
        )
        research_coord = MockCoordinator(return_value={"data": "result"})
        sub_coordinators = {"research": research_coord}

        coordinator = HierarchicalCoordinator(root, sub_coordinators, parallel_execution=False)
        result = await coordinator.execute("simple task")

        assert result["subtask_count"] == 1
        assert "t1" in result["subtask_results"]

    @pytest.mark.asyncio
    async def test_execute_unknown_coordinator(self) -> None:
        """存在しない coordinator_type の場合のテスト."""
        root = MockAgent(
            return_sequence=[
                {"subtasks": [{"id": "t1", "task": "unknown", "coordinator_type": "unknown"}]},
                {"final_result": "done"},
            ]
        )
        research_coord = MockCoordinator()
        sub_coordinators = {"research": research_coord}

        coordinator = HierarchicalCoordinator(root, sub_coordinators, parallel_execution=True)
        result = await coordinator.execute("task with unknown coordinator")

        # unknown coordinator はエラーを返す
        assert "t1" in result["subtask_results"]
        assert "error" in result["subtask_results"]["t1"]

    @pytest.mark.asyncio
    async def test_execute_empty_subtasks(self) -> None:
        """サブタスクが空の場合のテスト."""
        root = MockAgent(
            return_sequence=[
                {"subtasks": []},  # 空のサブタスク
                {"final_result": "nothing to do"},
            ]
        )
        research_coord = MockCoordinator()
        sub_coordinators = {"research": research_coord}

        coordinator = HierarchicalCoordinator(root, sub_coordinators)
        result = await coordinator.execute("empty task")

        assert result["subtask_count"] == 0
        assert result["subtask_results"] == {}

    @pytest.mark.asyncio
    async def test_execute_sequential_skips_unknown(self) -> None:
        """順次実行で存在しない coordinator_type をスキップすることをテスト."""
        root = MockAgent(
            return_sequence=[
                {
                    "subtasks": [
                        {"id": "t1", "task": "known", "coordinator_type": "research"},
                        {"id": "t2", "task": "unknown", "coordinator_type": "unknown"},
                    ]
                },
                {"final_result": "partial"},
            ]
        )
        research_coord = MockCoordinator(return_value={"data": "ok"})
        sub_coordinators = {"research": research_coord}

        coordinator = HierarchicalCoordinator(root, sub_coordinators, parallel_execution=False)
        result = await coordinator.execute("mixed task")

        # 既知の coordinator のみ結果がある
        assert "t1" in result["subtask_results"]
        assert "t2" not in result["subtask_results"]

