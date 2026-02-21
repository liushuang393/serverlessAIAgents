"""AgentComposer テスト."""

from __future__ import annotations

import pytest

from agentflow.core.agent_block import AgentBlock
from agentflow.patterns.composer import (
    AgentComposer,
    AgentNode,
    AgentRole,
    CapabilityBasedRouter,
    CompositionConfig,
    CompositionPattern,
    CompositionResult,
    RoundRobinRouter,
    TaskAssignment,
)


class MockAgent(AgentBlock):
    """テスト用モックAgent."""

    def __init__(self, name: str = "mock", return_value: dict | None = None) -> None:
        """初期化."""
        super().__init__()
        self.name = name
        self.return_value = return_value or {"result": f"{name}_result"}
        self.call_count = 0
        self.last_input: dict | None = None

    async def run(self, input_data: dict) -> dict:
        """実行."""
        self.call_count += 1
        self.last_input = input_data
        return self.return_value


class TestAgentNode:
    """AgentNode テストクラス."""

    def test_create_node(self) -> None:
        """ノードを作成できること."""
        agent = MockAgent("test")
        node = AgentNode(
            agent_id="node-001",
            agent=agent,
            role=AgentRole.WORKER,
            capabilities=["analysis", "report"],
        )

        assert node.agent_id == "node-001"
        assert node.role == AgentRole.WORKER
        assert "analysis" in node.capabilities

    def test_node_with_hierarchy(self) -> None:
        """階層構造を持つノードを作成できること."""
        agent = MockAgent("child")
        node = AgentNode(
            agent_id="child-001",
            agent=agent,
            role=AgentRole.WORKER,
            parent_id="parent-001",
        )

        assert node.parent_id == "parent-001"
        assert node.children_ids == []


class TestTaskAssignment:
    """TaskAssignment テストクラス."""

    def test_create_assignment(self) -> None:
        """タスク割り当てを作成できること."""
        assignment = TaskAssignment(
            task_id="task-001",
            task="データ分析",
            assigned_agent_id="worker-001",
        )

        assert assignment.task_id == "task-001"
        assert assignment.status == "pending"
        assert assignment.result is None


class TestCapabilityBasedRouter:
    """CapabilityBasedRouter テストクラス."""

    def test_route_by_capability(self) -> None:
        """能力に基づいてルーティングできること."""
        router = CapabilityBasedRouter()

        node1 = AgentNode(
            agent_id="analyzer",
            agent=MockAgent("analyzer"),
            role=AgentRole.WORKER,
            capabilities=["analysis", "data"],
        )
        node2 = AgentNode(
            agent_id="reporter",
            agent=MockAgent("reporter"),
            role=AgentRole.WORKER,
            capabilities=["report", "document"],
        )

        # "analysis"タスクは analyzer にルーティング
        result = router.route("データ analysis を実行", [node1, node2])
        assert result is not None
        assert result.agent_id == "analyzer"

        # "report"タスクは reporter にルーティング
        result = router.route("レポート report を作成", [node1, node2])
        assert result is not None
        assert result.agent_id == "reporter"

    def test_route_fallback(self) -> None:
        """マッチしない場合は最初のAgentを返すこと."""
        router = CapabilityBasedRouter()

        node = AgentNode(
            agent_id="generic",
            agent=MockAgent("generic"),
            role=AgentRole.WORKER,
            capabilities=["xyz"],
        )

        result = router.route("unknown task", [node])
        assert result is not None
        assert result.agent_id == "generic"


class TestRoundRobinRouter:
    """RoundRobinRouter テストクラス."""

    def test_round_robin(self) -> None:
        """ラウンドロビンでルーティングできること."""
        router = RoundRobinRouter()

        node1 = AgentNode(
            agent_id="worker-1",
            agent=MockAgent("w1"),
            role=AgentRole.WORKER,
        )
        node2 = AgentNode(
            agent_id="worker-2",
            agent=MockAgent("w2"),
            role=AgentRole.WORKER,
        )
        nodes = [node1, node2]

        # 順番に選択される
        assert router.route("task1", nodes).agent_id == "worker-1"
        assert router.route("task2", nodes).agent_id == "worker-2"
        assert router.route("task3", nodes).agent_id == "worker-1"


class TestAgentComposer:
    """AgentComposer テストクラス."""

    @pytest.fixture
    def composer(self) -> AgentComposer:
        """Composerを作成."""
        config = CompositionConfig(
            pattern=CompositionPattern.SUPERVISOR_WORKER,
            max_concurrent_workers=3,
        )
        return AgentComposer(config=config)

    def test_create_composer(self) -> None:
        """Composerを作成できること."""
        composer = AgentComposer()

        assert composer.pattern == CompositionPattern.SUPERVISOR_WORKER
        assert len(composer.agents) == 0

    def test_set_supervisor(self, composer: AgentComposer) -> None:
        """Supervisorを設定できること."""
        agent = MockAgent("supervisor")
        agent_id = composer.set_supervisor(agent, "sup-001", ["coordination"])

        assert agent_id == "sup-001"
        assert composer.supervisor is not None
        assert composer.supervisor.role == AgentRole.SUPERVISOR

    def test_add_worker(self, composer: AgentComposer) -> None:
        """Workerを追加できること."""
        supervisor = MockAgent("supervisor")
        composer.set_supervisor(supervisor)

        worker = MockAgent("worker")
        agent_id = composer.add_worker(worker, "worker-001", ["analysis"])

        assert agent_id == "worker-001"
        assert len(composer.workers) == 1
        assert composer.workers[0].agent_id == "worker-001"

    def test_add_agent_generic(self) -> None:
        """汎用Agentを追加できること."""
        config = CompositionConfig(pattern=CompositionPattern.MESH)
        composer = AgentComposer(config=config)

        agent1 = MockAgent("agent1")
        agent2 = MockAgent("agent2")

        composer.add_agent(agent1, "agent-1", capabilities=["task1"])
        composer.add_agent(agent2, "agent-2", capabilities=["task2"])

        assert len(composer.agents) == 2

    def test_remove_agent(self, composer: AgentComposer) -> None:
        """Agentを削除できること."""
        agent = MockAgent("worker")
        agent_id = composer.add_worker(agent, "worker-to-remove")

        assert agent_id in composer.agents
        result = composer.remove_agent(agent_id)
        assert result is True
        assert agent_id not in composer.agents

    def test_remove_nonexistent_agent(self, composer: AgentComposer) -> None:
        """存在しないAgentの削除はFalseを返すこと."""
        result = composer.remove_agent("nonexistent")
        assert result is False

    async def test_execute_supervisor_worker(self, composer: AgentComposer) -> None:
        """Supervisor/Workerパターンで実行できること."""
        supervisor = MockAgent("supervisor", {"aggregated": "done"})
        composer.set_supervisor(supervisor, capabilities=["coordination"])

        worker1 = MockAgent("worker1", {"data": "result1"})
        worker2 = MockAgent("worker2", {"data": "result2"})
        composer.add_worker(worker1, "w1", ["analysis"])
        composer.add_worker(worker2, "w2", ["report"])

        result = await composer.execute(
            task="タスク実行",
            subtasks=["analysis タスク", "report タスク"],
        )

        assert isinstance(result, CompositionResult)
        assert result.pattern == CompositionPattern.SUPERVISOR_WORKER
        assert result.status in ["success", "partial_success"]

    async def test_execute_mesh(self) -> None:
        """Meshパターンで実行できること."""
        config = CompositionConfig(pattern=CompositionPattern.MESH)
        composer = AgentComposer(config=config)

        agent1 = MockAgent("agent1", {"result": "a1"})
        agent2 = MockAgent("agent2", {"result": "a2"})

        composer.add_agent(agent1, "agent-1")
        composer.add_agent(agent2, "agent-2")

        result = await composer.execute(task="並行タスク")

        assert result.pattern == CompositionPattern.MESH
        assert "agent_results" in result.results
        assert len(result.results["agent_results"]) == 2

    async def test_execute_pipeline(self) -> None:
        """Pipelineパターンで実行できること."""
        config = CompositionConfig(pattern=CompositionPattern.PIPELINE)
        composer = AgentComposer(config=config)

        agent1 = MockAgent("step1", {"step": 1})
        agent2 = MockAgent("step2", {"step": 2})

        composer.add_agent(agent1, "a-step1")
        composer.add_agent(agent2, "b-step2")

        result = await composer.execute(task="パイプライン実行")

        assert result.pattern == CompositionPattern.PIPELINE
        assert "pipeline_results" in result.results
        assert len(result.results["pipeline_results"]) == 2

    async def test_execute_broadcast(self) -> None:
        """Broadcastパターンで実行できること."""
        config = CompositionConfig(pattern=CompositionPattern.BROADCAST)
        composer = AgentComposer(config=config)

        agent1 = MockAgent("receiver1")
        agent2 = MockAgent("receiver2")

        composer.add_agent(agent1, "r1")
        composer.add_agent(agent2, "r2")

        result = await composer.execute(task="ブロードキャスト")

        assert result.pattern == CompositionPattern.BROADCAST

    def test_get_stats(self, composer: AgentComposer) -> None:
        """統計情報を取得できること."""
        supervisor = MockAgent("sup")
        composer.set_supervisor(supervisor)

        worker = MockAgent("worker")
        composer.add_worker(worker)

        stats = composer.get_stats()

        assert stats["pattern"] == "supervisor_worker"
        assert stats["total_agents"] == 2
        assert stats["has_supervisor"] is True
        assert "role_distribution" in stats

    async def test_progress_callback(self) -> None:
        """進捗コールバックが呼ばれること."""
        config = CompositionConfig(pattern=CompositionPattern.PIPELINE)
        composer = AgentComposer(config=config)

        progress_values: list[float] = []

        def on_progress(progress: float, message: str) -> None:
            progress_values.append(progress)

        composer.on_progress(on_progress)

        agent1 = MockAgent("s1")
        agent2 = MockAgent("s2")
        composer.add_agent(agent1, "step1")
        composer.add_agent(agent2, "step2")

        await composer.execute(task="進捗テスト")

        # 2ステップなので0.5と1.0が記録される
        assert len(progress_values) == 2
        assert progress_values[-1] == 1.0
