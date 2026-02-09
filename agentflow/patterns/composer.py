"""AgentComposer - 標準Agent組合パターン.

Supervisor/Worker、Mesh、Hierarchy などの組合パターンを提供。

使用例:
    >>> from agentflow.patterns import AgentComposer, CompositionPattern
    >>>
    >>> # Supervisor/Worker パターン
    >>> composer = AgentComposer(pattern=CompositionPattern.SUPERVISOR_WORKER)
    >>> composer.set_supervisor(coordinator_agent)
    >>> composer.add_worker("analyzer", analysis_agent)
    >>> composer.add_worker("generator", report_agent)
    >>> result = await composer.execute(task="市場分析レポート作成")
"""

from __future__ import annotations

import asyncio
import logging
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import Callable

    from agentflow.core.agent_block import AgentBlock


logger = logging.getLogger(__name__)


class CompositionPattern(str, Enum):
    """Agent組合パターン."""

    SUPERVISOR_WORKER = "supervisor_worker"  # 主管-作業者パターン
    MESH = "mesh"  # メッシュ（対等通信）パターン
    HIERARCHY = "hierarchy"  # 階層パターン
    PIPELINE = "pipeline"  # パイプライン（順次実行）パターン
    BROADCAST = "broadcast"  # ブロードキャスト（並行配信）パターン

    # エイリアス（使いやすさ向上）
    SEQUENTIAL = "pipeline"  # 順次実行（PIPELINE のエイリアス）
    PARALLEL = "broadcast"  # 並行実行（BROADCAST のエイリアス）


class AgentRole(str, Enum):
    """Agent役割."""

    # 基本役割
    SUPERVISOR = "supervisor"  # 主管
    WORKER = "worker"  # 作業者
    COORDINATOR = "coordinator"  # 調整者
    VALIDATOR = "validator"  # 検証者

    # 拡張役割（使いやすさ向上）
    ANALYZER = "analyzer"  # 分析者
    EXECUTOR = "executor"  # 実行者
    REPORTER = "reporter"  # レポーター
    RESEARCHER = "researcher"  # 調査者
    REVIEWER = "reviewer"  # レビュアー
    PLANNER = "planner"  # 計画者


@dataclass
class AgentNode:
    """組合内のAgentノード."""

    agent_id: str
    agent: AgentBlock
    role: AgentRole
    capabilities: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)
    parent_id: str | None = None
    children_ids: list[str] = field(default_factory=list)


@dataclass
class CompositionConfig:
    """組合設定."""

    pattern: CompositionPattern = CompositionPattern.SUPERVISOR_WORKER
    max_concurrent_workers: int = 5
    enable_load_balancing: bool = True
    timeout_seconds: float = 300.0
    retry_failed_tasks: bool = True
    max_retries: int = 3


@dataclass
class TaskAssignment:
    """タスク割り当て."""

    task_id: str
    task: str
    assigned_agent_id: str
    status: str = "pending"  # pending, running, completed, failed
    result: dict[str, Any] | None = None
    error: str | None = None
    created_at: datetime = field(default_factory=datetime.now)
    completed_at: datetime | None = None


@dataclass
class CompositionResult:
    """組合実行結果."""

    composition_id: str
    pattern: CompositionPattern
    status: str  # success, partial_success, failed
    results: dict[str, Any]
    task_assignments: list[TaskAssignment]
    total_duration_ms: float
    metadata: dict[str, Any] = field(default_factory=dict)


class AgentRouter(ABC):
    """Agentルーター（タスク割り当て戦略）の基底クラス."""

    @abstractmethod
    def route(
        self,
        task: str,
        available_agents: list[AgentNode],
        context: dict[str, Any] | None = None,
    ) -> AgentNode | None:
        """タスクを適切なAgentにルーティング."""


class CapabilityBasedRouter(AgentRouter):
    """能力ベースのルーター."""

    def route(
        self,
        task: str,
        available_agents: list[AgentNode],
        context: dict[str, Any] | None = None,
    ) -> AgentNode | None:
        """タスクキーワードに基づいてAgentを選択."""
        task_lower = task.lower()

        # 能力マッチングスコアを計算
        best_match: AgentNode | None = None
        best_score = 0

        for agent_node in available_agents:
            score = 0
            for capability in agent_node.capabilities:
                if capability.lower() in task_lower:
                    score += 1
            if score > best_score:
                best_score = score
                best_match = agent_node

        # マッチがなければ最初のAgentを返す
        if best_match is None and available_agents:
            return available_agents[0]

        return best_match


class RoundRobinRouter(AgentRouter):
    """ラウンドロビンルーター."""

    def __init__(self) -> None:
        """初期化."""
        self._current_index = 0

    def route(
        self,
        task: str,
        available_agents: list[AgentNode],
        context: dict[str, Any] | None = None,
    ) -> AgentNode | None:
        """ラウンドロビンでAgentを選択."""
        if not available_agents:
            return None

        agent = available_agents[self._current_index % len(available_agents)]
        self._current_index += 1
        return agent


class AgentComposer:
    """Agent組合器 - 複数Agentの組合と協調実行を管理."""

    def __init__(
        self,
        config: CompositionConfig | None = None,
        router: AgentRouter | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 組合設定
            router: タスクルーター
        """
        self._config = config or CompositionConfig()
        self._router = router or CapabilityBasedRouter()
        self._nodes: dict[str, AgentNode] = {}
        self._supervisor_id: str | None = None
        self._task_queue: list[TaskAssignment] = []
        self._completed_tasks: list[TaskAssignment] = []
        self._on_task_complete: Callable[[TaskAssignment], Any] | None = None
        self._on_progress: Callable[[float, str], Any] | None = None

    @property
    def pattern(self) -> CompositionPattern:
        """組合パターンを取得."""
        return self._config.pattern

    @property
    def agents(self) -> dict[str, AgentNode]:
        """全Agentノードを取得."""
        return self._nodes.copy()

    @property
    def supervisor(self) -> AgentNode | None:
        """Supervisorノードを取得."""
        if self._supervisor_id:
            return self._nodes.get(self._supervisor_id)
        return None

    @property
    def workers(self) -> list[AgentNode]:
        """Workerノードリストを取得."""
        return [
            node for node in self._nodes.values() if node.role == AgentRole.WORKER
        ]

    def set_supervisor(
        self,
        agent: AgentBlock,
        agent_id: str | None = None,
        capabilities: list[str] | None = None,
    ) -> str:
        """Supervisorを設定.

        Args:
            agent: SupervisorとなるAgent
            agent_id: Agent ID（省略時は自動生成）
            capabilities: Agent能力リスト

        Returns:
            Agent ID
        """
        agent_id = agent_id or f"supervisor-{uuid.uuid4().hex[:8]}"
        node = AgentNode(
            agent_id=agent_id,
            agent=agent,
            role=AgentRole.SUPERVISOR,
            capabilities=capabilities or [],
        )
        self._nodes[agent_id] = node
        self._supervisor_id = agent_id
        logger.info("Supervisor設定: %s", agent_id)
        return agent_id

    def add_worker(
        self,
        agent: AgentBlock,
        agent_id: str | None = None,
        capabilities: list[str] | None = None,
        parent_id: str | None = None,
    ) -> str:
        """Workerを追加.

        Args:
            agent: WorkerとなるAgent
            agent_id: Agent ID（省略時は自動生成）
            capabilities: Agent能力リスト
            parent_id: 親Agent ID（Hierarchyパターン用）

        Returns:
            Agent ID
        """
        agent_id = agent_id or f"worker-{uuid.uuid4().hex[:8]}"
        node = AgentNode(
            agent_id=agent_id,
            agent=agent,
            role=AgentRole.WORKER,
            capabilities=capabilities or [],
            parent_id=parent_id or self._supervisor_id,
        )
        self._nodes[agent_id] = node

        # 親ノードに子として登録
        if node.parent_id and node.parent_id in self._nodes:
            self._nodes[node.parent_id].children_ids.append(agent_id)

        logger.info("Worker追加: %s (capabilities: %s)", agent_id, capabilities)
        return agent_id

    def add_agent(
        self,
        agent: AgentBlock,
        agent_id: str | None = None,
        role: AgentRole = AgentRole.WORKER,
        capabilities: list[str] | None = None,
    ) -> str:
        """汎用Agent追加（Mesh/Hierarchyパターン用）.

        Args:
            agent: Agent
            agent_id: Agent ID
            role: 役割
            capabilities: 能力リスト

        Returns:
            Agent ID
        """
        agent_id = agent_id or f"{role.value}-{uuid.uuid4().hex[:8]}"
        node = AgentNode(
            agent_id=agent_id,
            agent=agent,
            role=role,
            capabilities=capabilities or [],
        )
        self._nodes[agent_id] = node
        logger.info("Agent追加: %s (role: %s)", agent_id, role.value)
        return agent_id

    def remove_agent(self, agent_id: str) -> bool:
        """Agentを削除.

        Args:
            agent_id: 削除するAgent ID

        Returns:
            削除成功したかどうか
        """
        if agent_id not in self._nodes:
            return False

        node = self._nodes[agent_id]

        # 親から削除
        if node.parent_id and node.parent_id in self._nodes:
            parent = self._nodes[node.parent_id]
            if agent_id in parent.children_ids:
                parent.children_ids.remove(agent_id)

        # 子の親参照をクリア
        for child_id in node.children_ids:
            if child_id in self._nodes:
                self._nodes[child_id].parent_id = None

        del self._nodes[agent_id]

        if self._supervisor_id == agent_id:
            self._supervisor_id = None

        logger.info("Agent削除: %s", agent_id)
        return True

    def on_task_complete(
        self, callback: Callable[[TaskAssignment], Any]
    ) -> None:
        """タスク完了コールバックを設定."""
        self._on_task_complete = callback

    def on_progress(self, callback: Callable[[float, str], Any]) -> None:
        """進捗コールバックを設定."""
        self._on_progress = callback

    async def execute(
        self,
        task: str,
        context: dict[str, Any] | None = None,
        subtasks: list[str] | None = None,
    ) -> CompositionResult:
        """組合を実行.

        Args:
            task: メインタスク
            context: 実行コンテキスト
            subtasks: サブタスクリスト（指定時は自動分解しない）

        Returns:
            実行結果
        """
        composition_id = f"comp-{uuid.uuid4().hex[:8]}"
        start_time = datetime.now()

        logger.info(
            "組合実行開始: %s (pattern: %s)",
            composition_id,
            self._config.pattern.value,
        )

        try:
            if self._config.pattern == CompositionPattern.SUPERVISOR_WORKER:
                results = await self._execute_supervisor_worker(
                    task, context, subtasks
                )
            elif self._config.pattern == CompositionPattern.MESH:
                results = await self._execute_mesh(task, context)
            elif self._config.pattern == CompositionPattern.HIERARCHY:
                results = await self._execute_hierarchy(task, context, subtasks)
            elif self._config.pattern == CompositionPattern.PIPELINE:
                results = await self._execute_pipeline(task, context)
            elif self._config.pattern == CompositionPattern.BROADCAST:
                results = await self._execute_broadcast(task, context)
            else:
                msg = f"未対応のパターン: {self._config.pattern}"
                raise ValueError(msg)

            # 結果判定
            failed_count = sum(
                1 for t in self._completed_tasks if t.status == "failed"
            )
            if failed_count == 0:
                status = "success"
            elif failed_count < len(self._completed_tasks):
                status = "partial_success"
            else:
                status = "failed"

            duration_ms = (datetime.now() - start_time).total_seconds() * 1000

            return CompositionResult(
                composition_id=composition_id,
                pattern=self._config.pattern,
                status=status,
                results=results,
                task_assignments=self._completed_tasks.copy(),
                total_duration_ms=duration_ms,
            )

        except Exception as e:
            logger.exception("組合実行エラー: %s", e)
            duration_ms = (datetime.now() - start_time).total_seconds() * 1000
            return CompositionResult(
                composition_id=composition_id,
                pattern=self._config.pattern,
                status="failed",
                results={"error": str(e)},
                task_assignments=self._completed_tasks.copy(),
                total_duration_ms=duration_ms,
            )
        finally:
            # クリーンアップ
            self._task_queue.clear()
            self._completed_tasks.clear()

    async def _execute_supervisor_worker(
        self,
        task: str,
        context: dict[str, Any] | None,
        subtasks: list[str] | None,
    ) -> dict[str, Any]:
        """Supervisor/Workerパターンで実行."""
        if not self._supervisor_id:
            # Supervisorなしの場合、最初のAgentをSupervisorとして扱う
            if self._nodes:
                first_id = next(iter(self._nodes))
                self._supervisor_id = first_id
                self._nodes[first_id].role = AgentRole.SUPERVISOR
            else:
                msg = "Agentが登録されていません"
                raise ValueError(msg)

        supervisor_node = self._nodes[self._supervisor_id]
        workers = self.workers

        # サブタスク準備
        if subtasks:
            task_list = subtasks
        else:
            # Supervisorにタスク分解を依頼（簡易実装）
            task_list = [task]

        # タスクキューに追加
        for i, subtask in enumerate(task_list):
            assignment = TaskAssignment(
                task_id=f"task-{i:03d}",
                task=subtask,
                assigned_agent_id="",  # 後で割り当て
            )
            self._task_queue.append(assignment)

        # Workerにタスクを配布して実行
        results: dict[str, Any] = {"subtask_results": []}
        semaphore = asyncio.Semaphore(self._config.max_concurrent_workers)

        async def process_task(assignment: TaskAssignment) -> None:
            async with semaphore:
                # ルーティング
                target = self._router.route(assignment.task, workers, context)
                if not target:
                    assignment.status = "failed"
                    assignment.error = "適切なWorkerが見つかりません"
                    self._completed_tasks.append(assignment)
                    return

                assignment.assigned_agent_id = target.agent_id
                assignment.status = "running"

                try:
                    result = await target.agent.run({
                        "task": assignment.task,
                        "context": context or {},
                    })
                    assignment.status = "completed"
                    assignment.result = result
                    assignment.completed_at = datetime.now()
                    results["subtask_results"].append({
                        "task_id": assignment.task_id,
                        "agent_id": target.agent_id,
                        "result": result,
                    })
                except Exception as e:
                    assignment.status = "failed"
                    assignment.error = str(e)
                    logger.warning("タスク失敗: %s - %s", assignment.task_id, e)
                finally:
                    self._completed_tasks.append(assignment)
                    if self._on_task_complete:
                        self._on_task_complete(assignment)

                    # 進捗更新
                    if self._on_progress:
                        progress = len(self._completed_tasks) / len(task_list)
                        self._on_progress(
                            progress,
                            f"完了: {len(self._completed_tasks)}/{len(task_list)}",
                        )

        # 並行実行
        await asyncio.gather(
            *[process_task(a) for a in self._task_queue],
            return_exceptions=True,
        )

        # Supervisorによる集約（オプション）
        if supervisor_node and results["subtask_results"]:
            try:
                aggregated = await supervisor_node.agent.run({
                    "task": "結果の集約",
                    "subtask_results": results["subtask_results"],
                    "context": context or {},
                })
                results["aggregated"] = aggregated
            except Exception as e:
                logger.warning("結果集約エラー: %s", e)

        return results

    async def _execute_mesh(
        self,
        task: str,
        context: dict[str, Any] | None,
    ) -> dict[str, Any]:
        """Meshパターンで実行（全Agent並行、結果マージ）."""
        results: dict[str, Any] = {"agent_results": {}}

        async def run_agent(node: AgentNode) -> tuple[str, Any]:
            try:
                result = await node.agent.run({
                    "task": task,
                    "context": context or {},
                })
                return node.agent_id, {"status": "success", "result": result}
            except Exception as e:
                return node.agent_id, {"status": "failed", "error": str(e)}

        # 全Agent並行実行
        tasks = [run_agent(node) for node in self._nodes.values()]
        completed = await asyncio.gather(*tasks, return_exceptions=True)

        for item in completed:
            if isinstance(item, tuple):
                agent_id, result = item
                results["agent_results"][agent_id] = result

        return results

    async def _execute_hierarchy(
        self,
        task: str,
        context: dict[str, Any] | None,
        subtasks: list[str] | None,
    ) -> dict[str, Any]:
        """Hierarchyパターンで実行（階層的タスク委譲）."""
        results: dict[str, Any] = {"hierarchy_results": []}

        # ルートノードを特定
        root_nodes = [
            node for node in self._nodes.values() if node.parent_id is None
        ]

        async def process_node(
            node: AgentNode, node_task: str, depth: int = 0
        ) -> dict[str, Any]:
            """ノードとその子を再帰的に処理."""
            node_result: dict[str, Any] = {
                "agent_id": node.agent_id,
                "depth": depth,
                "children_results": [],
            }

            # 自身のタスク実行
            try:
                result = await node.agent.run({
                    "task": node_task,
                    "context": context or {},
                })
                node_result["result"] = result
                node_result["status"] = "success"
            except Exception as e:
                node_result["error"] = str(e)
                node_result["status"] = "failed"

            # 子ノードの処理
            for child_id in node.children_ids:
                if child_id in self._nodes:
                    child_node = self._nodes[child_id]
                    child_result = await process_node(
                        child_node, node_task, depth + 1
                    )
                    node_result["children_results"].append(child_result)

            return node_result

        # 各ルートノードを処理
        for root in root_nodes:
            root_result = await process_node(root, task)
            results["hierarchy_results"].append(root_result)

        return results

    async def _execute_pipeline(
        self,
        task: str,
        context: dict[str, Any] | None,
    ) -> dict[str, Any]:
        """Pipelineパターンで実行（順次実行、出力を次に渡す）."""
        results: dict[str, Any] = {"pipeline_results": []}
        current_input = {"task": task, "context": context or {}}

        # Agent ID順でソート
        sorted_nodes = sorted(self._nodes.values(), key=lambda n: n.agent_id)

        for i, node in enumerate(sorted_nodes):
            step_result: dict[str, Any] = {
                "step": i,
                "agent_id": node.agent_id,
            }

            try:
                result = await node.agent.run(current_input)
                step_result["result"] = result
                step_result["status"] = "success"

                # 次のステップへの入力を更新
                current_input = {
                    "previous_result": result,
                    "original_task": task,
                    "context": context or {},
                }
            except Exception as e:
                step_result["error"] = str(e)
                step_result["status"] = "failed"
                # パイプライン中断
                break
            finally:
                results["pipeline_results"].append(step_result)

                if self._on_progress:
                    progress = (i + 1) / len(sorted_nodes)
                    self._on_progress(
                        progress, f"ステップ {i + 1}/{len(sorted_nodes)} 完了"
                    )

        return results

    async def _execute_broadcast(
        self,
        task: str,
        context: dict[str, Any] | None,
    ) -> dict[str, Any]:
        """Broadcastパターンで実行（全Agentに同じタスクを配信）."""
        return await self._execute_mesh(task, context)

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        role_counts: dict[str, int] = {}
        for node in self._nodes.values():
            role = node.role.value
            role_counts[role] = role_counts.get(role, 0) + 1

        return {
            "pattern": self._config.pattern.value,
            "total_agents": len(self._nodes),
            "role_distribution": role_counts,
            "has_supervisor": self._supervisor_id is not None,
            "config": {
                "max_concurrent_workers": self._config.max_concurrent_workers,
                "timeout_seconds": self._config.timeout_seconds,
            },
        }
