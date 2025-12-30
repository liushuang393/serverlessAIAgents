"""Hierarchical 協調パターン - 階層型タスク分解.

このモジュールは Hierarchical パターンを実装します：
- 根 Agent がタスクを分解
- 子協調器に委譲
- 結果を集約

設計原則：
- 再帰的な階層構造をサポート
- 子協調器は任意のパターン（Sequential/Concurrent/Supervisor）
- 柔軟なタスク分解と結果集約

参考：
- LangGraph: Hierarchical Agent Teams
- Azure Architecture: AI Agent Orchestration Patterns
"""

import asyncio
import logging
from dataclasses import dataclass, field
from typing import Any

from agentflow.core.agent_block import AgentBlock
from agentflow.patterns.coordinator import CoordinationPattern, CoordinatorBase
from agentflow.patterns.multi_agent import SharedContext


@dataclass
class SubTask:
    """サブタスク定義.

    Attributes:
        id: サブタスク ID
        task: タスク内容
        coordinator_type: 委譲先協調器種別
        priority: 優先度（高い順）
        dependencies: 依存タスク ID リスト
    """

    id: str
    task: str
    coordinator_type: str
    priority: int = 0
    dependencies: list[str] = field(default_factory=list)


class HierarchicalCoordinator(CoordinatorBase):
    """Hierarchical 協調器 - 階層パターン.

    根 Agent がタスクを分解し、子協調器に委譲します。
    子協調器は任意の協調パターンを使用できます。

    Example:
        >>> root = MyRootAgent()
        >>> sub_coordinators = {
        ...     "research": SequentialCoordinator([...]),
        ...     "analysis": ConcurrentCoordinator([...]),
        ... }
        >>> coordinator = HierarchicalCoordinator(root, sub_coordinators)
        >>> result = await coordinator.execute("複雑な分析タスク")
    """

    def __init__(
        self,
        root: AgentBlock,
        sub_coordinators: dict[str, CoordinatorBase],
        parallel_execution: bool = True,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            root: 根 Agent（タスク分解・結果集約）
            sub_coordinators: 子協調器辞書 {"種別": Coordinator}
            parallel_execution: 並行実行するか
            **kwargs: 追加パラメータ

        Raises:
            TypeError: root が AgentBlock でない場合
            ValueError: sub_coordinators が空の場合
        """
        if not isinstance(root, AgentBlock):
            msg = f"root must be AgentBlock, got {type(root).__name__}"
            raise TypeError(msg)
        if not sub_coordinators:
            msg = "sub_coordinators cannot be empty"
            raise ValueError(msg)

        super().__init__()
        self._root = root
        self._sub_coordinators = sub_coordinators
        self._parallel_execution = parallel_execution
        self._logger = logging.getLogger(__name__)

    @property
    def pattern(self) -> CoordinationPattern:
        """協調パターン種別."""
        return CoordinationPattern.HIERARCHICAL

    async def execute(self, task: str, **kwargs: Any) -> dict[str, Any]:
        """タスクを実行.

        Args:
            task: 実行タスク
            **kwargs: 追加パラメータ

        Returns:
            実行結果
        """
        # 1. 根 Agent にタスクを分解させる
        decomposition = await self._decompose_task(task)
        subtasks = decomposition.get("subtasks", [])
        self._logger.info(f"Task decomposed into {len(subtasks)} subtasks")

        # 2. サブタスクを実行
        if self._parallel_execution:
            results = await self._execute_parallel(subtasks)
        else:
            results = await self._execute_sequential(subtasks)

        # 3. 根 Agent に結果を集約させる
        final_result = await self._aggregate_results(task, results)

        return {
            "result": final_result,
            "subtask_count": len(subtasks),
            "subtask_results": results,
        }

    async def _decompose_task(self, task: str) -> dict[str, Any]:
        """タスクを分解.

        Args:
            task: 元のタスク

        Returns:
            分解結果（subtasks リスト含む）
        """
        decompose_input = {
            "task": task,
            "action": "decompose",
            "available_coordinators": list(self._sub_coordinators.keys()),
        }
        return await self._root.run(decompose_input)

    async def _execute_parallel(
        self,
        subtasks: list[dict[str, Any]],
    ) -> dict[str, Any]:
        """サブタスクを並行実行.

        Args:
            subtasks: サブタスクリスト

        Returns:
            実行結果辞書
        """
        async def execute_subtask(subtask: dict) -> tuple[str, Any]:
            subtask_id = subtask.get("id", "unknown")
            coordinator_type = subtask.get("coordinator_type", "")
            task_content = subtask.get("task", "")

            coordinator = self._sub_coordinators.get(coordinator_type)
            if coordinator:
                result = await coordinator.execute(task_content)
                return subtask_id, result
            self._logger.warning(f"Unknown coordinator: {coordinator_type}")
            return subtask_id, {"error": f"Unknown coordinator: {coordinator_type}"}

        tasks = [execute_subtask(st) for st in subtasks]
        results_list = await asyncio.gather(*tasks, return_exceptions=True)

        return {
            task_id: result
            for task_id, result in results_list
            if not isinstance(result, Exception)
        }

    async def _execute_sequential(
        self,
        subtasks: list[dict[str, Any]],
    ) -> dict[str, Any]:
        """サブタスクを順次実行."""
        results: dict[str, Any] = {}
        for subtask in subtasks:
            subtask_id = subtask.get("id", "unknown")
            coordinator_type = subtask.get("coordinator_type", "")
            task_content = subtask.get("task", "")

            coordinator = self._sub_coordinators.get(coordinator_type)
            if coordinator:
                results[subtask_id] = await coordinator.execute(task_content)
        return results

    async def _aggregate_results(
        self,
        task: str,
        results: dict[str, Any],
    ) -> Any:
        """結果を集約."""
        aggregate_input = {
            "task": task,
            "action": "aggregate",
            "results": results,
        }
        aggregation = await self._root.run(aggregate_input)
        return aggregation.get("final_result", results)

