"""強化並列実行フロー.

タイムアウト、部分成功処理、結果集約戦略をサポートする並列実行拡張。

【機能】
- タイムアウト設定
- 部分成功の処理
- 結果集約戦略（all, first, majority）
- エラーハンドリングポリシー

使用例:
    >>> flow = (
    ...     create_parallel_flow("parallel-analysis")
    ...     .add(Agent1, timeout=30)
    ...     .add(Agent2, timeout=30)
    ...     .add(Agent3, timeout=30)
    ...     .aggregate("all")
    ...     .on_partial_failure("continue")
    ...     .build()
    ... )
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow.flow.nodes import FlowNode
from agentflow.flow.types import (
    AgentProtocol,
    NextAction,
    NodeResult,
    NodeType,
)


if TYPE_CHECKING:
    from collections.abc import Callable

    from agentflow.flow.context import FlowContext

logger = logging.getLogger(__name__)


class AggregationStrategy(Enum):
    """結果集約戦略."""

    ALL = "all"  # すべての結果を収集
    FIRST = "first"  # 最初に完了した結果を使用
    MAJORITY = "majority"  # 過半数が成功すれば成功


class PartialFailurePolicy(Enum):
    """部分失敗ポリシー."""

    FAIL_FAST = "fail_fast"  # 最初の失敗で停止
    CONTINUE = "continue"  # 失敗を無視して続行
    FAIL_MAJORITY = "fail_majority"  # 過半数失敗で停止


@dataclass
class ParallelTask:
    """並列タスク定義."""

    id: str
    agent: AgentProtocol
    timeout: float | None = None
    input_mapper: Callable[[FlowContext], dict[str, Any]] | None = None


@dataclass
class EnhancedParallelNode(FlowNode):
    """強化並列実行ノード.

    タイムアウト、部分成功、集約戦略をサポート。
    """

    tasks: list[ParallelTask] = field(default_factory=list)
    aggregation: AggregationStrategy = AggregationStrategy.ALL
    failure_policy: PartialFailurePolicy = PartialFailurePolicy.CONTINUE
    global_timeout: float | None = None

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.PARALLEL)

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """強化並列実行."""
        try:
            tasks_coro = [self._run_task_with_timeout(ctx, task) for task in self.tasks]

            if self.global_timeout:
                results = await asyncio.wait_for(
                    asyncio.gather(*tasks_coro, return_exceptions=True),
                    timeout=self.global_timeout,
                )
            else:
                results = await asyncio.gather(*tasks_coro, return_exceptions=True)

            return self._aggregate_results(ctx, results)

        except TimeoutError:
            self._logger.exception(f"グローバルタイムアウト: {self.global_timeout}秒")
            return NodeResult(
                success=False,
                data={"error": "global_timeout", "timeout": self.global_timeout},
                action=NextAction.STOP,
            )
        except Exception as e:
            self._logger.exception(f"並列実行失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)

    async def _run_task_with_timeout(
        self, ctx: FlowContext, task: ParallelTask
    ) -> tuple[str, dict[str, Any] | Exception]:
        """タイムアウト付きでタスクを実行."""
        try:
            inputs = task.input_mapper(ctx) if task.input_mapper else ctx.get_inputs()

            if task.timeout:
                result = await asyncio.wait_for(task.agent.run(inputs), timeout=task.timeout)
            else:
                result = await task.agent.run(inputs)

            ctx.set_result(task.id, result)
            return (task.id, result)

        except TimeoutError:
            self._logger.warning(f"タスク {task.id} タイムアウト")
            return (task.id, TimeoutError(f"タスク {task.id} がタイムアウト"))
        except Exception as e:
            self._logger.exception(f"タスク {task.id} 失敗: {e}")
            return (task.id, e)

    def _aggregate_results(
        self,
        ctx: FlowContext,
        results: list[tuple[str, dict[str, Any] | Exception] | BaseException],
    ) -> NodeResult:
        """結果を集約."""
        success_results: dict[str, Any] = {}
        errors: dict[str, str] = {}

        for item in results:
            if isinstance(item, BaseException):
                errors["unknown"] = str(item)
            else:
                task_id, result = item
                if isinstance(result, BaseException):
                    errors[task_id] = str(result)
                else:
                    success_results[task_id] = result

        total = len(self.tasks)
        success_count = len(success_results)
        failure_count = len(errors)

        # 失敗ポリシーチェック
        if self.failure_policy == PartialFailurePolicy.FAIL_FAST and failure_count > 0:
            return NodeResult(
                success=False,
                data={"results": success_results, "errors": errors},
                action=NextAction.STOP,
            )

        if self.failure_policy == PartialFailurePolicy.FAIL_MAJORITY:
            if failure_count > total // 2:
                return NodeResult(
                    success=False,
                    data={"results": success_results, "errors": errors},
                    action=NextAction.STOP,
                )

        # 集約戦略
        if self.aggregation == AggregationStrategy.FIRST:
            first_result: dict[str, Any] = next(iter(success_results.values()), {})
            return NodeResult(
                success=True,
                data={"result": first_result, "all_results": success_results},
                action=NextAction.CONTINUE,
            )

        if self.aggregation == AggregationStrategy.MAJORITY:
            if success_count > total // 2:
                return NodeResult(
                    success=True,
                    data={"results": success_results, "errors": errors},
                    action=NextAction.CONTINUE,
                )
            return NodeResult(
                success=False,
                data={"results": success_results, "errors": errors},
                action=NextAction.STOP,
            )

        # ALL戦略（デフォルト）
        return NodeResult(
            success=failure_count == 0,
            data={"results": success_results, "errors": errors},
            action=NextAction.CONTINUE if failure_count == 0 else NextAction.STOP,
        )

    def add_task(
        self,
        task_id: str,
        agent: AgentProtocol,
        timeout: float | None = None,
        input_mapper: Callable[[FlowContext], dict[str, Any]] | None = None,
    ) -> None:
        """タスクを追加."""
        self.tasks.append(ParallelTask(id=task_id, agent=agent, timeout=timeout, input_mapper=input_mapper))


class ParallelFlowBuilder:
    """強化並列フロービルダー."""

    def __init__(self, flow_id: str) -> None:
        """初期化."""
        self._flow_id = flow_id
        self._node = EnhancedParallelNode(id=flow_id, name=flow_id)

    def add(
        self,
        agent: AgentProtocol,
        task_id: str | None = None,
        timeout: float | None = None,
        input_mapper: Callable[[FlowContext], dict[str, Any]] | None = None,
    ) -> ParallelFlowBuilder:
        """Agentを追加."""
        raw_task_id = task_id or getattr(agent, "name", agent.__class__.__name__)
        tid = str(raw_task_id)
        self._node.add_task(tid, agent, timeout, input_mapper)
        return self

    def aggregate(self, strategy: str | AggregationStrategy) -> ParallelFlowBuilder:
        """集約戦略を設定."""
        if isinstance(strategy, str):
            strategy = AggregationStrategy(strategy)
        self._node.aggregation = strategy
        return self

    def on_partial_failure(self, policy: str | PartialFailurePolicy) -> ParallelFlowBuilder:
        """部分失敗ポリシーを設定."""
        if isinstance(policy, str):
            policy = PartialFailurePolicy(policy)
        self._node.failure_policy = policy
        return self

    def timeout(self, seconds: float) -> ParallelFlowBuilder:
        """グローバルタイムアウトを設定."""
        self._node.global_timeout = seconds
        return self

    def build(self) -> EnhancedParallelNode:
        """並列ノードを構築."""
        return self._node


def create_parallel_flow(flow_id: str) -> ParallelFlowBuilder:
    """強化並列フロービルダーを作成."""
    return ParallelFlowBuilder(flow_id)


__all__ = [
    "AggregationStrategy",
    "EnhancedParallelNode",
    "ParallelFlowBuilder",
    "ParallelTask",
    "PartialFailurePolicy",
    "create_parallel_flow",
]
