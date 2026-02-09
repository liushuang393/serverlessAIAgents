"""ロールバックフロー.

トランザクション風のロールバック機構をサポートするフロー拡張。

【機能】
- チェックポイント/リストア
- 補償アクション
- 自動ロールバック
- ネストトランザクション

使用例:
    >>> flow = (
    ...     create_rollback_flow("transaction-flow")
    ...     .checkpoint("step1")
    ...     .then(Step1Agent)
    ...     .checkpoint("step2")
    ...     .then(Step2Agent)
    ...     .on_failure("step1", CompensationAgent)
    ...     .build()
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow.flow.nodes import AgentNode, FlowNode
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


class RollbackPolicy(Enum):
    """ロールバックポリシー."""

    MANUAL = "manual"  # 手動ロールバック
    AUTO_ON_FAILURE = "auto"  # 失敗時自動ロールバック
    CHECKPOINT = "checkpoint"  # チェックポイントへロールバック


@dataclass
class Checkpoint:
    """チェックポイント."""

    id: str
    name: str
    state_snapshot: dict[str, Any] = field(default_factory=dict)
    compensation_action: Callable[[FlowContext], Any] | None = None


@dataclass
class RollbackNode(FlowNode):
    """ロールバックノード.

    トランザクション管理とチェックポイント機能を提供。
    """

    checkpoints: dict[str, Checkpoint] = field(default_factory=dict)
    nodes: list[FlowNode] = field(default_factory=list)
    compensation_actions: dict[str, Callable[[FlowContext], Any]] = field(default_factory=dict)
    policy: RollbackPolicy = RollbackPolicy.AUTO_ON_FAILURE
    current_checkpoint: str | None = None

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.ROLLBACK)

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """ロールバック可能なフローを実行."""
        executed_nodes: list[str] = []

        try:
            for node in self.nodes:
                # チェックポイントノードの場合
                if isinstance(node, CheckpointNode):
                    self._save_checkpoint(ctx, node.checkpoint_id)
                    self.current_checkpoint = node.checkpoint_id
                    continue

                # 通常ノード実行
                result = await node.execute(ctx)
                executed_nodes.append(node.id)

                if not result.success:
                    self._logger.warning(f"ノード {node.id} 失敗、ロールバック開始")

                    if self.policy == RollbackPolicy.AUTO_ON_FAILURE:
                        await self._execute_rollback(ctx, executed_nodes)

                    return NodeResult(
                        success=False,
                        data={"error": "node_failed", "failed_node": node.id},
                        action=NextAction.STOP,
                    )

                if result.action == NextAction.STOP:
                    return result

            return NodeResult(
                success=True,
                data={"executed_nodes": executed_nodes},
                action=NextAction.CONTINUE,
            )

        except Exception as e:
            self._logger.exception(f"ロールバックフロー実行失敗: {e}")

            if self.policy == RollbackPolicy.AUTO_ON_FAILURE:
                await self._execute_rollback(ctx, executed_nodes)

            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)

    def _save_checkpoint(self, ctx: FlowContext, checkpoint_id: str) -> None:
        """チェックポイントを保存."""
        self._logger.debug(f"チェックポイント保存: {checkpoint_id}")
        checkpoint = Checkpoint(
            id=checkpoint_id,
            name=checkpoint_id,
            state_snapshot=ctx.get_all_results().copy(),
            compensation_action=self.compensation_actions.get(checkpoint_id),
        )
        self.checkpoints[checkpoint_id] = checkpoint

    async def _execute_rollback(self, ctx: FlowContext, executed_nodes: list[str]) -> None:
        """ロールバックを実行."""
        self._logger.info(f"ロールバック実行: {len(executed_nodes)}ノード")

        # 逆順で補償アクションを実行
        for node_id in reversed(executed_nodes):
            if node_id in self.compensation_actions:
                self._logger.debug(f"補償アクション実行: {node_id}")
                try:
                    compensation = self.compensation_actions[node_id]
                    await self._run_compensation(compensation, ctx)
                except Exception as e:
                    self._logger.exception(f"補償アクション失敗: {node_id}: {e}")

    async def _run_compensation(
        self,
        compensation: Callable[[FlowContext], Any],
        ctx: FlowContext,
    ) -> Any:
        """補償アクションを実行."""
        import asyncio
        import inspect

        if inspect.iscoroutinefunction(compensation):
            return await compensation(ctx)
        return await asyncio.get_event_loop().run_in_executor(None, compensation, ctx)

    def restore_checkpoint(self, ctx: FlowContext, checkpoint_id: str) -> bool:
        """チェックポイントを復元."""
        if checkpoint_id not in self.checkpoints:
            self._logger.error(f"チェックポイント未発見: {checkpoint_id}")
            return False

        checkpoint = self.checkpoints[checkpoint_id]
        self._logger.info(f"チェックポイント復元: {checkpoint_id}")

        # 状態を復元
        for key, value in checkpoint.state_snapshot.items():
            ctx.set_result(key, value)

        return True


@dataclass
class CheckpointNode(FlowNode):
    """チェックポイントマーカーノード."""

    checkpoint_id: str = ""

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.ROLLBACK)

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """チェックポイントをマーク（実際の保存はRollbackNodeが行う）."""
        return NodeResult(
            success=True, data={"checkpoint": self.checkpoint_id}, action=NextAction.CONTINUE
        )


class RollbackFlowBuilder:
    """ロールバックフロービルダー."""

    def __init__(self, flow_id: str) -> None:
        """初期化."""
        self._flow_id = flow_id
        self._node = RollbackNode(id=flow_id, name=flow_id)
        self._checkpoint_counter = 0

    def checkpoint(self, checkpoint_id: str | None = None) -> RollbackFlowBuilder:
        """チェックポイントを追加."""
        if checkpoint_id is None:
            self._checkpoint_counter += 1
            checkpoint_id = f"checkpoint_{self._checkpoint_counter}"

        cp_node = CheckpointNode(
            id=f"cp_{checkpoint_id}",
            name=f"Checkpoint: {checkpoint_id}",
            checkpoint_id=checkpoint_id,
        )
        self._node.nodes.append(cp_node)
        return self

    def then(self, *agents: AgentProtocol) -> RollbackFlowBuilder:
        """Agentを追加."""
        for agent in agents:
            agent_id = getattr(agent, "name", agent.__class__.__name__)
            node = AgentNode(id=agent_id, name=agent_id, agent=agent)
            self._node.nodes.append(node)
        return self

    def on_failure(
        self,
        target: str,
        compensation: Callable[[FlowContext], Any] | AgentProtocol,
    ) -> RollbackFlowBuilder:
        """失敗時の補償アクションを設定."""
        if isinstance(compensation, AgentProtocol):
            # Agentをラップ
            async def agent_compensation(ctx: FlowContext) -> Any:
                return await compensation.run(ctx.get_all_results())

            self._node.compensation_actions[target] = agent_compensation
        else:
            self._node.compensation_actions[target] = compensation
        return self

    def policy(self, policy: str | RollbackPolicy) -> RollbackFlowBuilder:
        """ロールバックポリシーを設定."""
        if isinstance(policy, str):
            policy = RollbackPolicy(policy)
        self._node.policy = policy
        return self

    def build(self) -> RollbackNode:
        """ロールバックノードを構築."""
        return self._node


def create_rollback_flow(flow_id: str) -> RollbackFlowBuilder:
    """ロールバックフロービルダーを作成."""
    return RollbackFlowBuilder(flow_id)


__all__ = [
    "Checkpoint",
    "CheckpointNode",
    "RollbackFlowBuilder",
    "RollbackNode",
    "RollbackPolicy",
    "create_rollback_flow",
]
