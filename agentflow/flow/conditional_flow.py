"""条件分岐フロー.

複雑な条件分岐ロジックをサポートするフロー拡張。

【機能】
- 条件式評価による分岐
- switch-case形式の多分岐
- 条件のネスト
- デフォルト分岐

使用例:
    >>> flow = (
    ...     create_conditional_flow("decision-flow")
    ...     .when(lambda ctx: ctx.get("priority") == "high")
    ...     .then(HighPriorityAgent)
    ...     .when(lambda ctx: ctx.get("priority") == "medium")
    ...     .then(MediumPriorityAgent)
    ...     .otherwise()
    ...     .then(LowPriorityAgent)
    ...     .build()
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
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


@dataclass
class ConditionalBranch:
    """条件分岐."""

    condition: Callable[[FlowContext], bool] | None = None  # Noneはotherwise
    nodes: list[FlowNode] = field(default_factory=list)
    label: str = ""


@dataclass
class ConditionalNode(FlowNode):
    """条件分岐ノード.

    複数の条件分岐を評価し、最初に真となる分岐を実行。
    """

    branches: list[ConditionalBranch] = field(default_factory=list)

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.CONDITIONAL)

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """条件分岐を評価して実行."""
        try:
            # 各分岐を順番に評価
            for branch in self.branches:
                if branch.condition is None:
                    # otherwise（デフォルト分岐）
                    self._logger.debug(f"デフォルト分岐を実行: {branch.label}")
                    return await self._execute_branch(ctx, branch)

                if branch.condition(ctx):
                    self._logger.debug(f"条件分岐を実行: {branch.label}")
                    return await self._execute_branch(ctx, branch)

            # 条件を満たす分岐がない場合
            self._logger.warning("条件を満たす分岐がありません")
            return NodeResult(success=True, data={}, action=NextAction.CONTINUE)

        except Exception as e:
            self._logger.exception(f"条件分岐実行失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)

    async def _execute_branch(self, ctx: FlowContext, branch: ConditionalBranch) -> NodeResult:
        """分岐内のノードを順番に実行."""
        combined_data: dict[str, Any] = {}

        for node in branch.nodes:
            result = await node.execute(ctx)
            combined_data[node.id] = result.data

            if result.action in (NextAction.STOP, NextAction.EARLY_RETURN):
                return NodeResult(
                    success=result.success,
                    data=combined_data,
                    action=result.action,
                    early_return_data=result.early_return_data,
                )

        return NodeResult(success=True, data=combined_data, action=NextAction.CONTINUE)

    def add_branch(
        self,
        condition: Callable[[FlowContext], bool] | None,
        nodes: list[FlowNode],
        label: str = "",
    ) -> None:
        """分岐を追加."""
        self.branches.append(ConditionalBranch(condition=condition, nodes=nodes, label=label))


@dataclass
class SwitchCase:
    """switch-caseの1ケース."""

    value: Any
    nodes: list[FlowNode] = field(default_factory=list)


@dataclass
class SwitchNode(FlowNode):
    """switch-caseノード.

    評価式の結果に基づいて分岐を選択。
    """

    expression: Callable[[FlowContext], Any] | None = None
    cases: list[SwitchCase] = field(default_factory=list)
    default_nodes: list[FlowNode] = field(default_factory=list)

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", NodeType.SWITCH)

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """switch-caseを評価して実行."""
        try:
            if not self.expression:
                return NodeResult(
                    success=False, data={"error": "式が未定義"}, action=NextAction.STOP
                )

            value = self.expression(ctx)
            self._logger.debug(f"switch値: {value}")

            # マッチするcaseを探す
            for case in self.cases:
                if case.value == value:
                    self._logger.debug(f"case {case.value} を実行")
                    return await self._execute_nodes(ctx, case.nodes)

            # デフォルトケース
            if self.default_nodes:
                self._logger.debug("デフォルトケースを実行")
                return await self._execute_nodes(ctx, self.default_nodes)

            return NodeResult(success=True, data={}, action=NextAction.CONTINUE)

        except Exception as e:
            self._logger.exception(f"switch実行失敗: {e}")
            return NodeResult(success=False, data={"error": str(e)}, action=NextAction.STOP)

    async def _execute_nodes(self, ctx: FlowContext, nodes: list[FlowNode]) -> NodeResult:
        """ノードリストを順番に実行."""
        combined_data: dict[str, Any] = {}

        for node in nodes:
            result = await node.execute(ctx)
            combined_data[node.id] = result.data

            if result.action in (NextAction.STOP, NextAction.EARLY_RETURN):
                return NodeResult(
                    success=result.success,
                    data=combined_data,
                    action=result.action,
                    early_return_data=result.early_return_data,
                )

        return NodeResult(success=True, data=combined_data, action=NextAction.CONTINUE)

    def add_case(self, value: Any, nodes: list[FlowNode]) -> None:
        """caseを追加."""
        self.cases.append(SwitchCase(value=value, nodes=nodes))

    def set_default(self, nodes: list[FlowNode]) -> None:
        """デフォルトケースを設定."""
        self.default_nodes = nodes


def create_conditional_node(
    node_id: str,
    name: str,
) -> ConditionalNode:
    """条件分岐ノードを作成."""
    return ConditionalNode(id=node_id, name=name)


def create_switch_node(
    node_id: str,
    name: str,
    expression: Callable[[FlowContext], Any],
) -> SwitchNode:
    """switch-caseノードを作成."""
    return SwitchNode(id=node_id, name=name, expression=expression)


class ConditionalFlowBuilder:
    """条件分岐フロービルダー.

    流暢なAPIで条件分岐フローを構築。
    """

    def __init__(self, flow_id: str) -> None:
        """初期化."""
        self._flow_id = flow_id
        self._node = ConditionalNode(id=flow_id, name=flow_id)
        self._current_condition: Callable[[FlowContext], bool] | None = None
        self._current_nodes: list[FlowNode] = []
        self._current_label: str = ""

    def when(
        self,
        condition: Callable[[FlowContext], bool],
        label: str = "",
    ) -> ConditionalFlowBuilder:
        """条件分岐を追加."""
        self._flush_current_branch()
        self._current_condition = condition
        self._current_label = label
        return self

    def otherwise(self, label: str = "default") -> ConditionalFlowBuilder:
        """デフォルト分岐."""
        self._flush_current_branch()
        self._current_condition = None
        self._current_label = label
        return self

    def then(self, *agents: AgentProtocol) -> ConditionalFlowBuilder:
        """Agentを追加."""
        for agent in agents:
            agent_id = getattr(agent, "name", agent.__class__.__name__)
            node = AgentNode(id=agent_id, name=agent_id, agent=agent)
            self._current_nodes.append(node)
        return self

    def _flush_current_branch(self) -> None:
        """現在の分岐をフラッシュ."""
        if self._current_nodes:
            self._node.add_branch(
                condition=self._current_condition,
                nodes=self._current_nodes,
                label=self._current_label,
            )
            self._current_nodes = []

    def build(self) -> ConditionalNode:
        """条件分岐ノードを構築."""
        self._flush_current_branch()
        return self._node


def create_conditional_flow(flow_id: str) -> ConditionalFlowBuilder:
    """条件分岐フロービルダーを作成."""
    return ConditionalFlowBuilder(flow_id)


__all__ = [
    "ConditionalBranch",
    "ConditionalFlowBuilder",
    "ConditionalNode",
    "SwitchCase",
    "SwitchNode",
    "create_conditional_flow",
    "create_conditional_node",
    "create_switch_node",
]
