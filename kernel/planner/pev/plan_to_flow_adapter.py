"""PlanToFlowAdapter - HierarchicalPlan の SubGoal を Flow ノードに変換.

Planning層とFlow層を接続するアダプタ。
HierarchicalPlanner が生成した計画を FlowBuilder で実行可能な Flow に変換する。

変換ルール:
- 依存関係のない目標群 → parallel() ノード
- 依存関係のある目標 → then() ノード（依存順）
- assigned_agent が指定されていれば AgentRegistry から解決
- 未割当ての目標にはデフォルトAgentを使用

使用例:
    >>> adapter = PlanToFlowAdapter(
    ...     agent_registry={"analyzer": AnalyzerAgent(), "executor": ExecutorAgent()},
    ... )
    >>> flow = adapter.convert(plan)
    >>> result = await flow.run({"context": "..."})
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from kernel.flow.builder import FlowBuilder
from kernel.planner.pev.hierarchical_planner import (
    GoalStatus,
    HierarchicalPlan,
    SubGoal,
)


if TYPE_CHECKING:
    from kernel.flow.types import AgentProtocol


class PlanToFlowError(Exception):
    """計画からFlowへの変換エラー."""


@dataclass
class PlanToFlowAdapter:
    """HierarchicalPlan を Flow に変換するアダプタ.

    SubGoal の依存関係を解析し、FlowBuilder API を使って
    並列・逐次実行可能な Flow を構築する。

    Attributes:
        agent_registry: Agent名からAgentインスタンスへのマッピング
        default_agent: assigned_agent 未指定時のフォールバック
        strict: True の場合、Agent未解決でエラーを発生させる
    """

    agent_registry: dict[str, AgentProtocol] = field(default_factory=dict)
    default_agent: AgentProtocol | None = None
    strict: bool = False
    _logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("agentflow.pev.plan_to_flow"),
    )

    def convert(
        self,
        plan: HierarchicalPlan,
        *,
        flow_id: str | None = None,
        flow_name: str | None = None,
    ) -> Any:
        """HierarchicalPlan を Flow に変換.

        Args:
            plan: 変換対象の階層的計画
            flow_id: 生成する Flow の ID（デフォルトは plan.id）
            flow_name: 生成する Flow の名前
                （デフォルトは plan.root_goal）

        Returns:
            実行可能な Flow インスタンス

        Raises:
            PlanToFlowError: 変換に失敗した場合
        """
        resolved_flow_id = flow_id or plan.id
        resolved_flow_name = flow_name or plan.root_goal

        self._logger.info(f"計画をFlowに変換開始: {resolved_flow_id} ({sum(len(lv.goals) for lv in plan.levels)}目標)")

        builder = FlowBuilder(resolved_flow_id, name=resolved_flow_name)

        # 各レベルを処理（depth 順）
        sorted_levels = sorted(plan.levels, key=lambda lv: lv.depth)

        for level in sorted_levels:
            pending_goals = [g for g in level.goals if g.status == GoalStatus.PENDING]
            if not pending_goals:
                continue

            # 依存関係を解析して実行グループに分割
            groups = self._group_by_dependencies(pending_goals)

            for group in groups:
                if len(group) == 1:
                    # 単一目標 → then() ノード
                    agent = self._resolve_agent(group[0])
                    builder.then(
                        agent,
                        ids=[group[0].id],
                        names=[group[0].name],
                    )
                else:
                    # 複数の独立目標 → parallel() ノード
                    parallel_agents: list[tuple[str, AgentProtocol]] = []
                    for goal in group:
                        agent = self._resolve_agent(goal)
                        parallel_agents.append((goal.id, agent))
                    builder.parallel(*parallel_agents)

        # ノードが1つも追加されていない場合
        if builder._graph.node_count == 0:
            msg = f"計画 '{plan.id}' に変換可能な目標がありません（全 {sum(len(lv.goals) for lv in plan.levels)} 目標）"
            raise PlanToFlowError(msg)

        flow = builder.build()
        self._logger.info(f"Flow変換完了: {flow.flow_id}, ノード数: {flow.node_count}")
        return flow

    def _resolve_agent(self, goal: SubGoal) -> AgentProtocol:
        """SubGoal に対応する Agent を解決.

        Args:
            goal: 対象のサブ目標

        Returns:
            解決された Agent インスタンス

        Raises:
            PlanToFlowError: strict=True でAgent未解決の場合
        """
        agent_name = goal.assigned_agent

        # assigned_agent が指定されていればレジストリから取得
        if agent_name and agent_name in self.agent_registry:
            return self.agent_registry[agent_name]

        # デフォルトAgentにフォールバック
        if self.default_agent is not None:
            if agent_name:
                self._logger.warning(
                    f"Agent '{agent_name}' が見つかりません（目標: {goal.name}）、デフォルトAgentを使用"
                )
            return self.default_agent

        # strict モードではエラー
        if self.strict:
            available = list(self.agent_registry.keys())
            msg = f"目標 '{goal.name}' のAgent '{agent_name}' が 解決できません（利用可能: {available}）"
            raise PlanToFlowError(msg)

        # 非strictモード: agent_name でレジストリを検索、なければエラー
        if agent_name:
            self._logger.warning(f"Agent '{agent_name}' が見つかりません（目標: {goal.name}）、デフォルトAgentもなし")

        msg = (
            f"目標 '{goal.name}' に対応するAgentがありません。"
            f"agent_registry にAgentを登録するか"
            f" default_agent を設定してください"
        )
        raise PlanToFlowError(msg)

    def _group_by_dependencies(
        self,
        goals: list[SubGoal],
    ) -> list[list[SubGoal]]:
        """目標を依存関係に基づいてグループ化.

        同じレベル内で依存関係のない目標群を並列実行グループに、
        依存関係のある目標を逐次実行グループにまとめる。

        Args:
            goals: グループ化対象の目標リスト

        Returns:
            実行順に並んだグループのリスト
        """
        if not goals:
            return []

        # このレベル内の目標IDセット
        goal_ids = {g.id for g in goals}
        goal_map = {g.id: g for g in goals}

        # レベル内依存関係のみ考慮（他レベルへの依存は事前に解決済み）
        has_internal_dep: dict[str, list[str]] = {}
        for goal in goals:
            internal_deps = [d for d in goal.dependencies if d in goal_ids]
            has_internal_dep[goal.id] = internal_deps

        # トポロジカルソートでグループ化
        groups: list[list[SubGoal]] = []
        resolved: set[str] = set()
        remaining = set(goal_ids)

        while remaining:
            # 依存が全て解決済みの目標を抽出
            ready: list[SubGoal] = []
            for gid in list(remaining):
                deps = has_internal_dep.get(gid, [])
                if all(d in resolved for d in deps):
                    goal = goal_map.get(gid)
                    if goal is not None:
                        ready.append(goal)

            if not ready:
                # 循環依存の検出 - 残りを全て1グループにして続行
                self._logger.warning(f"循環依存を検出、残り{len(remaining)}目標を強制的にグループ化")
                forced = [goal_map[gid] for gid in remaining if gid in goal_map]
                groups.append(forced)
                break

            # 優先度でソート（priority 値が小さいほど高優先度）
            ready.sort(key=lambda g: g.priority)
            groups.append(ready)

            for g in ready:
                resolved.add(g.id)
                remaining.discard(g.id)

        return groups


__all__ = [
    "PlanToFlowAdapter",
    "PlanToFlowError",
]
