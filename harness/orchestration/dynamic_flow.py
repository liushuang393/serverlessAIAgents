"""動的フロー生成 - ExecutionPlan から実行可能フローを構築.

Planner Agent が生成した ExecutionPlan を解析し、
依存関係に基づきトポロジカルソート → FlowBuilder DSL で
動的にフローを組み立てる。ミドルウェアも自動 attach する。
"""

from __future__ import annotations

import logging
from collections import defaultdict, deque
from typing import TYPE_CHECKING, Any

from kernel.flow.builder import FlowBuilder
from kernel.flow.types import AgentProtocol


if TYPE_CHECKING:
    from contracts.flow.contracts import FlowMiddleware
    from harness.orchestration.models import ExecutionPlan, PlanStep
    from kernel.flow.context import FlowContext
    from kernel.flow.flow import Flow

_logger = logging.getLogger(__name__)


# === トポロジカルソート（ステップの依存解決）===


def _topological_layers(steps: list[PlanStep]) -> list[list[PlanStep]]:
    """ステップを依存関係に基づき層ごとにグループ化.

    同じ層のステップは依存がなく並列実行可能。
    層は依存順に並ぶ（前の層が先に実行される）。

    Args:
        steps: 実行ステップ一覧

    Returns:
        層ごとのステップリスト

    Raises:
        ValueError: 循環依存が検出された場合
    """
    if not steps:
        return []

    step_map = {s.step_id: s for s in steps}
    valid_ids = set(step_map.keys())

    # 入次数を計算（存在しない依存は無視）
    in_degree: dict[str, int] = {s.step_id: 0 for s in steps}
    dependents: dict[str, list[str]] = defaultdict(list)

    for s in steps:
        for dep in s.dependencies:
            if dep in valid_ids:
                in_degree[s.step_id] += 1
                dependents[dep].append(s.step_id)

    # BFS でレイヤー分割
    layers: list[list[PlanStep]] = []
    queue: deque[str] = deque(sid for sid, deg in in_degree.items() if deg == 0)

    processed = 0
    while queue:
        current_layer_ids = list(queue)
        queue.clear()

        layer = [step_map[sid] for sid in current_layer_ids]
        layers.append(layer)
        processed += len(layer)

        for sid in current_layer_ids:
            for dependent in dependents[sid]:
                in_degree[dependent] -= 1
                if in_degree[dependent] == 0:
                    queue.append(dependent)

    if processed != len(steps):
        msg = "ExecutionPlan に循環依存が含まれています"
        raise ValueError(msg)

    return layers


# === エージェント解決 ===


class _AgentResolver:
    """エージェントID → AgentProtocol インスタンスの解決.

    AgentRegistry が利用可能な場合は Registry 経由で解決し、
    そうでない場合は事前登録されたマッピングを使用する。
    """

    def __init__(
        self,
        agent_map: dict[str, AgentProtocol] | None = None,
        registry: Any = None,
    ) -> None:
        """初期化.

        Args:
            agent_map: agent_id → AgentProtocol の事前マッピング
            registry: AgentRegistry インスタンス（オプション）
        """
        self._agent_map: dict[str, AgentProtocol] = agent_map or {}
        self._registry = registry

    def resolve(self, agent_id: str) -> AgentProtocol:
        """agent_id から AgentProtocol を解決.

        Args:
            agent_id: エージェントID

        Returns:
            AgentProtocol インスタンス

        Raises:
            KeyError: エージェントが見つからない場合
        """
        # 事前マッピングを優先
        if agent_id in self._agent_map:
            return self._agent_map[agent_id]

        # Registry フォールバック
        if self._registry is not None:
            instance = self._registry.get_instance(agent_id)
            if isinstance(instance, AgentProtocol):
                return instance

        msg = f"エージェント '{agent_id}' が見つかりません"
        raise KeyError(msg)


# === 入力マッパー ===


def _create_input_mapper(
    step: PlanStep,
) -> Any:
    """ステップ用の入力マッパーを生成.

    前ステップの結果と input_spec をマージする関数を返す。

    Args:
        step: 対象ステップ

    Returns:
        FlowContext → dict のマッパー関数
    """
    deps = list(step.dependencies)
    spec = dict(step.input_spec)

    def _mapper(ctx: FlowContext) -> dict[str, Any]:
        merged: dict[str, Any] = {}
        # 前ステップの結果を収集
        for dep_id in deps:
            dep_result = ctx.get_result(dep_id)
            if dep_result is not None:
                merged[dep_id] = dep_result
        # input_spec のテンプレートをマージ
        merged.update(spec)
        # 元の入力も渡す
        original = ctx.get("_original_inputs")
        if isinstance(original, dict):
            merged.setdefault("_original_inputs", original)
        return merged

    return _mapper


# === DynamicFlowGenerator ===


class DynamicFlowGenerator:
    """ExecutionPlan から動的にフローを生成.

    ステップの依存関係をトポロジカルソートし、
    並列実行可能なステップは ParallelNode、
    順次実行はシーケンシャルに FlowBuilder で構築する。

    Example:
        >>> generator = DynamicFlowGenerator(agent_map={"analyzer": analyzer_agent})
        >>> flow = generator.generate_flow(plan, middlewares=[audit_mw, risk_mw])
        >>> result = await flow.run(inputs)
    """

    def __init__(
        self,
        *,
        agent_map: dict[str, AgentProtocol] | None = None,
        registry: Any = None,
    ) -> None:
        """初期化.

        Args:
            agent_map: agent_id → AgentProtocol の事前マッピング
            registry: AgentRegistry インスタンス（オプション）
        """
        self._resolver = _AgentResolver(agent_map=agent_map, registry=registry)

    def generate_flow(
        self,
        plan: ExecutionPlan,
        *,
        middlewares: list[FlowMiddleware] | None = None,
    ) -> Flow:
        """ExecutionPlan からフローを生成.

        Args:
            plan: 実行計画
            middlewares: フローに attach するミドルウェアリスト

        Returns:
            実行可能な Flow インスタンス

        Raises:
            ValueError: 計画にステップがない、または循環依存がある場合
            KeyError: エージェントが解決できない場合
        """
        if not plan.steps:
            msg = "ExecutionPlan にステップが含まれていません"
            raise ValueError(msg)

        layers = _topological_layers(plan.steps)

        builder = FlowBuilder(
            plan.plan_id,
            name=f"動的フロー: {plan.goal[:50]}",
        )

        for layer in layers:
            self._add_layer(builder, layer)

        flow = builder.build()

        # ミドルウェアを attach
        if middlewares:
            for mw in middlewares:
                flow._executor.add_middleware(mw)

        _logger.info(
            "動的フロー生成完了: plan_id=%s, ステップ数=%d, レイヤー数=%d",
            plan.plan_id,
            len(plan.steps),
            len(layers),
        )

        return flow

    def _add_layer(self, builder: FlowBuilder, layer: list[PlanStep]) -> None:
        """レイヤーをビルダーに追加.

        1ステップ → then()、2ステップ以上 → parallel()。
        """
        if len(layer) == 1:
            step = layer[0]
            agent = self._resolver.resolve(step.agent_id)
            mapper = _create_input_mapper(step) if step.dependencies or step.input_spec else None
            builder.then(
                agent,
                ids=[step.step_id],
                names=[step.description],
                input_mappers={step.step_id: mapper} if mapper else None,
            )
        else:
            # 並列実行
            parallel_agents: list[tuple[str, AgentProtocol]] = []
            mappers: dict[str, Any] = {}
            for step in layer:
                agent = self._resolver.resolve(step.agent_id)
                parallel_agents.append((step.step_id, agent))
                mapper = _create_input_mapper(step)
                mappers[step.step_id] = mapper

            builder.parallel(
                *parallel_agents,
                name=f"並列実行({len(layer)}ステップ)",
                input_mappers=mappers,
            )


__all__ = ["DynamicFlowGenerator"]
