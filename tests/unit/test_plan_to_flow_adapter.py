"""PlanToFlowAdapter のテスト.

SubGoal → Flow ノードの変換ロジックを検証する。
"""

from __future__ import annotations

from typing import Any

import pytest

from agentflow.flow.flow import Flow
from agentflow.pev.hierarchical_planner import (
    GoalStatus,
    HierarchicalPlan,
    PlanLevel,
    SubGoal,
)
from agentflow.pev.plan_to_flow_adapter import PlanToFlowAdapter, PlanToFlowError


# ---------------------------------------------------------------------------
# テスト用モックAgent
# ---------------------------------------------------------------------------


class MockAgent:
    """テスト用のモックAgent."""

    def __init__(self, name: str = "mock") -> None:
        self.name = name

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """モック実行."""
        return {"agent": self.name, "status": "done"}


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


def _make_plan(
    goals: list[SubGoal],
    *,
    root_goal: str = "テスト目標",
) -> HierarchicalPlan:
    """テスト用の計画を作成."""
    level = PlanLevel(depth=0, goals=goals)
    return HierarchicalPlan(root_goal=root_goal, levels=[level])


# ---------------------------------------------------------------------------
# 基本変換テスト
# ---------------------------------------------------------------------------


class TestBasicConversion:
    """基本的なSubGoal → Flow変換のテスト."""

    def test_single_goal_converts_to_flow(self) -> None:
        """単一目標が1ノードのFlowに変換される."""
        agent = MockAgent("analyzer")
        adapter = PlanToFlowAdapter(
            agent_registry={"analyzer": agent},
        )
        plan = _make_plan(
            [
                SubGoal(name="分析", assigned_agent="analyzer"),
            ]
        )

        flow = adapter.convert(plan)

        assert isinstance(flow, Flow)
        assert flow.node_count == 1

    def test_multiple_independent_goals_become_parallel(self) -> None:
        """依存関係のない複数目標がparallelノードになる."""
        agents = {
            "a": MockAgent("a"),
            "b": MockAgent("b"),
        }
        adapter = PlanToFlowAdapter(agent_registry=agents)
        plan = _make_plan(
            [
                SubGoal(name="目標A", assigned_agent="a"),
                SubGoal(name="目標B", assigned_agent="b"),
            ]
        )

        flow = adapter.convert(plan)

        assert isinstance(flow, Flow)
        # 2つの独立目標は1つのparallelノードにまとまる
        assert flow.node_count == 1

    def test_sequential_goals_become_then_nodes(self) -> None:
        """依存関係のある目標がthenノード（逐次）になる."""
        agent = MockAgent("worker")
        goal_a = SubGoal(name="ステップ1", assigned_agent="worker")
        goal_b = SubGoal(
            name="ステップ2",
            assigned_agent="worker",
            dependencies=[goal_a.id],
        )
        adapter = PlanToFlowAdapter(
            agent_registry={"worker": agent},
        )
        plan = _make_plan([goal_a, goal_b])

        flow = adapter.convert(plan)

        assert isinstance(flow, Flow)
        # ステップ1（単独）→ ステップ2（単独）= 2ノード
        assert flow.node_count == 2

    def test_mixed_parallel_and_sequential(self) -> None:
        """並列と逐次が混在する計画を正しく変換."""
        agents = {
            "a": MockAgent("a"),
            "b": MockAgent("b"),
            "c": MockAgent("c"),
        }
        goal_a = SubGoal(name="A", assigned_agent="a")
        goal_b = SubGoal(name="B", assigned_agent="b")
        goal_c = SubGoal(
            name="C",
            assigned_agent="c",
            dependencies=[goal_a.id, goal_b.id],
        )
        adapter = PlanToFlowAdapter(agent_registry=agents)
        plan = _make_plan([goal_a, goal_b, goal_c])

        flow = adapter.convert(plan)

        assert isinstance(flow, Flow)
        # グループ1: A,B (parallel=1ノード), グループ2: C (then=1ノード)
        assert flow.node_count == 2


# ---------------------------------------------------------------------------
# Agent解決テスト
# ---------------------------------------------------------------------------


class TestAgentResolution:
    """Agent解決ロジックのテスト."""

    def test_default_agent_fallback(self) -> None:
        """assigned_agent未指定時にdefault_agentを使用."""
        default = MockAgent("default")
        adapter = PlanToFlowAdapter(default_agent=default)
        plan = _make_plan(
            [
                SubGoal(name="目標"),
            ]
        )

        flow = adapter.convert(plan)

        assert isinstance(flow, Flow)
        assert flow.node_count == 1

    def test_unknown_agent_with_default_fallback(self) -> None:
        """未知のagent名でもdefault_agentがあればフォールバック."""
        default = MockAgent("default")
        adapter = PlanToFlowAdapter(
            agent_registry={},
            default_agent=default,
        )
        plan = _make_plan(
            [
                SubGoal(name="目標", assigned_agent="unknown"),
            ]
        )

        flow = adapter.convert(plan)

        assert isinstance(flow, Flow)

    def test_missing_agent_raises_error(self) -> None:
        """Agent未解決でdefault_agentもない場合エラー."""
        adapter = PlanToFlowAdapter(agent_registry={})
        plan = _make_plan(
            [
                SubGoal(name="目標", assigned_agent="nonexistent"),
            ]
        )

        with pytest.raises(PlanToFlowError, match="Agentがありません"):
            adapter.convert(plan)

    def test_strict_mode_raises_on_unknown_agent(self) -> None:
        """strict=Trueで未知Agent指定時にエラー."""
        adapter = PlanToFlowAdapter(
            agent_registry={"known": MockAgent("known")},
            strict=True,
        )
        plan = _make_plan(
            [
                SubGoal(name="目標", assigned_agent="unknown"),
            ]
        )

        with pytest.raises(PlanToFlowError, match="解決できません"):
            adapter.convert(plan)


# ---------------------------------------------------------------------------
# エッジケーステスト
# ---------------------------------------------------------------------------


class TestEdgeCases:
    """エッジケースのテスト."""

    def test_empty_plan_raises_error(self) -> None:
        """空の計画はエラー."""
        adapter = PlanToFlowAdapter(default_agent=MockAgent())
        plan = HierarchicalPlan(root_goal="空の計画", levels=[])

        with pytest.raises(PlanToFlowError, match="変換可能な目標がありません"):
            adapter.convert(plan)

    def test_all_completed_goals_raises_error(self) -> None:
        """全目標が完了済みの場合はエラー."""
        adapter = PlanToFlowAdapter(default_agent=MockAgent())
        plan = _make_plan(
            [
                SubGoal(name="完了済み", status=GoalStatus.COMPLETED),
            ]
        )

        with pytest.raises(PlanToFlowError, match="変換可能な目標がありません"):
            adapter.convert(plan)

    def test_custom_flow_id_and_name(self) -> None:
        """カスタムflow_idとflow_nameを指定."""
        adapter = PlanToFlowAdapter(default_agent=MockAgent())
        plan = _make_plan([SubGoal(name="目標")])

        flow = adapter.convert(
            plan,
            flow_id="custom-id",
            flow_name="カスタム名",
        )

        assert flow.flow_id == "custom-id"
        assert flow.name == "カスタム名"

    def test_multi_level_plan(self) -> None:
        """複数レベルの計画を正しく処理."""
        agent = MockAgent("worker")
        level0 = PlanLevel(
            depth=0,
            goals=[SubGoal(name="L0目標", assigned_agent="worker")],
        )
        level1 = PlanLevel(
            depth=1,
            goals=[SubGoal(name="L1目標", assigned_agent="worker")],
        )
        plan = HierarchicalPlan(
            root_goal="多層計画",
            levels=[level1, level0],  # 逆順で渡してもdepthでソートされる
        )
        adapter = PlanToFlowAdapter(
            agent_registry={"worker": agent},
        )

        flow = adapter.convert(plan)

        assert isinstance(flow, Flow)
        assert flow.node_count == 2

    def test_circular_dependency_handled_gracefully(self) -> None:
        """循環依存があっても変換が完了する."""
        agent = MockAgent("worker")
        goal_a = SubGoal(name="A", assigned_agent="worker")
        goal_b = SubGoal(name="B", assigned_agent="worker")
        # 循環: A→B, B→A
        goal_a.dependencies = [goal_b.id]
        goal_b.dependencies = [goal_a.id]

        adapter = PlanToFlowAdapter(
            agent_registry={"worker": agent},
        )
        plan = _make_plan([goal_a, goal_b])

        # 循環依存は警告付きで強制グループ化される
        flow = adapter.convert(plan)

        assert isinstance(flow, Flow)
