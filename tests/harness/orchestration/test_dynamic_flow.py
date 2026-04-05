"""DynamicFlowGenerator のユニットテスト."""

from __future__ import annotations

from typing import Any
from unittest.mock import MagicMock

import pytest

from harness.orchestration.dynamic_flow import (
    DynamicFlowGenerator,
    _topological_layers,
)
from harness.orchestration.models import ExecutionPlan, PlanStep


class _MockAgent:
    """テスト用モックエージェント."""

    def __init__(self, name: str = "mock") -> None:
        self.name = name

    async def initialize(self) -> None:
        pass

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return {"agent": self.name, "input": input_data}

    async def cleanup(self) -> None:
        pass


class TestTopologicalLayers:
    """トポロジカルソートのテスト."""

    def test_empty_steps(self) -> None:
        assert _topological_layers([]) == []

    def test_no_dependencies(self) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1"),
            PlanStep(step_id="s2", agent_id="a2", description="d2"),
        ]
        layers = _topological_layers(steps)
        # 依存なし → 全て同じレイヤー
        assert len(layers) == 1
        assert len(layers[0]) == 2

    def test_sequential_dependencies(self) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1"),
            PlanStep(step_id="s2", agent_id="a2", description="d2", dependencies=["s1"]),
            PlanStep(step_id="s3", agent_id="a3", description="d3", dependencies=["s2"]),
        ]
        layers = _topological_layers(steps)
        assert len(layers) == 3
        assert layers[0][0].step_id == "s1"
        assert layers[1][0].step_id == "s2"
        assert layers[2][0].step_id == "s3"

    def test_mixed_parallel_sequential(self) -> None:
        """s1 → s2, s3（並列） → s4."""
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1"),
            PlanStep(step_id="s2", agent_id="a2", description="d2", dependencies=["s1"]),
            PlanStep(step_id="s3", agent_id="a3", description="d3", dependencies=["s1"]),
            PlanStep(step_id="s4", agent_id="a4", description="d4", dependencies=["s2", "s3"]),
        ]
        layers = _topological_layers(steps)
        assert len(layers) == 3
        # レイヤー 0: s1
        assert len(layers[0]) == 1
        # レイヤー 1: s2, s3（並列）
        assert len(layers[1]) == 2
        layer1_ids = {s.step_id for s in layers[1]}
        assert layer1_ids == {"s2", "s3"}
        # レイヤー 2: s4
        assert len(layers[2]) == 1

    def test_cycle_detection(self) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1", dependencies=["s2"]),
            PlanStep(step_id="s2", agent_id="a2", description="d2", dependencies=["s1"]),
        ]
        with pytest.raises(ValueError, match="循環依存"):
            _topological_layers(steps)

    def test_invalid_dependency_ignored(self) -> None:
        """存在しない依存は無視される."""
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1", dependencies=["nonexistent"]),
        ]
        layers = _topological_layers(steps)
        assert len(layers) == 1


class TestDynamicFlowGenerator:
    """DynamicFlowGenerator のテスト."""

    def test_generate_flow_single_step(self) -> None:
        agent = _MockAgent("agent1")
        gen = DynamicFlowGenerator(agent_map={"a1": agent})
        plan = ExecutionPlan(
            goal="単一ステップテスト",
            steps=[PlanStep(step_id="s1", agent_id="a1", description="ステップ1")],
        )

        flow = gen.generate_flow(plan)

        assert flow.flow_id == plan.plan_id

    def test_generate_flow_parallel_steps(self) -> None:
        agents = {f"a{i}": _MockAgent(f"agent{i}") for i in range(1, 4)}
        gen = DynamicFlowGenerator(agent_map=agents)
        plan = ExecutionPlan(
            goal="並列テスト",
            steps=[
                PlanStep(step_id="s1", agent_id="a1", description="d1"),
                PlanStep(step_id="s2", agent_id="a2", description="d2"),
                PlanStep(step_id="s3", agent_id="a3", description="d3"),
            ],
        )

        flow = gen.generate_flow(plan)
        assert flow is not None

    def test_generate_flow_empty_plan_raises(self) -> None:
        gen = DynamicFlowGenerator()
        plan = ExecutionPlan(goal="空")

        with pytest.raises(ValueError, match="ステップが含まれていません"):
            gen.generate_flow(plan)

    def test_generate_flow_unknown_agent_raises(self) -> None:
        gen = DynamicFlowGenerator(agent_map={})
        plan = ExecutionPlan(
            goal="テスト",
            steps=[PlanStep(step_id="s1", agent_id="unknown", description="d1")],
        )

        with pytest.raises(KeyError, match="unknown"):
            gen.generate_flow(plan)

    def test_generate_flow_with_middlewares(self) -> None:
        agent = _MockAgent("agent1")
        gen = DynamicFlowGenerator(agent_map={"a1": agent})
        plan = ExecutionPlan(
            goal="ミドルウェアテスト",
            steps=[PlanStep(step_id="s1", agent_id="a1", description="d1")],
        )

        # ダミーミドルウェア
        mock_mw = MagicMock()
        mock_mw.name = "TestMW"

        flow = gen.generate_flow(plan, middlewares=[mock_mw])
        # ミドルウェアが attach されていることを確認
        assert len(flow._executor._middlewares) >= 1
