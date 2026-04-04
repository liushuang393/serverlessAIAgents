"""AppOrchestrationAdapter のユニットテスト."""

from __future__ import annotations

from typing import Any

import pytest

from harness.orchestration.app_adapter import (
    AppOrchestrationAdapter,
    convert_app_plan_to_execution_plan,
)
from harness.orchestration.models import ExecutionPlan, PlanStep
from harness.risk.service import RiskLevel


# === テスト用 App 計画モデル ===


class _FakeAppPlan:
    """messaging_hub の HarnessPlan を模倣."""

    def __init__(
        self,
        *,
        goal: str = "テスト目標",
        blueprint_id: str = "test.blueprint",
        task_kind: str = "test",
        execution_plan: dict[str, Any] | None = None,
    ) -> None:
        self._goal = goal
        self._blueprint_id = blueprint_id
        self._task_kind = task_kind
        self._execution_plan = execution_plan or {}

    @property
    def goal(self) -> str:
        return self._goal

    @property
    def blueprint_id(self) -> str:
        return self._blueprint_id

    @property
    def task_kind(self) -> str:
        return self._task_kind

    @property
    def execution_plan(self) -> dict[str, Any]:
        return self._execution_plan


class _MockAgent:
    """テスト用モックエージェント."""

    def __init__(self, name: str = "mock") -> None:
        self.name = name

    async def initialize(self) -> None:
        pass

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return {"agent": self.name, "status": "ok"}

    async def cleanup(self) -> None:
        pass


# === convert_app_plan_to_execution_plan テスト ===


class TestConvertAppPlan:
    """App 計画 → ExecutionPlan 変換のテスト."""

    def test_empty_steps(self) -> None:
        app_plan = _FakeAppPlan(goal="空の計画")
        plan = convert_app_plan_to_execution_plan(app_plan)

        assert plan.goal == "空の計画"
        assert plan.steps == []
        assert plan.metadata["blueprint_id"] == "test.blueprint"

    def test_dict_steps(self) -> None:
        app_plan = _FakeAppPlan(
            goal="機票検索",
            blueprint_id="structured_monitoring.flight_watch",
            task_kind="structured_monitoring",
            execution_plan={
                "steps": [
                    {
                        "step_id": "s1",
                        "agent_id": "flight_search",
                        "description": "フライト検索",
                        "risk_level": "low",
                        "input_spec": {"origin": "NRT", "destination": "LAX"},
                    },
                    {
                        "step_id": "s2",
                        "agent_id": "flight_ranking",
                        "description": "ランキング",
                        "risk_level": "low",
                        "dependencies": ["s1"],
                    },
                ],
            },
        )

        plan = convert_app_plan_to_execution_plan(app_plan)

        assert len(plan.steps) == 2
        assert plan.steps[0].step_id == "s1"
        assert plan.steps[0].agent_id == "flight_search"
        assert plan.steps[0].input_spec == {"origin": "NRT", "destination": "LAX"}
        assert plan.steps[1].dependencies == ["s1"]
        assert plan.overall_risk == RiskLevel.LOW

    def test_string_steps(self) -> None:
        """文字列形式のステップも変換できる."""
        app_plan = _FakeAppPlan(
            goal="テスト",
            execution_plan={"steps": ["search", "rank", "subscribe"]},
        )

        plan = convert_app_plan_to_execution_plan(app_plan)

        assert len(plan.steps) == 3
        assert plan.steps[0].agent_id == "search"
        assert plan.steps[2].agent_id == "subscribe"

    def test_risk_level_propagation(self) -> None:
        """ステップのリスクが全体リスクに反映される."""
        app_plan = _FakeAppPlan(
            goal="テスト",
            execution_plan={
                "steps": [
                    {"agent_id": "safe", "description": "d", "risk_level": "low"},
                    {"agent_id": "risky", "description": "d", "risk_level": "high"},
                ],
            },
        )

        plan = convert_app_plan_to_execution_plan(app_plan)

        assert plan.overall_risk == RiskLevel.HIGH

    def test_capability_field_fallback(self) -> None:
        """agent_id がない場合 capability フィールドを使う."""
        app_plan = _FakeAppPlan(
            goal="テスト",
            execution_plan={
                "steps": [
                    {"capability": "flight_watch", "label": "検索", "inputs": {"q": "NRT"}},
                ],
            },
        )

        plan = convert_app_plan_to_execution_plan(app_plan)

        assert plan.steps[0].agent_id == "flight_watch"
        assert plan.steps[0].description == "検索"
        assert plan.steps[0].input_spec == {"q": "NRT"}

    def test_auto_step_ids(self) -> None:
        """step_id がない場合は自動採番."""
        app_plan = _FakeAppPlan(
            goal="テスト",
            execution_plan={
                "steps": [
                    {"agent_id": "a1", "description": "d1"},
                    {"agent_id": "a2", "description": "d2"},
                ],
            },
        )

        plan = convert_app_plan_to_execution_plan(app_plan)

        assert plan.steps[0].step_id == "step-1"
        assert plan.steps[1].step_id == "step-2"


# === AppOrchestrationAdapter テスト ===


class TestAppOrchestrationAdapter:
    """AppOrchestrationAdapter のテスト."""

    def test_instantiation(self) -> None:
        """基本的なインスタンス化."""
        adapter = AppOrchestrationAdapter(
            agent_map={"a1": _MockAgent("a1")},
        )
        assert adapter is not None

    @pytest.mark.asyncio
    async def test_execute_from_app_plan(self) -> None:
        """App 計画からフロー実行."""
        agents = {
            "flight_search": _MockAgent("flight_search"),
            "flight_ranking": _MockAgent("flight_ranking"),
        }
        adapter = AppOrchestrationAdapter(agent_map=agents)

        app_plan = _FakeAppPlan(
            goal="NRT→LAX 検索",
            blueprint_id="structured_monitoring.flight_watch",
            task_kind="structured_monitoring",
            execution_plan={
                "steps": [
                    {
                        "step_id": "s1",
                        "agent_id": "flight_search",
                        "description": "検索",
                        "risk_level": "low",
                    },
                    {
                        "step_id": "s2",
                        "agent_id": "flight_ranking",
                        "description": "ランキング",
                        "risk_level": "low",
                        "dependencies": ["s1"],
                    },
                ],
            },
        )

        result = await adapter.execute_from_app_plan(app_plan)

        assert result["status"] == "completed"
        assert "plan" in result
        assert "run_id" in result

    @pytest.mark.asyncio
    async def test_execute_from_app_plan_single_step(self) -> None:
        """単一ステップの計画."""
        adapter = AppOrchestrationAdapter(
            agent_map={"analyzer": _MockAgent("analyzer")},
        )

        app_plan = _FakeAppPlan(
            goal="分析",
            execution_plan={
                "steps": [
                    {"step_id": "s1", "agent_id": "analyzer", "description": "分析実行"},
                ],
            },
        )

        result = await adapter.execute_from_app_plan(app_plan)

        assert result["status"] == "completed"
