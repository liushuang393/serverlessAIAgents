"""PlannerAgent のユニットテスト."""

from __future__ import annotations

import json
from typing import Any

import pytest

from harness.orchestration.models import ExecutionPlan, PlanStep, ReplanRequest
from harness.orchestration.planner import (
    PlannerAgent,
    _build_user_prompt,
    _estimate_step_risk,
    _parse_plan_json,
)
from harness.orchestration.models import PlannerInput
from harness.risk.service import RiskLevel


class TestEstimateStepRisk:
    """リスク推定ヒューリスティックのテスト."""

    def test_low_risk_default(self) -> None:
        step = PlanStep(step_id="s1", agent_id="a1", description="データを分析する")
        assert _estimate_step_risk(step) == RiskLevel.LOW

    def test_high_risk_delete(self) -> None:
        step = PlanStep(step_id="s1", agent_id="a1", description="古いレコードをdelete")
        assert _estimate_step_risk(step) == RiskLevel.HIGH

    def test_high_risk_japanese(self) -> None:
        step = PlanStep(step_id="s1", agent_id="a1", description="不要なファイルを削除する")
        assert _estimate_step_risk(step) == RiskLevel.HIGH

    def test_medium_risk_update(self) -> None:
        step = PlanStep(step_id="s1", agent_id="a1", description="データベースをupdate")
        assert _estimate_step_risk(step) == RiskLevel.MEDIUM

    def test_explicit_risk_preserved(self) -> None:
        """LLM が明示的に指定した CRITICAL は上書きしない."""
        step = PlanStep(
            step_id="s1", agent_id="a1",
            description="安全な読み取り",
            risk_level=RiskLevel.CRITICAL,
        )
        assert _estimate_step_risk(step) == RiskLevel.CRITICAL


class TestParsePlanJson:
    """LLM 出力 JSON パースのテスト."""

    def test_valid_json(self) -> None:
        raw = json.dumps({
            "steps": [
                {
                    "step_id": "s1",
                    "agent_id": "analyzer",
                    "description": "データ分析",
                    "input_spec": {"key": "value"},
                    "dependencies": [],
                    "risk_level": "low",
                },
                {
                    "step_id": "s2",
                    "agent_id": "reporter",
                    "description": "レポート生成",
                    "dependencies": ["s1"],
                    "risk_level": "medium",
                },
            ],
            "reasoning": "分析→レポートの順序で実行",
        })

        plan = _parse_plan_json(raw, goal="テスト")

        assert plan.goal == "テスト"
        assert len(plan.steps) == 2
        assert plan.steps[0].step_id == "s1"
        assert plan.steps[1].dependencies == ["s1"]
        assert plan.overall_risk == RiskLevel.MEDIUM

    def test_json_in_code_block(self) -> None:
        raw = '```json\n{"steps": [{"step_id": "s1", "agent_id": "a1", "description": "d"}], "reasoning": ""}\n```'
        plan = _parse_plan_json(raw, goal="テスト")
        assert len(plan.steps) == 1

    def test_invalid_json_raises(self) -> None:
        with pytest.raises(ValueError, match="JSON パース"):
            _parse_plan_json("not json", goal="テスト")

    def test_steps_not_list_raises(self) -> None:
        with pytest.raises(ValueError, match="リストではありません"):
            _parse_plan_json('{"steps": "not a list"}', goal="テスト")

    def test_auto_step_id(self) -> None:
        raw = json.dumps({
            "steps": [{"agent_id": "a1", "description": "d"}],
        })
        plan = _parse_plan_json(raw, goal="テスト")
        assert plan.steps[0].step_id == "step-1"

    def test_risk_heuristic_applied(self) -> None:
        """risk_level=low でも description にキーワードがあれば上書き."""
        raw = json.dumps({
            "steps": [
                {"step_id": "s1", "agent_id": "a1", "description": "データを削除", "risk_level": "low"},
            ],
        })
        plan = _parse_plan_json(raw, goal="テスト")
        assert plan.steps[0].risk_level == RiskLevel.HIGH


class TestBuildUserPrompt:
    """プロンプト構築のテスト."""

    def test_minimal(self) -> None:
        inp = PlannerInput(user_request="分析して")
        prompt = _build_user_prompt(inp)
        assert "分析して" in prompt

    def test_with_agents_and_constraints(self) -> None:
        inp = PlannerInput(
            user_request="テスト",
            available_agents=["agent1", "agent2"],
            constraints=["5分以内"],
            context={"key": "value"},
        )
        prompt = _build_user_prompt(inp)
        assert "agent1" in prompt
        assert "5分以内" in prompt
        assert "key" in prompt


class TestPlannerValidation:
    """PlannerAgent._validate_plan のテスト."""

    @pytest.fixture
    def agent(self) -> PlannerAgent:
        return PlannerAgent()

    def test_empty_plan_warns(self, agent: PlannerAgent) -> None:
        plan = ExecutionPlan(goal="テスト")
        warnings = agent._validate_plan(plan, [])
        assert any("ステップが含まれていません" in w for w in warnings)

    def test_missing_dependency_warns(self, agent: PlannerAgent) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1", dependencies=["nonexistent"]),
        ]
        plan = ExecutionPlan(goal="テスト", steps=steps)
        warnings = agent._validate_plan(plan, [])
        assert any("存在しません" in w for w in warnings)

    def test_unknown_agent_warns(self, agent: PlannerAgent) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="unknown_agent", description="d1"),
        ]
        plan = ExecutionPlan(goal="テスト", steps=steps)
        warnings = agent._validate_plan(plan, ["known_agent"])
        assert any("利用可能リスト" in w for w in warnings)

    def test_cycle_warns(self, agent: PlannerAgent) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1", dependencies=["s2"]),
            PlanStep(step_id="s2", agent_id="a2", description="d2", dependencies=["s1"]),
        ]
        plan = ExecutionPlan(goal="テスト", steps=steps)
        warnings = agent._validate_plan(plan, [])
        assert any("循環依存" in w for w in warnings)

    def test_valid_plan_no_warnings(self, agent: PlannerAgent) -> None:
        steps = [
            PlanStep(step_id="s1", agent_id="a1", description="d1"),
            PlanStep(step_id="s2", agent_id="a2", description="d2", dependencies=["s1"]),
        ]
        plan = ExecutionPlan(goal="テスト", steps=steps)
        warnings = agent._validate_plan(plan, ["a1", "a2"])
        assert warnings == []
