"""AutonomousPipeline の統合テスト."""

from __future__ import annotations

import json
from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from harness.governance.audit import AuditEvent, AuditLogger
from harness.guardrails.pipeline import (
    Guardrail,
    GuardrailPipeline,
    GuardrailResult,
    PipelineResult,
    Severity,
)
from harness.orchestration.dynamic_flow import DynamicFlowGenerator
from harness.orchestration.models import ExecutionPlan, PlanStep, PlannerOutput
from harness.orchestration.pipeline import AutonomousPipeline
from harness.orchestration.planner import PlannerAgent
from harness.risk.service import RiskLevel


class _SpyAuditLogger(AuditLogger):
    def __init__(self) -> None:
        self.events: list[AuditEvent] = []

    def log_event(self, event: AuditEvent) -> None:
        self.events.append(event)


class _MockAgent:
    """テスト用モックエージェント."""

    def __init__(self, name: str = "mock", output: dict[str, Any] | None = None) -> None:
        self.name = name
        self._output = output or {"result": f"{name}_output"}

    async def initialize(self) -> None:
        pass

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return dict(self._output)

    async def cleanup(self) -> None:
        pass


class _AlwaysPassGuardrail:
    """テスト用: 常にパスするガードレール."""

    @property
    def name(self) -> str:
        return "AlwaysPass"

    async def check(
        self,
        input_data: dict[str, Any],
        output_data: dict[str, Any] | None = None,
    ) -> GuardrailResult:
        return GuardrailResult(passed=True, guardrail_name=self.name)


class _AlwaysFailGuardrail:
    """テスト用: 常に CRITICAL 失敗するガードレール."""

    @property
    def name(self) -> str:
        return "AlwaysFail"

    async def check(
        self,
        input_data: dict[str, Any],
        output_data: dict[str, Any] | None = None,
    ) -> GuardrailResult:
        return GuardrailResult(
            passed=False,
            guardrail_name=self.name,
            reason="テスト拒否",
            severity=Severity.CRITICAL,
        )


def _make_plan_output() -> PlannerOutput:
    """テスト用の PlannerOutput を生成."""
    plan = ExecutionPlan(
        goal="テスト分析",
        steps=[
            PlanStep(step_id="s1", agent_id="analyzer", description="データ分析"),
            PlanStep(
                step_id="s2", agent_id="reporter", description="レポート生成",
                dependencies=["s1"],
            ),
        ],
    )
    return PlannerOutput(plan=plan, reasoning="テスト", warnings=[])


@pytest.fixture
def spy_logger() -> _SpyAuditLogger:
    return _SpyAuditLogger()


@pytest.fixture
def agent_map() -> dict[str, _MockAgent]:
    return {
        "analyzer": _MockAgent("analyzer", {"analysis": "done"}),
        "reporter": _MockAgent("reporter", {"report": "done"}),
    }


class TestPipelinePreCheckRejection:
    """ガードレール事前チェックで拒否されるケース."""

    @pytest.mark.asyncio
    async def test_critical_guardrail_rejects(
        self,
        spy_logger: _SpyAuditLogger,
        agent_map: dict[str, _MockAgent],
    ) -> None:
        guardrails = GuardrailPipeline([_AlwaysFailGuardrail()])

        planner = MagicMock(spec=PlannerAgent)
        gen = DynamicFlowGenerator(agent_map=agent_map)

        pipeline = AutonomousPipeline(
            planner=planner,
            flow_generator=gen,
            guardrail_pipeline=guardrails,
            audit_logger=spy_logger,
        )

        result = await pipeline.execute("危険なリクエスト")

        assert result["status"] == "rejected"
        assert "ガードレール" in result["reason"]
        # planner は呼ばれない
        planner.run.assert_not_called()


class TestPipelineAuditTrail:
    """監査トレイルのテスト."""

    @pytest.mark.asyncio
    async def test_audit_events_emitted(
        self,
        spy_logger: _SpyAuditLogger,
        agent_map: dict[str, _MockAgent],
    ) -> None:
        guardrails = GuardrailPipeline([_AlwaysFailGuardrail()])

        planner = MagicMock(spec=PlannerAgent)
        gen = DynamicFlowGenerator(agent_map=agent_map)

        pipeline = AutonomousPipeline(
            planner=planner,
            flow_generator=gen,
            guardrail_pipeline=guardrails,
            audit_logger=spy_logger,
        )

        await pipeline.execute("テスト")

        # パイプライン開始 + 拒否 の最低2イベント
        assert len(spy_logger.events) >= 2
        decisions = [e.decision for e in spy_logger.events]
        assert "pipeline_start" in decisions
        assert "pipeline_rejected" in decisions
