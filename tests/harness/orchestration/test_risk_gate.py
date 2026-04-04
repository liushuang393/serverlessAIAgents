"""RiskGateMiddleware のユニットテスト."""

from __future__ import annotations

import pytest

from contracts.flow.contracts import MiddlewareDecision
from harness.governance.audit import AuditEvent, AuditLogger
from harness.orchestration.models import ExecutionPlan, PlanStep
from harness.orchestration.risk_gate import RiskGateMiddleware
from harness.risk.service import RiskLevel


class _SpyAuditLogger(AuditLogger):
    def __init__(self) -> None:
        self.events: list[AuditEvent] = []

    def log_event(self, event: AuditEvent) -> None:
        self.events.append(event)


def _make_plan(*steps: tuple[str, RiskLevel]) -> ExecutionPlan:
    """テスト用計画を生成."""
    plan_steps = [
        PlanStep(step_id=sid, agent_id=f"agent_{sid}", description=f"Step {sid}", risk_level=risk)
        for sid, risk in steps
    ]
    return ExecutionPlan(goal="テスト", steps=plan_steps)


@pytest.fixture
def spy_logger() -> _SpyAuditLogger:
    return _SpyAuditLogger()


class TestRiskGateLow:
    """LOW リスクのテスト."""

    @pytest.mark.asyncio
    async def test_low_risk_allows(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan(("s1", RiskLevel.LOW))
        mw = RiskGateMiddleware(plan, spy_logger)

        result = await mw.before_node("s1", "agent_s1", {"input": "data"})

        assert result.decision == MiddlewareDecision.ALLOW
        assert result.metadata.get("risk_level") == "low"
        assert len(spy_logger.events) == 1


class TestRiskGateMedium:
    """MEDIUM リスクのテスト."""

    @pytest.mark.asyncio
    async def test_medium_risk_allows_with_enhanced_logging(
        self, spy_logger: _SpyAuditLogger,
    ) -> None:
        plan = _make_plan(("s1", RiskLevel.MEDIUM))
        mw = RiskGateMiddleware(plan, spy_logger)

        result = await mw.before_node("s1", "agent_s1", {})

        assert result.decision == MiddlewareDecision.ALLOW
        assert result.metadata.get("enhanced_logging") is True


class TestRiskGateHigh:
    """HIGH リスクのテスト."""

    @pytest.mark.asyncio
    async def test_high_risk_requires_approval_no_manager(
        self, spy_logger: _SpyAuditLogger,
    ) -> None:
        """ApprovalManager 未設定の場合は APPROVAL_REQUIRED を返す."""
        plan = _make_plan(("s1", RiskLevel.HIGH))
        mw = RiskGateMiddleware(plan, spy_logger)

        result = await mw.before_node("s1", "agent_s1", {})

        assert result.decision == MiddlewareDecision.APPROVAL_REQUIRED
        assert "HIGH" in result.reason


class TestRiskGateCritical:
    """CRITICAL リスクのテスト."""

    @pytest.mark.asyncio
    async def test_critical_risk_denies(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan(("s1", RiskLevel.CRITICAL))
        mw = RiskGateMiddleware(plan, spy_logger)

        result = await mw.before_node("s1", "agent_s1", {})

        assert result.decision == MiddlewareDecision.DENY
        assert "CRITICAL" in result.reason

    @pytest.mark.asyncio
    async def test_critical_emits_audit(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan(("s1", RiskLevel.CRITICAL))
        mw = RiskGateMiddleware(plan, spy_logger)

        await mw.before_node("s1", "agent_s1", {})

        assert len(spy_logger.events) == 1
        assert spy_logger.events[0].decision == "deny"
        assert spy_logger.events[0].risk_level == "critical"


class TestRiskGateUnknownNode:
    """計画外ノードのテスト."""

    @pytest.mark.asyncio
    async def test_unknown_node_allows(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan(("s1", RiskLevel.LOW))
        mw = RiskGateMiddleware(plan, spy_logger)

        result = await mw.before_node("review_1", "ReviewAgent", {})

        assert result.decision == MiddlewareDecision.ALLOW


class TestRiskGateAfterNode:
    """after_node のテスト."""

    @pytest.mark.asyncio
    async def test_after_node_always_allows(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan(("s1", RiskLevel.HIGH))
        mw = RiskGateMiddleware(plan, spy_logger)

        result = await mw.after_node("s1", "agent_s1", {"result": "ok"}, success=True)

        assert result.decision == MiddlewareDecision.ALLOW

    @pytest.mark.asyncio
    async def test_after_node_emits_audit(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan(("s1", RiskLevel.MEDIUM))
        mw = RiskGateMiddleware(plan, spy_logger)

        await mw.after_node("s1", "agent_s1", {}, success=False)

        assert len(spy_logger.events) == 1
        assert spy_logger.events[0].decision == "node_result"
