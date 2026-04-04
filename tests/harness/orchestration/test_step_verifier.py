"""StepVerifierMiddleware のユニットテスト."""

from __future__ import annotations

import pytest

from contracts.flow.contracts import MiddlewareDecision
from harness.governance.audit import AuditEvent, AuditLogger
from harness.orchestration.models import ExecutionPlan, PlanStep
from harness.orchestration.step_verifier import StepVerifierMiddleware
from harness.risk.service import RiskLevel
from kernel.agents.dual_verifier import DualVerifier, VerifyResult, VerifyStatus, VerifyType


class _SpyAuditLogger(AuditLogger):
    def __init__(self) -> None:
        self.events: list[AuditEvent] = []

    def log_event(self, event: AuditEvent) -> None:
        self.events.append(event)


class _FixedVerifyStrategy:
    """テスト用: 固定結果を返す検証戦略."""

    def __init__(self, status: VerifyStatus, message: str = "") -> None:
        self._status = status
        self._message = message

    async def verify(
        self,
        data: dict[str, object],
        context: dict[str, object],
    ) -> VerifyResult:
        return VerifyResult(
            status=self._status,
            verify_type=VerifyType.SCHEMA,
            message=self._message,
        )


def _make_plan() -> ExecutionPlan:
    return ExecutionPlan(
        goal="テスト",
        steps=[PlanStep(step_id="s1", agent_id="a1", description="Step 1")],
    )


def _make_verifier(status: VerifyStatus, message: str = "") -> DualVerifier:
    v = DualVerifier()
    v.add_strategy(_FixedVerifyStrategy(status, message))
    return v


@pytest.fixture
def spy_logger() -> _SpyAuditLogger:
    return _SpyAuditLogger()


class TestVerificationPass:
    """PASS 検証のテスト."""

    @pytest.mark.asyncio
    async def test_pass_allows(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan()
        verifier = _make_verifier(VerifyStatus.PASS, "OK")
        mw = StepVerifierMiddleware(plan, verifier, spy_logger)

        result = await mw.after_node("s1", "a1", {"out": 1}, success=True)

        assert result.decision == MiddlewareDecision.ALLOW
        assert len(spy_logger.events) == 1
        assert spy_logger.events[0].decision == "verification_passed"


class TestVerificationWarning:
    """WARNING 検証のテスト."""

    @pytest.mark.asyncio
    async def test_warning_allows_with_metadata(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan()
        verifier = _make_verifier(VerifyStatus.WARNING, "要注意")
        mw = StepVerifierMiddleware(plan, verifier, spy_logger)

        result = await mw.after_node("s1", "a1", {"out": 1}, success=True)

        assert result.decision == MiddlewareDecision.ALLOW
        assert "verification_warning" in str(result.metadata)


class TestVerificationFail:
    """FAIL 検証のテスト."""

    @pytest.mark.asyncio
    async def test_fail_denies_with_retry(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan()
        verifier = _make_verifier(VerifyStatus.FAIL, "スキーマ不一致")
        mw = StepVerifierMiddleware(plan, verifier, spy_logger, max_retries=2)

        result = await mw.after_node("s1", "a1", {"bad": True}, success=True)

        assert result.decision == MiddlewareDecision.DENY
        assert result.metadata.get("retry_requested") is True
        assert result.metadata.get("retry_count") == 1

    @pytest.mark.asyncio
    async def test_fail_retry_count_increments(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan()
        verifier = _make_verifier(VerifyStatus.FAIL, "エラー")
        mw = StepVerifierMiddleware(plan, verifier, spy_logger, max_retries=2)

        # 1回目
        r1 = await mw.after_node("s1", "a1", {}, success=True)
        assert r1.metadata.get("retry_count") == 1

        # 2回目
        r2 = await mw.after_node("s1", "a1", {}, success=True)
        assert r2.metadata.get("retry_count") == 2

    @pytest.mark.asyncio
    async def test_fail_exceeds_max_retries_requests_replan(
        self, spy_logger: _SpyAuditLogger,
    ) -> None:
        plan = _make_plan()
        verifier = _make_verifier(VerifyStatus.FAIL, "致命的エラー")
        mw = StepVerifierMiddleware(plan, verifier, spy_logger, max_retries=1)

        # 1回目: リトライ
        await mw.after_node("s1", "a1", {}, success=True)

        # 2回目: 上限超過 → 再計画要求
        result = await mw.after_node("s1", "a1", {}, success=True)

        assert result.decision == MiddlewareDecision.DENY
        assert result.metadata.get("replan_requested") is True
        assert "failure_reason" in result.metadata


class TestVerificationNeedHuman:
    """NEED_HUMAN 検証のテスト."""

    @pytest.mark.asyncio
    async def test_need_human_requires_approval(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan()
        verifier = _make_verifier(VerifyStatus.NEED_HUMAN, "危険操作検出")
        mw = StepVerifierMiddleware(plan, verifier, spy_logger)

        result = await mw.after_node("s1", "a1", {"sql": "DROP TABLE"}, success=True)

        assert result.decision == MiddlewareDecision.APPROVAL_REQUIRED
        assert "人間確認" in result.reason


class TestBeforeNode:
    """before_node のテスト."""

    @pytest.mark.asyncio
    async def test_before_always_allows(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan()
        verifier = _make_verifier(VerifyStatus.PASS)
        mw = StepVerifierMiddleware(plan, verifier, spy_logger)

        result = await mw.before_node("s1", "a1", {})

        assert result.decision == MiddlewareDecision.ALLOW


class TestExecutionFailure:
    """ノード実行失敗時のテスト."""

    @pytest.mark.asyncio
    async def test_failure_skips_verification(self, spy_logger: _SpyAuditLogger) -> None:
        plan = _make_plan()
        verifier = _make_verifier(VerifyStatus.FAIL, "エラー")
        mw = StepVerifierMiddleware(plan, verifier, spy_logger)

        result = await mw.after_node("s1", "a1", {}, success=False)

        assert result.decision == MiddlewareDecision.ALLOW
        assert spy_logger.events[0].decision == "skip_verification"
