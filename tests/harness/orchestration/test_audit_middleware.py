"""AuditMiddleware のユニットテスト."""

from __future__ import annotations

import pytest

from contracts.flow.contracts import MiddlewareDecision
from harness.governance.audit import AuditEvent, AuditLogger
from harness.orchestration.audit_middleware import AuditMiddleware


class _SpyAuditLogger(AuditLogger):
    """テスト用: log_event 呼び出しを記録する監査ロガー."""

    def __init__(self) -> None:
        self.events: list[AuditEvent] = []

    def log_event(self, event: AuditEvent) -> None:
        self.events.append(event)


@pytest.fixture
def spy_logger() -> _SpyAuditLogger:
    return _SpyAuditLogger()


@pytest.fixture
def middleware(spy_logger: _SpyAuditLogger) -> AuditMiddleware:
    return AuditMiddleware(spy_logger, flow_id="test-flow", run_id="test-run")


@pytest.mark.asyncio
async def test_before_node_emits_start_event(
    middleware: AuditMiddleware,
    spy_logger: _SpyAuditLogger,
) -> None:
    result = await middleware.before_node("n1", "TestNode", {"key": "value"})

    assert result.decision == MiddlewareDecision.ALLOW
    assert len(spy_logger.events) == 1

    event = spy_logger.events[0]
    assert event.operation_type == "node_start"
    assert event.tool_name == "TestNode"
    assert event.flow_id == "test-flow"
    assert event.run_id == "test-run"
    assert "n1" in str(event.metadata)


@pytest.mark.asyncio
async def test_after_node_success_emits_complete(
    middleware: AuditMiddleware,
    spy_logger: _SpyAuditLogger,
) -> None:
    # before を先に呼んでタイマー開始
    await middleware.before_node("n1", "TestNode", {})
    spy_logger.events.clear()

    result = await middleware.after_node("n1", "TestNode", {"out": 1}, success=True)

    assert result.decision == MiddlewareDecision.ALLOW
    assert len(spy_logger.events) == 1

    event = spy_logger.events[0]
    assert event.operation_type == "node_complete"
    assert event.metadata["success"] is True
    assert event.metadata["elapsed_ms"] >= 0


@pytest.mark.asyncio
async def test_after_node_failure_emits_error(
    middleware: AuditMiddleware,
    spy_logger: _SpyAuditLogger,
) -> None:
    result = await middleware.after_node("n2", "FailNode", {}, success=False)

    assert result.decision == MiddlewareDecision.ALLOW
    assert len(spy_logger.events) == 1

    event = spy_logger.events[0]
    assert event.operation_type == "node_error"
    assert event.metadata["success"] is False


@pytest.mark.asyncio
async def test_always_returns_allow(
    middleware: AuditMiddleware,
) -> None:
    """監査ミドルウェアは判定に関与せず常に ALLOW を返す."""
    r1 = await middleware.before_node("x", "X", {})
    r2 = await middleware.after_node("x", "X", {}, success=False)
    assert r1.decision == MiddlewareDecision.ALLOW
    assert r2.decision == MiddlewareDecision.ALLOW
