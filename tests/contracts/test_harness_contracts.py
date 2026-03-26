"""Harness contracts のテスト."""


def test_auth_service_protocol_exists() -> None:
    from contracts.harness.auth_service import AuthServiceProtocol
    assert hasattr(AuthServiceProtocol, "authenticate")
    assert hasattr(AuthServiceProtocol, "authorize")


def test_execution_event_types_exist() -> None:
    from contracts.harness.execution_events import (
        ApprovalRequiredEvent,
        ExecutionEvent,
        ExecutionEventType,
    )
    assert ExecutionEventType.APPROVAL_REQUIRED.value == "approval_required"
    event = ApprovalRequiredEvent(approver="admin", reason="high-risk")
    assert event.approver == "admin"
    assert event.event_type == ExecutionEventType.APPROVAL_REQUIRED
