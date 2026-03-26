"""Harness 用 contracts — 認証・イベント型の抽象定義."""

from contracts.harness.auth_service import AuthServiceProtocol
from contracts.harness.execution_events import (
    ApprovalRequiredEvent,
    ExecutionEvent,
    ExecutionEventType,
)


__all__ = [
    "ApprovalRequiredEvent",
    "AuthServiceProtocol",
    "ExecutionEvent",
    "ExecutionEventType",
]
