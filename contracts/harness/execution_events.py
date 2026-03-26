"""Harness が消費する実行イベントの型定義."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Any


class ExecutionEventType(Enum):
    """実行イベントの種別."""

    APPROVAL_REQUIRED = "approval_required"
    STEP_COMPLETED = "step_completed"
    EXECUTION_FAILED = "execution_failed"


@dataclass(frozen=True)
class ExecutionEvent:
    """実行イベントの基底.

    注意: frozen dataclass 継承で子クラスがデフォルト値を持つ場合、
    親フィールドもデフォルト値が必要。
    """

    event_type: ExecutionEventType = ExecutionEventType.STEP_COMPLETED
    trace_id: str = ""
    payload: dict[str, Any] | None = None


@dataclass(frozen=True)
class ApprovalRequiredEvent(ExecutionEvent):
    """承認要求イベント."""

    event_type: ExecutionEventType = ExecutionEventType.APPROVAL_REQUIRED
    approver: str = ""
    reason: str = ""
