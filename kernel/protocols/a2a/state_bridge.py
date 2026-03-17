"""AgentFlow TaskState ↔ A2A TaskState 双方向マッピング.

AgentFlow は 11 状態、A2A は 9 状態を持つ。
情報欠損を防ぐため、マッピング時に元状態を metadata に保持する。
"""

from __future__ import annotations

import functools
from typing import TYPE_CHECKING, Any

from kernel.protocols.a2a.types import A2ATaskState

if TYPE_CHECKING:
    from kernel.state.task_state import TaskState


@functools.lru_cache(maxsize=1)
def _af_to_a2a() -> dict[TaskState, A2ATaskState]:
    """AgentFlow → A2A マッピング（遅延ビルド）."""
    from kernel.state.task_state import TaskState  # noqa: PLC0415

    return {
        TaskState.CREATED: A2ATaskState.SUBMITTED,
        TaskState.PLANNED: A2ATaskState.SUBMITTED,
        TaskState.RUNNING: A2ATaskState.WORKING,
        TaskState.VERIFIED: A2ATaskState.WORKING,
        TaskState.DONE: A2ATaskState.COMPLETED,
        TaskState.FAILED: A2ATaskState.FAILED,
        TaskState.PAUSED: A2ATaskState.WORKING,
        TaskState.WAITING_INPUT: A2ATaskState.INPUT_REQUIRED,
        TaskState.BLOCKED: A2ATaskState.WORKING,
        TaskState.REPLANNING: A2ATaskState.WORKING,
        TaskState.CANCELLED: A2ATaskState.CANCELED,
    }


@functools.lru_cache(maxsize=1)
def _lossy_states() -> set[TaskState]:
    """1:1 マッピングできない状態（metadata に元状態を保持する必要あり）."""
    from kernel.state.task_state import TaskState  # noqa: PLC0415

    return {
        TaskState.VERIFIED,
        TaskState.PAUSED,
        TaskState.BLOCKED,
        TaskState.REPLANNING,
        TaskState.PLANNED,
    }


@functools.lru_cache(maxsize=1)
def _a2a_to_af() -> dict[A2ATaskState, TaskState]:
    """A2A → AgentFlow マッピング（デフォルト・遅延ビルド）."""
    from kernel.state.task_state import TaskState  # noqa: PLC0415

    return {
        A2ATaskState.SUBMITTED: TaskState.CREATED,
        A2ATaskState.WORKING: TaskState.RUNNING,
        A2ATaskState.INPUT_REQUIRED: TaskState.WAITING_INPUT,
        A2ATaskState.COMPLETED: TaskState.DONE,
        A2ATaskState.CANCELED: TaskState.CANCELLED,
        A2ATaskState.FAILED: TaskState.FAILED,
        A2ATaskState.REJECTED: TaskState.FAILED,
        A2ATaskState.AUTH_REQUIRED: TaskState.WAITING_INPUT,
        A2ATaskState.UNKNOWN: TaskState.CREATED,
    }


def agentflow_to_a2a_state(
    state: TaskState,
) -> tuple[A2ATaskState, dict[str, Any]]:
    """AgentFlow TaskState → A2A TaskState に変換.

    情報欠損がある場合は metadata に元状態を保持する。

    Args:
        state: AgentFlow タスク状態

    Returns:
        (A2A タスク状態, 元状態を保持する metadata)
    """
    a2a_state = _af_to_a2a().get(state, A2ATaskState.UNKNOWN)
    metadata: dict[str, Any] = {}

    if state in _lossy_states():
        metadata["agentflow_state"] = state.value

    return a2a_state, metadata


def a2a_to_agentflow_state(
    state: A2ATaskState,
    metadata: dict[str, Any] | None = None,
) -> TaskState:
    """A2A TaskState → AgentFlow TaskState に変換.

    metadata に agentflow_state が保持されている場合はそちらを優先する。

    Args:
        state: A2A タスク状態
        metadata: タスクメタデータ（agentflow_state を含む可能性あり）

    Returns:
        AgentFlow タスク状態
    """
    # metadata に元状態が保持されている場合は復元
    if metadata:
        original = metadata.get("agentflow_state")
        if original:
            from kernel.state.task_state import TaskState as _TS  # noqa: PLC0415

            try:
                return _TS(original)
            except ValueError:
                pass

    from kernel.state.task_state import TaskState as _TS2  # noqa: PLC0415

    return _a2a_to_af().get(state, _TS2.CREATED)
