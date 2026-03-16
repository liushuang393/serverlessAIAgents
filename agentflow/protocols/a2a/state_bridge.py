"""AgentFlow TaskState ↔ A2A TaskState 双方向マッピング.

AgentFlow は 11 状態、A2A は 9 状態を持つ。
情報欠損を防ぐため、マッピング時に元状態を metadata に保持する。
"""

from __future__ import annotations

from typing import Any

from agentflow.protocols.a2a.types import A2ATaskState
from agentflow.task.task_state import TaskState


# AgentFlow → A2A マッピング
_AF_TO_A2A: dict[TaskState, A2ATaskState] = {
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

# 1:1 マッピングできない状態（metadata に元状態を保持する必要あり）
_LOSSY_STATES: set[TaskState] = {
    TaskState.VERIFIED,
    TaskState.PAUSED,
    TaskState.BLOCKED,
    TaskState.REPLANNING,
    TaskState.PLANNED,
}

# A2A → AgentFlow マッピング（デフォルト）
_A2A_TO_AF: dict[A2ATaskState, TaskState] = {
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
    a2a_state = _AF_TO_A2A.get(state, A2ATaskState.UNKNOWN)
    metadata: dict[str, Any] = {}

    if state in _LOSSY_STATES:
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
            try:
                return TaskState(original)
            except ValueError:
                pass

    return _A2A_TO_AF.get(state, TaskState.CREATED)
