"""Agent OS task lifecycle states and transitions.

The task state machine defines all valid states a task can be in
and the valid transitions between them.
"""

from enum import Enum


class TaskState(str, Enum):
    """Agent OS task lifecycle states.

    States follow a progression from creation to completion:
    - CREATED: Task has been defined but not yet planned
    - PLANNED: Task has a plan and is ready for execution
    - RUNNING: Task is currently being executed
    - VERIFIED: Task execution is complete and verified
    - DONE: Task is fully complete
    - FAILED: Task has failed and cannot continue
    - PAUSED: Task is temporarily suspended
    - WAITING_INPUT: Task is waiting for external input
    - BLOCKED: Task is blocked by dependencies
    - REPLANNING: Task is being replanned after verification failure
    - CANCELLED: Task has been cancelled
    """

    CREATED = "created"
    PLANNED = "planned"
    RUNNING = "running"
    VERIFIED = "verified"
    DONE = "done"
    FAILED = "failed"
    PAUSED = "paused"
    WAITING_INPUT = "waiting_input"
    BLOCKED = "blocked"
    REPLANNING = "replanning"
    CANCELLED = "cancelled"


TASK_TRANSITIONS: dict[TaskState, set[TaskState]] = {
    TaskState.CREATED: {TaskState.PLANNED, TaskState.CANCELLED},
    TaskState.PLANNED: {TaskState.RUNNING, TaskState.BLOCKED, TaskState.CANCELLED},
    TaskState.RUNNING: {
        TaskState.VERIFIED,
        TaskState.FAILED,
        TaskState.PAUSED,
        TaskState.WAITING_INPUT,
    },
    TaskState.VERIFIED: {TaskState.DONE, TaskState.REPLANNING},
    TaskState.PAUSED: {TaskState.RUNNING, TaskState.CANCELLED},
    TaskState.WAITING_INPUT: {TaskState.RUNNING, TaskState.CANCELLED},
    TaskState.BLOCKED: {TaskState.PLANNED, TaskState.CANCELLED},
    TaskState.REPLANNING: {TaskState.PLANNED, TaskState.FAILED},
    TaskState.DONE: set(),
    TaskState.FAILED: set(),
    TaskState.CANCELLED: set(),
}
"""Valid state transitions for tasks.

Key: current state
Value: set of valid next states
"""


def can_transition(from_state: TaskState, to_state: TaskState) -> bool:
    """Check if a state transition is valid.

    Args:
        from_state: The current state
        to_state: The desired next state

    Returns:
        True if the transition is valid, False otherwise
    """
    return to_state in TASK_TRANSITIONS.get(from_state, set())


def is_terminal(state: TaskState) -> bool:
    """Check if a state is terminal (no further transitions possible).

    Args:
        state: The state to check

    Returns:
        True if the state is terminal
    """
    return state in {TaskState.DONE, TaskState.FAILED, TaskState.CANCELLED}
