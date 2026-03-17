"""agentflow.task.task_state 後方互換shim. 実体は kernel.state.task_state."""

from kernel.state.task_state import (  # noqa: F401
    TASK_TRANSITIONS,
    TaskState,
    can_transition,
    is_terminal,
)

__all__ = ["TASK_TRANSITIONS", "TaskState", "can_transition", "is_terminal"]
