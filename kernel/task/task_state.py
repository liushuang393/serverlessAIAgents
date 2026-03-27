"""kernel.task.task_state — kernel.state.task_state への再エクスポート."""

from kernel.state.task_state import (
    TASK_TRANSITIONS,
    TaskState,
    can_transition,
    is_terminal,
)


__all__ = ["TASK_TRANSITIONS", "TaskState", "can_transition", "is_terminal"]
