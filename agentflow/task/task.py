"""Task entity with validated state machine for Agent OS."""

from datetime import datetime
from typing import Any

from pydantic import BaseModel, Field

from agentflow.task.task_id import TaskID
from agentflow.task.task_state import TaskState, can_transition


class Task(BaseModel):
    """Task entity with validated state machine.

    A Task represents a unit of work in the Agent OS. It tracks:
    - Identity: id, name, description
    - Hierarchy: parent_id, children_ids
    - Dependencies: blocks, blocked_by
    - Lifecycle: state, state_history, timestamps
    - Data: inputs, outputs, error

    State transitions are validated through the transition_to() method.
    """

    id: TaskID
    name: str
    description: str = ""
    parent_id: TaskID | None = None
    children_ids: list[TaskID] = Field(default_factory=list)
    blocks: list[TaskID] = Field(default_factory=list)
    blocked_by: list[TaskID] = Field(default_factory=list)
    state: TaskState = TaskState.CREATED
    state_history: list[tuple[TaskState, datetime]] = Field(default_factory=list)
    inputs: dict[str, Any] = Field(default_factory=dict)
    outputs: dict[str, Any] = Field(default_factory=dict)
    error: str | None = None
    created_at: datetime = Field(default_factory=datetime.now)
    started_at: datetime | None = None
    completed_at: datetime | None = None

    model_config = {"arbitrary_types_allowed": True}

    def transition_to(self, new_state: TaskState) -> bool:
        """Transition the task to a new state.

        Validates that the transition is allowed according to TASK_TRANSITIONS.
        Records the previous state in state_history and updates timestamps.

        Args:
            new_state: The desired new state

        Returns:
            True if transition succeeded

        Raises:
            ValueError: If the transition is invalid
        """
        if not can_transition(self.state, new_state):
            raise ValueError(
                f"Invalid transition from {self.state} to {new_state}"
            )

        # Record the transition
        self.state_history.append((self.state, datetime.now()))
        self.state = new_state

        # Update timestamps based on state
        if new_state == TaskState.RUNNING and self.started_at is None:
            self.started_at = datetime.now()
        elif new_state in {TaskState.DONE, TaskState.FAILED, TaskState.CANCELLED}:
            self.completed_at = datetime.now()

        return True

    def is_blocked(self) -> bool:
        """Check if the task has blocking dependencies.

        Returns:
            True if blocked_by list is non-empty
        """
        return len(self.blocked_by) > 0

    def add_child(self, child_id: TaskID) -> None:
        """Add a child task ID to this task.

        Args:
            child_id: The ID of the child task
        """
        if child_id not in self.children_ids:
            self.children_ids.append(child_id)

    def set_error(self, error_message: str) -> None:
        """Set an error message and transition to FAILED state if possible.

        Args:
            error_message: Description of the error
        """
        self.error = error_message
        if can_transition(self.state, TaskState.FAILED):
            self.transition_to(TaskState.FAILED)
