"""Execution Plane interface for Agent OS.

The Execution Plane is the 'doing' layer responsible for:
- Task execution
- Checkpointing and recovery
- Observable execution with event streaming
"""

from abc import ABC, abstractmethod
from collections.abc import AsyncIterator
from typing import TYPE_CHECKING

from agentflow.task.task import Task
from agentflow.task.task_id import TaskID


if TYPE_CHECKING:
    from agentflow.task.execution_event import ExecutionEvent


class ExecutionPlane(ABC):
    """Execution Plane - 'doing' layer.

    Responsible for:
    - Execution: running tasks and producing results
    - Observability: streaming execution events
    - Checkpointing: saving execution state for recovery
    - Abort: gracefully stopping running tasks

    The Execution Plane is designed to be:
    - Replaceable: different implementations for different environments
    - Isolatable: can run in sandboxed environments
    - Observable: all execution produces events

    Key design principle: The model is a component OF the execution,
    not the execution itself. System structure is non-negotiable.
    """

    @abstractmethod
    async def execute(self, task: Task) -> AsyncIterator["ExecutionEvent"]:
        """Execute a task and yield execution events.

        This method streams execution events as the task progresses,
        allowing for real-time observability of task execution.

        Args:
            task: The task to execute

        Yields:
            ExecutionEvent objects representing execution progress

        Note:
            The final event should indicate task completion or failure.
        """
        ...

    @abstractmethod
    async def abort(self, task_id: TaskID) -> bool:
        """Abort a running task.

        Gracefully stops the execution of a task. This should:
        - Stop any ongoing work
        - Clean up resources
        - Emit an abort event

        Args:
            task_id: The ID of the task to abort

        Returns:
            True if the task was successfully aborted, False otherwise
        """
        ...

    @abstractmethod
    async def checkpoint(self, task_id: TaskID) -> str:
        """Create a checkpoint of the current execution state.

        Checkpoints allow for recovery if execution is interrupted.
        The returned string is an opaque checkpoint ID that can be
        used to restore state later.

        Args:
            task_id: The ID of the task to checkpoint

        Returns:
            A checkpoint ID string that can be used for recovery
        """
        ...
