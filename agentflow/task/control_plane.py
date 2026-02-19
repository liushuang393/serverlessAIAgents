"""Control Plane interface for Agent OS.

The Control Plane is the 'thinking' layer responsible for:
- Task state management
- Planning and re-planning
- Task scheduling and coordination
"""

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any

from agentflow.task.task import Task
from agentflow.task.task_id import TaskID
from agentflow.task.task_state import TaskState


if TYPE_CHECKING:
    from agentflow.task.task_graph import TaskGraph


class ControlPlane(ABC):
    """Control Plane - 'thinking' layer.

    Responsible for:
    - State management: getting and updating task states
    - Planning: creating initial plans from goals
    - Re-planning: adapting plans based on execution feedback
    - Scheduling: determining which tasks are ready to execute

    The Control Plane operates at a higher level than the Execution Plane,
    making decisions about what to do next based on the current state.
    """

    @abstractmethod
    async def get_task(self, task_id: TaskID) -> Task | None:
        """Retrieve a task by its ID.

        Args:
            task_id: The ID of the task to retrieve

        Returns:
            The Task if found, None otherwise
        """
        ...

    @abstractmethod
    async def update_task_state(self, task_id: TaskID, new_state: TaskState) -> bool:
        """Update the state of a task.

        Args:
            task_id: The ID of the task to update
            new_state: The new state to transition to

        Returns:
            True if the update succeeded, False otherwise
        """
        ...

    @abstractmethod
    async def create_plan(self, goal: str, context: dict[str, Any]) -> "TaskGraph":
        """Create a plan (TaskGraph) to achieve a goal.

        Args:
            goal: The high-level goal to achieve
            context: Additional context for planning

        Returns:
            A TaskGraph representing the plan
        """
        ...

    @abstractmethod
    async def replan(self, failed_task: Task, feedback: str) -> "TaskGraph":
        """Re-plan after a task failure or verification issue.

        Args:
            failed_task: The task that failed or needs replanning
            feedback: Feedback about why replanning is needed

        Returns:
            An updated TaskGraph with the new plan
        """
        ...

    @abstractmethod
    async def get_ready_tasks(self) -> list[Task]:
        """Get all tasks that are ready to execute.

        A task is ready if:
        - It is in PLANNED state
        - All its dependencies (blocked_by) are DONE

        Returns:
            List of tasks ready for execution
        """
        ...
