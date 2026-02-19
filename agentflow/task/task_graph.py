"""DAG-based task representation with re-planning support for Agent OS."""

from dataclasses import dataclass, field

from agentflow.task.task import Task
from agentflow.task.task_id import TaskID
from agentflow.task.task_state import TaskState


@dataclass
class TaskGraph:
    """DAG-based task representation with re-planning support.

    A TaskGraph represents a plan as a directed acyclic graph (DAG) of tasks.
    Tasks can have dependencies (blocked_by) and can block other tasks (blocks).

    Key features:
    - Add tasks with dependencies
    - Query for ready tasks (dependencies satisfied)
    - Insert tasks for re-planning scenarios
    - Version tracking for plan changes

    The graph maintains referential integrity: when adding dependencies,
    both the blocked_by and blocks lists are updated.
    """

    id: str
    root_goal: str
    tasks: dict[TaskID, Task] = field(default_factory=dict)
    version: int = 1

    def add_task(self, task: Task, blocked_by: list[TaskID] | None = None) -> TaskID:
        """Add a task to the graph with optional dependencies.

        Args:
            task: The task to add
            blocked_by: List of TaskIDs that must complete before this task

        Returns:
            The ID of the added task
        """
        self.tasks[task.id] = task

        if blocked_by:
            task.blocked_by = blocked_by
            for dep_id in blocked_by:
                if dep_id in self.tasks:
                    self.tasks[dep_id].blocks.append(task.id)

        self.version += 1
        return task.id

    def get_ready_tasks(self) -> list[Task]:
        """Get all tasks that are ready to execute.

        A task is ready if:
        - It is in PLANNED state
        - All its dependencies (blocked_by) are DONE

        Returns:
            List of tasks ready for execution
        """
        ready = []
        for task in self.tasks.values():
            if task.state == TaskState.PLANNED:
                # Check if all dependencies are done
                all_deps_done = all(
                    self.tasks[bid].state == TaskState.DONE
                    for bid in task.blocked_by
                    if bid in self.tasks
                )
                if all_deps_done:
                    ready.append(task)
        return ready

    def insert_after(self, existing_id: TaskID, new_task: Task) -> TaskID:
        """Insert a task after an existing task (for re-planning).

        This is useful when re-planning requires inserting additional
        steps between existing tasks. The new task:
        - Will be blocked by the existing task
        - Will block everything the existing task was blocking
        - The existing task will now only block the new task

        Args:
            existing_id: ID of the task to insert after
            new_task: The new task to insert

        Returns:
            The ID of the inserted task
        """
        existing = self.tasks[existing_id]

        # New task is blocked by the existing task
        new_task.blocked_by = [existing_id]

        # New task blocks what the existing task was blocking
        new_task.blocks = existing.blocks.copy()

        # Update the blocked_by of tasks that were blocked by existing
        for blocked_id in existing.blocks:
            if blocked_id in self.tasks:
                blocked_task = self.tasks[blocked_id]
                # Remove existing_id from blocked_by
                if existing_id in blocked_task.blocked_by:
                    blocked_task.blocked_by.remove(existing_id)
                # Add new_task.id to blocked_by
                blocked_task.blocked_by.append(new_task.id)

        # Existing task now only blocks the new task
        existing.blocks = [new_task.id]

        # Add the new task to the graph
        self.tasks[new_task.id] = new_task
        self.version += 1

        return new_task.id

    def get_task(self, task_id: TaskID) -> Task | None:
        """Get a task by its ID.

        Args:
            task_id: The ID of the task to retrieve

        Returns:
            The Task if found, None otherwise
        """
        return self.tasks.get(task_id)

    def get_all_tasks(self) -> list[Task]:
        """Get all tasks in the graph.

        Returns:
            List of all tasks
        """
        return list(self.tasks.values())

    def get_blocked_tasks(self) -> list[Task]:
        """Get all tasks that are currently blocked.

        Returns:
            List of tasks in BLOCKED state or with unsatisfied dependencies
        """
        blocked = []
        for task in self.tasks.values():
            if task.state == TaskState.BLOCKED:
                blocked.append(task)
            elif task.state == TaskState.PLANNED and task.blocked_by:
                # Check if any dependency is not done
                has_pending_dep = any(
                    self.tasks[bid].state != TaskState.DONE
                    for bid in task.blocked_by
                    if bid in self.tasks
                )
                if has_pending_dep:
                    blocked.append(task)
        return blocked
