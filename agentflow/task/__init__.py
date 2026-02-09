"""Task lifecycle management for Agent OS.

This module provides first-class task concepts including:
- TaskID: Unique task identifiers
- TaskState: Task lifecycle states and transitions
- Task: Task entity with validated state machine
- TaskGraph: DAG-based task representation with re-planning
- Control/Execution Plane interfaces
"""

from agentflow.task.control_plane import ControlPlane
from agentflow.task.execution_plane import ExecutionPlane
from agentflow.task.task import Task
from agentflow.task.task_graph import TaskGraph
from agentflow.task.task_id import TaskID
from agentflow.task.task_state import TaskState, can_transition, is_terminal


__all__ = [
    "ControlPlane",
    "ExecutionPlane",
    "Task",
    "TaskGraph",
    "TaskID",
    "TaskState",
    "can_transition",
    "is_terminal",
]
