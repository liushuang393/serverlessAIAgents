"""First-class Task identifier for Agent OS."""

import uuid
from dataclasses import dataclass
from datetime import datetime


@dataclass(frozen=True)
class TaskID:
    """First-class Task identifier for Agent OS.

    TaskIDs are immutable identifiers that encode:
    - type: The task type (exec, sub, gate, review)
    - timestamp: When the task was created (YYYYMMDD)
    - random: A random hex string for uniqueness

    Format: task_{type}_{timestamp}_{random}
    Example: task_exec_20240101_abc12345
    """

    type: str  # "exec", "sub", "gate", "review"
    timestamp: str
    random: str

    @classmethod
    def generate(cls, task_type: str = "exec") -> "TaskID":
        """Generate a new TaskID with the given type.

        Args:
            task_type: The type of task (default: "exec")

        Returns:
            A new TaskID instance
        """
        return cls(
            type=task_type,
            timestamp=datetime.now().strftime("%Y%m%d"),
            random=uuid.uuid4().hex[:8],
        )

    def __str__(self) -> str:
        """Return string representation of TaskID."""
        return f"task_{self.type}_{self.timestamp}_{self.random}"

    def __hash__(self) -> int:
        """Make TaskID hashable for use as dict key."""
        return hash(str(self))

    @classmethod
    def parse(cls, task_id_str: str) -> "TaskID":
        """Parse a TaskID string back into a TaskID object.

        Args:
            task_id_str: String in format task_{type}_{timestamp}_{random}

        Returns:
            TaskID instance

        Raises:
            ValueError: If the string format is invalid
        """
        parts = task_id_str.split("_")
        if len(parts) != 4 or parts[0] != "task":
            msg = f"Invalid TaskID format: {task_id_str}"
            raise ValueError(msg)
        return cls(type=parts[1], timestamp=parts[2], random=parts[3])
