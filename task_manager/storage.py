from __future__ import annotations

import json
import uuid
from collections.abc import Iterable
from pathlib import Path

from .models import VALID_PRIORITIES, VALID_STATUSES, Task, utc_now_iso


class TaskStore:
    def __init__(self, root_dir: str | Path) -> None:
        self.root_dir = Path(root_dir)
        self.tasks_dir = self.root_dir / "tasks"

    def init(self) -> None:
        self.tasks_dir.mkdir(parents=True, exist_ok=True)

    def _ensure_ready(self) -> None:
        if not self.tasks_dir.exists():
            raise FileNotFoundError(
                f"Task store not initialized. Run init() or create {self.tasks_dir}."
            )

    def _task_path(self, task_id: str) -> Path:
        return self.tasks_dir / f"task_{task_id}.json"

    def _load_task(self, path: Path) -> Task:
        data = json.loads(path.read_text(encoding="utf-8"))
        return Task.from_dict(data)

    def _write_task(self, task: Task) -> None:
        self._ensure_ready()
        path = self._task_path(task.id)
        tmp_path = path.with_suffix(".json.tmp")
        payload = json.dumps(task.to_dict(), ensure_ascii=True, indent=2, sort_keys=True)
        tmp_path.write_text(payload + "\n", encoding="utf-8")
        tmp_path.replace(path)

    def _validate_status(self, status: str) -> None:
        if status not in VALID_STATUSES:
            raise ValueError(f"Invalid status: {status}. Allowed: {sorted(VALID_STATUSES)}")

    def _validate_priority(self, priority: str) -> None:
        if priority not in VALID_PRIORITIES:
            raise ValueError(f"Invalid priority: {priority}. Allowed: {sorted(VALID_PRIORITIES)}")

    def create_task(
        self,
        title: str,
        description: str = "",
        status: str = "todo",
        priority: str = "medium",
        tags: Iterable[str] | None = None,
        due_date: str | None = None,
    ) -> Task:
        self._ensure_ready()
        if not title.strip():
            raise ValueError("title is required")
        self._validate_status(status)
        self._validate_priority(priority)

        task = Task(
            id=uuid.uuid4().hex,
            title=title.strip(),
            description=description.strip(),
            status=status,
            priority=priority,
            tags=list(tags) if tags else [],
            due_date=due_date,
        )
        self._write_task(task)
        return task

    def list_tasks(self, status: str | None = None) -> list[Task]:
        self._ensure_ready()
        if status is not None:
            self._validate_status(status)
        tasks: list[Task] = []
        for path in sorted(self.tasks_dir.glob("task_*.json")):
            task = self._load_task(path)
            if status is None or task.status == status:
                tasks.append(task)
        tasks.sort(key=lambda t: t.created_at)
        return tasks

    def get_task(self, task_id: str) -> Task:
        self._ensure_ready()
        path = self._task_path(task_id)
        if not path.exists():
            raise FileNotFoundError(f"Task not found: {task_id}")
        return self._load_task(path)

    def update_task(self, task_id: str, **fields: object) -> Task:
        task = self.get_task(task_id)
        for key in ("title", "description", "status", "priority", "tags", "due_date"):
            if key in fields and fields[key] is not None:
                value = fields[key]
                if key == "status":
                    self._validate_status(str(value))
                if key == "priority":
                    self._validate_priority(str(value))
                if key == "tags" and not isinstance(value, list):
                    raise ValueError("tags must be a list")
                setattr(task, key, value)
        task.updated_at = utc_now_iso()
        self._write_task(task)
        return task

    def set_status(self, task_id: str, status: str) -> Task:
        return self.update_task(task_id, status=status)

    def delete_task(self, task_id: str) -> None:
        self._ensure_ready()
        path = self._task_path(task_id)
        if not path.exists():
            raise FileNotFoundError(f"Task not found: {task_id}")
        path.unlink()
