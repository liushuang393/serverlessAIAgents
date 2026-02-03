from __future__ import annotations

from dataclasses import dataclass, field
from datetime import UTC, datetime


VALID_STATUSES = {"todo", "in_progress", "blocked", "done"}
VALID_PRIORITIES = {"low", "medium", "high"}


def utc_now_iso() -> str:
    return datetime.now(UTC).replace(microsecond=0).isoformat().replace("+00:00", "Z")


@dataclass
class Task:
    id: str
    title: str
    description: str = ""
    status: str = "todo"
    priority: str = "medium"
    tags: list[str] = field(default_factory=list)
    created_at: str = field(default_factory=utc_now_iso)
    updated_at: str = field(default_factory=utc_now_iso)
    due_date: str | None = None

    def to_dict(self) -> dict:
        return {
            "id": self.id,
            "title": self.title,
            "description": self.description,
            "status": self.status,
            "priority": self.priority,
            "tags": self.tags,
            "created_at": self.created_at,
            "updated_at": self.updated_at,
            "due_date": self.due_date,
        }

    @classmethod
    def from_dict(cls, data: dict) -> Task:
        return cls(
            id=str(data.get("id", "")),
            title=str(data.get("title", "")),
            description=str(data.get("description", "")),
            status=str(data.get("status", "todo")),
            priority=str(data.get("priority", "medium")),
            tags=list(data.get("tags", [])) if data.get("tags") is not None else [],
            created_at=str(data.get("created_at", utc_now_iso())),
            updated_at=str(data.get("updated_at", utc_now_iso())),
            due_date=data.get("due_date"),
        )
