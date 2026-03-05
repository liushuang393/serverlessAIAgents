"""Backlog contract models for long-running migration sessions."""

from __future__ import annotations

from datetime import UTC, datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class BacklogTaskStatus(str, Enum):
    """Backlog task lifecycle states."""

    PENDING = "pending"
    RUNNING = "running"
    DONE = "done"
    BLOCKED = "blocked"
    NEEDS_FIX = "needs_fix"
    SKIPPED = "skipped"


class ImmutableTaskFields(BaseModel):
    """Immutable task fields."""

    module: str
    stage: str
    description: str
    acceptance_criteria: list[str] = Field(default_factory=list)
    dependencies: list[str] = Field(default_factory=list)


class BacklogTask(BaseModel):
    """Single backlog task."""

    task_id: str
    immutable: ImmutableTaskFields
    immutable_hash: str
    immutable_snapshot: ImmutableTaskFields

    status: BacklogTaskStatus = BacklogTaskStatus.PENDING
    evidence_paths: list[str] = Field(default_factory=list)
    notes: list[str] = Field(default_factory=list)
    unknowns: list[dict[str, Any]] = Field(default_factory=list)
    attempts: int = 0
    last_session_id: str | None = None
    updated_at: str

    @property
    def module(self) -> str:
        return self.immutable.module

    @property
    def stage(self) -> str:
        return self.immutable.stage

    @property
    def dependencies(self) -> list[str]:
        return list(self.immutable.dependencies)


class BacklogState(BaseModel):
    """Backlog state persisted per run."""

    run_id: str
    source_path: str
    output_root: str
    migration_type: str
    fast_mode: bool
    created_at: str
    updated_at: str
    tasks: list[BacklogTask] = Field(default_factory=list)
    metadata: dict[str, Any] = Field(default_factory=dict)

    def get_task(self, task_id: str) -> BacklogTask | None:
        for task in self.tasks:
            if task.task_id == task_id:
                return task
        return None


class SessionStatus(str, Enum):
    """Session status contract for cli run output."""

    DONE = "done"
    NEEDS_FIX = "needs_fix"
    BLOCKED = "blocked"
    BACKLOG_COMPLETED = "backlog_completed"
    INPUT_ERROR = "input_error"
    ENV_ERROR = "env_error"


def now_iso() -> str:
    """Return utc timestamp in ISO8601."""
    return datetime.now(tz=UTC).isoformat()

