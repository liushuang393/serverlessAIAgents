"""Dispatcher for one-session-one-task execution."""

from __future__ import annotations

from typing import Any

from apps.code_migration_assistant.workflow.backlog_models import (
    BacklogState,
    BacklogTask,
    BacklogTaskStatus,
    now_iso,
)


_DEP_SATISFIED_STATUSES = {
    BacklogTaskStatus.DONE,
    BacklogTaskStatus.NEEDS_FIX,
    BacklogTaskStatus.SKIPPED,
}


class BacklogDispatcher:
    """Select and transition a single executable backlog task."""

    def select_next_task(self, state: BacklogState) -> BacklogTask | None:
        task_map = {task.task_id: task for task in state.tasks}
        for task in state.tasks:
            if task.status != BacklogTaskStatus.PENDING:
                continue
            if self._deps_satisfied(task, task_map):
                return task
        return None

    @staticmethod
    def _deps_satisfied(task: BacklogTask, task_map: dict[str, BacklogTask]) -> bool:
        for dep_id in task.dependencies:
            dep = task_map.get(dep_id)
            if dep is None:
                return False
            if dep.status not in _DEP_SATISFIED_STATUSES:
                return False
        return True

    @staticmethod
    def to_running(task: BacklogTask, session_id: str) -> dict[str, Any]:
        return {
            "status": BacklogTaskStatus.RUNNING,
            "attempts": task.attempts + 1,
            "last_session_id": session_id,
            "updated_at": now_iso(),
        }

    @staticmethod
    def to_done(
        *,
        evidence_paths: list[str],
        notes: list[str],
        unknowns: list[dict[str, Any]],
        session_id: str,
    ) -> dict[str, Any]:
        return {
            "status": BacklogTaskStatus.DONE,
            "evidence_paths": evidence_paths,
            "notes": notes,
            "unknowns": unknowns,
            "last_session_id": session_id,
            "updated_at": now_iso(),
        }

    @staticmethod
    def to_blocked(
        *,
        notes: list[str],
        unknowns: list[dict[str, Any]],
        session_id: str,
    ) -> dict[str, Any]:
        return {
            "status": BacklogTaskStatus.BLOCKED,
            "notes": notes,
            "unknowns": unknowns,
            "last_session_id": session_id,
            "updated_at": now_iso(),
        }

    @staticmethod
    def to_needs_fix(
        *,
        evidence_paths: list[str],
        notes: list[str],
        unknowns: list[dict[str, Any]],
        session_id: str,
    ) -> dict[str, Any]:
        return {
            "status": BacklogTaskStatus.NEEDS_FIX,
            "evidence_paths": evidence_paths,
            "notes": notes,
            "unknowns": unknowns,
            "last_session_id": session_id,
            "updated_at": now_iso(),
        }
