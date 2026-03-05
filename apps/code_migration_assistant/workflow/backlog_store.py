"""Backlog persistence and immutability guard."""

from __future__ import annotations

import hashlib
import json
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from pathlib import Path

from apps.code_migration_assistant.workflow.backlog_models import (
    BacklogState,
    BacklogTask,
    BacklogTaskStatus,
    ImmutableTaskFields,
    now_iso,
)


STAGE_ORDER: tuple[str, ...] = (
    "analysis",
    "business_semantics",
    "design",
    "transform",
    "tests",
    "diff",
    "quality",
    "fix",
    "report",
)


_MUTABLE_FIELDS = {
    "status",
    "evidence_paths",
    "notes",
    "unknowns",
    "attempts",
    "last_session_id",
    "updated_at",
}


class BacklogStore:
    """File-based backlog state store."""

    def __init__(self, run_root: Path) -> None:
        self.run_root = run_root
        self.backlog_dir = run_root / "backlog"
        self.logs_dir = run_root / "logs"
        self.progress_dir = run_root / "progress"
        self.evidence_root = run_root / "evidence"
        self.backlog_path = self.backlog_dir / "backlog.json"
        self.dispatch_log_path = self.logs_dir / "dispatch_log.jsonl"
        self.decision_log_path = self.logs_dir / "decision_log.jsonl"
        self.failure_log_path = self.logs_dir / "failure_log.jsonl"
        self.progress_path = self.progress_dir / "PROGRESS.md"
        self.mutation_log_path = self.logs_dir / "mutation_log.jsonl"

    def ensure_dirs(self) -> None:
        self.run_root.mkdir(parents=True, exist_ok=True)
        self.backlog_dir.mkdir(parents=True, exist_ok=True)
        self.logs_dir.mkdir(parents=True, exist_ok=True)
        self.progress_dir.mkdir(parents=True, exist_ok=True)
        self.evidence_root.mkdir(parents=True, exist_ok=True)

    def initialize_if_missing(
        self,
        *,
        run_id: str,
        source_path: str,
        output_root: str,
        migration_type: str,
        fast_mode: bool,
        modules: list[str],
    ) -> BacklogState:
        self.ensure_dirs()
        if self.backlog_path.exists():
            return self.load()

        created_at = now_iso()
        tasks: list[BacklogTask] = []
        for module in modules:
            prev_task_id: str | None = None
            for stage in STAGE_ORDER:
                task_id = f"{module}:{stage}"
                immutable = ImmutableTaskFields(
                    module=module,
                    stage=stage,
                    description=f"{module} / {stage}",
                    acceptance_criteria=_build_acceptance_criteria(stage),
                    dependencies=[] if prev_task_id is None else [prev_task_id],
                )
                immutable_hash = self.compute_immutable_hash(immutable)
                tasks.append(
                    BacklogTask(
                        task_id=task_id,
                        immutable=immutable,
                        immutable_hash=immutable_hash,
                        immutable_snapshot=immutable.model_copy(deep=True),
                        status=BacklogTaskStatus.PENDING,
                        updated_at=created_at,
                    )
                )
                prev_task_id = task_id

        state = BacklogState(
            run_id=run_id,
            source_path=source_path,
            output_root=output_root,
            migration_type=migration_type,
            fast_mode=fast_mode,
            created_at=created_at,
            updated_at=created_at,
            tasks=tasks,
        )
        self.save(state)
        return state

    def load(self) -> BacklogState:
        raw = json.loads(self.backlog_path.read_text(encoding="utf-8"))
        return BacklogState.model_validate(raw)

    def save(self, state: BacklogState) -> None:
        state.updated_at = now_iso()
        self.backlog_path.write_text(
            json.dumps(state.model_dump(mode="json"), ensure_ascii=False, indent=2),
            encoding="utf-8",
        )

    @staticmethod
    def compute_immutable_hash(immutable: ImmutableTaskFields) -> str:
        raw = json.dumps(immutable.model_dump(mode="json"), ensure_ascii=False, sort_keys=True)
        return hashlib.sha256(raw.encode("utf-8")).hexdigest()

    def enforce_immutability(self, state: BacklogState) -> list[dict[str, Any]]:
        corrections: list[dict[str, Any]] = []
        for task in state.tasks:
            expected_hash = self.compute_immutable_hash(task.immutable_snapshot)
            current_hash = self.compute_immutable_hash(task.immutable)
            if task.immutable_hash == expected_hash and current_hash == expected_hash:
                continue

            before = task.immutable.model_dump(mode="json")
            ts = now_iso()
            try:
                task.immutable = task.immutable_snapshot.model_copy(deep=True)
                task.immutable_hash = expected_hash
                task.updated_at = ts
                after = task.immutable.model_dump(mode="json")
                correction = {
                    "timestamp": task.updated_at,
                    "task_id": task.task_id,
                    "action": "immutable_autocorrect",
                    "before": before,
                    "after": after,
                }
            except Exception as exc:
                note = f"immutable correction failed: {exc}"
                task.status = BacklogTaskStatus.BLOCKED
                task.notes = [*task.notes, note]
                task.updated_at = ts
                correction = {
                    "timestamp": task.updated_at,
                    "task_id": task.task_id,
                    "action": "immutable_uncorrectable",
                    "before": before,
                    "error": str(exc),
                }
                self._append_jsonl(
                    self.failure_log_path,
                    {
                        "timestamp": task.updated_at,
                        "task_id": task.task_id,
                        "reason": "immutable_uncorrectable",
                        "error": str(exc),
                    },
                )
            corrections.append(correction)
            self._append_jsonl(self.mutation_log_path, correction)

        if corrections:
            self.save(state)
        return corrections

    def update_task_mutable(
        self,
        state: BacklogState,
        task_id: str,
        updates: dict[str, Any],
    ) -> BacklogTask:
        task = state.get_task(task_id)
        if task is None:
            msg = f"task not found: {task_id}"
            raise KeyError(msg)

        invalid_fields = [key for key in updates if key not in _MUTABLE_FIELDS]
        if invalid_fields:
            msg = f"immutable fields cannot be updated via mutable update: {invalid_fields}"
            raise ValueError(msg)

        for key, value in updates.items():
            setattr(task, key, value)
        task.updated_at = now_iso()
        self.save(state)
        return task

    def add_task(self, state: BacklogState, immutable: ImmutableTaskFields) -> BacklogTask:
        task_id = f"{immutable.module}:{immutable.stage}"
        existing = state.get_task(task_id)
        if existing is not None:
            return existing
        ts = now_iso()
        immutable_hash = self.compute_immutable_hash(immutable)
        task = BacklogTask(
            task_id=task_id,
            immutable=immutable,
            immutable_hash=immutable_hash,
            immutable_snapshot=immutable.model_copy(deep=True),
            status=BacklogTaskStatus.PENDING,
            updated_at=ts,
        )
        state.tasks.append(task)
        self.save(state)
        return task

    def replace_dependencies(self, state: BacklogState, task_id: str, dependencies: list[str]) -> None:
        task = state.get_task(task_id)
        if task is None:
            msg = f"task not found: {task_id}"
            raise KeyError(msg)
        snapshot = task.immutable_snapshot.model_copy(deep=True)
        snapshot.dependencies = list(dict.fromkeys(dependencies))
        task.immutable = snapshot.model_copy(deep=True)
        task.immutable_snapshot = snapshot
        task.immutable_hash = self.compute_immutable_hash(snapshot)
        task.updated_at = now_iso()
        self.save(state)

    def append_dispatch_log(self, payload: dict[str, Any]) -> None:
        self._append_jsonl(self.dispatch_log_path, payload)

    def append_decision_log(self, payload: dict[str, Any]) -> None:
        self._append_jsonl(self.decision_log_path, payload)

    def append_failure_log(self, payload: dict[str, Any]) -> None:
        self._append_jsonl(self.failure_log_path, payload)

    def append_progress(self, markdown_line: str) -> None:
        self.progress_path.parent.mkdir(parents=True, exist_ok=True)
        with self.progress_path.open("a", encoding="utf-8") as handle:
            handle.write(markdown_line + "\n")

    @staticmethod
    def _append_jsonl(path: Path, payload: dict[str, Any]) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("a", encoding="utf-8") as handle:
            handle.write(json.dumps(payload, ensure_ascii=False, default=str) + "\n")


def _build_acceptance_criteria(stage: str) -> list[str]:
    criteria = {
        "analysis": ["legacy_analysis artifact exists"],
        "business_semantics": ["business_semantics artifact exists"],
        "design": ["migration_design artifact exists"],
        "transform": ["transformation artifact exists", "transformation_iterations artifact exists"],
        "tests": ["test_synthesis artifact exists"],
        "diff": ["differential artifact exists", "diff evidence exists"],
        "quality": ["quality_gate artifact exists", "quality decision exists"],
        "fix": ["limited_fix artifact exists"],
        "report": ["compliance report exists"],
        "strict_verification": ["strict differential artifact exists"],
    }
    return criteria.get(stage, [f"{stage} artifact exists"])
