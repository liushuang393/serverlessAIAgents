# Code Migration Assistant Architecture

> Version: 2.0.0
> Last Updated: 2026-03-05

## 1. Scope
- Main path only:
  - `cli run`
  - backend migration router (`/api/migrate/*`)
  - workflow runtime session execution
- Legacy execution path (`pipeline/engine.py`) is preserved for compatibility and is not part of the long-running orchestration contract.

## 2. Core Model: Session-Based Long Running
- Execution is backlog-driven.
- One session executes exactly one backlog task.
- Backend loops sessions until backlog completion or terminal block.

High-level flow:
1. Initialize/load backlog (`module x stage` tasks).
2. Enforce immutability.
3. Dispatch one task.
4. Run preflight/smoke checks.
5. Execute stage logic.
6. Evidence gate validates artifacts.
7. Persist backlog/logs and return session summary.

## 3. Component Map

### CLI Contract Layer
- File: `apps/code_migration_assistant/cli.py`
- Role:
  - Implements `run` session contract.
  - Emits session/stage events.
  - Returns session status + next task context.

### Backlog + Dispatch Layer
- Files:
  - `workflow/backlog_models.py`
  - `workflow/backlog_store.py`
  - `workflow/dispatcher.py`
- Role:
  - Immutable/mutable task model separation.
  - Hash + snapshot based immutability enforcement.
  - Single-task dispatch selection.

### Runtime + Quality Layer
- Files:
  - `workflow/pipeline_runtime.py`
  - `workflow/preflight.py`
  - `workflow/evidence_gate.py`
- Role:
  - Single-stage task execution entrypoint.
  - Strict preflight and smoke checks.
  - Evidence-first completion gate with per-task manifests.

### Backend Adapter + Router Layer
- Files:
  - `backend/migration_execution_adapter.py`
  - `backend/migration_router.py`
- Role:
  - Starts `cli run`.
  - Streams session events.
  - Repeats sessions to completion.
  - Exposes backlog read APIs.

## 4. Public API Contracts

### Existing APIs (unchanged path)
- `POST /api/migrate/upload`
- `GET /api/migrate/{task_id}/stream`
- `GET /api/migrate/{task_id}/status`
- `GET /api/migrate/{task_id}/download`

### New Backlog APIs
- `GET /api/migrate/{task_id}/backlog`
- `GET /api/migrate/{task_id}/backlog/tasks/{backlog_task_id}`

## 5. Session Contract (`cli run`)

Output summary fields include:
- `run_id`
- `session_id`
- `session_status`
- `dispatched_task`
- `remaining_tasks`
- `next_task_id`
- `backlog_path`
- `evidence_root`

Exit code:
- `0`: session done or backlog completed
- `1`: blocked/needs_fix
- `2`: input/env error

## 6. Persistence and Audit

Per run:
- `backlog/backlog.json`
- `evidence/<backlog_task_id>/manifest.json`
- `progress/PROGRESS.md`
- `logs/dispatch_log.jsonl`
- `logs/decision_log.jsonl`
- `logs/failure_log.jsonl`
- `logs/mutation_log.jsonl`

## 7. Fast Mode Behavior
- `--fast` can skip strict verification.
- Unknowns must be recorded.
- Strict verification follow-up task is added to backlog automatically.

## 8. Reference
- Long-running operational patterns:
  - `apps/code_migration_assistant/docs/long_running_patterns.md`
