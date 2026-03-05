# Long-Running Patterns (`code_migration_assistant`)

## Scope
- This document defines the official long-running operation model for CMA main path only:
  - `cli run`
  - backend migration router (`/api/migrate/*`)
  - workflow runtime/session orchestration
- Legacy path (`pipeline/engine.py`) and other apps are out of scope.

## Core Invariants
1. `1 session = 1 backlog task`.
2. Backlog immutable fields (`description`, `acceptance_criteria`, `dependencies`) are protected.
3. Task completion is evidence-first (no evidence => not `done`).
4. Every session starts with strict preflight/smoke checks.
5. Fast-mode skipped verification must leave unknowns and enqueue strict follow-up tasks.
6. Session end always records backlog/progress/decision/failure logs.

## Official Patterns

### 1. Backlog-Driven Pattern
- Backlog is the source of truth for migration progress.
- Task unit: `module x stage`.
- Storage:
  - `backlog/backlog.json`
  - `logs/mutation_log.jsonl`
- Implementation:
  - `workflow/backlog_models.py`
  - `workflow/backlog_store.py`

### 2. Dispatcher Pattern
- Dispatcher emits exactly one dispatchable task per session.
- Only `pending` tasks with satisfied dependencies can be selected.
- Implementation:
  - `workflow/dispatcher.py`

### 3. Evidence-First Pattern
- Stage output is validated against required evidence artifacts.
- Per-task evidence folder:
  - `evidence/<backlog_task_id>/manifest.json`
- Missing evidence keeps task out of `done` (`blocked` / `needs_fix` flow).
- Implementation:
  - `workflow/evidence_gate.py`

### 4. Quality Gate Pattern
- Session summary returns explicit status and next action context.
- Standard statuses:
  - `done`
  - `needs_fix`
  - `blocked`
  - `backlog_completed`
  - `input_error`
  - `env_error`
- Fast mode unknowns force strict verification follow-up task creation.

### 5. Reflection Loop Pattern
- Each session writes operational traces:
  - `progress/PROGRESS.md`
  - `logs/decision_log.jsonl`
  - `logs/failure_log.jsonl`
  - `logs/dispatch_log.jsonl`
- Root-cause and next-action are carried across sessions via backlog state and logs.

### 6. Plugin-First Pattern
- Stage execution keeps runtime extension points in workflow runtime.
- Long-running control (backlog/dispatch/evidence/preflight) is layered outside legacy engine path.

## Session Lifecycle
1. Load/initialize backlog.
2. Enforce immutability (auto-correct or mark blocked if uncorrectable).
3. Dispatch one task.
4. Run preflight (dependency/health/smoke).
5. Execute single stage task.
6. Evaluate evidence and update task state.
7. Persist logs and summary fields (`next_task_id`, `remaining_tasks`, etc.).

## CLI Contract (`run`)
- Output includes:
  - `run_id`
  - `session_id`
  - `session_status`
  - `dispatched_task`
  - `remaining_tasks`
  - `backlog_path`
  - `evidence_root`
  - `next_task_id`
- Exit code:
  - `0`: session succeeded (`done`) or backlog completed
  - `1`: `blocked` / `needs_fix`
  - `2`: input/environment error

## Backend Loop Contract
- Existing APIs (`upload/stream/status/download`) remain stable.
- Internally, backend repeatedly invokes `cli run` sessions until:
  - backlog completed
  - blocked/error terminal state
