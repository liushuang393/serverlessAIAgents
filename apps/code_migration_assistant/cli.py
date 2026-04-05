"""Code Migration Assistant CLI.

契約実行モード（run）:
    python -m apps.code_migration_assistant.cli run \
      --input /path/to/input.json \
      --output /path/to/output.json \
      --events /path/to/events.ndjson

入力 JSON 例:
{
  "task_id": "task-123",
  "source_path": "/tmp/sample.cbl",
  "output_root": "/tmp/migration_output",
  "fast_mode": true,
  "migration_type": "cobol-to-java",
  "model": "platform_text_default",
  "options": {}
}
"""

from __future__ import annotations

import argparse
import asyncio
import json
import os
import sys
import tempfile
import uuid
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.cobol_project import COBOLProject
from apps.code_migration_assistant.engine import CodeMigrationEngine
from apps.code_migration_assistant.workflow.backlog_models import (
    BacklogTaskStatus,
    ImmutableTaskFields,
    SessionStatus,
    now_iso,
)
from apps.code_migration_assistant.workflow.backlog_store import STAGE_ORDER, BacklogStore
from apps.code_migration_assistant.workflow.dispatcher import BacklogDispatcher
from apps.code_migration_assistant.workflow.evidence_gate import EvidenceGate
from apps.code_migration_assistant.workflow.pipeline_runtime import execute_stage_task
from apps.code_migration_assistant.workflow.preflight import PreflightRunner


_PASS_DECISIONS = {"PASSED", "KNOWN_LEGACY"}
_NODE_STAGE_MAP = {
    "migration.analyze_code": "analyzer",
    "migration.design_architecture": "designer",
    "migration.transform_code": "transformer",
    "migration.synthesize_tests": "test_generator",
    "migration.verify_diff": "verifier",
    "migration.evaluate_quality": "quality_gate",
}
_RETRY_STAGE_MAP = {
    "analyzer": "analysis",
    "designer": "business_semantics",
    "transformer": "transform",
    "test_generator": "tests",
    "verifier": "diff",
    "quality_gate": "quality",
}


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")


def _append_event(path: Path, event: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(event, ensure_ascii=False) + "\n")


def _normalize_stream_event(
    *,
    program_name: str,
    raw_event: dict[str, Any],
) -> dict[str, Any] | None:
    """Engine 内部イベントを Studio 互換イベントへ変換する."""
    raw_event_type = str(raw_event.get("event_type") or raw_event.get("event") or "")

    if raw_event_type in {"node.start", "node_start"}:
        stage = _NODE_STAGE_MAP.get(str(raw_event.get("node_name") or raw_event.get("node") or ""))
        if stage is None:
            return None
        return {
            "type": "stage_start",
            "stage": stage,
            "program_name": program_name,
            "message": f"{stage} 実行中...",
        }

    if raw_event_type in {"node.complete", "node_complete"}:
        node_id = str(raw_event.get("node_name") or raw_event.get("node") or "")
        if node_id == "migration_pipeline":
            return None
        stage = _NODE_STAGE_MAP.get(node_id)
        if stage is None:
            return None
        event: dict[str, Any] = {
            "type": "stage_complete",
            "stage": stage,
            "program_name": program_name,
        }
        result = raw_event.get("data") or raw_event.get("result")
        if isinstance(result, dict) and stage == "quality_gate":
            decision = result.get("decision")
            if isinstance(decision, str):
                event["decision"] = decision
        return event

    if raw_event_type == "approval_required":
        return {
            "type": "hitl_required",
            "stage": "designer",
            "program_name": program_name,
            "request_id": raw_event.get("request_id"),
            "reason": raw_event.get("reason", ""),
            "context": raw_event.get("context", {}),
        }

    return None


async def _run_engine_for_program(
    *,
    source_code: str,
    task_id: str,
    program_name: str,
    artifacts_dir: Path,
    fast_mode: bool,
    migration_type: str,
    options: dict[str, Any],
    on_event: Any,
) -> dict[str, Any]:
    """単一プログラムを実行し、進捗イベントをコールバックへ渡す."""
    engine = CodeMigrationEngine(migration_type=migration_type)
    await engine.initialize()

    inputs = {
        "source_code": source_code,
        "task_id": task_id,
        "module": program_name,
        "artifacts_dir": str(artifacts_dir),
        "fast_mode": fast_mode,
        "options": options,
    }

    final_result: dict[str, Any] | None = None
    async for raw_event in engine.execute_stream(inputs):
        normalized = _normalize_stream_event(program_name=program_name, raw_event=raw_event)
        if normalized is not None:
            await on_event(normalized)

        event_type = str(raw_event.get("event_type") or raw_event.get("event") or "")
        node_id = str(raw_event.get("node_name") or raw_event.get("node") or "")
        if event_type in {"node.complete", "node_complete"} and node_id == "migration_pipeline":
            result_obj = raw_event.get("data") or raw_event.get("result")
            if isinstance(result_obj, dict):
                final_result = result_obj

    if final_result is None:
        return {
            "success": False,
            "quality_gate": {"decision": "ENV_ISSUE"},
            "error": "pipeline_result_missing",
            "artifact_paths": {},
        }
    return final_result


def _extract_program_result(
    *,
    program_name: str,
    result: dict[str, Any],
) -> dict[str, Any]:
    """Engine 結果から CLI 契約のプログラム結果へ正規化する."""
    quality_gate = result.get("quality_gate")
    decision = "ENV_ISSUE"
    if isinstance(quality_gate, dict):
        candidate = quality_gate.get("decision")
        if isinstance(candidate, str) and candidate:
            decision = candidate

    check_result = result.get("check_result")
    confidence = 0.0
    if isinstance(check_result, dict):
        confidence_raw = check_result.get("confidence")
        if isinstance(confidence_raw, (float, int)):
            confidence = float(confidence_raw)

    artifact_paths = result.get("artifact_paths")
    if not isinstance(artifact_paths, dict):
        artifact_paths = {}

    return {
        "program_name": program_name,
        "success": bool(result.get("success", False)),
        "decision": decision,
        "class_name": str(result.get("class_name", "MigratedProgram")),
        "target_code": str(result.get("target_code", "")),
        "iterations": int(result.get("iterations", 1)),
        "quality_score": round(confidence * 100.0, 2),
        "artifact_paths": artifact_paths,
        "raw_result": result,
    }


def _resolve_final_decision(program_results: list[dict[str, Any]]) -> str:
    for item in program_results:
        decision = str(item.get("decision", "ENV_ISSUE"))
        if decision not in _PASS_DECISIONS:
            return decision
    return "PASSED"


def _session_exit_code(status: SessionStatus) -> int:
    if status in {SessionStatus.DONE, SessionStatus.BACKLOG_COMPLETED}:
        return 0
    if status in {SessionStatus.BLOCKED, SessionStatus.NEEDS_FIX}:
        return 1
    return 2


def _load_cobol_modules(source_path: Path) -> tuple[dict[str, str], list[str]]:
    with tempfile.TemporaryDirectory(prefix="cma_cli_") as work_dir_str:
        project = COBOLProject(source=source_path, work_dir=Path(work_dir_str))
        project.setup()
        cobol_files = project.get_cobol_files()
        modules = {entry.program_name: entry.content for entry in cobol_files}
    return modules, sorted(modules.keys())


def _is_backlog_completed(state: Any) -> bool:
    return all(task.status in {BacklogTaskStatus.DONE, BacklogTaskStatus.SKIPPED} for task in state.tasks)


def _remaining_tasks(state: Any) -> int:
    return sum(1 for task in state.tasks if task.status not in {BacklogTaskStatus.DONE, BacklogTaskStatus.SKIPPED})


def _seed_resume_state(
    backlog_store: BacklogStore,
    state: Any,
    *,
    start_stage: str,
) -> None:
    if start_stage not in STAGE_ORDER:
        return
    start_index = STAGE_ORDER.index(start_stage)
    for task in state.tasks:
        stage = str(task.immutable.stage)
        if stage not in STAGE_ORDER:
            continue
        updates: dict[str, Any] = {
            "status": BacklogTaskStatus.DONE if STAGE_ORDER.index(stage) < start_index else BacklogTaskStatus.PENDING,
            "notes": [],
            "unknowns": [],
            "evidence_paths": [],
            "attempts": 0,
            "last_session_id": None,
        }
        backlog_store.update_task_mutable(state, task.task_id, updates)


def _should_stop_session_loop(summary: dict[str, Any]) -> bool:
    if bool(summary.get("backlog_completed", False)):
        return True
    status = str(summary.get("session_status", ""))
    if status in {
        SessionStatus.BLOCKED.value,
        SessionStatus.INPUT_ERROR.value,
        SessionStatus.ENV_ERROR.value,
    }:
        return True
    return status == SessionStatus.NEEDS_FIX.value and not summary.get("next_task_id")


async def _run_session_loop(
    payload: dict[str, Any],
    *,
    on_event: Any,
    max_sessions: int,
) -> tuple[dict[str, Any], int]:
    last_summary: dict[str, Any] = {}
    exit_code = 1
    for _ in range(max_sessions):
        summary, exit_code = await run_contract_payload(payload, on_event=on_event)
        last_summary = summary
        if _should_stop_session_loop(summary):
            break
    return last_summary, exit_code


async def run_contract_payload(
    payload: dict[str, Any],
    *,
    on_event: Any,
) -> tuple[dict[str, Any], int]:
    """Run one backlog task per session."""
    source_path_raw = payload.get("source_path")
    if not isinstance(source_path_raw, str) or not source_path_raw:
        summary = {
            "success": False,
            "session_status": SessionStatus.INPUT_ERROR.value,
            "error": "source_path is required",
        }
        return summary, 2

    source_path = Path(source_path_raw).resolve()
    if not source_path.exists():
        summary = {
            "success": False,
            "session_status": SessionStatus.INPUT_ERROR.value,
            "error": f"source_path not found: {source_path}",
        }
        return summary, 2

    output_root_raw = payload.get("output_root", "migration_output")
    if not isinstance(output_root_raw, str) or not output_root_raw:
        summary = {
            "success": False,
            "session_status": SessionStatus.INPUT_ERROR.value,
            "error": "output_root must be a non-empty string",
        }
        return summary, 2
    output_root = Path(output_root_raw).resolve()
    output_root.mkdir(parents=True, exist_ok=True)

    run_id_raw = payload.get("task_id")
    run_id = str(run_id_raw) if isinstance(run_id_raw, str) and run_id_raw else f"task-{uuid.uuid4().hex[:12]}"
    fast_mode = bool(payload.get("fast_mode", True))

    migration_type_raw = payload.get("migration_type", "cobol-to-java")
    migration_type = str(migration_type_raw) if isinstance(migration_type_raw, str) else "cobol-to-java"

    model = payload.get("model")
    options_raw = payload.get("options", {})
    options: dict[str, Any] = options_raw if isinstance(options_raw, dict) else {}
    if isinstance(model, str) and model:
        options.setdefault("model", model)

    run_output_dir = output_root / run_id
    run_output_dir.mkdir(parents=True, exist_ok=True)
    try:
        modules_by_name, module_names = _load_cobol_modules(source_path)
    except Exception as exc:
        await on_event({"type": "error", "stage": None, "message": str(exc)})
        summary = {
            "success": False,
            "run_id": run_id,
            "task_id": run_id,
            "session_status": SessionStatus.INPUT_ERROR.value,
            "error": str(exc),
            "output_dir": str(run_output_dir),
        }
        return summary, 2

    if not module_names:
        summary = {
            "success": False,
            "run_id": run_id,
            "task_id": run_id,
            "session_status": SessionStatus.INPUT_ERROR.value,
            "error": "COBOL files not found in source_path",
            "output_dir": str(run_output_dir),
        }
        return summary, 2

    backlog_store = BacklogStore(run_output_dir)
    backlog_exists = backlog_store.backlog_path.exists()
    state = backlog_store.initialize_if_missing(
        run_id=run_id,
        source_path=str(source_path),
        output_root=str(output_root),
        migration_type=migration_type,
        fast_mode=fast_mode,
        modules=module_names,
    )
    resume_from_stage_raw = payload.get("resume_from_stage")
    resume_from_stage = str(resume_from_stage_raw) if isinstance(resume_from_stage_raw, str) else ""
    if resume_from_stage and not backlog_exists:
        _seed_resume_state(backlog_store, state, start_stage=resume_from_stage)
        state = backlog_store.load()
    corrections = backlog_store.enforce_immutability(state)

    dispatcher = BacklogDispatcher()
    dispatched = dispatcher.select_next_task(state)
    if dispatched is None and _is_backlog_completed(state):
        summary = {
            "success": True,
            "run_id": run_id,
            "task_id": run_id,
            "session_id": None,
            "session_status": SessionStatus.BACKLOG_COMPLETED.value,
            "dispatched_task": None,
            "remaining_tasks": 0,
            "next_task_id": None,
            "backlog_path": str(backlog_store.backlog_path),
            "evidence_root": str(backlog_store.evidence_root),
            "output_dir": str(run_output_dir),
            "backlog_completed": True,
            "decision": "PASSED",
            "corrections": corrections,
            "error": None,
        }
        await on_event(
            {
                "type": "complete",
                "stage": "pipeline",
                "program_name": "MULTI",
                "program_names": module_names,
                "decision": "PASSED",
                "output_dir": str(run_output_dir),
                "version": 1,
                "run_id": run_id,
                "session_status": SessionStatus.BACKLOG_COMPLETED.value,
            }
        )
        return summary, 0

    if dispatched is None:
        summary = {
            "success": False,
            "run_id": run_id,
            "task_id": run_id,
            "session_id": None,
            "session_status": SessionStatus.BLOCKED.value,
            "dispatched_task": None,
            "remaining_tasks": _remaining_tasks(state),
            "next_task_id": None,
            "backlog_path": str(backlog_store.backlog_path),
            "evidence_root": str(backlog_store.evidence_root),
            "output_dir": str(run_output_dir),
            "backlog_completed": False,
            "decision": "BLOCKED",
            "corrections": corrections,
            "error": "no dispatchable task",
        }
        return summary, 1

    session_id = f"session-{uuid.uuid4().hex[:10]}"
    backlog_store.update_task_mutable(
        state,
        dispatched.task_id,
        dispatcher.to_running(dispatched, session_id),
    )
    backlog_store.append_dispatch_log(
        {
            "timestamp": now_iso(),
            "run_id": run_id,
            "session_id": session_id,
            "task_id": dispatched.task_id,
            "module": dispatched.module,
            "stage": dispatched.stage,
            "dependencies": dispatched.dependencies,
        }
    )

    source_code = modules_by_name.get(dispatched.module, "")
    program_task_id = f"{run_id}-{dispatched.module.lower()}"
    module_root = run_output_dir / dispatched.module
    module_root.mkdir(parents=True, exist_ok=True)

    await on_event(
        {
            "type": "session_start",
            "stage": dispatched.stage,
            "run_id": run_id,
            "session_id": session_id,
            "backlog_task_id": dispatched.task_id,
            "program_name": dispatched.module,
            "message": f"{dispatched.task_id} started",
        }
    )
    await on_event(
        {
            "type": "stage_start",
            "stage": dispatched.stage,
            "program_name": dispatched.module,
            "message": f"{dispatched.stage} 実行中...",
        }
    )

    preflight_runner = PreflightRunner()
    preflight = preflight_runner.run(
        stage=dispatched.stage,
        module=dispatched.module,
        source_code=source_code,
        run_root=run_output_dir,
        health_url=str(options.get("health_url") or ""),
    )
    if not preflight.ok:
        notes = [f"preflight failed: {item.name}={item.detail}" for item in preflight.checks if not item.ok]
        unknowns = [{"field": "preflight", "reason": note} for note in notes]
        backlog_store.update_task_mutable(
            state,
            dispatched.task_id,
            dispatcher.to_blocked(
                notes=notes,
                unknowns=unknowns,
                session_id=session_id,
            ),
        )
        backlog_store.append_failure_log(
            {
                "timestamp": now_iso(),
                "run_id": run_id,
                "session_id": session_id,
                "task_id": dispatched.task_id,
                "reason": "preflight_failed",
                "checks": preflight.to_dict(),
            }
        )
        await on_event(
            {
                "type": "error",
                "stage": dispatched.stage,
                "program_name": dispatched.module,
                "message": "preflight failed",
                "checks": preflight.to_dict(),
            }
        )
        session_status = SessionStatus.BLOCKED
        stage_result: dict[str, Any] = {
            "success": False,
            "stage": dispatched.stage,
            "module": dispatched.module,
            "artifact_paths": {},
            "decision": "BLOCKED",
            "unknowns": unknowns,
            "evidence": {"preflight": preflight.to_dict()},
            "error": "preflight failed",
        }
        manifest_paths: list[str] = []
    else:
        engine = CodeMigrationEngine(migration_type=migration_type)
        await engine.initialize()
        stage_result = await execute_stage_task(
            engine,
            {
                "stage": dispatched.stage,
                "source_code": source_code,
                "task_id": run_id,
                "trace_id": f"{run_id}:{session_id}",
                "module": dispatched.module,
                "program_task_id": program_task_id,
                "module_root": str(module_root),
                "fast_mode": fast_mode,
                "expected_outputs": payload.get("expected_outputs", {}),
                "options": options,
            },
        )

        evidence_gate = EvidenceGate()
        evidence_ok, manifest = evidence_gate.evaluate(
            stage=dispatched.stage,
            backlog_task_id=dispatched.task_id,
            stage_result=stage_result,
            evidence_root=backlog_store.evidence_root,
        )
        manifest_paths = [str(manifest.get("manifest_path", ""))] if manifest.get("manifest_path") else []
        stage_artifact_paths_raw = stage_result.get("artifact_paths", {})
        if isinstance(stage_artifact_paths_raw, dict):
            manifest_paths.extend(
                [path for path in stage_artifact_paths_raw.values() if isinstance(path, str) and path]
            )

        unknowns_raw = stage_result.get("unknowns", [])
        unknowns = unknowns_raw if isinstance(unknowns_raw, list) else []
        notes = []
        if not evidence_ok:
            notes = list(manifest.get("missing", []))
        elif not bool(stage_result.get("success", False)):
            notes = [str(stage_result.get("error", "stage execution failed"))]

        if not evidence_ok or not bool(stage_result.get("success", False)):
            backlog_store.update_task_mutable(
                state,
                dispatched.task_id,
                dispatcher.to_blocked(
                    notes=notes,
                    unknowns=[item for item in unknowns if isinstance(item, dict)],
                    session_id=session_id,
                ),
            )
            backlog_store.append_failure_log(
                {
                    "timestamp": now_iso(),
                    "run_id": run_id,
                    "session_id": session_id,
                    "task_id": dispatched.task_id,
                    "reason": "evidence_or_stage_failure",
                    "result": stage_result,
                    "missing_evidence": manifest.get("missing", []),
                }
            )
            session_status = SessionStatus.BLOCKED
        else:
            backlog_store.update_task_mutable(
                state,
                dispatched.task_id,
                dispatcher.to_done(
                    evidence_paths=manifest_paths,
                    notes=notes,
                    unknowns=[item for item in unknowns if isinstance(item, dict)],
                    session_id=session_id,
                ),
            )
            decision = str(stage_result.get("decision") or "")
            session_status = SessionStatus.DONE
            if dispatched.stage == "quality" and decision and decision not in _PASS_DECISIONS:
                session_status = SessionStatus.NEEDS_FIX
            if dispatched.stage == "diff" and fast_mode and len(unknowns) > 0:
                session_status = SessionStatus.NEEDS_FIX
                strict_task = backlog_store.add_task(
                    state,
                    ImmutableTaskFields(
                        module=dispatched.module,
                        stage="strict_verification",
                        description=f"{dispatched.module} / strict_verification",
                        acceptance_criteria=["strict differential artifact exists"],
                        dependencies=[dispatched.task_id],
                    ),
                )
                quality_task_id = f"{dispatched.module}:quality"
                quality_task = state.get_task(quality_task_id)
                if quality_task is not None:
                    deps = [dep for dep in quality_task.dependencies if dep != dispatched.task_id]
                    if strict_task.task_id not in deps:
                        deps.append(strict_task.task_id)
                    backlog_store.replace_dependencies(state, quality_task_id, deps)
                backlog_store.append_decision_log(
                    {
                        "timestamp": now_iso(),
                        "run_id": run_id,
                        "session_id": session_id,
                        "task_id": dispatched.task_id,
                        "decision": "strict_verification_added",
                        "module": dispatched.module,
                    }
                )
            backlog_store.append_decision_log(
                {
                    "timestamp": now_iso(),
                    "run_id": run_id,
                    "session_id": session_id,
                    "task_id": dispatched.task_id,
                    "stage": dispatched.stage,
                    "decision": stage_result.get("decision"),
                    "artifact_paths": stage_result.get("artifact_paths", {}),
                }
            )

    state = backlog_store.load()
    next_task = dispatcher.select_next_task(state)
    remaining_tasks = _remaining_tasks(state)
    backlog_completed = _is_backlog_completed(state)
    next_task_id = next_task.task_id if next_task is not None else None

    backlog_store.append_progress(
        f"- {now_iso()} run={run_id} session={session_id} task={dispatched.task_id} "
        f"status={session_status.value} next={next_task_id or '-'} remaining={remaining_tasks}"
    )

    stage_decision = str(stage_result.get("decision") or "")
    await on_event(
        {
            "type": "stage_complete",
            "stage": dispatched.stage,
            "program_name": dispatched.module,
            "decision": stage_decision or session_status.value.upper(),
            "run_id": run_id,
            "session_id": session_id,
            "backlog_task_id": dispatched.task_id,
        }
    )
    await on_event(
        {
            "type": "session_complete",
            "stage": dispatched.stage,
            "run_id": run_id,
            "session_id": session_id,
            "backlog_task_id": dispatched.task_id,
            "session_status": session_status.value,
            "remaining_tasks": remaining_tasks,
            "next_task_id": next_task_id,
        }
    )

    if backlog_completed:
        await on_event(
            {
                "type": "complete",
                "stage": "pipeline",
                "program_name": dispatched.module if len(module_names) == 1 else "MULTI",
                "program_names": module_names,
                "decision": "PASSED",
                "output_dir": str(run_output_dir),
                "version": 1,
                "run_id": run_id,
                "session_status": SessionStatus.BACKLOG_COMPLETED.value,
            }
        )

    summary = {
        "success": session_status in {SessionStatus.DONE, SessionStatus.BACKLOG_COMPLETED},
        "run_id": run_id,
        "task_id": run_id,
        "session_id": session_id,
        "session_status": (SessionStatus.BACKLOG_COMPLETED.value if backlog_completed else session_status.value),
        "dispatched_task": {
            "task_id": dispatched.task_id,
            "module": dispatched.module,
            "stage": dispatched.stage,
        },
        "remaining_tasks": remaining_tasks,
        "next_task_id": next_task_id,
        "backlog_path": str(backlog_store.backlog_path),
        "evidence_root": str(backlog_store.evidence_root),
        "output_dir": str(run_output_dir),
        "backlog_completed": backlog_completed,
        "decision": stage_decision or ("PASSED" if backlog_completed else session_status.value.upper()),
        "artifact_paths": stage_result.get("artifact_paths", {}),
        "corrections": corrections,
        "error": stage_result.get("error"),
    }

    effective_status = SessionStatus.BACKLOG_COMPLETED if backlog_completed else session_status
    return summary, _session_exit_code(effective_status)


async def migrate_cobol_file(file_path: str) -> dict[str, Any]:
    """既存互換用の単一ファイル移行ヘルパー."""
    payload: dict[str, Any] = {
        "task_id": f"legacy-{uuid.uuid4().hex[:8]}",
        "source_path": str(Path(file_path).resolve()),
        "output_root": str((Path.cwd() / "migration_output").resolve()),
        "fast_mode": True,
        "migration_type": "cobol-to-java",
        "options": {},
    }

    async def _ignore_event(_event: dict[str, Any]) -> None:
        return None

    last_summary, _ = await _run_session_loop(payload, on_event=_ignore_event, max_sessions=200)

    output_dir_raw = last_summary.get("output_dir")
    output_dir = Path(output_dir_raw) if isinstance(output_dir_raw, str) else Path()
    dispatched_task_raw = last_summary.get("dispatched_task", {})
    module = "UNKNOWN"
    if isinstance(dispatched_task_raw, dict):
        module_raw = dispatched_task_raw.get("module")
        if isinstance(module_raw, str) and module_raw:
            module = module_raw
    run_id = str(payload["task_id"])
    program_task_id = f"{run_id}-{module.lower()}"
    java_path = output_dir / module / "code" / f"{program_task_id}_target_code.java"
    java_code = java_path.read_text(encoding="utf-8") if java_path.exists() else ""

    return {
        "success": bool(last_summary.get("backlog_completed", False)),
        "class_name": "MigratedProgram",
        "score": 0.0,
        "iterations": 1,
        "is_acceptable": bool(last_summary.get("backlog_completed", False)),
        "java_code": java_code,
        "feedback": [],
        "errors": []
        if last_summary.get("backlog_completed", False)
        else [str(last_summary.get("error", "migration failed"))],
    }


async def _run_contract_command(args: argparse.Namespace) -> int:
    input_path = Path(args.input).resolve()
    output_path = Path(args.output).resolve()
    events_path = Path(args.events).resolve()

    if not input_path.exists():
        _write_json(output_path, {"success": False, "error": f"input file not found: {input_path}"})
        return 2

    try:
        payload_raw = json.loads(input_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        _write_json(output_path, {"success": False, "error": f"invalid input json: {exc}"})
        return 2
    if not isinstance(payload_raw, dict):
        _write_json(output_path, {"success": False, "error": "input json root must be object"})
        return 2

    events_path.parent.mkdir(parents=True, exist_ok=True)
    if events_path.exists():
        events_path.unlink()

    async def _event_writer(event: dict[str, Any]) -> None:
        _append_event(events_path, event)

    summary, exit_code = await run_contract_payload(payload_raw, on_event=_event_writer)
    _write_json(output_path, summary)
    return exit_code


async def _run_migrate_command(args: argparse.Namespace) -> int:
    source_path = Path(args.source).resolve()
    output_root = Path(args.output).resolve()

    payload: dict[str, Any] = {
        "task_id": f"cli-{uuid.uuid4().hex[:8]}",
        "source_path": str(source_path),
        "output_root": str(output_root),
        "fast_mode": bool(args.fast),
        "migration_type": "cobol-to-java",
        "model": args.model,
        "options": {},
    }

    async def _print_event(event: dict[str, Any]) -> None:
        event_type = str(event.get("type", ""))
        stage = str(event.get("stage", ""))
        message = str(event.get("message", event.get("decision", "")))
        if event_type in {"stage_start", "stage_complete"}:
            print(f"[{event_type}] {stage} {message}")
        elif event_type == "complete":
            print(f"✅ [complete] decision={event.get('decision')} output={event.get('output_dir')}")
        elif event_type == "error":
            print(f"[error] {event.get('message')}", file=sys.stderr)

    summary, exit_code = await _run_session_loop(payload, on_event=_print_event, max_sessions=5000)

    if summary.get("backlog_completed"):
        print(json.dumps(summary, ensure_ascii=False, indent=2))
        return 0
    print(json.dumps(summary, ensure_ascii=False, indent=2), file=sys.stderr)
    return exit_code


def cmd_migrate(args: argparse.Namespace) -> int:
    """Human-friendly sync wrapper over the engine/backlog runtime."""
    source_path = Path(args.source).resolve()
    output_root = Path(args.output).resolve()
    model = args.model or os.environ.get("MIGRATION_MODEL", "platform_text_default")

    print("\n🚀 Code Migration Assistant")
    print(f"  ソース: {source_path}")
    print(f"  出力:   {output_root}")
    print(f"  モード: {'高速（実行比較スキップ）' if args.fast else '通常'}")
    print(f"  モデル: {model}")
    print()

    with tempfile.TemporaryDirectory(prefix="migration_") as work_dir_str:
        work_dir = Path(work_dir_str)
        project = COBOLProject(source=source_path, work_dir=work_dir)
        try:
            project.setup()
        except (FileNotFoundError, ValueError) as exc:
            print(f"エラー: {exc}", file=sys.stderr)
            return 1

        cobol_files = project.get_cobol_files()
        if not cobol_files:
            print("エラー: 変換対象のCOBOLファイルが見つかりませんでした。", file=sys.stderr)
            return 1

        print(f"変換対象: {len(cobol_files)} ファイル")
        for cobol_file in cobol_files:
            print(f"  - {cobol_file.program_name} ({cobol_file.relative_path})")
        print()

    payload: dict[str, Any] = {
        "task_id": f"cli-{uuid.uuid4().hex[:8]}",
        "source_path": str(source_path),
        "output_root": str(output_root),
        "fast_mode": bool(args.fast),
        "migration_type": "cobol-to-java",
        "model": model,
        "options": {},
    }

    async def _print_event(event: dict[str, Any]) -> None:
        event_type = str(event.get("type", ""))
        stage = str(event.get("stage", ""))
        message = str(event.get("message", event.get("decision", "")))
        if event_type in {"stage_start", "stage_complete"}:
            print(f"[{event_type}] {stage} {message}")
        elif event_type == "complete":
            print(f"✅ [complete] decision={event.get('decision')} output={event.get('output_dir')}")
        elif event_type == "error":
            print(f"[error] {event.get('message')}", file=sys.stderr)

    summary, exit_code = asyncio.run(_run_session_loop(payload, on_event=_print_event, max_sessions=5000))
    if summary.get("decision") == "ENV_ISSUE" and not bool(args.fast):
        print("ヒント: --fast フラグを使うと実行比較をスキップできます。")
    if summary.get("backlog_completed"):
        print(json.dumps(summary, ensure_ascii=False, indent=2))
        return 0
    print(json.dumps(summary, ensure_ascii=False, indent=2), file=sys.stderr)
    return exit_code


def _print_progress_cli(event: Any) -> None:
    """CLIにイベント進捗を表示する."""
    icon_map = {
        "stage_start": "▶",
        "stage_complete": "✓",
        "evolution": "↺",
        "complete": "✅",
        "error": "✗",
    }
    event_type = str(getattr(event, "event_type", ""))
    icon = icon_map.get(event_type, "·")
    stage = str(getattr(event, "stage", "") or "")
    data = getattr(event, "data", {})
    msg = ""
    if isinstance(data, dict):
        msg = str(data.get("message", "") or data.get("decision", ""))
    print(f"  {icon} [{stage}] {msg}")


def cmd_show(args: argparse.Namespace) -> int:
    """show コマンドを実行する（プログラムの成果物詳細表示）."""
    from apps.code_migration_assistant.output.organizer import OutputOrganizer

    output_root = Path(args.output).resolve()
    program_name = str(args.program).upper()

    organizer = OutputOrganizer(output_root)
    program_dir = organizer.get_program_dir(program_name)

    if not program_dir.exists():
        print(f"エラー: プログラムが見つかりません: {program_name}", file=sys.stderr)
        print(f"  検索パス: {program_dir}", file=sys.stderr)
        return 1

    version_count = organizer.get_version_count(program_name)
    print(f"\n📦 {program_name}  ({version_count} バージョン)")
    print(f"   ディレクトリ: {program_dir}")

    stage_dirs = {
        "analyzer": "01_analysis/analyzer.json",
        "designer": "02_design/designer.json",
        "transformer": "03_transform/transformer.json",
        "test_generator": "03_transform/test_generator.json",
        "verifier": "04_verification/verifier.json",
        "quality_gate": "04_verification/quality_gate.json",
    }
    for version in range(1, version_count + 1):
        version_dir = organizer.get_version_dir(program_name, version)
        print(f"\n  ── v{version} ──────────────────────")
        for stage, rel_path in stage_dirs.items():
            artifact = version_dir / rel_path
            if artifact.exists():
                try:
                    data = json.loads(artifact.read_text(encoding="utf-8"))
                except Exception:
                    data = {}
                decision = data.get("decision") if isinstance(data, dict) else None
                summary = f" → {decision}" if isinstance(decision, str) and decision else ""
                print(f"    ✓ {stage}{summary}")
            else:
                print(f"    · {stage}  (未実行)")

        report = version_dir / "05_report" / "report.md"
        if report.exists():
            print(f"    ✓ report  ({report})")

        java_dir = version_dir / "03_transform" / "src"
        if java_dir.exists():
            java_files = list(java_dir.rglob("*.java"))
            print(f"    📝 Java ファイル: {len(java_files)} 個")
            if bool(args.verbose):
                for file_path in sorted(java_files):
                    print(f"       {file_path.relative_to(version_dir)}")

    evolution_file = program_dir / "evolution.json"
    if evolution_file.exists():
        try:
            evolution = json.loads(evolution_file.read_text(encoding="utf-8"))
            total = int(evolution.get("total_iterations", 0))
            print(f"\n  🔄 Evolution 履歴: {total} 回")
        except Exception:
            print("\n  🔄 Evolution 履歴: 読み込み失敗")

    print()
    return 0


def cmd_list(args: argparse.Namespace) -> int:
    """list コマンドを実行する（移行成果物一覧表示）."""
    output_root = Path(args.output).resolve()
    if not output_root.exists():
        print(f"エラー: 出力ディレクトリが存在しません: {output_root}", file=sys.stderr)
        return 1

    candidate_dirs = [item for item in output_root.iterdir() if item.is_dir() and not item.name.startswith("_")]
    programs: list[Path] = []
    for program_dir in candidate_dirs:
        has_version = any(
            child.is_dir() and child.name.startswith("v") and child.name[1:].isdigit()
            for child in program_dir.iterdir()
        )
        if has_version:
            programs.append(program_dir)
    programs.sort()
    if not programs:
        print("移行済みプログラムはありません。")
        return 0

    print("\n📚 移行済みプログラム一覧")
    for program_dir in programs:
        versions = [item for item in program_dir.iterdir() if item.is_dir() and item.name.startswith("v")]
        version_count = len(versions)
        latest = max((item.name for item in versions), default="-")
        report_marker = "·"
        if version_count > 0:
            latest_dir = program_dir / latest
            report_path = latest_dir / "05_report" / "report.md"
            report_marker = "✓" if report_path.exists() else "·"
        print(f"  - {program_dir.name}: {version_count} バージョン (latest={latest}) report={report_marker}")
    print()
    return 0


def cmd_retry(args: argparse.Namespace) -> int:
    """retry コマンドを実行する（engine/backlog 上で特定ステージから再開）."""
    from apps.code_migration_assistant.output.organizer import OutputOrganizer

    source_path = Path(args.source).resolve()
    output_root = Path(args.output).resolve()
    model = args.model or os.environ.get("MIGRATION_MODEL", "platform_text_default")
    start_stage = str(args.stage)

    valid_stages = ["analyzer", "designer", "transformer", "test_generator", "verifier", "quality_gate"]
    if start_stage not in valid_stages:
        print(f"エラー: 無効なステージ名: {start_stage}", file=sys.stderr)
        print(f"  有効なステージ: {', '.join(valid_stages)}", file=sys.stderr)
        return 1

    print("\n🔄 Code Migration Assistant — ステージ再実行")
    print(f"  ソース: {source_path}")
    print(f"  出力:   {output_root}")
    print(f"  再実行ステージ: {start_stage} 以降")
    print(f"  モデル: {model}")
    print()

    with tempfile.TemporaryDirectory(prefix="migration_retry_") as work_dir_str:
        work_dir = Path(work_dir_str)
        project = COBOLProject(source=source_path, work_dir=work_dir)
        try:
            project.setup()
        except (FileNotFoundError, ValueError) as exc:
            print(f"エラー: {exc}", file=sys.stderr)
            return 1

        cobol_files = project.get_cobol_files()
        if not cobol_files:
            print("エラー: 変換対象のCOBOLファイルが見つかりませんでした。", file=sys.stderr)
            return 1

        organizer = OutputOrganizer(output_root)
        missing_programs: list[str] = []
        for cobol_file in cobol_files:
            version = organizer.get_version_count(cobol_file.program_name)
            if version == 0:
                missing_programs.append(str(cobol_file.program_name))

        if missing_programs:
            for program_name in missing_programs:
                print(
                    f"  ⚠  {program_name}: 既存バージョンがありません。migrate を先に実行してください。",
                    file=sys.stderr,
                )
            return 1

    payload: dict[str, Any] = {
        "task_id": f"retry-{uuid.uuid4().hex[:8]}",
        "source_path": str(source_path),
        "output_root": str(output_root),
        "fast_mode": bool(args.fast),
        "migration_type": "cobol-to-java",
        "model": model,
        "resume_from_stage": _RETRY_STAGE_MAP[start_stage],
        "options": {},
    }

    async def _print_event(event: dict[str, Any]) -> None:
        event_type = str(event.get("type", ""))
        stage = str(event.get("stage", ""))
        message = str(event.get("message", event.get("decision", "")))
        if event_type in {"stage_start", "stage_complete"}:
            print(f"[{event_type}] {stage} {message}")
        elif event_type == "complete":
            print(f"✅ [complete] decision={event.get('decision')} output={event.get('output_dir')}")
        elif event_type == "error":
            print(f"[error] {event.get('message')}", file=sys.stderr)

    summary, exit_code = asyncio.run(_run_session_loop(payload, on_event=_print_event, max_sessions=5000))
    if summary.get("backlog_completed"):
        print(json.dumps(summary, ensure_ascii=False, indent=2))
        return 0
    print(json.dumps(summary, ensure_ascii=False, indent=2), file=sys.stderr)
    return exit_code


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="code_migration_assistant",
        description="Code Migration Assistant CLI",
    )
    subparsers = parser.add_subparsers(dest="command")

    run_parser = subparsers.add_parser(
        "run",
        help="JSON入出力契約で非対話実行する",
    )
    run_parser.add_argument("--input", required=True, help="入力JSONファイル")
    run_parser.add_argument("--output", required=True, help="出力JSONファイル")
    run_parser.add_argument("--events", required=True, help="NDJSONイベント出力ファイル")

    migrate_parser = subparsers.add_parser(
        "migrate",
        help="単体CLI実行（人間向け）",
    )
    migrate_parser.add_argument("source", help="COBOLファイル/zip/ディレクトリ")
    migrate_parser.add_argument(
        "--output",
        "-o",
        default="migration_output",
        help="成果物出力ディレクトリ",
    )
    migrate_parser.add_argument(
        "--fast",
        action="store_true",
        help="高速モード（実行比較をスキップ）",
    )
    migrate_parser.add_argument(
        "--model",
        default="platform_text_default",
        help="使用モデル識別子（オプション情報として保存）",
    )

    show_parser = subparsers.add_parser(
        "show",
        help="プログラム成果物の詳細を表示する",
    )
    show_parser.add_argument("program", help="プログラム名（大小文字不問）")
    show_parser.add_argument(
        "--output",
        "-o",
        default="migration_output",
        help="成果物出力ディレクトリ",
    )
    show_parser.add_argument(
        "--verbose",
        action="store_true",
        help="Javaファイル一覧まで表示する",
    )

    list_parser = subparsers.add_parser(
        "list",
        help="移行済みプログラム一覧を表示する",
    )
    list_parser.add_argument(
        "--output",
        "-o",
        default="migration_output",
        help="成果物出力ディレクトリ",
    )

    retry_parser = subparsers.add_parser(
        "retry",
        help="既存成果物を使って特定ステージから再実行する",
    )
    retry_parser.add_argument("source", help="COBOLファイル/zip/ディレクトリ")
    retry_parser.add_argument(
        "--stage",
        required=True,
        help="再開ステージ: analyzer/designer/transformer/test_generator/verifier/quality_gate",
    )
    retry_parser.add_argument(
        "--output",
        "-o",
        default="migration_output",
        help="成果物出力ディレクトリ",
    )
    retry_parser.add_argument(
        "--fast",
        action="store_true",
        help="高速モード（実行比較をスキップ）",
    )
    retry_parser.add_argument(
        "--model",
        default="platform_text_default",
        help="使用モデル識別子",
    )

    return parser


async def _async_main(argv: list[str] | None = None) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)

    if args.command == "run":
        return await _run_contract_command(args)
    if args.command == "migrate":
        return await _run_migrate_command(args)
    if args.command == "show":
        return await asyncio.to_thread(cmd_show, args)
    if args.command == "list":
        return await asyncio.to_thread(cmd_list, args)
    if args.command == "retry":
        return await asyncio.to_thread(cmd_retry, args)

    parser.print_help()
    return 2


def main(argv: list[str] | None = None) -> None:
    exit_code = asyncio.run(_async_main(argv))
    raise SystemExit(exit_code)


if __name__ == "__main__":
    main()
