"""Long-running backlog/session workflow tests."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import pytest

from apps.code_migration_assistant import cli
from apps.code_migration_assistant.workflow.backlog_models import (
    BacklogTaskStatus,
    ImmutableTaskFields,
)
from apps.code_migration_assistant.workflow.backlog_store import BacklogStore
from apps.code_migration_assistant.workflow.dispatcher import BacklogDispatcher
from apps.code_migration_assistant.workflow.evidence_gate import EvidenceGate
from apps.code_migration_assistant.workflow.preflight import PreflightCheck, PreflightReport


def _write_sample_cobol(path: Path) -> None:
    path.write_text(
        "\n".join(
            [
                "       IDENTIFICATION DIVISION.",
                "       PROGRAM-ID. SAMPLE.",
                "       PROCEDURE DIVISION.",
                "           DISPLAY 'HELLO'.",
                "           STOP RUN.",
            ]
        ),
        encoding="utf-8",
    )


def _ok_preflight(*_args: Any, **_kwargs: Any) -> PreflightReport:
    return PreflightReport(
        ok=True,
        checks=[PreflightCheck(name="smoke", ok=True, detail="ok")],
    )


def _ng_preflight(*_args: Any, **_kwargs: Any) -> PreflightReport:
    return PreflightReport(
        ok=False,
        checks=[PreflightCheck(name="dependency_java", ok=False, detail="java command not found")],
    )


def _read_jsonl(path: Path) -> list[dict[str, Any]]:
    if not path.exists():
        return []
    lines = [line.strip() for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]
    out: list[dict[str, Any]] = []
    for line in lines:
        obj = json.loads(line)
        if isinstance(obj, dict):
            out.append(obj)
    return out


def test_immutable_autocorrect_records_audit_log(tmp_path: Path) -> None:
    store = BacklogStore(tmp_path / "run")
    state = store.initialize_if_missing(
        run_id="run-1",
        source_path="/tmp/source.cbl",
        output_root=str(tmp_path / "output"),
        migration_type="cobol-to-java",
        fast_mode=True,
        modules=["SAMPLE"],
    )

    task = state.get_task("SAMPLE:analysis")
    assert task is not None
    task.immutable.description = "tampered"
    store.save(state)

    corrections = store.enforce_immutability(store.load())
    assert any(item.get("action") == "immutable_autocorrect" for item in corrections)

    repaired = store.load().get_task("SAMPLE:analysis")
    assert repaired is not None
    assert repaired.immutable.description == "SAMPLE / analysis"
    assert store.mutation_log_path.exists()


def test_immutable_uncorrectable_marks_task_blocked(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    store = BacklogStore(tmp_path / "run")
    state = store.initialize_if_missing(
        run_id="run-2",
        source_path="/tmp/source.cbl",
        output_root=str(tmp_path / "output"),
        migration_type="cobol-to-java",
        fast_mode=True,
        modules=["SAMPLE"],
    )
    task = state.get_task("SAMPLE:analysis")
    assert task is not None
    task.immutable.description = "tampered"
    store.save(state)

    def _raise_model_copy(
        _self: ImmutableTaskFields,
        *_args: Any,
        **_kwargs: Any,
    ) -> ImmutableTaskFields:
        message = "snapshot broken"
        raise RuntimeError(message)

    monkeypatch.setattr(ImmutableTaskFields, "model_copy", _raise_model_copy, raising=True)

    corrections = store.enforce_immutability(store.load())
    assert any(item.get("action") == "immutable_uncorrectable" for item in corrections)

    blocked = store.load().get_task("SAMPLE:analysis")
    assert blocked is not None
    assert blocked.status == BacklogTaskStatus.BLOCKED
    assert any("immutable correction failed" in note for note in blocked.notes)
    failures = _read_jsonl(store.failure_log_path)
    assert any(item.get("reason") == "immutable_uncorrectable" for item in failures)


def test_dispatcher_selects_one_task_and_skips_unresolved_dependencies(tmp_path: Path) -> None:
    store = BacklogStore(tmp_path / "run")
    state = store.initialize_if_missing(
        run_id="run-3",
        source_path="/tmp/source.cbl",
        output_root=str(tmp_path / "output"),
        migration_type="cobol-to-java",
        fast_mode=True,
        modules=["A", "B"],
    )
    dispatcher = BacklogDispatcher()

    first = dispatcher.select_next_task(state)
    assert first is not None
    assert first.task_id == "A:analysis"

    store.update_task_mutable(
        state,
        "A:analysis",
        {"status": BacklogTaskStatus.BLOCKED},
    )
    next_state = store.load()
    second = dispatcher.select_next_task(next_state)
    assert second is not None
    assert second.task_id == "B:analysis"


def test_evidence_gate_rejects_missing_artifacts(tmp_path: Path) -> None:
    gate = EvidenceGate()
    ok, manifest = gate.evaluate(
        stage="analysis",
        backlog_task_id="SAMPLE:analysis",
        stage_result={
            "success": True,
            "artifact_paths": {"analysis": str(tmp_path / "missing.json")},
            "unknowns": [],
            "decision": "PASSED",
            "evidence": {},
        },
        evidence_root=tmp_path / "evidence",
    )
    assert ok is False
    assert manifest["missing"]
    assert Path(str(manifest["manifest_path"])).exists()


@pytest.mark.asyncio
async def test_fast_mode_unknowns_create_strict_followup_task(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    sample = tmp_path / "sample.cbl"
    _write_sample_cobol(sample)

    modules = {"SAMPLE": sample.read_text(encoding="utf-8")}
    monkeypatch.setattr(cli, "_load_cobol_modules", lambda _path: (modules, ["SAMPLE"]))
    monkeypatch.setattr(cli.PreflightRunner, "run", _ok_preflight)

    async def _noop_init(self: Any) -> None:
        return None

    monkeypatch.setattr(cli.CodeMigrationEngine, "_initialize", _noop_init)

    run_root = tmp_path / "output" / "task-fast"
    backlog_store = BacklogStore(run_root)
    state = backlog_store.initialize_if_missing(
        run_id="task-fast",
        source_path=str(sample),
        output_root=str(tmp_path / "output"),
        migration_type="cobol-to-java",
        fast_mode=True,
        modules=["SAMPLE"],
    )
    for task_id in (
        "SAMPLE:analysis",
        "SAMPLE:business_semantics",
        "SAMPLE:design",
        "SAMPLE:transform",
        "SAMPLE:tests",
    ):
        backlog_store.update_task_mutable(
            state,
            task_id,
            {"status": BacklogTaskStatus.DONE},
        )

    diff_path = tmp_path / "diff.json"
    diff_path.write_text("{}", encoding="utf-8")

    async def _stage_stub(_engine: Any, _inputs: dict[str, Any]) -> dict[str, Any]:
        return {
            "success": True,
            "stage": "diff",
            "module": "SAMPLE",
            "artifact_paths": {"diff": str(diff_path)},
            "unknowns": [{"field": "runtime_compare", "reason": "fast mode skipped strict run"}],
            "decision": "PASSED",
            "evidence": {"comparison": "fast_only"},
        }

    monkeypatch.setattr(cli, "execute_stage_task", _stage_stub)

    async def _ignore_event(_event: dict[str, Any]) -> None:
        return None

    summary, exit_code = await cli.run_contract_payload(
        {
            "task_id": "task-fast",
            "source_path": str(sample),
            "output_root": str(tmp_path / "output"),
            "fast_mode": True,
            "options": {},
        },
        on_event=_ignore_event,
    )

    assert exit_code == 1
    assert summary["session_status"] == "needs_fix"
    assert summary["next_task_id"] == "SAMPLE:strict_verification"

    state_after = backlog_store.load()
    strict_task = state_after.get_task("SAMPLE:strict_verification")
    quality_task = state_after.get_task("SAMPLE:quality")
    assert strict_task is not None
    assert quality_task is not None
    assert strict_task.dependencies == ["SAMPLE:diff"]
    assert quality_task.dependencies == ["SAMPLE:strict_verification"]


@pytest.mark.asyncio
async def test_preflight_failure_blocks_session_and_writes_failure_log(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    sample = tmp_path / "sample.cbl"
    _write_sample_cobol(sample)

    modules = {"SAMPLE": sample.read_text(encoding="utf-8")}
    monkeypatch.setattr(cli, "_load_cobol_modules", lambda _path: (modules, ["SAMPLE"]))
    monkeypatch.setattr(cli.PreflightRunner, "run", _ng_preflight)

    async def _noop_init(self: Any) -> None:
        return None

    monkeypatch.setattr(cli.CodeMigrationEngine, "_initialize", _noop_init)

    async def _should_not_run(_engine: Any, _inputs: dict[str, Any]) -> dict[str, Any]:
        pytest.fail("execute_stage_task should not run when preflight fails")

    monkeypatch.setattr(cli, "execute_stage_task", _should_not_run)

    async def _ignore_event(_event: dict[str, Any]) -> None:
        return None

    summary, exit_code = await cli.run_contract_payload(
        {
            "task_id": "task-preflight",
            "source_path": str(sample),
            "output_root": str(tmp_path / "output"),
            "fast_mode": True,
            "options": {},
        },
        on_event=_ignore_event,
    )

    assert exit_code == 1
    assert summary["session_status"] == "blocked"
    assert summary["dispatched_task"]["task_id"] == "SAMPLE:analysis"

    backlog_store = BacklogStore(tmp_path / "output" / "task-preflight")
    blocked_task = backlog_store.load().get_task("SAMPLE:analysis")
    assert blocked_task is not None
    assert blocked_task.status == BacklogTaskStatus.BLOCKED

    failures = _read_jsonl(backlog_store.failure_log_path)
    assert any(item.get("reason") == "preflight_failed" for item in failures)


@pytest.mark.asyncio
async def test_missing_evidence_keeps_task_not_done(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    sample = tmp_path / "sample.cbl"
    _write_sample_cobol(sample)

    modules = {"SAMPLE": sample.read_text(encoding="utf-8")}
    monkeypatch.setattr(cli, "_load_cobol_modules", lambda _path: (modules, ["SAMPLE"]))
    monkeypatch.setattr(cli.PreflightRunner, "run", _ok_preflight)

    async def _noop_init(self: Any) -> None:
        return None

    monkeypatch.setattr(cli.CodeMigrationEngine, "_initialize", _noop_init)

    async def _stage_stub(_engine: Any, _inputs: dict[str, Any]) -> dict[str, Any]:
        return {
            "success": True,
            "stage": "analysis",
            "module": "SAMPLE",
            "artifact_paths": {"analysis": str(tmp_path / "missing_artifact.json")},
            "unknowns": [],
            "decision": "PASSED",
            "evidence": {},
        }

    monkeypatch.setattr(cli, "execute_stage_task", _stage_stub)

    async def _ignore_event(_event: dict[str, Any]) -> None:
        return None

    summary, exit_code = await cli.run_contract_payload(
        {
            "task_id": "task-missing-evidence",
            "source_path": str(sample),
            "output_root": str(tmp_path / "output"),
            "fast_mode": True,
            "options": {},
        },
        on_event=_ignore_event,
    )

    assert exit_code == 1
    assert summary["session_status"] == "blocked"
    backlog_store = BacklogStore(tmp_path / "output" / "task-missing-evidence")
    blocked_task = backlog_store.load().get_task("SAMPLE:analysis")
    assert blocked_task is not None
    assert blocked_task.status == BacklogTaskStatus.BLOCKED
