"""CMA CLI session contract tests."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pytest

from apps.code_migration_assistant import cli
from apps.code_migration_assistant.workflow.backlog_models import BacklogTaskStatus
from apps.code_migration_assistant.workflow.backlog_store import BacklogStore
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


@pytest.mark.asyncio
async def test_run_contract_payload_executes_single_backlog_task(
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

    artifact_path = tmp_path / "analysis.json"
    artifact_path.write_text("{}", encoding="utf-8")

    async def _stage_stub(_engine: Any, _inputs: dict[str, Any]) -> dict[str, Any]:
        return {
            "success": True,
            "stage": "analysis",
            "module": "SAMPLE",
            "artifact_paths": {"analysis": str(artifact_path)},
            "unknowns": [],
            "decision": "PASSED",
            "evidence": {"records": 1},
        }

    monkeypatch.setattr(cli, "execute_stage_task", _stage_stub)

    events: list[dict[str, Any]] = []

    async def _collect_event(event: dict[str, Any]) -> None:
        events.append(event)

    summary, exit_code = await cli.run_contract_payload(
        {
            "task_id": "task-success",
            "source_path": str(sample),
            "output_root": str(tmp_path / "output"),
            "fast_mode": True,
            "options": {},
        },
        on_event=_collect_event,
    )

    assert exit_code == 0
    assert summary["session_status"] == "done"
    assert summary["run_id"] == "task-success"
    assert summary["dispatched_task"]["task_id"] == "SAMPLE:analysis"
    assert summary["remaining_tasks"] > 0
    assert Path(summary["backlog_path"]).exists()
    assert Path(summary["evidence_root"]).exists()
    assert any(event.get("type") == "session_start" for event in events)
    assert any(event.get("type") == "stage_complete" for event in events)


@pytest.mark.asyncio
async def test_run_contract_payload_input_error(tmp_path: Path) -> None:
    events: list[dict[str, Any]] = []

    async def _collect_event(event: dict[str, Any]) -> None:
        events.append(event)

    summary, exit_code = await cli.run_contract_payload(
        {
            "task_id": "task-input-error",
            "output_root": str(tmp_path / "output"),
        },
        on_event=_collect_event,
    )

    assert exit_code == 2
    assert summary["success"] is False
    assert summary["session_status"] == "input_error"
    assert "source_path" in str(summary["error"])
    assert events == []


@pytest.mark.asyncio
async def test_run_contract_payload_quality_issue_returns_needs_fix(
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

    run_root = (tmp_path / "output" / "task-business-failure").resolve()
    backlog_store = BacklogStore(run_root)
    state = backlog_store.initialize_if_missing(
        run_id="task-business-failure",
        source_path=str(sample),
        output_root=str(tmp_path / "output"),
        migration_type="cobol-to-java",
        fast_mode=True,
        modules=["SAMPLE"],
    )
    for stage in ("analysis", "business_semantics", "design", "transform", "tests", "diff"):
        backlog_store.update_task_mutable(
            state,
            f"SAMPLE:{stage}",
            {"status": BacklogTaskStatus.DONE},
        )
    backlog_store.update_task_mutable(
        state,
        "SAMPLE:quality",
        {"status": BacklogTaskStatus.PENDING},
    )

    quality_artifact = tmp_path / "quality.json"
    quality_artifact.write_text("{}", encoding="utf-8")

    async def _quality_stub(_engine: Any, _inputs: dict[str, Any]) -> dict[str, Any]:
        return {
            "success": True,
            "stage": "quality",
            "module": "SAMPLE",
            "artifact_paths": {"quality": str(quality_artifact)},
            "unknowns": [],
            "decision": "TRANSFORM_ISSUE",
            "evidence": {"classification": "logic"},
        }

    monkeypatch.setattr(cli, "execute_stage_task", _quality_stub)

    async def _ignore_event(_event: dict[str, Any]) -> None:
        return None

    summary, exit_code = await cli.run_contract_payload(
        {
            "task_id": "task-business-failure",
            "source_path": str(sample),
            "output_root": str(tmp_path / "output"),
            "fast_mode": True,
            "options": {},
        },
        on_event=_ignore_event,
    )

    assert exit_code == 1
    assert summary["session_status"] == "needs_fix"
    assert summary["dispatched_task"]["task_id"] == "SAMPLE:quality"
    assert summary["backlog_completed"] is False

