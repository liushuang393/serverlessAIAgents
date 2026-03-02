"""CMA CLI 契約実行テスト."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import pytest

from apps.code_migration_assistant import cli


if TYPE_CHECKING:
    from pathlib import Path


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


@pytest.mark.asyncio
async def test_run_contract_payload_success(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    """契約実行が成功時に exit_code=0 を返す."""
    sample = tmp_path / "sample.cbl"
    _write_sample_cobol(sample)

    async def _stub_run_engine_for_program(**kwargs: Any) -> dict[str, Any]:
        on_event = kwargs["on_event"]
        await on_event({"type": "stage_start", "stage": "analyzer", "program_name": "SAMPLE"})
        await on_event({"type": "stage_complete", "stage": "quality_gate", "decision": "PASSED", "program_name": "SAMPLE"})
        return {
            "success": True,
            "class_name": "SampleService",
            "target_code": "public class SampleService {}",
            "iterations": 1,
            "check_result": {"confidence": 0.98},
            "quality_gate": {"decision": "PASSED"},
            "artifact_paths": {"report": str(tmp_path / "report.md")},
        }

    monkeypatch.setattr(cli, "_run_engine_for_program", _stub_run_engine_for_program)

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
    assert summary["success"] is True
    assert summary["decision"] == "PASSED"
    assert any(event.get("type") == "complete" for event in events)


@pytest.mark.asyncio
async def test_run_contract_payload_input_error(tmp_path: Path) -> None:
    """入力不正時に exit_code=2 を返す."""
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
    assert "source_path" in str(summary["error"])
    assert events == []


@pytest.mark.asyncio
async def test_run_contract_payload_business_failure(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    """業務判定NG時に exit_code=1 を返す."""
    sample = tmp_path / "sample.cbl"
    _write_sample_cobol(sample)

    async def _stub_run_engine_for_program(**kwargs: Any) -> dict[str, Any]:
        on_event = kwargs["on_event"]
        await on_event({"type": "stage_start", "stage": "quality_gate", "program_name": "SAMPLE"})
        await on_event(
            {
                "type": "stage_complete",
                "stage": "quality_gate",
                "decision": "TRANSFORM_ISSUE",
                "program_name": "SAMPLE",
            }
        )
        return {
            "success": True,
            "class_name": "SampleService",
            "target_code": "public class SampleService {}",
            "iterations": 1,
            "check_result": {"confidence": 0.42},
            "quality_gate": {"decision": "TRANSFORM_ISSUE"},
            "artifact_paths": {},
        }

    monkeypatch.setattr(cli, "_run_engine_for_program", _stub_run_engine_for_program)

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
    assert summary["success"] is False
    assert summary["decision"] == "TRANSFORM_ISSUE"
