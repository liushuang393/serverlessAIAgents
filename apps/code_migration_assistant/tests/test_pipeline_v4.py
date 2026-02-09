# -*- coding: utf-8 -*-
"""工程固定パイプラインのテスト."""

from __future__ import annotations

from pathlib import Path

import pytest

from apps.code_migration_assistant.agents import (
    LegacyAnalysisAgent,
    QualityGateAgent,
)
from apps.code_migration_assistant.engine import CodeMigrationEngine
from apps.code_migration_assistant.workflow.models import QualityDecision


def test_legacy_analysis_generates_required_fields() -> None:
    """分析成果物が必須フィールドを持つことを確認."""
    agent = LegacyAnalysisAgent()
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VALUE PIC 9(5).
       PROCEDURE DIVISION.
           DISPLAY WS-VALUE.
           STOP RUN.
    """

    result = agent.process(
        {
            "source_code": cobol_code,
            "task_spec": {
                "task_id": "task-test",
                "trace_id": "trace-test",
                "module": "TEST",
                "expected_outputs": {"VALUE": "100"},
            },
        }
    )

    assert "meta" in result
    assert "unknowns" in result
    assert "extensions" in result
    assert "programs" in result
    assert result["programs"][0]["program_id"] == "TEST-PROGRAM"


def test_quality_gate_known_legacy_decision() -> None:
    """既知旧不具合がある場合は KNOWN_LEGACY を返すことを確認."""
    agent = QualityGateAgent()
    differential = {
        "meta": {"task_id": "task-1", "trace_id": "trace-1", "module": "M1"},
        "equivalence": False,
        "classification": "logic",
        "diffs": [
            {
                "location": "response.amount",
                "legacy": "100",
                "new": "99",
            }
        ],
    }

    result = agent.process(
        {
            "differential": differential,
            "known_legacy_issues": [
                {
                    "location": "response.amount",
                    "legacy": "100",
                    "new": "99",
                }
            ],
        }
    )

    assert result["decision"] == QualityDecision.KNOWN_LEGACY.value


@pytest.mark.asyncio
async def test_engine_pipeline_writes_artifacts(tmp_path: Path) -> None:
    """Engine が固定工程成果物を書き出すことを確認."""
    engine = CodeMigrationEngine()

    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM PIC 9(5).
       PROCEDURE DIVISION.
           MOVE 100 TO WS-NUM.
           DISPLAY WS-NUM.
           STOP RUN.
    """

    result = await engine.run(
        {
            "source_code": cobol_code,
            "artifacts_dir": str(tmp_path / "artifacts"),
            "decisions_path": str(tmp_path / "DECISIONS.md"),
            "failures_path": str(tmp_path / "FAILURES.md"),
        }
    )

    assert "artifact_paths" in result
    assert "analysis" in result["artifact_paths"]
    assert "design" in result["artifact_paths"]
    assert "code" in result["artifact_paths"]
    assert "tests" in result["artifact_paths"]
    assert "diff" in result["artifact_paths"]
    assert "quality" in result["artifact_paths"]
    assert "fix" in result["artifact_paths"]

    for path_str in result["artifact_paths"].values():
        assert Path(path_str).exists()
