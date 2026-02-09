# -*- coding: utf-8 -*-
"""Unit tests for v4 fixed pipeline components."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pytest

from apps.code_migration_assistant.agents import DifferentialVerificationAgent
from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator
from apps.code_migration_assistant.workflow.artifacts import ArtifactStore


class _StubEngine:
    """テスト用スタブEngine."""

    def __init__(self) -> None:
        self.last_inputs: dict[str, Any] | None = None

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        self.last_inputs = inputs
        return {
            "success": True,
            "artifact_paths": {},
            "quality_gate": {"decision": "PASSED"},
            "fast_mode": inputs.get("fast_mode"),
        }


@pytest.mark.asyncio
async def test_artifact_store_round_trip(tmp_path: Path) -> None:
    """ArtifactStore の読み書きテスト."""
    store = ArtifactStore(
        base_dir=tmp_path / "artifacts",
        decisions_path=tmp_path / "DECISIONS.md",
        failures_path=tmp_path / "FAILURES.md",
    )
    await store.initialize()

    path = await store.write_json(
        stage="analysis",
        task_id="task-1",
        artifact_name="legacy_analysis",
        payload={"meta": {"task_id": "task-1"}},
    )

    loaded = await store.read_json(path)
    assert loaded["meta"]["task_id"] == "task-1"

    await store.append_decision("task-1", "分析完了")
    await store.append_failure(
        task_id="task-1",
        stage="analysis",
        responsible_stage="analysis",
        reason="none",
    )

    assert (tmp_path / "DECISIONS.md").exists()
    assert (tmp_path / "FAILURES.md").exists()


@pytest.mark.asyncio
async def test_orchestrator_sets_fast_mode_by_run_tests_flag() -> None:
    """run_tests フラグと fast_mode の連動を確認."""
    orchestrator = CodeMigrationOrchestrator()
    stub = _StubEngine()
    orchestrator._engine = stub

    await orchestrator.migrate("ID DIVISION.", run_tests=False)
    assert stub.last_inputs is not None
    assert stub.last_inputs["fast_mode"] is True

    await orchestrator.migrate("ID DIVISION.", run_tests=True)
    assert stub.last_inputs is not None
    assert stub.last_inputs["fast_mode"] is False


def test_differential_verification_fast_mode_returns_test_classification() -> None:
    """差分検証Agentのfast_mode分岐テスト."""
    agent = DifferentialVerificationAgent()

    result = agent.process(
        {
            "fast_mode": True,
            "transformation": {
                "meta": {"task_id": "task-1", "trace_id": "trace-1", "module": "M1"},
                "target_code": "public class A {}",
            },
            "test_synthesis": {"test_cases": []},
        }
    )

    assert result["classification"] == "test"
    assert result["equivalence"] is False
    assert len(result["unknowns"]) == 1


def test_differential_verification_compare_outputs_whitespace_and_value() -> None:
    """比較ロジックの差分種別テスト."""
    agent = DifferentialVerificationAgent()

    diffs = agent._compare_outputs(  # noqa: SLF001
        expected={"A": "X", "B": "100"},
        actual={"A": " X ", "B": "101"},
    )

    types = {diff["type"] for diff in diffs}
    assert "whitespace" in types
    assert "value" in types
