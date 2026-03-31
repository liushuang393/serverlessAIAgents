"""Strategy refresh unit tests for Code Migration Assistant."""

from __future__ import annotations

import asyncio
import json
from copy import deepcopy
from pathlib import Path
from types import SimpleNamespace
from typing import Any

import pytest

from apps.code_migration_assistant.agents.business_semantics_agent import BusinessSemanticsAgent
from apps.code_migration_assistant.agents.migration_design_agent import MigrationDesignAgent
from apps.code_migration_assistant.engine import MIGRATION_ANALYSIS, CodeMigrationEngine
from apps.code_migration_assistant.workflow.capabilities import CAPABILITY_MAP, StageCapabilityRunner
from apps.code_migration_assistant.workflow.models import (
    BusinessSemanticsArtifact,
    DifferentialVerificationArtifact,
    GeneratedFile,
    LegacyAnalysisArtifact,
    LimitedFixArtifact,
    MigrationDesignArtifact,
    QualityDecision,
    QualityGateArtifact,
    TransformationArtifact,
    TransformationIterationArtifact,
    TransformationIterationRecord,
    build_meta,
)
from apps.code_migration_assistant.workflow.models import (
    TestSynthesisArtifact as _TestSynthesisArtifact,
)
from apps.code_migration_assistant.workflow.pipeline_runtime import execute_stage_task, run_pipeline
from apps.code_migration_assistant.workflow.reflection import run_reflection_loop
from shared.integrations.context_bridge import FlowContext, SourceSystemType


class _StaticAgent:
    def __init__(self, payload: dict[str, Any]) -> None:
        self._payload = payload

    def process(self, _input_data: dict[str, Any]) -> dict[str, Any]:
        return deepcopy(self._payload)


class _SequenceAgent:
    def __init__(self, payloads: list[dict[str, Any]]) -> None:
        self._payloads = [deepcopy(item) for item in payloads]
        self.calls = 0

    def process(self, _input_data: dict[str, Any]) -> dict[str, Any]:
        index = min(self.calls, len(self._payloads) - 1)
        self.calls += 1
        return deepcopy(self._payloads[index])


class _CaptureSequenceAgent(_SequenceAgent):
    def __init__(self, payloads: list[dict[str, Any]]) -> None:
        super().__init__(payloads)
        self.last_input: dict[str, Any] | None = None

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        self.last_input = deepcopy(input_data)
        return super().process(input_data)


class _FixerSpy:
    def __init__(self, payload: dict[str, Any]) -> None:
        self._payload = payload
        self.calls = 0

    def process(self, _input_data: dict[str, Any]) -> dict[str, Any]:
        self.calls += 1
        return deepcopy(self._payload)


class _FailIfCalledFixer:
    def process(self, _input_data: dict[str, Any]) -> dict[str, Any]:
        msg = "limited fixer must not be called"
        raise AssertionError(msg)


def _build_flow_context() -> FlowContext:
    return FlowContext(
        session_id="session-test",
        user_id="test-user",
        tenant_id="test-tenant",
        source_system="pytest",
        source_system_type=SourceSystemType.CLI,
        user_context={
            "role": "manager",
            "permissions": ["read", "write", "execute", "manage"],
        },
    )


def _analysis_payload() -> dict[str, Any]:
    return LegacyAnalysisArtifact(
        meta=build_meta(
            task_id="task-1",
            trace_id="trace-1",
            stage="analysis",
            source_language="COBOL",
            target_language="Java",
            module="SAMPLE",
        ),
        programs=[{"program_id": "SAMPLE"}],
        entry_points=[{"name": "MAIN", "type": "paragraph"}],
        io_contracts=[],
        data_structures=[{"name": "WS-STATUS"}],
        control_flow=[{"statement": "IF A > B"}],
        db_access=[],
        external_calls=[],
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")


def _semantics_payload() -> dict[str, Any]:
    return BusinessSemanticsArtifact(
        meta=build_meta(
            task_id="task-1",
            trace_id="trace-1",
            stage="business_semantics",
            source_language="COBOL",
            target_language="Java",
            module="SAMPLE",
        ),
        business_processes=[{"name": "受注業務", "entry_point": "MAIN", "steps": ["入力", "検証"]}],
        business_events=[{"name": "受注受付", "trigger": "MAIN", "type": "business"}],
        state_model={"states": ["初期", "受付", "完了"]},
        business_rules=[{"name": "rule_1", "condition": "IF A > B", "action": "受注保留"}],
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")


def _design_payload() -> dict[str, Any]:
    return MigrationDesignArtifact(
        meta=build_meta(
            task_id="task-1",
            trace_id="trace-1",
            stage="design",
            source_language="COBOL",
            target_language="Java",
            module="SAMPLE",
        ),
        package_mapping={"default": "com.migration.generated"},
        class_mapping={"primary_class": "Sample"},
        transaction_policy={"mode": "preserve"},
        state_model={"states": ["初期", "完了"]},
        framework_mapping={"style": "event-driven", "event_count": 1},
        rationale={"class_mapping": "1:1"},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")


def _tests_payload() -> dict[str, Any]:
    return _TestSynthesisArtifact(
        meta=build_meta(
            task_id="task-1",
            trace_id="trace-1",
            stage="tests",
            source_language="COBOL",
            target_language="Java",
            module="SAMPLE",
        ),
        test_cases=[{"name": "default", "inputs": {}, "expected_outputs": {"RESULT": "100"}}],
        golden_master={"default": {"RESULT": "100"}},
        evidence={"case_count": 1},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")


def _transformation_artifacts() -> tuple[TransformationArtifact, TransformationIterationArtifact]:
    transformation = TransformationArtifact(
        meta=build_meta(
            task_id="task-1",
            trace_id="trace-1",
            stage="code",
            source_language="COBOL",
            target_language="Java",
            module="SAMPLE",
        ),
        target_code="public class Sample {}",
        generated_files=[GeneratedFile(path="generated/Sample.java", content="public class Sample {}")],
        rule_hits=["class_mapping.primary_class"],
        warnings=[],
        unknowns=[],
        extensions={},
    )
    iterations = TransformationIterationArtifact(
        meta=build_meta(
            task_id="task-1",
            trace_id="trace-1",
            stage="code",
            source_language="COBOL",
            target_language="Java",
            module="SAMPLE",
        ),
        iterations=[
            TransformationIterationRecord(
                iteration=1,
                score=92.0,
                accepted=True,
                feedback=[],
                suggestions=[],
            )
        ],
        accepted=True,
        final_score=92.0,
        unknowns=[],
        extensions={},
    )
    return transformation, iterations


def _build_stub_engine(
    *,
    diff_payloads: list[dict[str, Any]],
    quality_payloads: list[dict[str, Any]],
    fixer: Any,
) -> tuple[CodeMigrationEngine, _SequenceAgent, _SequenceAgent, Any]:
    engine = CodeMigrationEngine()
    engine._legacy_analysis_agent = _StaticAgent(_analysis_payload())
    engine._business_semantics_agent = _StaticAgent(_semantics_payload())
    engine._migration_design_agent = _StaticAgent(_design_payload())
    engine._code_transformation_agent = _StaticAgent({})
    engine._test_synthesis_agent = _StaticAgent(_tests_payload())
    engine._compliance_reporter_agent = _StaticAgent({"report_markdown": "# report"})

    diff_agent = _SequenceAgent(diff_payloads)
    quality_agent = _SequenceAgent(quality_payloads)
    engine._differential_agent = diff_agent
    engine._quality_gate_agent = quality_agent
    engine._limited_fixer_agent = fixer

    async def _noop(*_args: Any, **_kwargs: Any) -> None:
        return None

    async def _transform_stub(**_kwargs: Any) -> tuple[TransformationArtifact, TransformationIterationArtifact]:
        return _transformation_artifacts()

    async def _legacy_issues_stub(_inputs: dict[str, Any]) -> list[dict[str, Any]]:
        return []

    engine._check_governance = _noop  # type: ignore[method-assign]
    engine._log_completion = _noop  # type: ignore[method-assign]
    engine._run_transformation_with_reflection = _transform_stub  # type: ignore[method-assign]
    engine._load_known_legacy_issues = _legacy_issues_stub  # type: ignore[method-assign]

    return engine, diff_agent, quality_agent, fixer


def _build_capture_stub_engine(
    *,
    diff_payloads: list[dict[str, Any]],
    quality_payloads: list[dict[str, Any]],
    fixer: Any,
) -> tuple[CodeMigrationEngine, _CaptureSequenceAgent, _SequenceAgent, Any]:
    engine = CodeMigrationEngine()
    engine._legacy_analysis_agent = _StaticAgent(_analysis_payload())
    engine._business_semantics_agent = _StaticAgent(_semantics_payload())
    engine._migration_design_agent = _StaticAgent(_design_payload())
    engine._code_transformation_agent = _StaticAgent({})
    engine._test_synthesis_agent = _StaticAgent(_tests_payload())
    engine._compliance_reporter_agent = _StaticAgent({"report_markdown": "# report"})

    diff_agent = _CaptureSequenceAgent(diff_payloads)
    quality_agent = _SequenceAgent(quality_payloads)
    engine._differential_agent = diff_agent
    engine._quality_gate_agent = quality_agent
    engine._limited_fixer_agent = fixer

    async def _noop(*_args: Any, **_kwargs: Any) -> None:
        return None

    async def _transform_stub(**_kwargs: Any) -> tuple[TransformationArtifact, TransformationIterationArtifact]:
        return _transformation_artifacts()

    async def _legacy_issues_stub(_inputs: dict[str, Any]) -> list[dict[str, Any]]:
        return []

    engine._check_governance = _noop  # type: ignore[method-assign]
    engine._log_completion = _noop  # type: ignore[method-assign]
    engine._run_transformation_with_reflection = _transform_stub  # type: ignore[method-assign]
    engine._load_known_legacy_issues = _legacy_issues_stub  # type: ignore[method-assign]

    return engine, diff_agent, quality_agent, fixer


def test_engine_governance_allows_with_context() -> None:
    engine = CodeMigrationEngine()

    async def _run() -> None:
        await engine._check_governance(
            MIGRATION_ANALYSIS,
            "task-ctx-allow",
            {"module": "SAMPLE"},
            _build_flow_context(),
        )

    asyncio.run(_run())


def test_engine_governance_rejects_without_context() -> None:
    engine = CodeMigrationEngine()

    async def _run() -> None:
        await engine._check_governance(
            MIGRATION_ANALYSIS,
            "task-ctx-deny",
            {"module": "SAMPLE"},
            None,
        )

    with pytest.raises(PermissionError):
        asyncio.run(_run())


def test_request_design_approval_returns_true_after_submit(tmp_path: Path) -> None:
    engine = CodeMigrationEngine()
    engine._event_queue = asyncio.Queue()
    design = MigrationDesignArtifact.model_validate(_design_payload())
    design_path = tmp_path / "migration_design.json"
    design_path.write_text("{}", encoding="utf-8")

    async def _approve_when_pending() -> None:
        for _ in range(50):
            pending = engine._approval_flow.get_pending_requests()
            if pending:
                await engine._approval_flow.submit_response(
                    request_id=pending[0].id,
                    approved=True,
                    approver="tester",
                    comment="ok",
                )
                return
            await asyncio.sleep(0.01)
        pytest.fail("approval request was not registered")

    async def _run() -> bool:
        submit_task = asyncio.create_task(_approve_when_pending())
        try:
            return await engine._request_design_approval(
                "task-approval-ok",
                "SAMPLE",
                design,
                design_path,
            )
        finally:
            await submit_task

    approved = asyncio.run(_run())

    assert approved is True
    assert engine._approval_flow.get_pending_requests() == []
    queued_events: list[dict[str, Any]] = []
    while not engine._event_queue.empty():
        queued_events.append(engine._event_queue.get_nowait())
    event_types = [str(item.get("event_type")) for item in queued_events]
    assert "approval_required" in event_types
    assert "approval_submitted" in event_types


def test_request_design_approval_returns_false_on_reject(tmp_path: Path) -> None:
    engine = CodeMigrationEngine()
    design = MigrationDesignArtifact.model_validate(_design_payload())
    design_path = tmp_path / "migration_design.json"
    design_path.write_text("{}", encoding="utf-8")

    async def _reject_when_pending() -> None:
        for _ in range(50):
            pending = engine._approval_flow.get_pending_requests()
            if pending:
                await engine._approval_flow.submit_response(
                    request_id=pending[0].id,
                    approved=False,
                    approver="tester",
                    comment="reject",
                )
                return
            await asyncio.sleep(0.01)
        pytest.fail("approval request was not registered")

    async def _run() -> bool:
        submit_task = asyncio.create_task(_reject_when_pending())
        try:
            return await engine._request_design_approval(
                "task-approval-ng",
                "SAMPLE",
                design,
                design_path,
            )
        finally:
            await submit_task

    approved = asyncio.run(_run())

    assert approved is False


def test_run_pipeline_returns_design_error_when_manual_approval_rejected(tmp_path: Path) -> None:
    diff_payload = DifferentialVerificationArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
        equivalence=True,
        diffs=[],
        classification="none",
        confidence=1.0,
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    quality_payload = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.PASSED,
        target_agent="None",
        reason="差分なし",
        severity="LOW",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    engine, _diff, _quality, _fixer = _build_stub_engine(
        diff_payloads=[diff_payload],
        quality_payloads=[quality_payload],
        fixer=_FailIfCalledFixer(),
    )

    async def _reject_design(
        _task_id: str,
        _module: str,
        _design_artifact: MigrationDesignArtifact,
        _design_path: Path,
    ) -> bool:
        return False

    engine._request_design_approval = _reject_design  # type: ignore[method-assign]

    result = asyncio.run(
        run_pipeline(
            engine,
            {
                "source_code": "IDENTIFICATION DIVISION.",
                "module": "SAMPLE",
                "artifacts_dir": str(tmp_path / "artifacts"),
                "options": {
                    "human_policy": "manual_all",
                    "risk_profile": "normal",
                },
                "expected_outputs": {"RESULT": "100"},
            },
        )
    )

    assert result["success"] is False
    assert result["stage"] == "design"
    assert "Rejected" in result["error"]


def test_execute_stage_task_design_requests_approval_under_manual_all(tmp_path: Path) -> None:
    analysis_payload = _analysis_payload()
    semantics_payload = _semantics_payload()
    engine = CodeMigrationEngine()
    engine._legacy_analysis_agent = _StaticAgent(analysis_payload)
    engine._business_semantics_agent = _StaticAgent(semantics_payload)
    engine._migration_design_agent = _StaticAgent(_design_payload())
    engine._code_transformation_agent = _StaticAgent({})
    engine._test_synthesis_agent = _StaticAgent(_tests_payload())
    engine._differential_agent = _StaticAgent({})
    engine._quality_gate_agent = _StaticAgent({})
    engine._limited_fixer_agent = _FailIfCalledFixer()
    engine._compliance_reporter_agent = _StaticAgent({"report_markdown": "# report"})

    async def _noop(*_args: Any, **_kwargs: Any) -> None:
        return None

    approval_calls: list[dict[str, Any]] = []

    async def _approve_design(
        task_id: str,
        module: str,
        design_artifact: MigrationDesignArtifact,
        design_path: Path,
    ) -> bool:
        approval_calls.append(
            {
                "task_id": task_id,
                "module": module,
                "design_path": str(design_path),
                "unknowns": list(design_artifact.unknowns),
            }
        )
        return True

    engine._check_governance = _noop  # type: ignore[method-assign]
    engine._log_completion = _noop  # type: ignore[method-assign]
    engine._request_design_approval = _approve_design  # type: ignore[method-assign]
    analysis_path = tmp_path / "analysis" / "task-1_legacy_analysis.json"
    analysis_path.parent.mkdir(parents=True, exist_ok=True)
    analysis_path.write_text(json.dumps(analysis_payload, ensure_ascii=False), encoding="utf-8")
    semantics_path = tmp_path / "business_semantics" / "task-1_business_semantics.json"
    semantics_path.parent.mkdir(parents=True, exist_ok=True)
    semantics_path.write_text(json.dumps(semantics_payload, ensure_ascii=False), encoding="utf-8")

    result = asyncio.run(
        execute_stage_task(
            engine,
            {
                "stage": "design",
                "source_code": "IDENTIFICATION DIVISION.",
                "task_id": "task-1",
                "trace_id": "trace-1",
                "module": "SAMPLE",
                "program_task_id": "task-1",
                "module_root": str(tmp_path),
                "fast_mode": False,
                "options": {
                    "human_policy": "manual_all",
                    "risk_profile": "normal",
                },
            },
        )
    )

    assert result["success"] is True
    assert result["artifact_paths"]["design"].endswith("migration_design.json")
    assert len(approval_calls) == 1
    assert approval_calls[0]["module"] == "SAMPLE"


def test_business_semantics_agent_extracts_process_event_rule_state() -> None:
    agent = BusinessSemanticsAgent()
    legacy_analysis = {
        "meta": {
            "task_id": "task-semantics",
            "trace_id": "trace-semantics",
            "module": "M1",
            "source_language": "COBOL",
            "target_language": "Java",
        },
        "entry_points": [{"name": "MAIN", "type": "paragraph"}],
        "control_flow": [
            {"statement": "READ INPUT-FILE"},
            {"statement": "IF AMOUNT > LIMIT"},
            {"statement": "DISPLAY RESULT"},
        ],
        "external_calls": [{"type": "sql", "line": "SELECT * FROM T"}],
        "data_structures": [{"name": "WS-STATUS"}],
    }

    result = agent.process({"legacy_analysis": legacy_analysis})

    assert result["business_processes"]
    assert result["business_events"]
    assert result["business_rules"]
    assert result["state_model"]["states"]


def test_business_semantics_agent_falls_back_to_unknowns() -> None:
    agent = BusinessSemanticsAgent()
    legacy_analysis = {
        "meta": {
            "task_id": "task-semantics-empty",
            "trace_id": "trace-semantics-empty",
            "module": "M1",
        },
        "entry_points": [],
        "control_flow": [],
        "external_calls": [],
        "data_structures": [],
    }

    result = agent.process({"legacy_analysis": legacy_analysis})
    unknown_fields = {item["field"] for item in result["unknowns"]}

    assert "business_processes" in unknown_fields
    assert "business_rules" in unknown_fields


def test_design_agent_requires_business_semantics() -> None:
    agent = MigrationDesignAgent()
    result = agent.process({"legacy_analysis": _analysis_payload()})

    assert result["success"] is False
    assert "business_semantics is required" in result["error"]


def test_design_agent_reflects_business_events() -> None:
    agent = MigrationDesignAgent()
    result = agent.process(
        {
            "legacy_analysis": _analysis_payload(),
            "business_semantics": _semantics_payload(),
        }
    )

    assert result["framework_mapping"]["style"] == "event-driven"
    assert result["framework_mapping"]["event_count"] == 1


def test_reflection_loop_retries_until_threshold() -> None:
    state = {"iteration": 0}

    def _generate(_feedback: list[str]) -> dict[str, Any]:
        state["iteration"] += 1
        return {"target_code": f"code-{state['iteration']}"}

    async def _evaluate(payload: dict[str, Any]) -> dict[str, Any]:
        idx = int(str(payload["target_code"]).split("-")[-1])
        score = 50.0 + idx * 20.0
        return {
            "score": score,
            "is_acceptable": score >= 85.0,
            "feedback": [f"improve-{idx}"],
            "suggestions": [],
        }

    payload, records, accepted, final_score = asyncio.run(
        run_reflection_loop(
            generate=_generate,
            evaluate=_evaluate,
            acceptance_threshold=85.0,
            max_iterations=5,
        )
    )

    assert accepted is True
    assert final_score is not None
    assert final_score >= 85.0
    assert len(records) == 2
    assert payload["target_code"] == "code-2"


def test_reflection_loop_respects_max_iterations() -> None:
    state = {"iteration": 0}

    def _generate(_feedback: list[str]) -> dict[str, Any]:
        state["iteration"] += 1
        return {"target_code": f"code-{state['iteration']}"}

    async def _evaluate(_payload: dict[str, Any]) -> dict[str, Any]:
        return {
            "score": 40.0,
            "is_acceptable": False,
            "feedback": ["keep improving"],
            "suggestions": [],
        }

    _payload, records, accepted, final_score = asyncio.run(
        run_reflection_loop(
            generate=_generate,
            evaluate=_evaluate,
            acceptance_threshold=85.0,
            max_iterations=3,
        )
    )

    assert accepted is False
    assert len(records) == 3
    assert final_score == 40.0


def test_verification_mode_strict_defaults_to_full_verification(tmp_path: Path) -> None:
    diff_payload = DifferentialVerificationArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
        equivalence=True,
        diffs=[],
        classification="none",
        confidence=1.0,
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    quality_payload = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.PASSED,
        target_agent="None",
        reason="差分なし",
        severity="LOW",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    engine, diff_agent, _quality, _fixer = _build_capture_stub_engine(
        diff_payloads=[diff_payload],
        quality_payloads=[quality_payload],
        fixer=_FailIfCalledFixer(),
    )

    result = asyncio.run(
        run_pipeline(
            engine,
            {
                "source_code": "IDENTIFICATION DIVISION.",
                "module": "SAMPLE",
                "artifacts_dir": str(tmp_path / "artifacts"),
                "options": {
                    "human_policy": "auto_with_sampling",
                    "risk_profile": "normal",
                    "verification_mode": "strict",
                },
                "expected_outputs": {"RESULT": "100"},
            },
        )
    )

    assert result["success"] is True
    assert diff_agent.last_input is not None
    assert diff_agent.last_input["fast_mode"] is False


def test_stage_capability_runner_skill_first_and_fallback() -> None:
    async def _skill_success(
        capability_id: str,
        payload: dict[str, Any],
        _capability: Any,
    ) -> dict[str, Any]:
        return {"output": {"capability_id": capability_id, "value": payload["value"]}}

    runner = StageCapabilityRunner(skill_mode="skill_first", skill_executor=_skill_success)
    execution = asyncio.run(
        runner.run_stage(
            stage="analysis",
            payload={"value": 10},
            native_runner=lambda _payload: {"value": -1},
        )
    )

    assert execution.output["value"] == 10
    assert execution.trace["provider"] == "skill"
    assert execution.trace["status"] == "applied"

    async def _skill_fail(
        _capability_id: str,
        _payload: dict[str, Any],
        _capability: Any,
    ) -> dict[str, Any]:
        msg = "skill failed"
        raise RuntimeError(msg)

    fallback_runner = StageCapabilityRunner(skill_mode="skill_first", skill_executor=_skill_fail)
    fallback_execution = asyncio.run(
        fallback_runner.run_stage(
            stage="analysis",
            payload={"value": 20},
            native_runner=lambda payload: {"value": payload["value"] + 1},
        )
    )

    assert fallback_execution.output["value"] == 21
    assert fallback_execution.trace["provider"] == "native"
    assert fallback_execution.trace["status"] == "fallback_applied"


def test_capability_map_covers_primary_closed_loop_stages() -> None:
    expected = {"analysis", "business_semantics", "design", "transform", "tests", "diff", "quality", "fix", "report"}
    assert expected.issubset(set(CAPABILITY_MAP))


def test_run_pipeline_blocks_on_analysis_input_safety(tmp_path: Path) -> None:
    diff_payload = DifferentialVerificationArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
        equivalence=True,
        diffs=[],
        classification="none",
        confidence=1.0,
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    quality_payload = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.PASSED,
        target_agent="None",
        reason="差分なし",
        severity="LOW",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    engine, _diff, _quality, _fixer = _build_stub_engine(
        diff_payloads=[diff_payload],
        quality_payloads=[quality_payload],
        fixer=_FailIfCalledFixer(),
    )

    async def _unsafe_input(_text: str, _context: dict[str, Any] | None = None) -> Any:
        return SimpleNamespace(is_safe=False, sanitized_input="blocked", warnings=["unsafe input"])

    engine.check_input_safety = _unsafe_input  # type: ignore[method-assign]

    result = asyncio.run(
        run_pipeline(
            engine,
            {
                "source_code": "IDENTIFICATION DIVISION.",
                "module": "SAMPLE",
                "artifacts_dir": str(tmp_path / "artifacts"),
                "options": {
                    "human_policy": "auto_with_sampling",
                    "risk_profile": "normal",
                },
                "expected_outputs": {"RESULT": "100"},
            },
        )
    )

    assert result["success"] is False
    assert result["stage"] == "analysis"
    assert "safety" in result["error"].lower()


def test_execute_stage_task_transform_records_output_safety_review(tmp_path: Path) -> None:
    analysis_payload = _analysis_payload()
    design_payload = _design_payload()
    engine = CodeMigrationEngine()
    engine._legacy_analysis_agent = _StaticAgent(analysis_payload)
    engine._business_semantics_agent = _StaticAgent(_semantics_payload())
    engine._migration_design_agent = _StaticAgent(design_payload)
    engine._code_transformation_agent = _StaticAgent({})
    engine._test_synthesis_agent = _StaticAgent(_tests_payload())
    engine._differential_agent = _StaticAgent({})
    engine._quality_gate_agent = _StaticAgent({})
    engine._limited_fixer_agent = _FailIfCalledFixer()
    engine._compliance_reporter_agent = _StaticAgent({"report_markdown": "# report"})

    async def _noop(*_args: Any, **_kwargs: Any) -> None:
        return None

    async def _safe_input(_text: str, _context: dict[str, Any] | None = None) -> Any:
        return SimpleNamespace(is_safe=True, sanitized_input=_text, warnings=[])

    async def _review_output(_text: str, _context: dict[str, Any] | None = None) -> Any:
        return SimpleNamespace(
            is_reliable=True,
            needs_review=True,
            confidence_score=0.4,
            sanitized_output=_text,
            issues=[{"type": "policy", "message": "review"}],
        )

    engine._check_governance = _noop  # type: ignore[method-assign]
    engine._log_completion = _noop  # type: ignore[method-assign]
    engine.check_input_safety = _safe_input  # type: ignore[method-assign]
    engine.check_output_safety = _review_output  # type: ignore[method-assign]

    async def _transform_stub(**_kwargs: Any) -> tuple[TransformationArtifact, TransformationIterationArtifact]:
        return _transformation_artifacts()

    engine._run_transformation_with_reflection = _transform_stub  # type: ignore[method-assign]

    analysis_path = tmp_path / "analysis" / "task-1_legacy_analysis.json"
    analysis_path.parent.mkdir(parents=True, exist_ok=True)
    analysis_path.write_text(json.dumps(analysis_payload, ensure_ascii=False), encoding="utf-8")
    design_path = tmp_path / "design" / "task-1_migration_design.json"
    design_path.parent.mkdir(parents=True, exist_ok=True)
    design_path.write_text(json.dumps(design_payload, ensure_ascii=False), encoding="utf-8")

    result = asyncio.run(
        execute_stage_task(
            engine,
            {
                "stage": "transform",
                "source_code": "IDENTIFICATION DIVISION.",
                "task_id": "task-1",
                "trace_id": "trace-1",
                "module": "SAMPLE",
                "program_task_id": "task-1",
                "module_root": str(tmp_path),
                "fast_mode": False,
                "options": {
                    "human_policy": "auto_with_sampling",
                    "risk_profile": "normal",
                },
            },
        )
    )

    assert result["success"] is True
    assert result["evidence"]["safety_review"] is True
    assert result["evidence"]["safety_issue_count"] == 1


def test_execute_stage_task_report_writes_sanitized_output(tmp_path: Path) -> None:
    analysis_payload = _analysis_payload()
    design_payload = _design_payload()
    quality_payload = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.PASSED,
        target_agent="None",
        reason="差分なし",
        severity="LOW",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")

    engine = CodeMigrationEngine()
    engine._legacy_analysis_agent = _StaticAgent(analysis_payload)
    engine._business_semantics_agent = _StaticAgent(_semantics_payload())
    engine._migration_design_agent = _StaticAgent(design_payload)
    engine._code_transformation_agent = _StaticAgent({})
    engine._test_synthesis_agent = _StaticAgent(_tests_payload())
    engine._differential_agent = _StaticAgent({})
    engine._quality_gate_agent = _StaticAgent(quality_payload)
    engine._limited_fixer_agent = _FailIfCalledFixer()
    engine._compliance_reporter_agent = _StaticAgent({"report_markdown": "# internal"})

    async def _noop(*_args: Any, **_kwargs: Any) -> None:
        return None

    async def _safe_input(_text: str, _context: dict[str, Any] | None = None) -> Any:
        return SimpleNamespace(is_safe=True, sanitized_input=_text, warnings=[])

    async def _sanitize_output(_text: str, _context: dict[str, Any] | None = None) -> Any:
        return SimpleNamespace(
            is_reliable=True,
            needs_review=True,
            confidence_score=0.3,
            sanitized_output="# sanitized",
            issues=[{"type": "policy", "message": "mask"}],
        )

    engine._check_governance = _noop  # type: ignore[method-assign]
    engine._log_completion = _noop  # type: ignore[method-assign]
    engine.check_input_safety = _safe_input  # type: ignore[method-assign]
    engine.check_output_safety = _sanitize_output  # type: ignore[method-assign]

    async def _transform_stub(**_kwargs: Any) -> tuple[TransformationArtifact, TransformationIterationArtifact]:
        return _transformation_artifacts()

    engine._run_transformation_with_reflection = _transform_stub  # type: ignore[method-assign]

    analysis_path = tmp_path / "analysis" / "task-1_legacy_analysis.json"
    analysis_path.parent.mkdir(parents=True, exist_ok=True)
    analysis_path.write_text(json.dumps(analysis_payload, ensure_ascii=False), encoding="utf-8")

    semantics_path = tmp_path / "business_semantics" / "task-1_business_semantics.json"
    semantics_path.parent.mkdir(parents=True, exist_ok=True)
    semantics_path.write_text(json.dumps(_semantics_payload(), ensure_ascii=False), encoding="utf-8")

    design_path = tmp_path / "design" / "task-1_migration_design.json"
    design_path.parent.mkdir(parents=True, exist_ok=True)
    design_path.write_text(json.dumps(design_payload, ensure_ascii=False), encoding="utf-8")

    transform_artifact, reflection_artifact = _transformation_artifacts()
    transform_path = tmp_path / "code" / "task-1_transformation.json"
    transform_path.parent.mkdir(parents=True, exist_ok=True)
    transform_path.write_text(transform_artifact.model_dump_json(indent=2), encoding="utf-8")
    reflection_path = tmp_path / "code" / "task-1_transformation_iterations.json"
    reflection_path.write_text(reflection_artifact.model_dump_json(indent=2), encoding="utf-8")

    diff_path = tmp_path / "diff" / "task-1_differential_verification.json"
    diff_path.parent.mkdir(parents=True, exist_ok=True)
    diff_path.write_text(
        DifferentialVerificationArtifact(
            meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
            equivalence=True,
            diffs=[],
            classification="none",
            confidence=1.0,
            evidence={},
            unknowns=[],
            extensions={},
        ).model_dump_json(indent=2),
        encoding="utf-8",
    )

    quality_path = tmp_path / "quality" / "task-1_quality_gate.json"
    quality_path.parent.mkdir(parents=True, exist_ok=True)
    quality_path.write_text(json.dumps(quality_payload, ensure_ascii=False), encoding="utf-8")

    fix_path = tmp_path / "fix" / "task-1_limited_fix.json"
    fix_path.parent.mkdir(parents=True, exist_ok=True)
    fix_path.write_text(
        LimitedFixArtifact(
            meta=build_meta(task_id="task-1", trace_id="trace-1", stage="fix", module="SAMPLE"),
            applied=False,
            target_code=transform_artifact.target_code,
            patch_summary=["no fix required"],
            retest_required=False,
            unknowns=[],
            extensions={},
        ).model_dump_json(indent=2),
        encoding="utf-8",
    )

    result = asyncio.run(
        execute_stage_task(
            engine,
            {
                "stage": "report",
                "task_id": "task-1",
                "trace_id": "trace-1",
                "module": "SAMPLE",
                "program_task_id": "task-1",
                "module_root": str(tmp_path),
                "fast_mode": False,
                "options": {
                    "human_policy": "auto_with_sampling",
                    "risk_profile": "normal",
                },
            },
        )
    )

    assert result["success"] is True
    assert result["evidence"]["safety_review"] is True
    report_text = Path(result["artifact_paths"]["report"]).read_text(encoding="utf-8")
    assert report_text == "# sanitized"


def test_execute_stage_task_uses_skill_executor_for_analysis(tmp_path: Path) -> None:
    diff_payload = DifferentialVerificationArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
        equivalence=True,
        diffs=[],
        classification="none",
        confidence=1.0,
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    quality_payload = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.PASSED,
        target_agent="None",
        reason="差分なし",
        severity="LOW",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    engine, _diff, _quality, _fixer = _build_stub_engine(
        diff_payloads=[diff_payload],
        quality_payloads=[quality_payload],
        fixer=_FailIfCalledFixer(),
    )

    async def _noop(*_args: Any, **_kwargs: Any) -> None:
        return None

    skill_payload = _analysis_payload()
    skill_payload["programs"] = [{"program_id": "SKILL"}]

    async def _skill_executor(
        capability_id: str,
        payload: dict[str, Any],
        _capability: Any,
    ) -> dict[str, Any]:
        assert capability_id == "legacy-ingestion"
        assert "task_spec" in payload
        return {"output": deepcopy(skill_payload)}

    engine._check_governance = _noop  # type: ignore[method-assign]
    engine._log_completion = _noop  # type: ignore[method-assign]

    result = asyncio.run(
        execute_stage_task(
            engine,
            {
                "stage": "analysis",
                "source_code": "IDENTIFICATION DIVISION.",
                "module": "SAMPLE",
                "task_id": "task-skill-stage",
                "program_task_id": "task-skill-stage",
                "module_root": str(tmp_path / "stage"),
                "skill_executor": _skill_executor,
            },
        )
    )

    assert result["success"] is True
    artifact_path = Path(result["artifact_paths"]["analysis"])
    stored = artifact_path.read_text(encoding="utf-8")
    assert '"program_id": "SKILL"' in stored
    trace = engine.get_capability_trace()
    assert trace
    assert trace[0]["capability_id"] == "legacy-ingestion"
    assert trace[0]["provider"] == "skill"


def test_stage_execution_matches_pipeline_mapped_capability_trace(tmp_path: Path) -> None:
    diff_payload = DifferentialVerificationArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
        equivalence=True,
        diffs=[],
        classification="none",
        confidence=1.0,
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    quality_payload = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.PASSED,
        target_agent="None",
        reason="差分なし",
        severity="LOW",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")

    pipeline_engine, _diff, _quality, _fixer = _build_stub_engine(
        diff_payloads=[diff_payload],
        quality_payloads=[quality_payload],
        fixer=_FailIfCalledFixer(),
    )
    pipeline_result = asyncio.run(
        run_pipeline(
            pipeline_engine,
            {
                "source_code": "IDENTIFICATION DIVISION.",
                "module": "SAMPLE",
                "artifacts_dir": str(tmp_path / "pipeline"),
                "options": {
                    "human_policy": "auto_with_sampling",
                    "risk_profile": "normal",
                },
                "expected_outputs": {"RESULT": "100"},
            },
        )
    )
    assert pipeline_result["success"] is True

    stage_engine, _stage_diff, _stage_quality, _stage_fixer = _build_stub_engine(
        diff_payloads=[diff_payload],
        quality_payloads=[quality_payload],
        fixer=_FailIfCalledFixer(),
    )
    module_root = tmp_path / "stage"
    for stage in ("analysis", "business_semantics", "design", "transform", "tests"):
        stage_result = asyncio.run(
            execute_stage_task(
                stage_engine,
                {
                    "stage": stage,
                    "source_code": "IDENTIFICATION DIVISION.",
                    "module": "SAMPLE",
                    "task_id": "task-stage",
                    "trace_id": "trace-stage",
                    "program_task_id": "task-stage",
                    "module_root": str(module_root),
                    "expected_outputs": {"RESULT": "100"},
                    "options": {
                        "human_policy": "auto_with_sampling",
                        "risk_profile": "normal",
                    },
                },
            )
        )
        assert stage_result["success"] is True

    pipeline_mapped = [
        (item["stage"], item["capability_id"], item["provider"])
        for item in pipeline_result["capability_trace"]
        if item["capability_id"] in {"legacy-ingestion", "business-semantics", "modernization-generator"}
    ]
    stage_mapped = [
        (item["stage"], item["capability_id"], item["provider"])
        for item in stage_engine.get_capability_trace()
        if item["capability_id"] in {"legacy-ingestion", "business-semantics", "modernization-generator"}
    ]

    assert stage_mapped == pipeline_mapped


def test_engine_resets_human_facts_per_run() -> None:
    engine = CodeMigrationEngine()

    async def _fake_pipeline(inputs: dict[str, Any]) -> dict[str, Any]:
        for fact in inputs.get("human_facts", []):
            if isinstance(fact, dict):
                engine._human_facts.append(fact)
        return {"fact_count": len(engine._human_facts)}

    engine._run_pipeline = _fake_pipeline  # type: ignore[method-assign]

    first = asyncio.run(
        engine._execute(
            {
                "source_code": "IDENTIFICATION DIVISION.",
                "human_facts": [{"kind": "event", "name": "A"}],
            }
        )
    )
    second = asyncio.run(
        engine._execute(
            {
                "source_code": "IDENTIFICATION DIVISION.",
                "human_facts": [{"kind": "event", "name": "B"}],
            }
        )
    )

    assert first["fact_count"] == 1
    assert second["fact_count"] == 1
    assert engine._human_facts == [{"kind": "event", "name": "B"}]


def test_capability_trace_contains_mcp_tools() -> None:
    engine = CodeMigrationEngine()
    transformation, _iterations = _transformation_artifacts()
    engine._code_transformation_agent = _StaticAgent(transformation.model_dump(mode="json"))

    transformed, _reflection = asyncio.run(
        engine._run_transformation_with_reflection(
            source_code="IDENTIFICATION DIVISION.",
            analysis={"control_flow": [], "data_structures": []},
            migration_design={"class_mapping": {"primary_class": "Sample"}},
            fast_mode=False,
            acceptance_threshold=85.0,
            max_auto_iterations=1,
        )
    )

    assert transformed.target_code
    capability_ids = {item["capability_id"] for item in engine.get_capability_trace()}
    assert "reflection_pattern" in capability_ids
    assert "code_validator" in capability_ids
    assert "memory_system" in capability_ids


def test_fix_stage_skips_when_decision_is_not_transform_issue(tmp_path: Path) -> None:
    diff_payload = DifferentialVerificationArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
        equivalence=True,
        diffs=[],
        classification="none",
        confidence=1.0,
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    quality_payload = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.PASSED,
        target_agent="None",
        reason="差分なし",
        severity="LOW",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")

    engine, _diff, _quality, _fixer = _build_stub_engine(
        diff_payloads=[diff_payload],
        quality_payloads=[quality_payload],
        fixer=_FailIfCalledFixer(),
    )

    result = asyncio.run(
        run_pipeline(
            engine,
            {
                "source_code": "IDENTIFICATION DIVISION.",
                "module": "SAMPLE",
                "artifacts_dir": str(tmp_path / "artifacts"),
                "options": {
                    "human_policy": "auto_with_sampling",
                    "risk_profile": "normal",
                },
                "fast_mode": True,
                "expected_outputs": {"RESULT": "100"},
            },
        )
    )

    assert result["success"] is True
    assert "diff_post_fix" not in result["artifact_paths"]


def test_fix_stage_runs_only_for_transform_issue_and_reverify(tmp_path: Path) -> None:
    first_diff = DifferentialVerificationArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
        equivalence=False,
        diffs=[{"location": "amount", "legacy": "100", "new": "99"}],
        classification="logic",
        confidence=0.8,
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    second_diff = DifferentialVerificationArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="diff", module="SAMPLE"),
        equivalence=True,
        diffs=[],
        classification="none",
        confidence=1.0,
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")

    first_quality = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.TRANSFORM_ISSUE,
        target_agent="LimitedFixerAgent",
        reason="変換差分",
        severity="HIGH",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")
    second_quality = QualityGateArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="quality", module="SAMPLE"),
        decision=QualityDecision.PASSED,
        target_agent="None",
        reason="再検証OK",
        severity="LOW",
        evidence={},
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")

    fix_payload = LimitedFixArtifact(
        meta=build_meta(task_id="task-1", trace_id="trace-1", stage="fix", module="SAMPLE"),
        applied=True,
        target_code="public class Sample { /* fixed */ }",
        patch_summary=["logic fix"],
        retest_required=True,
        unknowns=[],
        extensions={},
    ).model_dump(mode="json")

    fixer = _FixerSpy(fix_payload)
    engine, _diff, _quality, _fixer = _build_stub_engine(
        diff_payloads=[first_diff, second_diff],
        quality_payloads=[first_quality, second_quality],
        fixer=fixer,
    )

    result = asyncio.run(
        run_pipeline(
            engine,
            {
                "source_code": "IDENTIFICATION DIVISION.",
                "module": "SAMPLE",
                "artifacts_dir": str(tmp_path / "artifacts"),
                "options": {
                    "human_policy": "auto_with_sampling",
                    "risk_profile": "normal",
                },
                "fast_mode": True,
                "expected_outputs": {"RESULT": "100"},
            },
        )
    )

    assert result["success"] is True
    assert fixer.calls == 1
    assert result["iterations"] == 2
    assert "diff_post_fix" in result["artifact_paths"]
    assert "quality_post_fix" in result["artifact_paths"]
