# -*- coding: utf-8 -*-
"""Strategy refresh unit tests for Code Migration Assistant."""

from __future__ import annotations

import asyncio
from copy import deepcopy
from pathlib import Path
from typing import Any

import pytest

from agentflow.integrations.context_bridge import FlowContext, SourceSystemType
from apps.code_migration_assistant.agents.business_semantics_agent import BusinessSemanticsAgent
from apps.code_migration_assistant.agents.migration_design_agent import MigrationDesignAgent
from apps.code_migration_assistant.engine import CodeMigrationEngine, MIGRATION_ANALYSIS
from apps.code_migration_assistant.workflow.capabilities import StageCapabilityRunner
from apps.code_migration_assistant.workflow.models import (
    BusinessSemanticsArtifact,
    DifferentialVerificationArtifact,
    GeneratedFile,
    LegacyAnalysisArtifact,
    LimitedFixArtifact,
    MigrationDesignArtifact,
    QualityDecision,
    QualityGateArtifact,
    TestSynthesisArtifact as _TestSynthesisArtifact,
    TransformationArtifact,
    TransformationIterationArtifact,
    TransformationIterationRecord,
    build_meta,
)
from apps.code_migration_assistant.workflow.pipeline_runtime import run_pipeline
from apps.code_migration_assistant.workflow.reflection import run_reflection_loop


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
        raise AssertionError("limited fixer must not be called")


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
    assert final_score is not None and final_score >= 85.0
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
        raise RuntimeError("skill failed")

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
