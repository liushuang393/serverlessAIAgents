"""単一ステージ実行ヘルパー（backlog dispatcher 用）.

pipeline_runtime.py からファイルサイズ分割のため抽出。
execute_stage_task() は BacklogDispatcher が個別ステージを実行する際に使用する。
"""

from __future__ import annotations

import json
import uuid
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.workflow.capabilities import StageCapabilityRunner
from apps.code_migration_assistant.workflow.control_plane import (
    resolve_execution_options,
    should_require_human_approval,
)
from apps.code_migration_assistant.workflow.models import (
    BusinessSemanticsArtifact,
    DifferentialVerificationArtifact,
    LimitedFixArtifact,
    MigrationDesignArtifact,
    QualityDecision,
    QualityGateArtifact,
    TestSynthesisArtifact,
    TransformationArtifact,
    TransformationIterationArtifact,
    TransformationIterationRecord,
    build_meta,
)
from apps.code_migration_assistant.workflow.pipeline_runtime import (
    _check_safe_output,
    _ensure_safe_input,
)
from shared.integrations.context_bridge import get_current_context


# =========================================================================
# ヘルパー
# =========================================================================

_ARTIFACT_STAGE_DIRS: dict[str, str] = {
    "analysis": "analysis",
    "business_semantics": "business_semantics",
    "design": "design",
    "code": "code",
    "tests": "tests",
    "diff": "diff",
    "quality": "quality",
    "fix": "fix",
    "report": "report",
}


def _artifact_path(
    module_root: Path,
    *,
    stage: str,
    task_id: str,
    artifact_name: str,
    extension: str = "json",
) -> Path:
    """ステージ成果物パスを構築."""
    stage_dir = _ARTIFACT_STAGE_DIRS[stage]
    return module_root / stage_dir / f"{task_id}_{artifact_name}.{extension}"


def _read_json_if_exists(path: Path) -> dict[str, Any] | None:
    """JSON ファイルが存在すれば読み込む."""
    if not path.exists():
        return None
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return None
    return raw if isinstance(raw, dict) else None


def _build_capability_runner(
    *,
    execution_options: Any,
    inputs: dict[str, Any],
) -> StageCapabilityRunner:
    """StageCapabilityRunner を構築."""
    skill_executor = inputs.get("skill_executor")
    if not callable(skill_executor):
        skill_executor = None
    return StageCapabilityRunner(
        skill_mode=execution_options.skill_mode,
        skill_executor=skill_executor,
    )


async def _run_stage_with_capability(
    *,
    engine: Any,
    capability_runner: StageCapabilityRunner,
    stage: str,
    payload: dict[str, Any],
    native_runner: Any,
) -> dict[str, Any]:
    """Capability 経由でステージを実行し、トレースを記録."""

    execution = await capability_runner.run_stage(
        stage=stage,
        payload=payload,
        native_runner=native_runner,
    )
    engine._append_capability_trace(execution.trace)
    return execution.output


# =========================================================================
# メイン関数
# =========================================================================


async def execute_stage_task(engine: Any, inputs: dict[str, Any]) -> dict[str, Any]:
    """Execute a single stage task for backlog dispatcher."""
    from apps.code_migration_assistant import engine as engine_module

    engine._ensure_agents_ready()
    stage = str(inputs.get("stage", "")).strip()
    if not stage:
        return {"success": False, "error": "stage is required"}

    source_code = str(inputs.get("source_code") or "")
    if stage in {"analysis", "transform"} and not source_code:
        return {"success": False, "error": "source_code is required"}

    module = str(inputs.get("module") or "UNKNOWN")
    task_id = str(inputs.get("task_id") or f"task-{uuid.uuid4().hex[:12]}")
    trace_id = str(inputs.get("trace_id") or task_id)
    program_task_id = str(inputs.get("program_task_id") or f"{task_id}-{module.lower()}")
    module_root = Path(str(inputs.get("module_root") or (Path("migration_output") / task_id / module)))

    expected_outputs = inputs.get("expected_outputs", {})
    if not isinstance(expected_outputs, dict):
        expected_outputs = {}

    execution_options = resolve_execution_options(inputs)
    fast_mode_raw = inputs.get("fast_mode")
    fast_mode = execution_options.verification_mode == "fast" if fast_mode_raw is None else bool(fast_mode_raw)
    capability_runner = _build_capability_runner(
        execution_options=execution_options,
        inputs=inputs,
    )

    artifact_store = engine_module.ArtifactStore(
        base_dir=module_root,
        decisions_path=module_root.parent / "DECISIONS.md",
        failures_path=module_root.parent / "FAILURES.md",
    )
    await artifact_store.initialize()
    lock_path = await artifact_store.acquire_lock(f"{program_task_id}-{stage}")
    flow_context = get_current_context()
    if flow_context is None and hasattr(engine, "_resolve_flow_context"):
        flow_context = engine._resolve_flow_context()

    task_spec = engine._build_task_spec(
        task_id=program_task_id,
        trace_id=trace_id,
        module=module,
        expected_outputs=expected_outputs,
        options=execution_options.to_dict(),
    )

    tool_map = {
        "analysis": engine_module.MIGRATION_ANALYSIS,
        "business_semantics": engine_module.MIGRATION_BUSINESS_SEMANTICS,
        "design": engine_module.MIGRATION_DESIGN,
        "transform": engine_module.MIGRATION_TRANSFORM,
        "tests": engine_module.MIGRATION_TEST_GEN,
        "diff": engine_module.MIGRATION_VERIFY_DIFF,
        "strict_verification": engine_module.MIGRATION_VERIFY_DIFF,
        "quality": engine_module.MIGRATION_QUALITY_GATE,
        "fix": engine_module.MIGRATION_FIX,
        "report": engine_module.MIGRATION_REPORT,
    }

    artifact_paths: dict[str, str] = {}
    decision: str | None = None
    unknowns: list[dict[str, Any]] = []
    evidence: dict[str, Any] = {}

    try:
        tool = tool_map.get(stage)
        if tool is None:
            return {"success": False, "error": f"unsupported stage: {stage}"}
        await engine._check_governance(tool, program_task_id, {"stage": stage, "module": module}, flow_context)

        if stage == "analysis":
            input_safety_error = await _ensure_safe_input(engine, text=source_code, stage="analysis", module=module)
            if input_safety_error is not None:
                input_safety_error["module"] = module
                input_safety_error["artifact_paths"] = artifact_paths
                input_safety_error["unknowns"] = unknowns
                input_safety_error["decision"] = decision
                return input_safety_error
            payload = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage="analysis",
                payload={
                    "source_code": source_code,
                    "task_spec": task_spec.model_dump(mode="json"),
                },
                native_runner=engine._legacy_analysis_agent.process,
            )
            artifact = engine._validate_or_fail(engine_module.LegacyAnalysisArtifact, payload, "analysis")
            if artifact is None:
                return {"success": False, "stage": stage, "error": "invalid analysis artifact"}
            path = await artifact_store.write_json(
                stage="analysis",
                task_id=program_task_id,
                artifact_name="legacy_analysis",
                payload=artifact.model_dump(mode="json"),
            )
            artifact_paths["analysis"] = str(path)
            unknowns = artifact.model_dump(mode="json").get("unknowns", [])

        elif stage == "business_semantics":
            analysis_path = _artifact_path(
                module_root,
                stage="analysis",
                task_id=program_task_id,
                artifact_name="legacy_analysis",
            )
            analysis = _read_json_if_exists(analysis_path)
            if analysis is None:
                return {"success": False, "stage": stage, "error": "analysis artifact missing"}
            payload = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage="business_semantics",
                payload={
                    "legacy_analysis": analysis,
                    "business_context": execution_options.business_context or {},
                    "human_facts": getattr(engine, "_human_facts", []),
                },
                native_runner=engine._business_semantics_agent.process,
            )
            artifact = engine._validate_or_fail(BusinessSemanticsArtifact, payload, "business_semantics")
            if artifact is None:
                return {"success": False, "stage": stage, "error": "invalid business semantics artifact"}
            path = await artifact_store.write_json(
                stage="business_semantics",
                task_id=program_task_id,
                artifact_name="business_semantics",
                payload=artifact.model_dump(mode="json"),
            )
            artifact_paths["business_semantics"] = str(path)
            unknowns = artifact.model_dump(mode="json").get("unknowns", [])

        elif stage == "design":
            analysis = _read_json_if_exists(
                _artifact_path(module_root, stage="analysis", task_id=program_task_id, artifact_name="legacy_analysis")
            )
            semantics = _read_json_if_exists(
                _artifact_path(
                    module_root,
                    stage="business_semantics",
                    task_id=program_task_id,
                    artifact_name="business_semantics",
                )
            )
            if analysis is None or semantics is None:
                return {"success": False, "stage": stage, "error": "analysis/business_semantics artifact missing"}
            payload = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage="design",
                payload={
                    "legacy_analysis": analysis,
                    "business_semantics": semantics,
                },
                native_runner=engine._migration_design_agent.process,
            )
            artifact = engine._validate_or_fail(MigrationDesignArtifact, payload, "design")
            if artifact is None:
                return {"success": False, "stage": stage, "error": "invalid design artifact"}
            path = await artifact_store.write_json(
                stage="design",
                task_id=program_task_id,
                artifact_name="migration_design",
                payload=artifact.model_dump(mode="json"),
            )
            artifact_paths["design"] = str(path)
            unknowns = artifact.model_dump(mode="json").get("unknowns", [])
            need_approval = should_require_human_approval(
                tool_risk=engine_module.MIGRATION_DESIGN.risk_level,
                execution_options=execution_options,
            )
            if need_approval:
                approved = await engine._request_design_approval(program_task_id, module, artifact, path)
                approval_result = getattr(engine, "_last_approval_result", {}) or {}
                evidence = {
                    "approval_required": True,
                    "approval_status": str(approval_result.get("status", "approved" if approved else "rejected")),
                    "approval_request_id": str(approval_result.get("request_id", "")),
                }
                if not approved:
                    return {
                        "success": False,
                        "stage": stage,
                        "module": module,
                        "artifact_paths": artifact_paths,
                        "decision": "APPROVAL_REJECTED",
                        "unknowns": unknowns,
                        "evidence": evidence,
                        "error": "migration design approval rejected or timed out",
                    }
            else:
                evidence = {
                    "approval_required": False,
                    "approval_status": "auto_approved",
                }

        elif stage == "transform":
            analysis = _read_json_if_exists(
                _artifact_path(module_root, stage="analysis", task_id=program_task_id, artifact_name="legacy_analysis")
            )
            design = _read_json_if_exists(
                _artifact_path(module_root, stage="design", task_id=program_task_id, artifact_name="migration_design")
            )
            if analysis is None or design is None:
                return {"success": False, "stage": stage, "error": "analysis/design artifact missing"}

            async def _native_transform(payload: dict[str, Any]) -> dict[str, Any]:
                transformation, iterations = await engine._run_transformation_with_reflection(
                    source_code=str(payload["source_code"]),
                    analysis=payload["analysis"],
                    migration_design=payload["migration_design"],
                    fast_mode=bool(payload["fast_mode"]),
                    acceptance_threshold=float(payload["acceptance_threshold"]),
                    max_auto_iterations=int(payload["max_auto_iterations"]),
                )
                return {
                    "transformation": transformation.model_dump(mode="json"),
                    "reflection": iterations.model_dump(mode="json"),
                }

            transform_output = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage="transform",
                payload={
                    "source_code": source_code,
                    "analysis": analysis,
                    "migration_design": design,
                    "fast_mode": fast_mode,
                    "acceptance_threshold": execution_options.acceptance_threshold,
                    "max_auto_iterations": execution_options.max_auto_iterations,
                    "task_spec": task_spec.model_dump(mode="json"),
                },
                native_runner=_native_transform,
            )
            transformation_payload = (
                transform_output.get("transformation")
                if isinstance(transform_output.get("transformation"), dict)
                else transform_output
            )
            reflection_payload = transform_output.get("reflection")
            if not isinstance(reflection_payload, dict):
                reflection_payload = transform_output.get("iterations")
            transformation = engine._validate_or_fail(
                TransformationArtifact,
                transformation_payload,
                "code",
            )
            if transformation is None:
                return {"success": False, "stage": stage, "error": "invalid transformation artifact"}
            if isinstance(reflection_payload, dict):
                iterations = engine._validate_or_fail(
                    TransformationIterationArtifact,
                    reflection_payload,
                    "code",
                )
            else:
                iterations = TransformationIterationArtifact(
                    meta=build_meta(
                        task_id=program_task_id,
                        trace_id=trace_id,
                        stage="code",
                        source_language=task_spec.source_language,
                        target_language=task_spec.target_language,
                        module=module,
                    ),
                    iterations=[
                        TransformationIterationRecord(
                            iteration=1,
                            score=None,
                            accepted=True,
                            feedback=["skill output did not include reflection details"],
                            suggestions=[],
                        )
                    ],
                    accepted=True,
                    final_score=None,
                    unknowns=[],
                    extensions={},
                )
            if iterations is None:
                return {
                    "success": False,
                    "stage": stage,
                    "error": "invalid transformation iteration artifact",
                }
            transformation_path = await artifact_store.write_json(
                stage="code",
                task_id=program_task_id,
                artifact_name="transformation",
                payload=transformation.model_dump(mode="json"),
            )
            iteration_path = await artifact_store.write_json(
                stage="code",
                task_id=program_task_id,
                artifact_name="transformation_iterations",
                payload=iterations.model_dump(mode="json"),
            )
            artifact_paths["code"] = str(transformation_path)
            artifact_paths["code_iterations"] = str(iteration_path)
            for generated in transformation.generated_files:
                await artifact_store.write_text(
                    stage="code",
                    task_id=program_task_id,
                    artifact_name=generated.path.replace("/", "_").replace(".java", ""),
                    extension="java",
                    content=generated.content,
                )
            await artifact_store.write_text(
                stage="code",
                task_id=program_task_id,
                artifact_name="target_code",
                extension="java",
                content=transformation.target_code,
            )
            unknowns = transformation.model_dump(mode="json").get("unknowns", [])
            _, safety_evidence = await _check_safe_output(
                engine,
                text=transformation.target_code,
                stage="transform",
                module=module,
            )
            evidence = {
                "reflection_iterations": len(iterations.iterations),
                "warnings": list(transformation.warnings),
            }
            evidence.update(safety_evidence)
            if evidence.get("safety_review"):
                unknowns.append(
                    {
                        "field": "target_code",
                        "reason": "transform output requires safety review",
                    }
                )

        elif stage == "tests":
            analysis = _read_json_if_exists(
                _artifact_path(module_root, stage="analysis", task_id=program_task_id, artifact_name="legacy_analysis")
            )
            semantics = _read_json_if_exists(
                _artifact_path(
                    module_root,
                    stage="business_semantics",
                    task_id=program_task_id,
                    artifact_name="business_semantics",
                )
            )
            if analysis is None or semantics is None:
                return {"success": False, "stage": stage, "error": "analysis/business_semantics artifact missing"}
            payload = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage="tests",
                payload={
                    "legacy_analysis": analysis,
                    "business_semantics": semantics,
                    "expected_outputs": expected_outputs,
                    "target_language": task_spec.target_language,
                },
                native_runner=engine._test_synthesis_agent.process,
            )
            artifact = engine._validate_or_fail(TestSynthesisArtifact, payload, "tests")
            if artifact is None:
                return {"success": False, "stage": stage, "error": "invalid test artifact"}
            path = await artifact_store.write_json(
                stage="tests",
                task_id=program_task_id,
                artifact_name="test_synthesis",
                payload=artifact.model_dump(mode="json"),
            )
            artifact_paths["tests"] = str(path)
            unknowns = artifact.model_dump(mode="json").get("unknowns", [])
            evidence = dict(artifact.evidence)

        elif stage in {"diff", "strict_verification"}:
            transformation = _read_json_if_exists(
                _artifact_path(module_root, stage="code", task_id=program_task_id, artifact_name="transformation")
            )
            tests = _read_json_if_exists(
                _artifact_path(module_root, stage="tests", task_id=program_task_id, artifact_name="test_synthesis")
            )
            if transformation is None or tests is None:
                return {"success": False, "stage": stage, "error": "transformation/tests artifact missing"}
            force_strict = stage == "strict_verification"
            payload = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage=stage,
                payload={
                    "transformation": transformation,
                    "test_synthesis": tests,
                    "fast_mode": False if force_strict else fast_mode,
                },
                native_runner=engine._differential_agent.process,
            )
            artifact = engine._validate_or_fail(DifferentialVerificationArtifact, payload, "diff")
            if artifact is None:
                return {"success": False, "stage": stage, "error": "invalid differential artifact"}
            artifact_name = "differential_strict" if force_strict else "differential"
            key = "diff_strict" if force_strict else "diff"
            path = await artifact_store.write_json(
                stage="diff",
                task_id=program_task_id,
                artifact_name=artifact_name,
                payload=artifact.model_dump(mode="json"),
            )
            artifact_paths[key] = str(path)
            unknowns = artifact.model_dump(mode="json").get("unknowns", [])
            evidence = dict(artifact.evidence)
            decision = "PASSED" if artifact.equivalence else "ISSUE"

        elif stage == "quality":
            design = _read_json_if_exists(
                _artifact_path(module_root, stage="design", task_id=program_task_id, artifact_name="migration_design")
            )
            tests = _read_json_if_exists(
                _artifact_path(module_root, stage="tests", task_id=program_task_id, artifact_name="test_synthesis")
            )
            differential = _read_json_if_exists(
                _artifact_path(module_root, stage="diff", task_id=program_task_id, artifact_name="differential_strict")
            )
            if differential is None:
                differential = _read_json_if_exists(
                    _artifact_path(module_root, stage="diff", task_id=program_task_id, artifact_name="differential")
                )
            if design is None or tests is None or differential is None:
                return {"success": False, "stage": stage, "error": "design/tests/diff artifact missing"}
            known_legacy_issues = await engine._load_known_legacy_issues(inputs)
            payload = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage="quality",
                payload={
                    "differential": differential,
                    "migration_design": design,
                    "test_synthesis": tests,
                    "known_legacy_issues": known_legacy_issues,
                },
                native_runner=engine._quality_gate_agent.process,
            )
            artifact = engine._validate_or_fail(QualityGateArtifact, payload, "quality")
            if artifact is None:
                return {"success": False, "stage": stage, "error": "invalid quality artifact"}
            path = await artifact_store.write_json(
                stage="quality",
                task_id=program_task_id,
                artifact_name="quality_gate",
                payload=artifact.model_dump(mode="json"),
            )
            artifact_paths["quality"] = str(path)
            unknowns = artifact.model_dump(mode="json").get("unknowns", [])
            evidence = dict(artifact.evidence)
            decision = artifact.decision.value

        elif stage == "fix":
            transformation = _read_json_if_exists(
                _artifact_path(module_root, stage="code", task_id=program_task_id, artifact_name="transformation")
            )
            design = _read_json_if_exists(
                _artifact_path(module_root, stage="design", task_id=program_task_id, artifact_name="migration_design")
            )
            quality = _read_json_if_exists(
                _artifact_path(module_root, stage="quality", task_id=program_task_id, artifact_name="quality_gate")
            )
            if transformation is None or design is None or quality is None:
                return {"success": False, "stage": stage, "error": "transformation/design/quality artifact missing"}
            quality_decision = str(quality.get("decision", QualityDecision.PASSED.value))

            def _native_fix(payload: dict[str, Any]) -> dict[str, Any]:
                if quality_decision == QualityDecision.TRANSFORM_ISSUE.value:
                    fix_payload = engine._limited_fixer_agent.process(payload)
                    if not isinstance(fix_payload, dict):
                        msg = "limited fixer must return dict payload"
                        raise TypeError(msg)
                    return fix_payload
                transformed = TransformationArtifact.model_validate(transformation)
                return LimitedFixArtifact(
                    meta=build_meta(
                        task_id=program_task_id,
                        trace_id=trace_id,
                        stage="fix",
                        source_language=task_spec.source_language,
                        target_language=task_spec.target_language,
                        module=module,
                    ),
                    applied=False,
                    target_code=transformed.target_code,
                    patch_summary=["no fix required by quality decision"],
                    retest_required=False,
                    unknowns=[],
                    extensions={},
                ).model_dump(mode="json")

            payload = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage="fix",
                payload={
                    "quality_gate": quality,
                    "transformation": transformation,
                    "migration_design": design,
                },
                native_runner=_native_fix,
            )
            artifact = engine._validate_or_fail(LimitedFixArtifact, payload, "fix")
            if artifact is None:
                return {"success": False, "stage": stage, "error": "invalid fix artifact"}
            path = await artifact_store.write_json(
                stage="fix",
                task_id=program_task_id,
                artifact_name="limited_fix",
                payload=artifact.model_dump(mode="json"),
            )
            artifact_paths["fix"] = str(path)
            unknowns = artifact.model_dump(mode="json").get("unknowns", [])
            evidence = {
                "applied": artifact.applied,
                "retest_required": artifact.retest_required,
            }

        elif stage == "report":
            analysis = _read_json_if_exists(
                _artifact_path(module_root, stage="analysis", task_id=program_task_id, artifact_name="legacy_analysis")
            )
            semantics = _read_json_if_exists(
                _artifact_path(
                    module_root,
                    stage="business_semantics",
                    task_id=program_task_id,
                    artifact_name="business_semantics",
                )
            )
            design = _read_json_if_exists(
                _artifact_path(module_root, stage="design", task_id=program_task_id, artifact_name="migration_design")
            )
            transformation = _read_json_if_exists(
                _artifact_path(module_root, stage="code", task_id=program_task_id, artifact_name="transformation")
            )
            quality = _read_json_if_exists(
                _artifact_path(module_root, stage="quality", task_id=program_task_id, artifact_name="quality_gate")
            )
            fix = _read_json_if_exists(
                _artifact_path(module_root, stage="fix", task_id=program_task_id, artifact_name="limited_fix")
            )
            if (
                analysis is None
                or semantics is None
                or design is None
                or transformation is None
                or quality is None
                or fix is None
            ):
                return {"success": False, "stage": stage, "error": "required artifacts missing for report"}
            report = await _run_stage_with_capability(
                engine=engine,
                capability_runner=capability_runner,
                stage="report",
                payload={
                    "task_id": program_task_id,
                    "migration_type": engine._migration_type,
                    "source_code": source_code,
                    "analysis": analysis,
                    "business_semantics": semantics,
                    "design": design,
                    "transformation": transformation,
                    "quality": quality,
                    "fix": fix,
                },
                native_runner=engine._compliance_reporter_agent.process,
            )
            report_markdown = str(report.get("report_markdown", "# report generation failed"))
            report_markdown, safety_evidence = await _check_safe_output(
                engine,
                text=report_markdown,
                stage="report",
                module=module,
            )
            path = await artifact_store.write_text(
                stage="report",
                task_id=program_task_id,
                artifact_name="compliance_report",
                extension="md",
                content=report_markdown,
            )
            artifact_paths["report"] = str(path)
            evidence = {"report_length": len(report_markdown)}
            evidence.update(safety_evidence)

        await engine._log_completion(
            tool,
            program_task_id,
            {
                "stage": stage,
                "module": module,
                "artifact_paths": artifact_paths,
            },
            flow_context,
        )
        return {
            "success": True,
            "stage": stage,
            "module": module,
            "artifact_paths": artifact_paths,
            "unknowns": unknowns,
            "decision": decision,
            "evidence": evidence,
            "capability_trace": engine.get_capability_trace(),
        }
    except Exception as exc:
        return {
            "success": False,
            "stage": stage,
            "module": module,
            "error": str(exc),
            "artifact_paths": artifact_paths,
            "unknowns": unknowns,
            "decision": decision,
            "evidence": evidence,
            "capability_trace": engine.get_capability_trace(),
        }
    finally:
        await artifact_store.release_lock(lock_path)
