"""Code migration pipeline runtime helpers."""

from __future__ import annotations

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
from shared.integrations.context_bridge import get_current_context


async def run_pipeline(engine: Any, inputs: dict[str, Any]) -> dict[str, Any]:
    """Run migration pipeline using engine internals."""
    from apps.code_migration_assistant import engine as engine_module

    engine._ensure_agents_ready()
    if engine._killed:
        return {"success": False, "error": "Pipeline killed by Kill Switch"}

    source_code = inputs.get("source_code") or inputs.get("cobol_code", "")
    if not source_code:
        return {"success": False, "error": "source_code is required"}

    expected_outputs = inputs.get("expected_outputs", {})
    if not isinstance(expected_outputs, dict):
        return {"success": False, "error": "expected_outputs must be dict"}

    execution_options = resolve_execution_options(inputs)
    fast_mode_raw = inputs.get("fast_mode")
    fast_mode = execution_options.verification_mode == "fast" if fast_mode_raw is None else bool(fast_mode_raw)

    task_id = str(inputs.get("task_id") or f"task-{uuid.uuid4().hex[:12]}")
    trace_id = str(inputs.get("trace_id") or task_id)
    module = str(inputs.get("module") or "UNKNOWN")
    skill_executor = inputs.get("skill_executor")
    if not callable(skill_executor):
        skill_executor = None
    capability_runner = StageCapabilityRunner(
        skill_mode=execution_options.skill_mode,
        skill_executor=skill_executor,
    )

    flow_context = get_current_context()
    if flow_context is None and hasattr(engine, "_resolve_flow_context"):
        flow_context = engine._resolve_flow_context()
    incoming_human_facts = inputs.get("human_facts", [])
    if isinstance(incoming_human_facts, list):
        for fact in incoming_human_facts:
            if isinstance(fact, dict):
                engine._human_facts.append(fact)

    task_spec = engine._build_task_spec(
        task_id=task_id,
        trace_id=trace_id,
        module=module,
        expected_outputs=expected_outputs,
        options=execution_options.to_dict(),
    )

    artifacts_dir = inputs.get("artifacts_dir")
    decisions_path = inputs.get("decisions_path")
    failures_path = inputs.get("failures_path")
    artifact_store = engine_module.ArtifactStore(
        base_dir=Path(str(artifacts_dir)) if artifacts_dir else None,
        decisions_path=Path(str(decisions_path)) if decisions_path else None,
        failures_path=Path(str(failures_path)) if failures_path else None,
    )
    await artifact_store.initialize()
    lock_path = await artifact_store.acquire_lock(task_id)

    artifact_paths: dict[str, str] = {}

    try:
        await engine._check_governance(
            engine_module.MIGRATION_ANALYSIS,
            task_id,
            {"module": module},
            flow_context,
        )
        analysis_execution = await capability_runner.run_stage(
            stage="analysis",
            payload={
                "source_code": source_code,
                "task_spec": task_spec.model_dump(mode="json"),
            },
            native_runner=engine._legacy_analysis_agent.process,
        )
        engine._append_capability_trace(analysis_execution.trace)
        analysis = analysis_execution.output
        analysis_artifact = engine._validate_or_fail(
            engine_module.LegacyAnalysisArtifact,
            analysis,
            "analysis",
        )
        if analysis_artifact is None:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="analysis",
                responsible_stage="analysis",
                reason="schema validation failed",
            )
            return {"success": False, "stage": "analysis", "error": "invalid analysis artifact"}

        analysis_path = await artifact_store.write_json(
            stage="analysis",
            task_id=task_id,
            artifact_name="legacy_analysis",
            payload=analysis_artifact.model_dump(mode="json"),
        )
        artifact_paths["analysis"] = str(analysis_path)
        await artifact_store.append_decision(task_id, "分析工程を完了")
        analysis_for_next = await artifact_store.read_json(analysis_path)
        await engine._log_completion(
            engine_module.MIGRATION_ANALYSIS,
            task_id,
            {"path": str(analysis_path)},
            flow_context,
        )

        await engine._check_governance(engine_module.MIGRATION_BUSINESS_SEMANTICS, task_id, {}, flow_context)
        semantics_execution = await capability_runner.run_stage(
            stage="business_semantics",
            payload={
                "legacy_analysis": analysis_for_next,
                "business_context": execution_options.business_context or {},
                "human_facts": engine._human_facts,
            },
            native_runner=engine._business_semantics_agent.process,
        )
        engine._append_capability_trace(semantics_execution.trace)
        business_semantics = semantics_execution.output
        semantics_artifact = engine._validate_or_fail(
            BusinessSemanticsArtifact,
            business_semantics,
            "business_semantics",
        )
        if semantics_artifact is None:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="business_semantics",
                responsible_stage="business_semantics",
                reason="schema validation failed",
            )
            return {
                "success": False,
                "stage": "business_semantics",
                "error": "invalid business semantics artifact",
            }

        semantics_path = await artifact_store.write_json(
            stage="business_semantics",
            task_id=task_id,
            artifact_name="business_semantics",
            payload=semantics_artifact.model_dump(mode="json"),
        )
        artifact_paths["business_semantics"] = str(semantics_path)
        await artifact_store.append_decision(task_id, "業務語義工程を完了")
        semantics_for_next = await artifact_store.read_json(semantics_path)
        await engine._log_completion(
            engine_module.MIGRATION_BUSINESS_SEMANTICS,
            task_id,
            {"path": str(semantics_path)},
            flow_context,
        )

        await engine._check_governance(engine_module.MIGRATION_DESIGN, task_id, {}, flow_context)
        design = engine._migration_design_agent.process(
            {
                "legacy_analysis": analysis_for_next,
                "business_semantics": semantics_for_next,
            }
        )
        design_artifact = engine._validate_or_fail(MigrationDesignArtifact, design, "design")
        if design_artifact is None:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="design",
                responsible_stage="design",
                reason="schema validation failed",
            )
            return {"success": False, "stage": "design", "error": "invalid design artifact"}

        design_path = await artifact_store.write_json(
            stage="design",
            task_id=task_id,
            artifact_name="migration_design",
            payload=design_artifact.model_dump(mode="json"),
        )
        artifact_paths["design"] = str(design_path)
        await artifact_store.append_decision(task_id, "設計工程を完了")
        design_for_next = await artifact_store.read_json(design_path)
        await engine._log_completion(
            engine_module.MIGRATION_DESIGN,
            task_id,
            {"path": str(design_path)},
            flow_context,
        )

        if bool(inputs.get("plan_only", False)) or bool(inputs.get("assessment_only", False)):
            return {
                "success": True,
                "plan_only": True,
                "task_id": task_id,
                "trace_id": trace_id,
                "migration_type": engine._migration_type,
                "artifact_paths": artifact_paths,
                "analysis": analysis_artifact.model_dump(mode="json"),
                "business_semantics": semantics_artifact.model_dump(mode="json"),
                "design": design_artifact.model_dump(mode="json"),
                "execution_options": execution_options.to_dict(),
                "capability_trace": engine.get_capability_trace(),
            }

        if engine._killed:
            return {"success": False, "error": "Pipeline killed by Kill Switch"}

        need_approval = should_require_human_approval(
            tool_risk=engine_module.MIGRATION_DESIGN.risk_level,
            execution_options=execution_options,
        )
        if need_approval:
            approved = await engine._request_design_approval(task_id, module, design_artifact, design_path)
            if not approved:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="design",
                    responsible_stage="approval",
                    reason="Migration Design Rejected/Timeout",
                )
                return {
                    "success": False,
                    "stage": "design",
                    "error": "Migration Design Rejected or Timeout",
                }
        else:
            await artifact_store.append_decision(task_id, "設計承認を自動通過（risk-based policy）")
            if engine._event_queue:
                await engine._event_queue.put(
                    {
                        "event_type": "approval_submitted",
                        "request_id": f"auto-{task_id}",
                        "approved": True,
                        "approver": "system",
                        "comment": "Auto-approved by risk policy",
                    }
                )

        await engine._check_governance(
            engine_module.MIGRATION_TRANSFORM,
            task_id,
            {"fast_mode": fast_mode},
            flow_context,
        )
        transform_payload = {
            "source_code": source_code,
            "analysis": analysis_for_next,
            "migration_design": design_for_next,
            "fast_mode": fast_mode,
            "acceptance_threshold": execution_options.acceptance_threshold,
            "max_auto_iterations": execution_options.max_auto_iterations,
            "task_spec": task_spec.model_dump(mode="json"),
        }

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

        transform_execution = await capability_runner.run_stage(
            stage="transform",
            payload=transform_payload,
            native_runner=_native_transform,
        )
        engine._append_capability_trace(transform_execution.trace)
        try:
            transform_output = transform_execution.output

            transformation_payload = (
                transform_output.get("transformation")
                if isinstance(transform_output.get("transformation"), dict)
                else transform_output
            )
            reflection_payload = transform_output.get("reflection")
            if not isinstance(reflection_payload, dict):
                reflection_payload = transform_output.get("iterations")

            transformation_artifact = engine._validate_or_fail(
                TransformationArtifact,
                transformation_payload,
                "code",
            )
            if transformation_artifact is None:
                msg = "invalid transformation artifact"
                raise ValueError(msg)

            if isinstance(reflection_payload, dict):
                iteration_artifact = engine._validate_or_fail(
                    TransformationIterationArtifact,
                    reflection_payload,
                    "code",
                )
            else:
                iteration_artifact = TransformationIterationArtifact(
                    meta=build_meta(
                        task_id=task_id,
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
            if iteration_artifact is None:
                msg = "invalid transformation iteration artifact"
                raise ValueError(msg)
        except ValueError:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="code",
                responsible_stage="transform",
                reason="schema validation failed",
            )
            return {
                "success": False,
                "stage": "transform",
                "error": "invalid transformation artifact",
            }

        transformation_path = await artifact_store.write_json(
            stage="code",
            task_id=task_id,
            artifact_name="transformation",
            payload=transformation_artifact.model_dump(mode="json"),
        )
        artifact_paths["code"] = str(transformation_path)

        iteration_path = await artifact_store.write_json(
            stage="code",
            task_id=task_id,
            artifact_name="transformation_iterations",
            payload=iteration_artifact.model_dump(mode="json"),
        )
        artifact_paths["code_iterations"] = str(iteration_path)

        for gf in transformation_artifact.generated_files:
            await artifact_store.write_text(
                stage="code",
                task_id=task_id,
                artifact_name=gf.path.replace("/", "_").replace(".java", ""),
                extension="java",
                content=gf.content,
            )

        await artifact_store.write_text(
            stage="code",
            task_id=task_id,
            artifact_name="target_code",
            extension="java",
            content=transformation_artifact.target_code,
        )
        await artifact_store.append_decision(task_id, "変換工程を完了")
        transformation_for_next = await artifact_store.read_json(transformation_path)
        await engine._log_completion(
            engine_module.MIGRATION_TRANSFORM,
            task_id,
            {"path": str(transformation_path)},
            flow_context,
        )

        await engine._check_governance(engine_module.MIGRATION_TEST_GEN, task_id, {}, flow_context)
        test_synthesis = engine._test_synthesis_agent.process(
            {
                "legacy_analysis": analysis_for_next,
                "business_semantics": semantics_for_next,
                "expected_outputs": expected_outputs,
                "target_language": task_spec.target_language,
            }
        )
        test_artifact = engine._validate_or_fail(TestSynthesisArtifact, test_synthesis, "tests")
        if test_artifact is None:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="tests",
                responsible_stage="test",
                reason="schema validation failed",
            )
            return {"success": False, "stage": "tests", "error": "invalid test artifact"}

        tests_path = await artifact_store.write_json(
            stage="tests",
            task_id=task_id,
            artifact_name="test_synthesis",
            payload=test_artifact.model_dump(mode="json"),
        )
        artifact_paths["tests"] = str(tests_path)
        await artifact_store.append_decision(task_id, "テスト生成工程を完了")
        tests_for_next = await artifact_store.read_json(tests_path)
        await engine._log_completion(
            engine_module.MIGRATION_TEST_GEN,
            task_id,
            {"path": str(tests_path)},
            flow_context,
        )

        await engine._check_governance(engine_module.MIGRATION_VERIFY_DIFF, task_id, {}, flow_context)
        differential = engine._differential_agent.process(
            {
                "transformation": transformation_for_next,
                "test_synthesis": tests_for_next,
                "fast_mode": fast_mode,
            }
        )
        diff_artifact = engine._validate_or_fail(
            DifferentialVerificationArtifact,
            differential,
            "diff",
        )
        if diff_artifact is None:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="diff",
                responsible_stage="diff",
                reason="schema validation failed",
            )
            return {"success": False, "stage": "diff", "error": "invalid differential artifact"}

        diff_path = await artifact_store.write_json(
            stage="diff",
            task_id=task_id,
            artifact_name="differential",
            payload=diff_artifact.model_dump(mode="json"),
        )
        artifact_paths["diff"] = str(diff_path)
        await artifact_store.append_decision(task_id, "差分検証工程を完了")
        diff_for_next = await artifact_store.read_json(diff_path)
        await engine._log_completion(
            engine_module.MIGRATION_VERIFY_DIFF,
            task_id,
            {"path": str(diff_path)},
            flow_context,
        )

        await engine._check_governance(engine_module.MIGRATION_QUALITY_GATE, task_id, {}, flow_context)
        known_legacy_issues = await engine._load_known_legacy_issues(inputs)
        quality_gate = engine._quality_gate_agent.process(
            {
                "differential": diff_for_next,
                "migration_design": design_for_next,
                "test_synthesis": tests_for_next,
                "known_legacy_issues": known_legacy_issues,
            }
        )
        quality_artifact = engine._validate_or_fail(QualityGateArtifact, quality_gate, "quality")
        if quality_artifact is None:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="quality",
                responsible_stage="quality",
                reason="schema validation failed",
            )
            return {"success": False, "stage": "quality", "error": "invalid quality artifact"}

        quality_path = await artifact_store.write_json(
            stage="quality",
            task_id=task_id,
            artifact_name="quality_gate",
            payload=quality_artifact.model_dump(mode="json"),
        )
        artifact_paths["quality"] = str(quality_path)
        await artifact_store.append_decision(
            task_id,
            f"品質裁定: {quality_artifact.decision.value} / next={quality_artifact.target_agent}",
        )
        quality_for_next = await artifact_store.read_json(quality_path)
        await engine._log_completion(
            engine_module.MIGRATION_QUALITY_GATE,
            task_id,
            {"decision": quality_artifact.decision.value},
            flow_context,
        )

        fix_artifact: LimitedFixArtifact
        if quality_artifact.decision == QualityDecision.TRANSFORM_ISSUE:
            await engine._check_governance(engine_module.MIGRATION_FIX, task_id, {}, flow_context)
            fix = engine._limited_fixer_agent.process(
                {
                    "quality_gate": quality_for_next,
                    "transformation": transformation_for_next,
                    "migration_design": design_for_next,
                }
            )
            fix_artifact = engine._validate_or_fail(LimitedFixArtifact, fix, "fix")
        else:
            fix_artifact = LimitedFixArtifact(
                meta=build_meta(
                    task_id=task_id,
                    trace_id=trace_id,
                    stage="fix",
                    source_language=task_spec.source_language,
                    target_language=task_spec.target_language,
                    module=module,
                ),
                applied=False,
                target_code=transformation_artifact.target_code,
                patch_summary=["修正不要（裁定が TRANSFORM_ISSUE 以外）"],
                retest_required=False,
                unknowns=[],
                extensions={},
            )

        fix_path = await artifact_store.write_json(
            stage="fix",
            task_id=task_id,
            artifact_name="limited_fix",
            payload=fix_artifact.model_dump(mode="json"),
        )
        artifact_paths["fix"] = str(fix_path)
        await artifact_store.append_decision(task_id, "限定修正工程を完了")
        if quality_artifact.decision == QualityDecision.TRANSFORM_ISSUE:
            await engine._log_completion(
                engine_module.MIGRATION_FIX,
                task_id,
                {"applied": fix_artifact.applied},
                flow_context,
            )

        final_differential = diff_artifact
        final_quality = quality_artifact
        iterations = 1

        if fix_artifact.retest_required:
            iterations = 2
            adjusted_transformation = transformation_for_next.copy()
            adjusted_transformation["target_code"] = fix_artifact.target_code

            post_differential = engine._differential_agent.process(
                {
                    "transformation": adjusted_transformation,
                    "test_synthesis": tests_for_next,
                    "fast_mode": fast_mode,
                }
            )
            post_diff_artifact = DifferentialVerificationArtifact.model_validate(post_differential)
            post_diff_path = await artifact_store.write_json(
                stage="diff",
                task_id=task_id,
                artifact_name="differential_post_fix",
                payload=post_diff_artifact.model_dump(mode="json"),
            )
            artifact_paths["diff_post_fix"] = str(post_diff_path)

            post_quality = engine._quality_gate_agent.process(
                {
                    "differential": post_diff_artifact.model_dump(mode="json"),
                    "migration_design": design_for_next,
                    "test_synthesis": tests_for_next,
                    "known_legacy_issues": known_legacy_issues,
                }
            )
            post_quality_artifact = QualityGateArtifact.model_validate(post_quality)
            post_quality_path = await artifact_store.write_json(
                stage="quality",
                task_id=task_id,
                artifact_name="quality_gate_post_fix",
                payload=post_quality_artifact.model_dump(mode="json"),
            )
            artifact_paths["quality_post_fix"] = str(post_quality_path)

            final_differential = post_diff_artifact
            final_quality = post_quality_artifact
            await artifact_store.append_decision(task_id, f"再裁定: {final_quality.decision.value}")

        success = final_quality.decision in {
            QualityDecision.PASSED,
            QualityDecision.KNOWN_LEGACY,
        }

        if not success:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="quality",
                responsible_stage=final_quality.decision.value,
                reason=final_quality.reason,
            )

        await engine._check_governance(engine_module.MIGRATION_REPORT, task_id, {}, flow_context)
        report_execution = await capability_runner.run_stage(
            stage="report",
            payload={
                "task_id": task_id,
                "migration_type": engine._migration_type,
                "source_code": source_code,
                "analysis": analysis_artifact.model_dump(mode="json"),
                "business_semantics": semantics_artifact.model_dump(mode="json"),
                "design": design_artifact.model_dump(mode="json"),
                "transformation": transformation_artifact.model_dump(mode="json"),
                "quality": final_quality.model_dump(mode="json"),
                "fix": fix_artifact.model_dump(mode="json"),
            },
            native_runner=engine._compliance_reporter_agent.process,
        )
        engine._append_capability_trace(report_execution.trace)
        report = report_execution.output

        report_path = await artifact_store.write_text(
            stage="report",
            task_id=task_id,
            artifact_name="compliance_report",
            extension="md",
            content=report.get("report_markdown", "# Report Generation Failed"),
        )
        artifact_paths["report"] = str(report_path)
        await engine._log_completion(
            engine_module.MIGRATION_REPORT,
            task_id,
            {"path": str(report_path)},
            flow_context,
        )

        class_name = design_artifact.class_mapping.get("primary_class", "MigratedProgram")
        final_target_code = fix_artifact.target_code if fix_artifact.applied else transformation_artifact.target_code

        return {
            "success": success,
            "migration_type": engine._migration_type,
            "source_language": task_spec.source_language,
            "target_language": task_spec.target_language,
            "task_id": task_id,
            "trace_id": trace_id,
            "class_name": class_name,
            "target_code": final_target_code,
            "verdict": "PASS" if success else "FAIL",
            "iterations": iterations,
            "check_result": final_differential.model_dump(mode="json"),
            "quality_gate": final_quality.model_dump(mode="json"),
            "artifact_paths": artifact_paths,
            "business_semantics": semantics_artifact.model_dump(mode="json"),
            "reflection": iteration_artifact.model_dump(mode="json"),
            "execution_options": execution_options.to_dict(),
            "fast_mode": fast_mode,
            "capability_trace": engine.get_capability_trace(),
        }
    finally:
        await artifact_store.release_lock(lock_path)


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
    stage_dir = _ARTIFACT_STAGE_DIRS[stage]
    return module_root / stage_dir / f"{task_id}_{artifact_name}.{extension}"


def _read_json_if_exists(path: Path) -> dict[str, Any] | None:
    if not path.exists():
        return None
    try:
        import json

        raw = json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return None
    return raw if isinstance(raw, dict) else None


async def execute_stage_task(engine: Any, inputs: dict[str, Any]) -> dict[str, Any]:
    """Execute a single stage task for backlog dispatcher."""
    from apps.code_migration_assistant import engine as engine_module

    engine._ensure_agents_ready()
    stage = str(inputs.get("stage", "")).strip()
    if not stage:
        return {"success": False, "error": "stage is required"}

    source_code = str(inputs.get("source_code") or "")
    if not source_code:
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
            payload = engine._legacy_analysis_agent.process(
                {
                    "source_code": source_code,
                    "task_spec": task_spec.model_dump(mode="json"),
                }
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
            payload = engine._business_semantics_agent.process(
                {
                    "legacy_analysis": analysis,
                    "business_context": execution_options.business_context or {},
                    "human_facts": getattr(engine, "_human_facts", []),
                }
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
            payload = engine._migration_design_agent.process(
                {
                    "legacy_analysis": analysis,
                    "business_semantics": semantics,
                }
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

        elif stage == "transform":
            analysis = _read_json_if_exists(
                _artifact_path(module_root, stage="analysis", task_id=program_task_id, artifact_name="legacy_analysis")
            )
            design = _read_json_if_exists(
                _artifact_path(module_root, stage="design", task_id=program_task_id, artifact_name="migration_design")
            )
            if analysis is None or design is None:
                return {"success": False, "stage": stage, "error": "analysis/design artifact missing"}
            transformation, iterations = await engine._run_transformation_with_reflection(
                source_code=source_code,
                analysis=analysis,
                migration_design=design,
                fast_mode=fast_mode,
                acceptance_threshold=execution_options.acceptance_threshold,
                max_auto_iterations=execution_options.max_auto_iterations,
            )
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
            evidence = {
                "reflection_iterations": len(iterations.iterations),
                "warnings": list(transformation.warnings),
            }

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
            payload = engine._test_synthesis_agent.process(
                {
                    "legacy_analysis": analysis,
                    "business_semantics": semantics,
                    "expected_outputs": expected_outputs,
                    "target_language": task_spec.target_language,
                }
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
            payload = engine._differential_agent.process(
                {
                    "transformation": transformation,
                    "test_synthesis": tests,
                    "fast_mode": False if force_strict else fast_mode,
                }
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
            payload = engine._quality_gate_agent.process(
                {
                    "differential": differential,
                    "migration_design": design,
                    "test_synthesis": tests,
                    "known_legacy_issues": known_legacy_issues,
                }
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
            if quality_decision == QualityDecision.TRANSFORM_ISSUE.value:
                payload = engine._limited_fixer_agent.process(
                    {
                        "quality_gate": quality,
                        "transformation": transformation,
                        "migration_design": design,
                    }
                )
                artifact = engine._validate_or_fail(LimitedFixArtifact, payload, "fix")
            else:
                transformed = TransformationArtifact.model_validate(transformation)
                artifact = LimitedFixArtifact(
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
                )
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
            report = engine._compliance_reporter_agent.process(
                {
                    "task_id": program_task_id,
                    "migration_type": engine._migration_type,
                    "source_code": source_code,
                    "analysis": analysis,
                    "business_semantics": semantics,
                    "design": design,
                    "transformation": transformation,
                    "quality": quality,
                    "fix": fix,
                }
            )
            report_markdown = str(report.get("report_markdown", "# report generation failed"))
            path = await artifact_store.write_text(
                stage="report",
                task_id=program_task_id,
                artifact_name="compliance_report",
                extension="md",
                content=report_markdown,
            )
            artifact_paths["report"] = str(path)
            evidence = {"report_length": len(report_markdown)}

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
        }
    finally:
        await artifact_store.release_lock(lock_path)
