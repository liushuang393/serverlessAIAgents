# -*- coding: utf-8 -*-
"""Code migration pipeline runtime helpers."""

from __future__ import annotations

import uuid
from pathlib import Path
from typing import Any

from agentflow.integrations.context_bridge import get_current_context

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
    fast_mode = (
        execution_options.verification_mode == "fast"
        if fast_mode_raw is None
        else bool(fast_mode_raw)
    )

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
            native_runner=lambda payload: engine._legacy_analysis_agent.process(payload),
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
            native_runner=lambda payload: engine._business_semantics_agent.process(payload),
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
                raise ValueError("invalid transformation artifact")

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
                raise ValueError("invalid transformation iteration artifact")
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
            fix_artifact = engine._validate_or_fail(LimitedFixArtifact, fix, "fix")  # type: ignore[assignment]
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

        if fix_artifact is None:
            await artifact_store.append_failure(
                task_id=task_id,
                stage="fix",
                responsible_stage="fix",
                reason="schema validation failed",
            )
            return {"success": False, "stage": "fix", "error": "invalid fix artifact"}

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
            native_runner=lambda payload: engine._compliance_reporter_agent.process(payload),
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
