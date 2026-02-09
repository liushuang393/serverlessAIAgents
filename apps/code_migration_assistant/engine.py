# -*- coding: utf-8 -*-
"""Code Migration Engine - 工程固定型パイプライン.

固定工程:
分析 → 設計 → 変換 → テスト生成 → 差分検証 → 品質裁定 → 限定修正

工程間の受け渡しは artifacts/ 配下の JSON を介して行う。
"""

from __future__ import annotations

import json
import uuid
from pathlib import Path
from typing import Any

from agentflow.engines.base import BaseEngine, EngineConfig
from agentflow.security import SafetyMixin

from apps.code_migration_assistant.adapters import get_adapter_factory
from apps.code_migration_assistant.agents import (
    CodeTransformationAgent,
    DifferentialVerificationAgent,
    LegacyAnalysisAgent,
    LimitedFixerAgent,
    MigrationDesignAgent,
    QualityGateAgent,
    TestSynthesisAgent,
)
from apps.code_migration_assistant.workflow.artifacts import ArtifactStore
from apps.code_migration_assistant.workflow.models import (
    DifferentialVerificationArtifact,
    LegacyAnalysisArtifact,
    LimitedFixArtifact,
    MigrationDesignArtifact,
    QualityDecision,
    QualityGateArtifact,
    TaskSpec,
    TestSynthesisArtifact,
    TransformationArtifact,
)


class CodeMigrationEngine(BaseEngine, SafetyMixin):
    """コード移行 Engine（工程固定版）."""

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        config: EngineConfig | None = None,
        enable_safety: bool = True,
    ) -> None:
        """初期化."""
        engine_config = config or EngineConfig(name="code_migration_engine")
        super().__init__(config=engine_config)
        self._migration_type = migration_type
        self._factory = get_adapter_factory()

        self._legacy_analysis_agent: LegacyAnalysisAgent | None = None
        self._migration_design_agent: MigrationDesignAgent | None = None
        self._code_transformation_agent: CodeTransformationAgent | None = None
        self._test_synthesis_agent: TestSynthesisAgent | None = None
        self._differential_agent: DifferentialVerificationAgent | None = None
        self._quality_gate_agent: QualityGateAgent | None = None
        self._limited_fixer_agent: LimitedFixerAgent | None = None

        self.init_safety(enabled=enable_safety)

    async def _initialize(self) -> None:
        """内部 Agent を初期化."""
        self._legacy_analysis_agent = LegacyAnalysisAgent(migration_type=self._migration_type)
        self._migration_design_agent = MigrationDesignAgent(migration_type=self._migration_type)
        self._code_transformation_agent = CodeTransformationAgent(
            migration_type=self._migration_type
        )
        self._test_synthesis_agent = TestSynthesisAgent()
        self._differential_agent = DifferentialVerificationAgent(
            migration_type=self._migration_type
        )
        self._quality_gate_agent = QualityGateAgent()
        self._limited_fixer_agent = LimitedFixerAgent()

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """固定工程を実行."""
        return await self._run_pipeline(inputs)

    async def _execute_stream(self, inputs: dict[str, Any]):
        """ストリーム実行（ノード単位イベント）."""
        node_name = "migration_pipeline"
        node_start = self._emit_node_start(node_name)
        if node_start:
            yield node_start

        result = await self._run_pipeline(inputs)

        node_complete = self._emit_node_complete(node_name, result)
        if node_complete:
            yield node_complete

    async def _run_pipeline(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """工程固定パイプラインを実行."""
        self._ensure_agents_ready()

        source_code = inputs.get("source_code") or inputs.get("cobol_code", "")
        if not source_code:
            return {"success": False, "error": "source_code is required"}

        expected_outputs = inputs.get("expected_outputs", {})
        if not isinstance(expected_outputs, dict):
            return {"success": False, "error": "expected_outputs must be dict"}
        fast_mode = bool(inputs.get("fast_mode", True))

        task_id = str(inputs.get("task_id") or f"task-{uuid.uuid4().hex[:12]}")
        trace_id = str(inputs.get("trace_id") or task_id)
        module = str(inputs.get("module") or "UNKNOWN")

        task_spec = self._build_task_spec(
            task_id=task_id,
            trace_id=trace_id,
            module=module,
            expected_outputs=expected_outputs,
            options=inputs.get("options", {}),
        )

        artifact_store = ArtifactStore(
            base_dir=Path(str(inputs.get("artifacts_dir", "artifacts"))),
            decisions_path=Path(str(inputs.get("decisions_path", "DECISIONS.md"))),
            failures_path=Path(str(inputs.get("failures_path", "FAILURES.md"))),
        )
        await artifact_store.initialize()
        lock_path = await artifact_store.acquire_lock(task_id)

        artifact_paths: dict[str, str] = {}

        try:
            # 1) 分析
            analysis = self._legacy_analysis_agent.process(
                {
                    "source_code": source_code,
                    "task_spec": task_spec.model_dump(mode="json"),
                }
            )
            analysis_artifact = self._validate_or_fail(LegacyAnalysisArtifact, analysis, "analysis")
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

            # 2) 設計
            design = self._migration_design_agent.process({"legacy_analysis": analysis_for_next})
            design_artifact = self._validate_or_fail(MigrationDesignArtifact, design, "design")
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

            # 3) 変換
            transformation = self._code_transformation_agent.process(
                {
                    "source_code": source_code,
                    "migration_design": design_for_next,
                    "fast_mode": fast_mode,
                }
            )
            transformation_artifact = self._validate_or_fail(
                TransformationArtifact, transformation, "code"
            )
            if transformation_artifact is None:
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
            await artifact_store.write_text(
                stage="code",
                task_id=task_id,
                artifact_name="target_code",
                extension="java",
                content=transformation_artifact.target_code,
            )
            await artifact_store.append_decision(task_id, "変換工程を完了")
            transformation_for_next = await artifact_store.read_json(transformation_path)

            # 4) テスト生成
            test_synthesis = self._test_synthesis_agent.process(
                {
                    "legacy_analysis": analysis_for_next,
                    "expected_outputs": expected_outputs,
                    "target_language": task_spec.target_language,
                }
            )
            test_artifact = self._validate_or_fail(TestSynthesisArtifact, test_synthesis, "tests")
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

            # 5) 差分検証
            differential = self._differential_agent.process(
                {
                    "transformation": transformation_for_next,
                    "test_synthesis": tests_for_next,
                    "fast_mode": fast_mode,
                }
            )
            diff_artifact = self._validate_or_fail(
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

            # 6) 品質裁定
            known_legacy_issues = await self._load_known_legacy_issues(inputs)
            quality_gate = self._quality_gate_agent.process(
                {
                    "differential": diff_for_next,
                    "migration_design": design_for_next,
                    "test_synthesis": tests_for_next,
                    "known_legacy_issues": known_legacy_issues,
                }
            )
            quality_artifact = self._validate_or_fail(QualityGateArtifact, quality_gate, "quality")
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

            # 7) 限定修正
            fix = self._limited_fixer_agent.process(
                {
                    "quality_gate": quality_for_next,
                    "transformation": transformation_for_next,
                    "migration_design": design_for_next,
                }
            )
            fix_artifact = self._validate_or_fail(LimitedFixArtifact, fix, "fix")
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

            final_differential = diff_artifact
            final_quality = quality_artifact
            iterations = 1

            if fix_artifact.retest_required:
                iterations = 2
                adjusted_transformation = transformation_for_next.copy()
                adjusted_transformation["target_code"] = fix_artifact.target_code

                post_differential = self._differential_agent.process(
                    {
                        "transformation": adjusted_transformation,
                        "test_synthesis": tests_for_next,
                        "fast_mode": fast_mode,
                    }
                )
                post_diff_artifact = DifferentialVerificationArtifact.model_validate(
                    post_differential
                )
                post_diff_path = await artifact_store.write_json(
                    stage="diff",
                    task_id=task_id,
                    artifact_name="differential_post_fix",
                    payload=post_diff_artifact.model_dump(mode="json"),
                )
                artifact_paths["diff_post_fix"] = str(post_diff_path)

                post_quality = self._quality_gate_agent.process(
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
                await artifact_store.append_decision(
                    task_id,
                    f"再裁定: {final_quality.decision.value}",
                )

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

            class_name = design_artifact.class_mapping.get("primary_class", "MigratedProgram")
            final_target_code = (
                fix_artifact.target_code
                if fix_artifact.applied
                else transformation_artifact.target_code
            )

            return {
                "success": success,
                "migration_type": self._migration_type,
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
                "fast_mode": fast_mode,
            }
        finally:
            await artifact_store.release_lock(lock_path)

    def _build_task_spec(
        self,
        *,
        task_id: str,
        trace_id: str,
        module: str,
        expected_outputs: dict[str, Any],
        options: Any,
    ) -> TaskSpec:
        """TaskSpec を構築."""
        migration_config = self._factory.get_migration_config(self._migration_type)
        source_language = str(migration_config.get("source", {}).get("language", "COBOL"))
        target_language = str(migration_config.get("target", {}).get("language", "Java"))
        normalized_options = options if isinstance(options, dict) else {}

        return TaskSpec(
            task_id=task_id,
            trace_id=trace_id,
            migration_type=self._migration_type,
            source_language=source_language,
            target_language=target_language,
            module=module,
            expected_outputs=expected_outputs,
            options=normalized_options,
        )

    async def _load_known_legacy_issues(self, inputs: dict[str, Any]) -> list[dict[str, Any]]:
        """既知旧不具合リストを読み込む."""
        raw_path = str(
            inputs.get(
                "known_legacy_issues_path",
                "apps/code_migration_assistant/specs/known_legacy_issues.json",
            )
        )
        path = Path(raw_path)
        if not path.exists():
            return []

        raw = path.read_text(encoding="utf-8")

        try:
            parsed = json.loads(raw)
        except json.JSONDecodeError:
            return []

        if not isinstance(parsed, list):
            return []

        normalized: list[dict[str, Any]] = []
        for item in parsed:
            if isinstance(item, dict):
                normalized.append(item)
        return normalized

    def _validate_or_fail(
        self,
        model: type[
            LegacyAnalysisArtifact
            | MigrationDesignArtifact
            | TransformationArtifact
            | TestSynthesisArtifact
            | DifferentialVerificationArtifact
            | QualityGateArtifact
            | LimitedFixArtifact
        ],
        payload: dict[str, Any],
        stage: str,
    ) -> (
        LegacyAnalysisArtifact
        | MigrationDesignArtifact
        | TransformationArtifact
        | TestSynthesisArtifact
        | DifferentialVerificationArtifact
        | QualityGateArtifact
        | LimitedFixArtifact
        | None
    ):
        """成果物のスキーマ検証を行う."""
        try:
            return model.model_validate(payload)
        except Exception as exc:  # noqa: BLE001
            self._logger.error("%s stage artifact validation failed: %s", stage, exc)
            return None

    def _ensure_agents_ready(self) -> None:
        """Agent 初期化を確認."""
        if (
            self._legacy_analysis_agent is None
            or self._migration_design_agent is None
            or self._code_transformation_agent is None
            or self._test_synthesis_agent is None
            or self._differential_agent is None
            or self._quality_gate_agent is None
            or self._limited_fixer_agent is None
        ):
            msg = "Agents are not initialized"
            raise RuntimeError(msg)
