"""Limited Fixer Agent - 限定修正."""

from __future__ import annotations

from typing import Any

from apps.code_migration_assistant.agents.prompts import LIMITED_FIXER_PROMPT
from apps.code_migration_assistant.workflow.models import (
    LimitedFixArtifact,
    QualityDecision,
    UnknownItem,
    build_meta,
)

from agentflow import agent


@agent
class LimitedFixerAgent:
    """限定修正 Agent.

    品質裁定で許可された範囲のみ修正し、適用内容を明示する。
    """

    system_prompt = LIMITED_FIXER_PROMPT

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """限定修正成果物を生成."""
        quality_gate = input_data.get("quality_gate")
        transformation = input_data.get("transformation")
        migration_design = input_data.get("migration_design", {})

        if not isinstance(quality_gate, dict):
            return {"success": False, "error": "quality_gate is required"}
        if not isinstance(transformation, dict):
            return {"success": False, "error": "transformation is required"}

        meta = transformation.get("meta", {})
        task_id = str(meta.get("task_id", "unknown-task"))
        trace_id = str(meta.get("trace_id", task_id))

        original_code = str(transformation.get("target_code", ""))
        decision_value = str(quality_gate.get("decision", QualityDecision.PASSED.value))
        decision = QualityDecision(decision_value)

        applied = False
        patch_summary: list[str] = []
        unknowns: list[UnknownItem] = []
        fixed_code = original_code

        if decision != QualityDecision.TRANSFORM_ISSUE:
            patch_summary.append("裁定が TRANSFORM_ISSUE 以外のため修正を実施しない")
        else:
            fixed_code, patch_summary, applied = self._apply_guarded_fix(
                original_code=original_code,
                package_name=str(migration_design.get("package_mapping", {}).get("default", "")),
            )
            if not applied:
                unknowns.append(
                    UnknownItem(field="patch", reason="自動修正可能な限定パターンに一致しない")
                )

        artifact = LimitedFixArtifact(
            meta=build_meta(
                task_id=task_id,
                trace_id=trace_id,
                stage="fix",
                source_language=meta.get("source_language"),
                target_language=meta.get("target_language"),
                module=meta.get("module"),
            ),
            applied=applied,
            target_code=fixed_code,
            patch_summary=patch_summary,
            retest_required=applied,
            unknowns=unknowns,
            extensions={},
        )
        return artifact.model_dump(mode="json")

    def _apply_guarded_fix(
        self,
        *,
        original_code: str,
        package_name: str,
    ) -> tuple[str, list[str], bool]:
        """限定パターンのみ修正."""
        summary: list[str] = []
        updated_code = original_code

        # 修正1: 末尾空白の削除（表示差分対策）
        normalized_lines = [line.rstrip() for line in updated_code.splitlines()]
        trimmed_code = "\n".join(normalized_lines).strip()
        if trimmed_code:
            trimmed_code = f"{trimmed_code}\n"

        if trimmed_code != updated_code:
            summary.append("末尾空白と余剰改行を正規化")
            updated_code = trimmed_code

        # 修正2: パッケージ宣言不足を補完（設計で指定されている場合のみ）
        if package_name and "package " not in updated_code:
            updated_code = f"package {package_name};\n\n{updated_code}"
            summary.append("設計に基づき package 宣言を補完")

        if not summary:
            summary.append("適用可能な限定修正なし")
            return updated_code, summary, False

        return updated_code, summary, True
