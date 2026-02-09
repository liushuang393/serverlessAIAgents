# -*- coding: utf-8 -*-
"""Quality Gate Agent - 責任工程の裁定."""

from __future__ import annotations

from typing import Any

from agentflow import agent

from apps.code_migration_assistant.agents.prompts import QUALITY_GATE_PROMPT
from apps.code_migration_assistant.workflow.models import (
    QualityDecision,
    QualityGateArtifact,
    UnknownItem,
    build_meta,
)


@agent
class QualityGateAgent:
    """品質裁定 Agent."""

    system_prompt = QUALITY_GATE_PROMPT

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """品質裁定成果物を生成."""
        differential = input_data.get("differential")
        migration_design = input_data.get("migration_design", {})
        test_synthesis = input_data.get("test_synthesis", {})
        known_legacy_issues = input_data.get("known_legacy_issues", [])

        if not isinstance(differential, dict):
            return {"success": False, "error": "differential is required"}

        meta = differential.get("meta", {})
        task_id = str(meta.get("task_id", "unknown-task"))
        trace_id = str(meta.get("trace_id", task_id))

        diffs = differential.get("diffs", [])
        classification = str(differential.get("classification", "logic"))
        equivalence = bool(differential.get("equivalence", False))

        decision, target_agent, reason, severity = self._decide(
            equivalence=equivalence,
            classification=classification,
            diffs=diffs,
            design_unknowns=migration_design.get("unknowns", []),
            test_unknowns=test_synthesis.get("unknowns", []),
            known_legacy_issues=known_legacy_issues,
        )

        unknowns: list[UnknownItem] = []
        if not diffs and not equivalence:
            unknowns.append(UnknownItem(field="diffs", reason="差分が空だが等価判定がFAIL"))

        artifact = QualityGateArtifact(
            meta=build_meta(
                task_id=task_id,
                trace_id=trace_id,
                stage="quality",
                source_language=meta.get("source_language"),
                target_language=meta.get("target_language"),
                module=meta.get("module"),
            ),
            decision=decision,
            target_agent=target_agent,
            reason=reason,
            severity=severity,
            evidence={
                "classification": classification,
                "diff_count": len(diffs),
            },
            unknowns=unknowns,
            extensions={},
        )
        return artifact.model_dump(mode="json")

    def _decide(
        self,
        *,
        equivalence: bool,
        classification: str,
        diffs: list[dict[str, Any]],
        design_unknowns: list[dict[str, Any]],
        test_unknowns: list[dict[str, Any]],
        known_legacy_issues: list[dict[str, Any]],
    ) -> tuple[QualityDecision, str, str, str]:
        """裁定ロジック."""
        if equivalence:
            return (
                QualityDecision.PASSED,
                "None",
                "差分なしのため品質ゲート通過",
                "LOW",
            )

        if self._matches_known_legacy(diffs, known_legacy_issues):
            return (
                QualityDecision.KNOWN_LEGACY,
                "None",
                "既知の旧システム不具合に一致",
                "LOW",
            )

        if classification == "environment":
            return (
                QualityDecision.ENV_ISSUE,
                "EnvironmentAgent",
                "実行環境依存のエラーを検出",
                "HIGH",
            )

        if classification == "test":
            return (
                QualityDecision.TEST_ISSUE,
                "TestSynthesisAgent",
                "テスト工程の簡易実行モードで完全検証が未実施",
                "MEDIUM",
            )

        if test_unknowns:
            return (
                QualityDecision.TEST_ISSUE,
                "TestSynthesisAgent",
                "テスト期待値または基準が不足",
                "MEDIUM",
            )

        if design_unknowns:
            return (
                QualityDecision.DESIGN_ISSUE,
                "MigrationDesignAgent",
                "設計成果物に未確定要素が残存",
                "MEDIUM",
            )

        if classification in {"logic", "format"}:
            return (
                QualityDecision.TRANSFORM_ISSUE,
                "LimitedFixerAgent",
                "変換成果物に差分を検出",
                "HIGH" if classification == "logic" else "MEDIUM",
            )

        return (
            QualityDecision.DESIGN_ISSUE,
            "MigrationDesignAgent",
            "責任工程を特定できないため設計再確認",
            "MEDIUM",
        )

    def _matches_known_legacy(
        self,
        diffs: list[dict[str, Any]],
        known_legacy_issues: list[dict[str, Any]],
    ) -> bool:
        """既知旧不具合との一致判定."""
        if not diffs or not known_legacy_issues:
            return False

        for diff in diffs:
            for issue in known_legacy_issues:
                if str(diff.get("location")) != str(issue.get("location")):
                    continue
                if str(diff.get("legacy")) != str(issue.get("legacy")):
                    continue
                if str(diff.get("new")) != str(issue.get("new")):
                    continue
                return True

        return False
