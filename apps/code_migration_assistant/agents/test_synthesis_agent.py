"""Test Synthesis Agent - Golden Master 生成."""

from __future__ import annotations

from typing import Any

from apps.code_migration_assistant.agents.prompts import TEST_SYNTHESIS_PROMPT
from apps.code_migration_assistant.workflow.models import (
    TestSynthesisArtifact,
    UnknownItem,
    build_meta,
)

from agentflow import agent


@agent
class TestSynthesisAgent:
    """テスト生成 Agent."""

    system_prompt = TEST_SYNTHESIS_PROMPT

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """テスト成果物を生成."""
        legacy_analysis = input_data.get("legacy_analysis")
        business_semantics = input_data.get("business_semantics", {})
        if not isinstance(legacy_analysis, dict):
            return {"success": False, "error": "legacy_analysis is required"}
        if not isinstance(business_semantics, dict):
            return {"success": False, "error": "business_semantics must be dict"}

        expected_outputs = input_data.get("expected_outputs", {})
        if not isinstance(expected_outputs, dict):
            return {"success": False, "error": "expected_outputs must be dict"}

        meta = legacy_analysis.get("meta", {})
        task_id = str(meta.get("task_id", "unknown-task"))
        trace_id = str(meta.get("trace_id", task_id))
        module = str(meta.get("module", "UNKNOWN"))

        unknowns: list[UnknownItem] = []
        if not expected_outputs:
            unknowns.append(UnknownItem(field="expected_outputs", reason="期待値が未指定"))

        test_cases = [
            {
                "name": "default",
                "inputs": {},
                "expected_outputs": expected_outputs,
                "description": "既定の等価性確認ケース",
            }
        ]
        for idx, event in enumerate(business_semantics.get("business_events", [])[:5], start=1):
            test_cases.append(
                {
                    "name": f"event_{idx}",
                    "inputs": {"event": event.get("name", f"event_{idx}")},
                    "expected_outputs": expected_outputs,
                    "description": f"業務イベント駆動ケース: {event.get('name', 'N/A')}",
                }
            )
        for idx, rule in enumerate(business_semantics.get("business_rules", [])[:3], start=1):
            test_cases.append(
                {
                    "name": f"rule_{idx}",
                    "inputs": {"rule": rule.get("name", f"rule_{idx}")},
                    "expected_outputs": expected_outputs,
                    "description": f"業務ルール確認ケース: {rule.get('condition', 'N/A')}",
                }
            )

        artifact = TestSynthesisArtifact(
            meta=build_meta(
                task_id=task_id,
                trace_id=trace_id,
                stage="tests",
                source_language=meta.get("source_language"),
                target_language=input_data.get("target_language"),
                module=module,
            ),
            test_cases=test_cases,
            golden_master={"default": expected_outputs},
            evidence={
                "source_of_truth": "expected_outputs",
                "case_count": len(test_cases),
                "event_based_case_count": len(business_semantics.get("business_events", [])),
            },
            unknowns=unknowns,
            extensions={},
        )
        return artifact.model_dump(mode="json")
