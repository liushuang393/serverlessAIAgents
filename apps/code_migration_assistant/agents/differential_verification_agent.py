"""Differential Verification Agent - 差分検証."""

from __future__ import annotations

import json
from decimal import Decimal, InvalidOperation
from typing import Any

from apps.code_migration_assistant.adapters import TargetLanguageAdapter, get_adapter_factory
from apps.code_migration_assistant.agents.prompts import DIFFERENTIAL_VERIFICATION_PROMPT
from apps.code_migration_assistant.workflow.models import (
    DifferentialVerificationArtifact,
    UnknownItem,
    build_meta,
)

from agentflow import agent


@agent
class DifferentialVerificationAgent:
    """差分検証 Agent."""

    system_prompt = DIFFERENTIAL_VERIFICATION_PROMPT

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        target_adapter: TargetLanguageAdapter | None = None,
    ) -> None:
        """初期化."""
        factory = get_adapter_factory()
        self._target_adapter = target_adapter or factory.get_target_adapter(migration_type)

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """差分検証成果物を生成."""
        transformation = input_data.get("transformation")
        test_synthesis = input_data.get("test_synthesis")
        fast_mode = bool(input_data.get("fast_mode", False))

        if not isinstance(transformation, dict):
            return {"success": False, "error": "transformation is required"}
        if not isinstance(test_synthesis, dict):
            return {"success": False, "error": "test_synthesis is required"}

        meta = transformation.get("meta", {})
        task_id = str(meta.get("task_id", "unknown-task"))
        trace_id = str(meta.get("trace_id", task_id))

        if fast_mode:
            artifact = DifferentialVerificationArtifact(
                meta=build_meta(
                    task_id=task_id,
                    trace_id=trace_id,
                    stage="diff",
                    source_language=meta.get("source_language"),
                    target_language=meta.get("target_language"),
                    module=meta.get("module"),
                ),
                equivalence=False,
                diffs=[],
                classification="test",
                confidence=0.0,
                evidence={"mode": "fast", "skipped_execution": True},
                unknowns=[
                    UnknownItem(
                        field="verification",
                        reason="fast_mode では実行検証を省略。完全検証は fast_mode=False で実施",
                    )
                ],
                extensions={},
            )
            return artifact.model_dump(mode="json")

        verification_result = self._verify_cases(
            target_code=str(transformation.get("target_code", "")),
            test_cases=test_synthesis.get("test_cases", []),
        )

        diffs = self._collect_diffs(verification_result)
        classification = self._classify(verification_result, diffs)
        equivalence = len(diffs) == 0 and not verification_result.get("has_exec_error", False)

        unknowns: list[UnknownItem] = []
        if not test_synthesis.get("golden_master"):
            unknowns.append(UnknownItem(field="golden_master", reason="比較基準が空"))

        artifact = DifferentialVerificationArtifact(
            meta=build_meta(
                task_id=task_id,
                trace_id=trace_id,
                stage="diff",
                source_language=meta.get("source_language"),
                target_language=meta.get("target_language"),
                module=meta.get("module"),
            ),
            equivalence=equivalence,
            diffs=diffs,
            classification=classification,
            confidence=1.0 if equivalence else 0.8,
            evidence={
                "verification_result": verification_result,
            },
            unknowns=unknowns,
            extensions={},
        )
        return artifact.model_dump(mode="json")

    def _verify_cases(self, target_code: str, test_cases: list[dict[str, Any]]) -> dict[str, Any]:
        """テストケースを実行し比較結果を返す."""
        if not target_code:
            return {
                "results": [{"name": "default", "execution_error": "target_code is empty"}],
                "has_exec_error": True,
            }

        normalized_cases = test_cases or [{"name": "default", "inputs": {}, "expected_outputs": {}}]
        results: list[dict[str, Any]] = []
        has_exec_error = False

        for case in normalized_cases:
            name = str(case.get("name", "default"))
            inputs = case.get("inputs", {})
            expected_outputs = case.get("expected_outputs", {})
            execution = self._target_adapter.execute(
                target_code, inputs if isinstance(inputs, dict) else {}
            )
            if not execution.success:
                has_exec_error = True
                results.append(
                    {
                        "name": name,
                        "execution_error": execution.error
                        or execution.stderr
                        or "execution failed",
                    }
                )
                continue

            actual_outputs = self._parse_stdout(execution.stdout)
            diffs = self._compare_outputs(
                expected_outputs if isinstance(expected_outputs, dict) else {},
                actual_outputs,
            )
            results.append(
                {
                    "name": name,
                    "actual_outputs": actual_outputs,
                    "diffs": diffs,
                }
            )

        return {"results": results, "has_exec_error": has_exec_error}

    def _collect_diffs(self, verification_result: dict[str, Any]) -> list[dict[str, Any]]:
        """検証結果から差分一覧を抽出."""
        diffs: list[dict[str, Any]] = []
        for test_result in verification_result.get("results", []):
            for diff in test_result.get("diffs", []):
                diffs.append(
                    {
                        "test_case": test_result.get("name", "unknown"),
                        "type": diff.get("type", "value"),
                        "location": diff.get("location", "unknown"),
                        "legacy": diff.get("expected"),
                        "new": diff.get("actual"),
                    }
                )
            if test_result.get("execution_error"):
                diffs.append(
                    {
                        "test_case": test_result.get("name", "unknown"),
                        "type": "execution_error",
                        "location": "runtime",
                        "legacy": "n/a",
                        "new": test_result.get("execution_error"),
                    }
                )
        return diffs

    def _classify(self, verification_result: dict[str, Any], diffs: list[dict[str, Any]]) -> str:
        """差分分類を決定."""
        if not diffs:
            return "none"

        for test_result in verification_result.get("results", []):
            if test_result.get("execution_error"):
                message = str(test_result.get("execution_error", "")).lower()
                if "not found" in message or "javac" in message or "java" in message:
                    return "environment"
                return "logic"

        diff_types = {str(diff.get("type", "value")) for diff in diffs}
        if "execution_error" in diff_types:
            for diff in diffs:
                if str(diff.get("type", "")) != "execution_error":
                    continue
                message = str(diff.get("new", "")).lower()
                if "not found" in message or "javac" in message or "java" in message:
                    return "environment"
            # 環境キーワードに一致しない実行エラーはロジック問題と判定
            return "logic"
        if diff_types <= {"whitespace", "format"}:
            return "format"

        return "logic"

    def _parse_stdout(self, stdout: str) -> dict[str, Any]:
        """標準出力を辞書へ変換."""
        result: dict[str, Any] = {}
        for raw_line in stdout.splitlines():
            line = raw_line.strip()
            if not line:
                continue

            if "=" in line:
                key, _, value = line.partition("=")
                result[key.strip()] = value.strip()
                continue

            if line.startswith("{") and line.endswith("}"):
                try:
                    parsed = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if isinstance(parsed, dict):
                    result.update(parsed)
                continue

            if "output_lines" not in result:
                result["output_lines"] = []
            output_lines = result["output_lines"]
            if isinstance(output_lines, list):
                output_lines.append(line)

        return result

    def _compare_outputs(self, expected: dict[str, Any], actual: dict[str, Any]) -> list[dict[str, Any]]:
        """期待値と実測値を比較し差分を返す."""
        diffs: list[dict[str, Any]] = []
        keys = set(expected.keys()) | set(actual.keys())

        for key in sorted(keys):
            expected_value = expected.get(key)
            actual_value = actual.get(key)
            if expected_value == actual_value:
                continue

            if expected_value is None or actual_value is None:
                diffs.append(
                    {
                        "type": "missing",
                        "location": key,
                        "expected": expected_value,
                        "actual": actual_value,
                    }
                )
                continue

            if self._is_numeric(expected_value) and self._is_numeric(actual_value):
                expected_number = Decimal(str(expected_value))
                actual_number = Decimal(str(actual_value))
                if expected_number == actual_number:
                    continue
                diffs.append(
                    {
                        "type": "value",
                        "location": key,
                        "expected": expected_value,
                        "actual": actual_value,
                    }
                )
                continue

            expected_text = str(expected_value)
            actual_text = str(actual_value)
            if expected_text.strip() == actual_text.strip():
                diffs.append(
                    {
                        "type": "whitespace",
                        "location": key,
                        "expected": expected_value,
                        "actual": actual_value,
                    }
                )
                continue

            if expected_text.upper() == actual_text.upper():
                diffs.append(
                    {
                        "type": "format",
                        "location": key,
                        "expected": expected_value,
                        "actual": actual_value,
                    }
                )
                continue

            diffs.append(
                {
                    "type": "value",
                    "location": key,
                    "expected": expected_value,
                    "actual": actual_value,
                }
            )

        return diffs

    def _is_numeric(self, value: Any) -> bool:
        """数値判定."""
        try:
            Decimal(str(value))
        except (InvalidOperation, ValueError):
            return False
        return True
