"""Legacy Analysis Agent - 事実抽出専用.

推測を避け、旧システムコードから観測可能な事実のみを抽出する。
"""

from __future__ import annotations

from typing import Any

from apps.code_migration_assistant.adapters import SourceLanguageAdapter, get_adapter_factory
from apps.code_migration_assistant.agents.prompts import LEGACY_ANALYSIS_PROMPT
from apps.code_migration_assistant.workflow.models import (
    LegacyAnalysisArtifact,
    UnknownItem,
    build_meta,
)

from agentflow import agent


@agent
class LegacyAnalysisAgent:
    """旧システム分析 Agent."""

    system_prompt = LEGACY_ANALYSIS_PROMPT

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        source_adapter: SourceLanguageAdapter | None = None,
    ) -> None:
        """初期化."""
        factory = get_adapter_factory()
        self._source_adapter = source_adapter or factory.get_source_adapter(migration_type)

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """分析成果物を生成."""
        source_code = input_data.get("source_code", "")
        if not source_code:
            return {"success": False, "error": "source_code is required"}

        task_spec = input_data.get("task_spec", {})
        task_id = str(task_spec.get("task_id", "unknown-task"))
        trace_id = str(task_spec.get("trace_id", task_id))
        module = str(task_spec.get("module", "UNKNOWN"))

        ast = self._source_adapter.parse(source_code)
        external_calls = self._source_adapter.identify_external_calls(ast)

        unknowns: list[UnknownItem] = []
        if ast.metadata.get("parse_error"):
            unknowns.append(UnknownItem(field="parse", reason=str(ast.metadata["parse_error"])))
        if not ast.procedures:
            unknowns.append(UnknownItem(field="entry_points", reason="PROCEDURE から段落名を抽出できなかった"))

        io_contracts = self._build_io_contracts(task_spec.get("expected_outputs", {}))
        db_access = [call for call in external_calls if call.get("type") == "sql"]

        artifact = LegacyAnalysisArtifact(
            meta=build_meta(
                task_id=task_id,
                trace_id=trace_id,
                stage="analysis",
                source_language=self._source_adapter.language_name,
                module=module,
            ),
            programs=[{"program_id": ast.program_id}],
            entry_points=[
                {
                    "name": proc.get("name", "UNKNOWN"),
                    "type": proc.get("type", "paragraph"),
                }
                for proc in ast.procedures
            ],
            io_contracts=io_contracts,
            data_structures=ast.variables,
            control_flow=self._extract_control_flow(ast.divisions.get("PROCEDURE DIVISION", [])),
            db_access=db_access,
            external_calls=external_calls,
            unknowns=unknowns,
            extensions={"metadata": ast.metadata},
        )
        return artifact.model_dump(mode="json")

    def _build_io_contracts(self, expected_outputs: dict[str, Any]) -> list[dict[str, Any]]:
        """期待値から I/O 契約を生成."""
        contracts: list[dict[str, Any]] = []
        for key, value in expected_outputs.items():
            contracts.append(
                {
                    "name": key,
                    "direction": "output",
                    "expected_example": value,
                }
            )
        return contracts

    def _extract_control_flow(self, procedure_lines: list[str]) -> list[dict[str, Any]]:
        """制御フロー情報を抽出."""
        control_flow: list[dict[str, Any]] = []

        for line in procedure_lines:
            normalized = line.strip()
            upper = normalized.upper()
            if not normalized:
                continue
            if any(token in upper for token in ["IF", "EVALUATE", "PERFORM", "GO TO"]):
                control_flow.append({"statement": normalized})

        return control_flow
