# -*- coding: utf-8 -*-
"""Migration Design Agent - 等価移行設計."""

from __future__ import annotations

from typing import Any

from agentflow import agent

from apps.code_migration_assistant.adapters import get_adapter_factory
from apps.code_migration_assistant.agents.prompts import MIGRATION_DESIGN_PROMPT
from apps.code_migration_assistant.workflow.models import (
    MigrationDesignArtifact,
    UnknownItem,
    build_meta,
)


@agent
class MigrationDesignAgent:
    """移行設計 Agent."""

    system_prompt = MIGRATION_DESIGN_PROMPT

    def __init__(self, migration_type: str = "cobol-to-java") -> None:
        """初期化."""
        self._migration_type = migration_type
        self._factory = get_adapter_factory()
        self._source_adapter = self._factory.get_source_adapter(migration_type)
        self._target_adapter = self._factory.get_target_adapter(migration_type)

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """設計成果物を生成."""
        legacy_analysis = input_data.get("legacy_analysis")
        if not isinstance(legacy_analysis, dict):
            return {"success": False, "error": "legacy_analysis is required"}

        meta = legacy_analysis.get("meta", {})
        task_id = str(meta.get("task_id", "unknown-task"))
        trace_id = str(meta.get("trace_id", task_id))
        module = str(meta.get("module", "UNKNOWN"))

        class_name = self._derive_class_name(legacy_analysis)

        unknowns: list[UnknownItem] = []
        if not legacy_analysis.get("entry_points"):
            unknowns.append(
                UnknownItem(field="entry_points", reason="入口定義が空のため手動確認が必要")
            )

        artifact = MigrationDesignArtifact(
            meta=build_meta(
                task_id=task_id,
                trace_id=trace_id,
                stage="design",
                source_language=self._source_adapter.language_name,
                target_language=self._target_adapter.language_name,
                module=module,
            ),
            package_mapping={"default": "com.migration.generated"},
            class_mapping={"primary_class": class_name},
            transaction_policy={"mode": "preserve", "rationale": "旧システムの境界を維持"},
            state_model={
                "variables": [
                    v.get("name", "UNKNOWN") for v in legacy_analysis.get("data_structures", [])
                ],
                "mutability": "preserve",
            },
            framework_mapping={
                "migration_type": self._migration_type,
                "target_runtime": self._target_adapter.language_name,
            },
            rationale={
                "class_mapping": "PROGRAM-ID をクラス名へ 1:1 変換",
                "package_mapping": "生成物の配置を固定し検証容易性を確保",
                "state_model": "WORKING-STORAGE を同等の状態変数として保持",
            },
            unknowns=unknowns,
            extensions={},
        )
        return artifact.model_dump(mode="json")

    def _derive_class_name(self, legacy_analysis: dict[str, Any]) -> str:
        """クラス名を導出."""
        programs = legacy_analysis.get("programs", [])
        if programs:
            raw = str(programs[0].get("program_id", "MigratedProgram"))
        else:
            raw = "MigratedProgram"

        normalized = raw.replace("-", "_").replace(".", "_")
        parts = [part for part in normalized.split("_") if part]
        if not parts:
            return "MigratedProgram"

        return "".join(part.lower().capitalize() for part in parts)
