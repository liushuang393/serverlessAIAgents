"""Code Transformation Agent - 設計拘束下の機械的変換."""

from __future__ import annotations

import re
from typing import Any

from apps.code_migration_assistant.adapters import (
    SourceLanguageAdapter,
    TargetLanguageAdapter,
    get_adapter_factory,
)
from apps.code_migration_assistant.agents.prompts import CODE_TRANSFORMATION_PROMPT
from apps.code_migration_assistant.workflow.models import (
    GeneratedFile,
    TransformationArtifact,
    UnknownItem,
    build_meta,
)

from agentflow import agent


@agent
class CodeTransformationAgent:
    """変換 Agent.

    設計成果物に従って決定的に変換し、適用ルールを記録する。
    """

    system_prompt = CODE_TRANSFORMATION_PROMPT

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        source_adapter: SourceLanguageAdapter | None = None,
        target_adapter: TargetLanguageAdapter | None = None,
    ) -> None:
        """初期化."""
        factory = get_adapter_factory()
        self._source_adapter = source_adapter or factory.get_source_adapter(migration_type)
        self._target_adapter = target_adapter or factory.get_target_adapter(migration_type)

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """変換成果物を生成."""
        source_code = input_data.get("source_code", "")
        migration_design = input_data.get("migration_design")
        fast_mode = bool(input_data.get("fast_mode", False))

        if not source_code:
            return {"success": False, "error": "source_code is required"}
        if not isinstance(migration_design, dict):
            return {"success": False, "error": "migration_design is required"}

        meta = migration_design.get("meta", {})
        task_id = str(meta.get("task_id", "unknown-task"))
        trace_id = str(meta.get("trace_id", task_id))
        module = str(meta.get("module", "UNKNOWN"))

        ast = self._source_adapter.parse(source_code)
        class_name = str(
            migration_design.get("class_mapping", {}).get("primary_class", "MigratedProgram")
        )
        target_code = self._target_adapter.generate_skeleton(ast, class_name)

        compile_success = True
        compile_errors: list[str] = []
        if not fast_mode:
            compile_success, compile_errors = self._target_adapter.compile(target_code)

        warnings: list[str] = []
        unknowns: list[UnknownItem] = []
        if not compile_success and not fast_mode:
            warnings.append("生成スケルトンはコンパイルに失敗した")
            unknowns.append(
                UnknownItem(field="compile", reason="自動生成スケルトンの文法/依存を要確認")
            )

        # マルチファイル対応: // --- [FILE: path] --- 形式があれば分割
        generated_files = []
        if "// --- [FILE:" in target_code:
            # 分割ロジック
            blocks = re.split(r"// --- \[FILE: (.+?)\] ---", target_code)
            # blocks[0] は最初のセパレータの前（通常は空）
            for i in range(1, len(blocks), 2):
                file_path = blocks[i]
                file_content = blocks[i + 1].strip()
                generated_files.append(GeneratedFile(path=file_path, content=file_content))
        else:
            generated_files = [
                GeneratedFile(path=f"generated/{class_name}.java", content=target_code)
            ]

        artifact = TransformationArtifact(
            meta=build_meta(
                task_id=task_id,
                trace_id=trace_id,
                stage="code",
                source_language=self._source_adapter.language_name,
                target_language=self._target_adapter.language_name,
                module=module,
            ),
            target_code=target_code,
            generated_files=generated_files,
            rule_hits=[
                "class_mapping.primary_class",
                "package_mapping.default",
                "state_model.variables",
            ],
            warnings=warnings,
            unknowns=unknowns,
            extensions={
                "compile": {
                    "success": compile_success,
                    "errors": compile_errors,
                }
            },
        )
        return artifact.model_dump(mode="json")
