# -*- coding: utf-8 -*-
"""Java Language Adapter.

Javaのターゲットコード生成と実行を担う。
"""

import re
import subprocess
import tempfile
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.adapters.base import (
    AST,
    ExecutionResult,
    TargetLanguageAdapter,
)


class JavaAdapter(TargetLanguageAdapter):
    """Java言語アダプター.

    Javaのコードスケルトンとテストコードを生成し、コンパイルと実行を行う。
    """

    # COBOL → Java 型マッピング
    TYPE_MAP = {
        "numeric": "int",
        "numeric_large": "long",
        "decimal": "BigDecimal",
        "string": "String",
        "comp3": "BigDecimal",
    }

    @property
    def language_name(self) -> str:
        """言語名称."""
        return "Java"

    def generate_skeleton(self, ast: AST, class_name: str) -> str:
        """Javaクラススケルトンを生成する.

        Args:
            ast: ソースコードのAST
            class_name: クラス名

        Returns:
            Javaコードのスケルトン（メソッド本体はLLMが補完）
        """
        lines: list[str] = []

        # Package and imports
        lines.append("package com.migration.generated;")
        lines.append("")

        # BigDecimalが必要か判定
        needs_bigdecimal = any(
            self.get_type_mapping(v.get("type", ""), v.get("pic_clause", "")) == "BigDecimal"
            for v in ast.variables
        )

        if needs_bigdecimal:
            lines.append("import java.math.BigDecimal;")
            lines.append("import java.math.RoundingMode;")
            lines.append("")

        # Class header
        lines.append(f"/**")
        lines.append(f" * 移行元: {ast.program_id}")
        lines.append(f" * 生成者: CodeMigrationAgent")
        lines.append(f" */")
        lines.append(f"public class {class_name} {{")
        lines.append("")

        # Fields (from variables)
        lines.append("    // === Fields (WORKING-STORAGE) ===")
        for var in ast.variables:
            java_type = self.get_type_mapping(var.get("type", ""), var.get("pic_clause", ""))
            java_name = self._to_camel_case(var["name"])
            lines.append(f"    private {java_type} {java_name};")
        lines.append("")

        # Method placeholder (to be filled by LLM)
        lines.append("    // === Methods (PROCEDURE DIVISION) ===")
        lines.append("    // TODO: LLM will generate method implementations")
        lines.append("")

        # Main method placeholder
        lines.append("    public static void main(String[] args) {")
        lines.append(f"        {class_name} instance = new {class_name}();")
        lines.append("        // TODO: Call entry point method")
        lines.append("    }")
        lines.append("}")

        return "\n".join(lines)

    def generate_test_skeleton(self, class_name: str, test_cases: list[dict]) -> str:
        """JUnitテストのスケルトンを生成する.

        Args:
            class_name: 対象クラス名
            test_cases: テストケース

        Returns:
            JUnitテストコード
        """
        lines: list[str] = []

        lines.append("package com.migration.generated;")
        lines.append("")
        lines.append("import org.junit.jupiter.api.Test;")
        lines.append("import org.junit.jupiter.api.BeforeEach;")
        lines.append("import static org.junit.jupiter.api.Assertions.*;")
        lines.append("")
        lines.append(f"class {class_name}Test {{")
        lines.append("")
        lines.append(f"    private {class_name} instance;")
        lines.append("")
        lines.append("    @BeforeEach")
        lines.append("    void setUp() {")
        lines.append(f"        instance = new {class_name}();")
        lines.append("    }")
        lines.append("")

        for i, tc in enumerate(test_cases):
            test_name = tc.get("name", f"test_{i}")
            description = tc.get("description", "")

            lines.append(f"    @Test")
            if description:
                lines.append(f"    // {description}")
            lines.append(f"    void {test_name}() {{")
            lines.append(f"        // Inputs: {tc.get('inputs', {})}")
            lines.append(f"        // Expected: {tc.get('expected_outputs', {})}")
            lines.append(f"        // TODO: Implement test")
            lines.append(f"        fail(\"Not implemented\");")
            lines.append(f"    }}")
            lines.append("")

        lines.append("}")

        return "\n".join(lines)

    def compile(self, code: str) -> tuple[bool, list[str]]:
        """Javaコードをコンパイルする.

        Args:
            code: Javaソースコード

        Returns:
            （成功, エラーリスト）
        """
        errors: list[str] = []

        # クラス名を抽出
        match = re.search(r"public\s+class\s+(\w+)", code)
        if not match:
            return False, ["Cannot find public class name"]

        class_name = match.group(1)

        try:
            with tempfile.TemporaryDirectory() as tmpdir:
                # ソースファイルを書き込む
                src_path = Path(tmpdir) / f"{class_name}.java"
                src_path.write_text(code, encoding="utf-8")

                # コンパイル
                result = subprocess.run(
                    ["javac", str(src_path)],
                    capture_output=True,
                    text=True,
                    timeout=30,
                )

                if result.returncode != 0:
                    errors = result.stderr.strip().split("\n")
                    return False, errors

                return True, []

        except FileNotFoundError:
            return False, ["javac not found - JDK not installed"]
        except subprocess.TimeoutExpired:
            return False, ["Compilation timeout"]
        except Exception as e:
            return False, [str(e)]

    def execute(self, code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """Javaコードを実行する.

        Args:
            code: Javaソースコード
            inputs: 入力パラメータ

        Returns:
            実行結果
        """
        # クラス名を抽出
        match = re.search(r"public\s+class\s+(\w+)", code)
        if not match:
            return ExecutionResult(success=False, error="Cannot find public class name")

        class_name = match.group(1)

        try:
            with tempfile.TemporaryDirectory() as tmpdir:
                # 書き込みとコンパイル
                src_path = Path(tmpdir) / f"{class_name}.java"
                src_path.write_text(code, encoding="utf-8")

                compile_result = subprocess.run(
                    ["javac", str(src_path)],
                    capture_output=True,
                    text=True,
                    timeout=30,
                )

                if compile_result.returncode != 0:
                    return ExecutionResult(
                        success=False,
                        error=f"Compilation failed: {compile_result.stderr}",
                    )

                # 実行
                run_result = subprocess.run(
                    ["java", "-cp", tmpdir, class_name],
                    capture_output=True,
                    text=True,
                    timeout=30,
                )

                return ExecutionResult(
                    success=run_result.returncode == 0,
                    stdout=run_result.stdout,
                    stderr=run_result.stderr,
                    return_code=run_result.returncode,
                )

        except FileNotFoundError:
            return ExecutionResult(success=False, error="java/javac not found")
        except subprocess.TimeoutExpired:
            return ExecutionResult(success=False, error="Execution timeout")
        except Exception as e:
            return ExecutionResult(success=False, error=str(e))

    def get_type_mapping(self, source_type: str, pic_clause: str = "") -> str:
        """型マッピングを取得する."""
        if "V" in pic_clause.upper() or source_type == "decimal":
            return "BigDecimal"
        if source_type == "numeric":
            match = re.search(r"9\((\d+)\)", pic_clause)
            if match and int(match.group(1)) > 9:
                return "long"
            return "int"
        if source_type == "string":
            return "String"
        return "Object"

    def _to_camel_case(self, cobol_name: str) -> str:
        """COBOL 命名转 camelCase."""
        name = cobol_name.replace("WS-", "").replace("ws-", "")
        parts = name.replace("-", "_").split("_")
        if not parts:
            return "field"
        return parts[0].lower() + "".join(p.capitalize() for p in parts[1:])
