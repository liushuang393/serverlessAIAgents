# -*- coding: utf-8 -*-
"""Java Language Adapter.

Java 目标代码生成和执行。
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
    """Java 语言适配器.

    生成 Java 代码骨架和测试代码，支持编译和执行。
    """

    # COBOL → Java 类型映射
    TYPE_MAP = {
        "numeric": "int",
        "numeric_large": "long",
        "decimal": "BigDecimal",
        "string": "String",
        "comp3": "BigDecimal",
    }

    @property
    def language_name(self) -> str:
        """语言名称."""
        return "Java"

    def generate_skeleton(self, ast: AST, class_name: str) -> str:
        """生成 Java 类骨架.

        Args:
            ast: 源代码 AST
            class_name: 类名

        Returns:
            Java 代码骨架（不包含方法体，由 LLM 填充）
        """
        lines: list[str] = []

        # Package and imports
        lines.append("package com.migration.generated;")
        lines.append("")

        # 检查是否需要 BigDecimal
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
        """生成 JUnit 测试骨架.

        Args:
            class_name: 被测类名
            test_cases: 测试用例

        Returns:
            JUnit 测试代码
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
        """编译 Java 代码.

        Args:
            code: Java 源代码

        Returns:
            (成功, 错误列表)
        """
        errors: list[str] = []

        # 提取类名
        match = re.search(r"public\s+class\s+(\w+)", code)
        if not match:
            return False, ["Cannot find public class name"]

        class_name = match.group(1)

        try:
            with tempfile.TemporaryDirectory() as tmpdir:
                # 写入源文件
                src_path = Path(tmpdir) / f"{class_name}.java"
                src_path.write_text(code, encoding="utf-8")

                # 编译
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
        """执行 Java 代码.

        Args:
            code: Java 源代码
            inputs: 输入参数

        Returns:
            执行结果
        """
        # 提取类名
        match = re.search(r"public\s+class\s+(\w+)", code)
        if not match:
            return ExecutionResult(success=False, error="Cannot find public class name")

        class_name = match.group(1)

        try:
            with tempfile.TemporaryDirectory() as tmpdir:
                # 写入并编译
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

                # 执行
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
        """获取类型映射."""
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

