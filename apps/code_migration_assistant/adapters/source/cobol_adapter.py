# -*- coding: utf-8 -*-
"""COBOL Language Adapter.

COBOL 源代码解析和分析。
"""

import re
from typing import Any

from apps.code_migration_assistant.adapters.base import (
    AST,
    ExecutionResult,
    SourceLanguageAdapter,
)
from apps.code_migration_assistant.parsers import PLYCobolParser


class CobolAdapter(SourceLanguageAdapter):
    """COBOL 语言适配器.

    使用 PLY 解析器解析 COBOL 代码。
    """

    @property
    def language_name(self) -> str:
        """语言名称."""
        return "COBOL"

    def parse(self, source_code: str) -> AST:
        """解析 COBOL 代码为 AST.

        Args:
            source_code: COBOL 源代码

        Returns:
            抽象语法树
        """
        try:
            parser = PLYCobolParser(source_code)
            raw_ast = parser.parse()
            variables = parser.extract_variables(raw_ast)

            return AST(
                program_id=raw_ast.get("program_id", "UNKNOWN"),
                divisions=raw_ast.get("divisions", {}),
                metadata=raw_ast.get("metadata", {}),
                variables=variables,
                procedures=self._extract_procedures(raw_ast),
            )

        except Exception as e:
            # フォールバック: 簡易解析
            return self._fallback_parse(source_code, str(e))

    def extract_variables(self, ast: AST) -> list[dict[str, Any]]:
        """提取变量定义.

        Args:
            ast: 抽象语法树

        Returns:
            变量列表（包含类型信息）
        """
        return ast.variables

    def identify_external_calls(self, ast: AST) -> list[dict[str, Any]]:
        """识别外部调用.

        Args:
            ast: 抽象语法树

        Returns:
            外部调用列表
        """
        calls: list[dict[str, Any]] = []

        # 检查 PROCEDURE DIVISION 中的外部调用
        proc_div = ast.divisions.get("PROCEDURE DIVISION", [])

        for line in proc_div:
            line_upper = line.upper()

            # 文件操作
            if any(kw in line_upper for kw in ["OPEN", "CLOSE", "READ", "WRITE"]):
                calls.append({
                    "type": "file_io",
                    "operation": self._extract_operation(line_upper),
                    "line": line.strip(),
                })

            # CALL 语句
            if "CALL" in line_upper:
                calls.append({
                    "type": "program_call",
                    "target": self._extract_call_target(line),
                    "line": line.strip(),
                })

            # SQL 操作
            if "EXEC SQL" in line_upper:
                calls.append({
                    "type": "sql",
                    "line": line.strip(),
                })

        return calls

    def execute(self, source_code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """执行 COBOL 代码（模拟执行）.

        注意: 实际 COBOL 执行需要 GnuCOBOL 等编译器。
        此处提供模拟实现，用于单元测试。

        Args:
            source_code: COBOL 源代码
            inputs: 输入参数

        Returns:
            执行结果
        """
        # TODO: 集成 GnuCOBOL 编译器进行实际执行
        return ExecutionResult(
            success=False,
            error="COBOL execution requires GnuCOBOL compiler (not implemented)",
        )

    def _fallback_parse(self, source_code: str, error: str) -> AST:
        """简易解析（fallback）."""
        lines = source_code.split("\n")

        # 提取 PROGRAM-ID
        program_id = "UNKNOWN"
        for line in lines:
            match = re.search(r"PROGRAM-ID\.\s+(\S+)", line, re.IGNORECASE)
            if match:
                program_id = match.group(1).rstrip(".")
                break

        # 简易 DIVISION 分割
        divisions: dict[str, list[str]] = {}
        current_div = None

        for line in lines:
            if "DIVISION" in line.upper():
                match = re.search(r"(\w+\s+DIVISION)", line, re.IGNORECASE)
                if match:
                    current_div = match.group(1).upper()
                    divisions[current_div] = []
            elif current_div:
                divisions[current_div].append(line)

        return AST(
            program_id=program_id,
            divisions=divisions,
            metadata={"parse_error": error, "fallback": True},
            variables=self._extract_variables_simple(divisions.get("DATA DIVISION", [])),
        )

    def _extract_variables_simple(self, data_lines: list[str]) -> list[dict[str, Any]]:
        """简易变量提取."""
        variables: list[dict[str, Any]] = []

        for line in data_lines:
            match = re.search(r"(\d{2})\s+(\S+)\s+PIC\s+([^\s.]+)", line, re.IGNORECASE)
            if match:
                pic_clause = match.group(3)
                variables.append({
                    "level": match.group(1),
                    "name": match.group(2),
                    "pic_clause": pic_clause,
                    "type": self._infer_type(pic_clause),
                })

        return variables

    def _infer_type(self, pic_clause: str) -> str:
        """从 PIC 子句推断类型."""
        if "V" in pic_clause.upper():
            return "decimal"
        elif "9" in pic_clause:
            return "numeric"
        elif "X" in pic_clause.upper():
            return "string"
        return "unknown"

    def _extract_procedures(self, ast: dict) -> list[dict[str, Any]]:
        """提取 PROCEDURE."""
        procedures: list[dict[str, Any]] = []
        proc_lines = ast.get("divisions", {}).get("PROCEDURE DIVISION", [])

        for line in proc_lines:
            match = re.search(r"^(\S+)\.", line)
            if match and not any(
                kw in line.upper() for kw in ["MOVE", "ADD", "DISPLAY", "STOP"]
            ):
                procedures.append({
                    "name": match.group(1),
                    "type": "paragraph",
                })

        return procedures

    def _extract_operation(self, line: str) -> str:
        """提取文件操作类型."""
        for op in ["OPEN", "CLOSE", "READ", "WRITE", "REWRITE", "DELETE"]:
            if op in line:
                return op
        return "UNKNOWN"

    def _extract_call_target(self, line: str) -> str:
        """提取 CALL 目标."""
        match = re.search(r"CALL\s+['\"]?(\S+)['\"]?", line, re.IGNORECASE)
        return match.group(1) if match else "UNKNOWN"

