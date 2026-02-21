"""COBOL Language Adapter.

COBOLソースコードの解析と分析を行う。
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
    """COBOL言語アダプター.

    PLYパーサーでCOBOLコードを解析する。
    """

    @property
    def language_name(self) -> str:
        """言語名称."""
        return "COBOL"

    def parse(self, source_code: str) -> AST:
        """COBOLコードをASTへ解析する.

        Args:
            source_code: COBOLソースコード

        Returns:
            抽象構文木
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
        """変数定義を抽出する.

        Args:
            ast: 抽象構文木

        Returns:
            変数リスト（型情報を含む）
        """
        return ast.variables

    def identify_external_calls(self, ast: AST) -> list[dict[str, Any]]:
        """外部呼び出しを識別する.

        Args:
            ast: 抽象構文木

        Returns:
            外部呼び出しリスト
        """
        calls: list[dict[str, Any]] = []

        # PROCEDURE DIVISION内の外部呼び出しを確認
        proc_div = ast.divisions.get("PROCEDURE DIVISION", [])

        for line in proc_div:
            line_upper = line.upper()

            # ファイル操作
            if any(kw in line_upper for kw in ["OPEN", "CLOSE", "READ", "WRITE"]):
                calls.append(
                    {
                        "type": "file_io",
                        "operation": self._extract_operation(line_upper),
                        "line": line.strip(),
                    }
                )

            # CALL文
            if "CALL" in line_upper:
                calls.append(
                    {
                        "type": "program_call",
                        "target": self._extract_call_target(line),
                        "line": line.strip(),
                    }
                )

            # SQL操作
            if "EXEC SQL" in line_upper:
                calls.append(
                    {
                        "type": "sql",
                        "line": line.strip(),
                    }
                )

        return calls

    def execute(self, source_code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """COBOLコードを実行する（模擬実行）.

        注意: 実際のCOBOL実行にはGnuCOBOL等のコンパイラが必要。
        ここでは単体テスト向けの簡易実装を提供する。

        Args:
            source_code: COBOLソースコード
            inputs: 入力パラメータ

        Returns:
            実行結果
        """
        # TODO: GnuCOBOLコンパイラを統合して実行対応する
        return ExecutionResult(
            success=False,
            error="COBOL execution requires GnuCOBOL compiler (not implemented)",
        )

    def _fallback_parse(self, source_code: str, error: str) -> AST:
        """簡易解析（フォールバック）."""
        lines = source_code.split("\n")

        # PROGRAM-IDを抽出
        program_id = "UNKNOWN"
        for line in lines:
            match = re.search(r"PROGRAM-ID\.\s+(\S+)", line, re.IGNORECASE)
            if match:
                program_id = match.group(1).rstrip(".")
                break

        # 簡易DIVISION分割
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
        """簡易な変数抽出."""
        variables: list[dict[str, Any]] = []

        for line in data_lines:
            match = re.search(r"(\d{2})\s+(\S+)\s+PIC\s+([^\s.]+)", line, re.IGNORECASE)
            if match:
                pic_clause = match.group(3)
                variables.append(
                    {
                        "level": match.group(1),
                        "name": match.group(2),
                        "pic_clause": pic_clause,
                        "type": self._infer_type(pic_clause),
                    }
                )

        return variables

    def _infer_type(self, pic_clause: str) -> str:
        """PIC句から型を推定する."""
        if "V" in pic_clause.upper():
            return "decimal"
        if "9" in pic_clause:
            return "numeric"
        if "X" in pic_clause.upper():
            return "string"
        return "unknown"

    def _extract_procedures(self, ast: dict) -> list[dict[str, Any]]:
        """PROCEDUREを抽出する."""
        procedures: list[dict[str, Any]] = []
        proc_lines = ast.get("divisions", {}).get("PROCEDURE DIVISION", [])

        for line in proc_lines:
            match = re.search(r"^(\S+)\.", line)
            if match and not any(kw in line.upper() for kw in ["MOVE", "ADD", "DISPLAY", "STOP"]):
                procedures.append(
                    {
                        "name": match.group(1),
                        "type": "paragraph",
                    }
                )

        return procedures

    def _extract_operation(self, line: str) -> str:
        """ファイル操作種別を抽出する."""
        for op in ["OPEN", "CLOSE", "READ", "WRITE", "REWRITE", "DELETE"]:
            if op in line:
                return op
        return "UNKNOWN"

    def _extract_call_target(self, line: str) -> str:
        """CALL対象を抽出する."""
        match = re.search(r"CALL\s+['\"]?(\S+)['\"]?", line, re.IGNORECASE)
        return match.group(1) if match else "UNKNOWN"
