"""FORTRAN Language Adapter.

FORTRAN（FORTRAN 77/90/95/2003/2008）源代码解析和分析。
レガシーFORTRANからモダンJava/Pythonへの移行をサポート。
"""

import re
from typing import Any

from apps.code_migration_assistant.adapters.base import (
    AST,
    ExecutionResult,
    SourceLanguageAdapter,
)


class FortranAdapter(SourceLanguageAdapter):
    """FORTRAN 语言适配器.

    FORTRAN 77/90/95/2003/2008 代码的解析。
    レガシーシステム移行のため、固定形式と自由形式の両方をサポート。
    """

    # FORTRAN キーワード
    KEYWORDS = {
        "PROGRAM",
        "SUBROUTINE",
        "FUNCTION",
        "MODULE",
        "END",
        "INTEGER",
        "REAL",
        "DOUBLE",
        "PRECISION",
        "COMPLEX",
        "LOGICAL",
        "CHARACTER",
        "DIMENSION",
        "PARAMETER",
        "COMMON",
        "DATA",
        "IMPLICIT",
        "EQUIVALENCE",
        "IF",
        "THEN",
        "ELSE",
        "ENDIF",
        "DO",
        "WHILE",
        "ENDDO",
        "CALL",
        "RETURN",
        "STOP",
        "GOTO",
        "CONTINUE",
        "READ",
        "WRITE",
        "PRINT",
        "OPEN",
        "CLOSE",
        "INQUIRE",
        "FORMAT",
        "SAVE",
        "EXTERNAL",
        "INTRINSIC",
    }

    # 型マッピング（FORTRAN → 汎用）
    TYPE_MAPPING = {
        "INTEGER": "integer",
        "INTEGER*2": "short",
        "INTEGER*4": "integer",
        "INTEGER*8": "long",
        "REAL": "float",
        "REAL*4": "float",
        "REAL*8": "double",
        "DOUBLE PRECISION": "double",
        "COMPLEX": "complex",
        "LOGICAL": "boolean",
        "CHARACTER": "string",
    }

    @property
    def language_name(self) -> str:
        """语言名称."""
        return "FORTRAN"

    def parse(self, source_code: str) -> AST:
        """解析 FORTRAN 代码为 AST.

        Args:
            source_code: FORTRAN 源代码

        Returns:
            抽象语法树
        """
        lines = self._preprocess(source_code)
        program_name = self._extract_program_name(lines)
        variables = self._extract_variables(lines)
        procedures = self._extract_procedures(lines)
        common_blocks = self._extract_common_blocks(lines)

        divisions = {
            "DECLARATIONS": self._extract_declarations(lines),
            "EXECUTABLE": self._extract_executable(lines),
        }

        return AST(
            program_id=program_name,
            divisions=divisions,
            metadata={
                "format": self._detect_format(source_code),
                "common_blocks": common_blocks,
                "implicit_rules": self._extract_implicit(lines),
            },
            variables=variables,
            procedures=procedures,
        )

    def extract_variables(self, ast: AST) -> list[dict[str, Any]]:
        """提取变量定义.

        Args:
            ast: 抽象语法树

        Returns:
            变量列表
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
        executable = ast.divisions.get("EXECUTABLE", [])

        for line in executable:
            line_upper = line.upper()

            # CALL 语句
            if "CALL" in line_upper:
                match = re.search(r"CALL\s+(\w+)", line_upper)
                if match:
                    calls.append(
                        {
                            "type": "subroutine_call",
                            "target": match.group(1),
                            "line": line.strip(),
                        }
                    )

            # 文件操作
            if any(kw in line_upper for kw in ["OPEN", "CLOSE", "READ", "WRITE"]):
                calls.append(
                    {
                        "type": "file_io",
                        "operation": self._extract_io_operation(line_upper),
                        "line": line.strip(),
                    }
                )

            # 外部関数呼び出し（EXTERNAL宣言）
            if "EXTERNAL" in line_upper:
                match = re.findall(r"\b(\w+)\b", line_upper.replace("EXTERNAL", ""))
                for name in match:
                    if name not in self.KEYWORDS:
                        calls.append(
                            {
                                "type": "external_function",
                                "target": name,
                                "line": line.strip(),
                            }
                        )

        return calls

    def execute(self, source_code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """执行 FORTRAN 代码（模拟执行）.

        Args:
            source_code: FORTRAN 源代码
            inputs: 输入参数

        Returns:
            执行结果
        """
        return ExecutionResult(
            success=False,
            error="FORTRAN execution requires gfortran compiler (not implemented)",
        )

    def _preprocess(self, source_code: str) -> list[str]:
        """前処理：継続行の結合、コメント除去.

        Args:
            source_code: 源代码

        Returns:
            処理済み行リスト
        """
        lines = source_code.split("\n")
        processed: list[str] = []
        continuation = ""

        for line in lines:
            # コメント行をスキップ（固定形式）
            if line and line[0] in ("C", "c", "*", "!"):
                continue

            # 行末コメント除去（自由形式）
            if "!" in line:
                line = line.split("!")[0]

            # 継続行処理（固定形式：6列目に非空白文字）
            if len(line) > 5 and line[5] not in (" ", "0"):
                continuation += line[6:].strip()
                continue

            if continuation:
                processed.append(continuation + " " + line.strip())
                continuation = ""
            else:
                stripped = line.strip()
                if stripped:
                    processed.append(stripped)

        return processed

    def _detect_format(self, source_code: str) -> str:
        """フォーマット検出（固定形式/自由形式）."""
        lines = source_code.split("\n")
        fixed_indicators = 0

        for line in lines[:20]:  # 最初の20行をチェック
            if len(line) > 72:  # 72列超え → 自由形式の可能性
                return "free"
            if len(line) > 5 and line[5] not in (" ", "0", ""):
                fixed_indicators += 1  # 継続行マーカー

        return "fixed" if fixed_indicators > 0 else "free"

    def _extract_program_name(self, lines: list[str]) -> str:
        """プログラム名を抽出."""
        for line in lines:
            match = re.match(r"PROGRAM\s+(\w+)", line, re.IGNORECASE)
            if match:
                return match.group(1)
        return "UNKNOWN"

    def _extract_variables(self, lines: list[str]) -> list[dict[str, Any]]:
        """変数宣言を抽出."""
        variables: list[dict[str, Any]] = []
        type_pattern = r"(INTEGER|REAL|DOUBLE\s+PRECISION|COMPLEX|LOGICAL|CHARACTER)(\*\d+)?\s+"

        for line in lines:
            match = re.match(type_pattern, line, re.IGNORECASE)
            if match:
                type_str = match.group(1).upper()
                if match.group(2):
                    type_str += match.group(2).upper()

                # 変数名を抽出
                rest = line[match.end() :].strip()
                var_names = re.findall(r"\b(\w+)(?:\([\d:,]+\))?", rest)

                for var_name in var_names:
                    if var_name.upper() not in self.KEYWORDS:
                        variables.append(
                            {
                                "name": var_name,
                                "type": self.TYPE_MAPPING.get(type_str, "unknown"),
                                "fortran_type": type_str,
                            }
                        )

        return variables

    def _extract_procedures(self, lines: list[str]) -> list[dict[str, Any]]:
        """サブルーチン/関数を抽出."""
        procedures: list[dict[str, Any]] = []

        for line in lines:
            # SUBROUTINE
            match = re.match(r"SUBROUTINE\s+(\w+)\s*\(([^)]*)\)?", line, re.IGNORECASE)
            if match:
                args = [a.strip() for a in match.group(2).split(",") if a.strip()]
                procedures.append(
                    {
                        "name": match.group(1),
                        "type": "subroutine",
                        "arguments": args,
                    }
                )
                continue

            # FUNCTION
            match = re.match(r"(?:(\w+)\s+)?FUNCTION\s+(\w+)\s*\(([^)]*)\)?", line, re.IGNORECASE)
            if match:
                args = [a.strip() for a in match.group(3).split(",") if a.strip()]
                procedures.append(
                    {
                        "name": match.group(2),
                        "type": "function",
                        "return_type": match.group(1) or "REAL",
                        "arguments": args,
                    }
                )

        return procedures

    def _extract_common_blocks(self, lines: list[str]) -> list[dict[str, Any]]:
        """COMMON ブロックを抽出."""
        commons: list[dict[str, Any]] = []

        for line in lines:
            match = re.match(r"COMMON\s*/(\w+)/\s*(.+)", line, re.IGNORECASE)
            if match:
                vars_str = match.group(2)
                var_names = [v.strip() for v in vars_str.split(",")]
                commons.append(
                    {
                        "name": match.group(1),
                        "variables": var_names,
                    }
                )

        return commons

    def _extract_implicit(self, lines: list[str]) -> dict[str, str]:
        """IMPLICIT 規則を抽出."""
        rules: dict[str, str] = {}

        for line in lines:
            if "IMPLICIT NONE" in line.upper():
                return {"mode": "none"}

            match = re.match(r"IMPLICIT\s+(\w+)\s*\(([A-Z]-[A-Z])\)", line, re.IGNORECASE)
            if match:
                type_str = match.group(1).upper()
                range_str = match.group(2).upper()
                start, end = range_str.split("-")
                for c in range(ord(start), ord(end) + 1):
                    rules[chr(c)] = type_str

        # FORTRAN デフォルト暗黙型規則
        if not rules:
            for c in "IJKLMN":
                rules[c] = "INTEGER"
            for c in "ABCDEFGHOPQRSTUVWXYZ":
                rules[c] = "REAL"

        return rules

    def _extract_declarations(self, lines: list[str]) -> list[str]:
        """宣言部分を抽出."""
        decl_keywords = {
            "INTEGER",
            "REAL",
            "DOUBLE",
            "COMPLEX",
            "LOGICAL",
            "CHARACTER",
            "DIMENSION",
            "PARAMETER",
            "COMMON",
            "DATA",
            "IMPLICIT",
            "EQUIVALENCE",
            "EXTERNAL",
        }
        declarations: list[str] = []

        for line in lines:
            first_word = line.split()[0].upper() if line.split() else ""
            if first_word in decl_keywords:
                declarations.append(line)

        return declarations

    def _extract_executable(self, lines: list[str]) -> list[str]:
        """実行部分を抽出."""
        exec_keywords = {
            "IF",
            "DO",
            "CALL",
            "RETURN",
            "STOP",
            "GOTO",
            "READ",
            "WRITE",
            "PRINT",
            "OPEN",
            "CLOSE",
            "CONTINUE",
        }
        executable: list[str] = []

        for line in lines:
            first_word = line.split()[0].upper() if line.split() else ""
            # 数字で始まる行（ラベル付き文）も実行文
            if first_word in exec_keywords or (first_word and first_word[0].isdigit()):
                executable.append(line)

        return executable

    def _extract_io_operation(self, line: str) -> str:
        """I/O操作タイプを抽出."""
        for op in ["OPEN", "CLOSE", "READ", "WRITE", "PRINT", "INQUIRE"]:
            if op in line:
                return op
        return "UNKNOWN"
