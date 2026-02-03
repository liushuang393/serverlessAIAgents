"""PL/I Language Adapter.

PL/I（Programming Language One）源代码解析和分析。
IBMメインフレームのレガシーPL/Iからモダン言語への移行をサポート。
"""

import re
from typing import Any

from apps.code_migration_assistant.adapters.base import (
    AST,
    ExecutionResult,
    SourceLanguageAdapter,
)


class PLIAdapter(SourceLanguageAdapter):
    """PL/I 语言适配器.

    IBM PL/I 代码の解析をサポート。
    メインフレームレガシーシステム移行向け。
    """

    # PL/I キーワード
    KEYWORDS = {
        "PROCEDURE",
        "PROC",
        "BEGIN",
        "END",
        "DECLARE",
        "DCL",
        "IF",
        "THEN",
        "ELSE",
        "DO",
        "WHILE",
        "UNTIL",
        "LEAVE",
        "ITERATE",
        "CALL",
        "RETURN",
        "GOTO",
        "GO",
        "TO",
        "GET",
        "PUT",
        "READ",
        "WRITE",
        "OPEN",
        "CLOSE",
        "ALLOCATE",
        "FREE",
        "BASED",
        "POINTER",
        "PTR",
        "FIXED",
        "FLOAT",
        "DECIMAL",
        "DEC",
        "BINARY",
        "BIN",
        "CHARACTER",
        "CHAR",
        "BIT",
        "PICTURE",
        "PIC",
        "AUTOMATIC",
        "AUTO",
        "STATIC",
        "CONTROLLED",
        "CTL",
        "ENTRY",
        "RETURNS",
        "EXTERNAL",
        "INTERNAL",
        "ON",
        "SIGNAL",
        "REVERT",
    }

    # 型マッピング（PL/I → 汎用）
    TYPE_MAPPING = {
        "FIXED BINARY": "integer",
        "FIXED DECIMAL": "decimal",
        "FLOAT BINARY": "double",
        "FLOAT DECIMAL": "double",
        "CHARACTER": "string",
        "BIT": "boolean",
        "POINTER": "pointer",
    }

    @property
    def language_name(self) -> str:
        """语言名称."""
        return "PL/I"

    def parse(self, source_code: str) -> AST:
        """解析 PL/I 代码为 AST.

        Args:
            source_code: PL/I 源代码

        Returns:
            抽象语法树
        """
        lines = self._preprocess(source_code)
        program_name = self._extract_program_name(lines)
        variables = self._extract_variables(lines)
        procedures = self._extract_procedures(lines)
        structures = self._extract_structures(lines)

        divisions = {
            "DECLARATIONS": self._extract_declarations(lines),
            "EXECUTABLE": self._extract_executable(lines),
        }

        return AST(
            program_id=program_name,
            divisions=divisions,
            metadata={
                "structures": structures,
                "on_conditions": self._extract_on_conditions(lines),
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
                            "type": "procedure_call",
                            "target": match.group(1),
                            "line": line.strip(),
                        }
                    )

            # ファイル操作
            if any(kw in line_upper for kw in ["OPEN", "CLOSE", "READ", "WRITE", "GET", "PUT"]):
                calls.append(
                    {
                        "type": "file_io",
                        "operation": self._extract_io_operation(line_upper),
                        "line": line.strip(),
                    }
                )

            # SQL（Embedded SQL）
            if "EXEC SQL" in line_upper:
                calls.append(
                    {
                        "type": "sql",
                        "line": line.strip(),
                    }
                )

        return calls

    def execute(self, source_code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """执行 PL/I 代码（模拟执行）.

        Args:
            source_code: PL/I 源代码
            inputs: 输入参数

        Returns:
            执行结果
        """
        return ExecutionResult(
            success=False,
            error="PL/I execution requires IBM Enterprise PL/I compiler (not implemented)",
        )

    def _preprocess(self, source_code: str) -> list[str]:
        """前処理：コメント除去、継続行結合.

        Args:
            source_code: 源代码

        Returns:
            処理済み行リスト
        """
        # コメント除去（/* ... */）
        source_code = re.sub(r"/\*.*?\*/", "", source_code, flags=re.DOTALL)

        lines = source_code.split("\n")
        processed: list[str] = []
        current_stmt = ""

        for line in lines:
            stripped = line.strip()
            if not stripped:
                continue

            current_stmt += " " + stripped

            # セミコロンで文が終了
            if ";" in current_stmt:
                statements = current_stmt.split(";")
                for stmt in statements[:-1]:
                    stmt = stmt.strip()
                    if stmt:
                        processed.append(stmt + ";")
                current_stmt = statements[-1]

        if current_stmt.strip():
            processed.append(current_stmt.strip())

        return processed

    def _extract_program_name(self, lines: list[str]) -> str:
        """プログラム名を抽出."""
        for line in lines:
            match = re.match(r"(\w+)\s*:\s*(?:PROCEDURE|PROC)", line, re.IGNORECASE)
            if match:
                return match.group(1)
        return "UNKNOWN"

    def _extract_variables(self, lines: list[str]) -> list[dict[str, Any]]:
        """変数宣言を抽出."""
        variables: list[dict[str, Any]] = []
        dcl_pattern = r"(?:DECLARE|DCL)\s+(\w+)"

        for line in lines:
            if not re.match(r"(?:DECLARE|DCL)\s", line, re.IGNORECASE):
                continue

            match = re.match(dcl_pattern, line, re.IGNORECASE)
            if match:
                var_name = match.group(1)
                pli_type = self._extract_pli_type(line)
                variables.append(
                    {
                        "name": var_name,
                        "type": self._map_type(pli_type),
                        "pli_type": pli_type,
                        "attributes": self._extract_attributes(line),
                    }
                )

        return variables

    def _extract_pli_type(self, line: str) -> str:
        """PL/I 型を抽出."""
        line_upper = line.upper()

        if "FIXED BINARY" in line_upper or "FIXED BIN" in line_upper:
            return "FIXED BINARY"
        if "FIXED DECIMAL" in line_upper or "FIXED DEC" in line_upper:
            return "FIXED DECIMAL"
        if "FLOAT BINARY" in line_upper or "FLOAT BIN" in line_upper:
            return "FLOAT BINARY"
        if "FLOAT DECIMAL" in line_upper or "FLOAT DEC" in line_upper:
            return "FLOAT DECIMAL"
        if "CHARACTER" in line_upper or "CHAR" in line_upper:
            return "CHARACTER"
        if "BIT" in line_upper:
            return "BIT"
        if "POINTER" in line_upper or "PTR" in line_upper:
            return "POINTER"

        return "UNKNOWN"

    def _map_type(self, pli_type: str) -> str:
        """型マッピング."""
        return self.TYPE_MAPPING.get(pli_type, "unknown")

    def _extract_attributes(self, line: str) -> list[str]:
        """属性を抽出."""
        attrs: list[str] = []
        line_upper = line.upper()

        for attr in [
            "AUTOMATIC",
            "AUTO",
            "STATIC",
            "CONTROLLED",
            "CTL",
            "EXTERNAL",
            "INTERNAL",
            "BASED",
            "DEFINED",
        ]:
            if attr in line_upper:
                attrs.append(attr)

        return attrs

    def _extract_procedures(self, lines: list[str]) -> list[dict[str, Any]]:
        """プロシージャを抽出."""
        procedures: list[dict[str, Any]] = []

        for line in lines:
            match = re.match(
                r"(\w+)\s*:\s*(?:PROCEDURE|PROC)(?:\s*\(([^)]*)\))?", line, re.IGNORECASE
            )
            if match:
                args = []
                if match.group(2):
                    args = [a.strip() for a in match.group(2).split(",") if a.strip()]

                proc_type = "function" if "RETURNS" in line.upper() else "procedure"
                procedures.append(
                    {
                        "name": match.group(1),
                        "type": proc_type,
                        "arguments": args,
                    }
                )

        return procedures

    def _extract_structures(self, lines: list[str]) -> list[dict[str, Any]]:
        """構造体を抽出."""
        structures: list[dict[str, Any]] = []
        current_struct = None

        for line in lines:
            # 構造体開始（レベル1）
            match = re.match(r"(?:DECLARE|DCL)\s+1\s+(\w+)", line, re.IGNORECASE)
            if match:
                if current_struct:
                    structures.append(current_struct)
                current_struct = {"name": match.group(1), "members": []}
                continue

            # 構造体メンバー（レベル2以上）
            if current_struct:
                match = re.match(r"(?:DECLARE|DCL)?\s*(\d+)\s+(\w+)", line, re.IGNORECASE)
                if match and int(match.group(1)) > 1:
                    current_struct["members"].append(
                        {
                            "level": int(match.group(1)),
                            "name": match.group(2),
                            "type": self._extract_pli_type(line),
                        }
                    )

        if current_struct:
            structures.append(current_struct)

        return structures

    def _extract_on_conditions(self, lines: list[str]) -> list[dict[str, Any]]:
        """ON条件ハンドラを抽出."""
        handlers: list[dict[str, Any]] = []

        for line in lines:
            match = re.match(r"ON\s+(\w+)", line, re.IGNORECASE)
            if match:
                handlers.append(
                    {
                        "condition": match.group(1).upper(),
                        "line": line.strip(),
                    }
                )

        return handlers

    def _extract_declarations(self, lines: list[str]) -> list[str]:
        """宣言部分を抽出."""
        return [l for l in lines if re.match(r"(?:DECLARE|DCL)\s", l, re.IGNORECASE)]

    def _extract_executable(self, lines: list[str]) -> list[str]:
        """実行部分を抽出."""
        exec_keywords = {
            "IF",
            "DO",
            "CALL",
            "RETURN",
            "GOTO",
            "GO",
            "GET",
            "PUT",
            "READ",
            "WRITE",
            "OPEN",
            "CLOSE",
            "ALLOCATE",
            "FREE",
            "SIGNAL",
            "ON",
        }
        executable: list[str] = []

        for line in lines:
            first_word = line.split()[0].upper() if line.split() else ""
            # 代入文も実行文
            if first_word in exec_keywords or "=" in line:
                executable.append(line)

        return executable

    def _extract_io_operation(self, line: str) -> str:
        """I/O操作タイプを抽出."""
        for op in ["OPEN", "CLOSE", "READ", "WRITE", "GET", "PUT"]:
            if op in line:
                return op
        return "UNKNOWN"
