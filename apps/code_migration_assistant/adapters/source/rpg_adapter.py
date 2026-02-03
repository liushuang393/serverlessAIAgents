"""RPG Language Adapter.

RPG（Report Program Generator）源代码解析和分析。
IBM AS/400（iSeries）のレガシーRPGからモダン言語への移行をサポート。
RPG II, RPG III, RPG IV（ILE RPG）, RPG Free形式をサポート。
"""

import re
from typing import Any

from apps.code_migration_assistant.adapters.base import (
    AST,
    ExecutionResult,
    SourceLanguageAdapter,
)


class RPGAdapter(SourceLanguageAdapter):
    """RPG 语言适配器.

    IBM RPG（II/III/IV/Free）代码の解析をサポート。
    AS/400（iSeries）レガシーシステム移行向け。
    """

    # RPG キーワード
    KEYWORDS = {
        "DCL-S",
        "DCL-DS",
        "DCL-C",
        "DCL-F",
        "DCL-PI",
        "DCL-PR",
        "IF",
        "ELSE",
        "ELSEIF",
        "ENDIF",
        "DOW",
        "DOU",
        "FOR",
        "ENDDO",
        "ENDFOR",
        "SELECT",
        "WHEN",
        "OTHER",
        "ENDSL",
        "BEGSR",
        "ENDSR",
        "EXSR",
        "CALLP",
        "RETURN",
        "READ",
        "WRITE",
        "UPDATE",
        "DELETE",
        "CHAIN",
        "SETLL",
        "SETGT",
        "OPEN",
        "CLOSE",
        "FEOD",
        "EVAL",
        "EVALR",
        "CLEAR",
        "RESET",
        "MONITOR",
        "ON-ERROR",
        "ENDMON",
        "CTL-OPT",
        "DATFMT",
        "TIMFMT",
    }

    # 型マッピング（RPG → 汎用）
    TYPE_MAPPING = {
        "PACKED": "decimal",
        "ZONED": "decimal",
        "INT": "integer",
        "UNS": "unsigned",
        "FLOAT": "double",
        "CHAR": "string",
        "VARCHAR": "string",
        "DATE": "date",
        "TIME": "time",
        "TIMESTAMP": "timestamp",
        "IND": "boolean",
        "POINTER": "pointer",
    }

    @property
    def language_name(self) -> str:
        """语言名称."""
        return "RPG"

    def parse(self, source_code: str) -> AST:
        """解析 RPG 代码为 AST.

        Args:
            source_code: RPG 源代码

        Returns:
            抽象语法树
        """
        format_type = self._detect_format(source_code)

        if format_type == "free":
            return self._parse_free_format(source_code)
        return self._parse_fixed_format(source_code)

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
        executable = ast.divisions.get("CALCULATIONS", [])

        for line in executable:
            line_upper = line.upper()

            # CALLP（プログラム呼び出し）
            if "CALLP" in line_upper:
                match = re.search(r"CALLP\s+(\w+)", line_upper)
                if match:
                    calls.append(
                        {
                            "type": "program_call",
                            "target": match.group(1),
                            "line": line.strip(),
                        }
                    )

            # EXSR（サブルーチン呼び出し）
            if "EXSR" in line_upper:
                match = re.search(r"EXSR\s+(\w+)", line_upper)
                if match:
                    calls.append(
                        {
                            "type": "subroutine_call",
                            "target": match.group(1),
                            "line": line.strip(),
                        }
                    )

            # ファイル操作
            file_ops = ["READ", "WRITE", "UPDATE", "DELETE", "CHAIN", "SETLL", "SETGT"]
            for op in file_ops:
                if op in line_upper:
                    calls.append(
                        {
                            "type": "file_io",
                            "operation": op,
                            "line": line.strip(),
                        }
                    )
                    break

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
        """执行 RPG 代码（模拟执行）.

        Args:
            source_code: RPG 源代码
            inputs: 输入参数

        Returns:
            执行结果
        """
        return ExecutionResult(
            success=False,
            error="RPG execution requires IBM i (AS/400) environment (not implemented)",
        )

    def _detect_format(self, source_code: str) -> str:
        """フォーマット検出（固定形式/自由形式）."""
        lines = source_code.split("\n")

        for line in lines[:30]:  # 最初の30行をチェック
            stripped = line.strip().upper()
            # 自由形式の特徴的なキーワード
            if stripped.startswith(("DCL-S", "DCL-DS", "DCL-F", "DCL-C", "CTL-OPT")):
                return "free"
            if stripped.startswith("**FREE"):
                return "free"

        return "fixed"

    def _parse_free_format(self, source_code: str) -> AST:
        """自由形式RPGを解析."""
        lines = self._preprocess_free(source_code)
        program_name = self._extract_program_name_free(lines)
        variables = self._extract_variables_free(lines)
        procedures = self._extract_procedures_free(lines)
        files = self._extract_files_free(lines)

        divisions = {
            "CONTROL": self._extract_control_options(lines),
            "FILES": files,
            "DECLARATIONS": self._extract_declarations_free(lines),
            "CALCULATIONS": self._extract_calculations_free(lines),
        }

        return AST(
            program_id=program_name,
            divisions=divisions,
            metadata={"format": "free", "files": files},
            variables=variables,
            procedures=procedures,
        )

    def _parse_fixed_format(self, source_code: str) -> AST:
        """固定形式RPGを解析."""
        lines = source_code.split("\n")
        program_name = self._extract_program_name_fixed(lines)

        # 仕様タイプで分類
        h_specs = [l for l in lines if len(l) > 5 and l[5].upper() == "H"]
        f_specs = [l for l in lines if len(l) > 5 and l[5].upper() == "F"]
        d_specs = [l for l in lines if len(l) > 5 and l[5].upper() == "D"]
        c_specs = [l for l in lines if len(l) > 5 and l[5].upper() == "C"]
        o_specs = [l for l in lines if len(l) > 5 and l[5].upper() == "O"]

        variables = self._extract_variables_fixed(d_specs)
        files = self._extract_files_fixed(f_specs)

        divisions = {
            "HEADER": h_specs,
            "FILES": files,
            "DEFINITIONS": d_specs,
            "CALCULATIONS": c_specs,
            "OUTPUT": o_specs,
        }

        return AST(
            program_id=program_name,
            divisions=divisions,
            metadata={"format": "fixed", "files": files},
            variables=variables,
            procedures=[],
        )

    def _preprocess_free(self, source_code: str) -> list[str]:
        """自由形式の前処理."""
        lines = source_code.split("\n")
        processed: list[str] = []

        for line in lines:
            # コメント除去
            if "//" in line:
                line = line.split("//")[0]
            stripped = line.strip()
            if stripped and not stripped.startswith("**"):
                processed.append(stripped)

        return processed

    def _extract_program_name_free(self, lines: list[str]) -> str:
        """プログラム名を抽出（自由形式）."""
        for line in lines:
            # CTL-OPT の main() または最初のプロシージャ
            match = re.search(r"DCL-PROC\s+(\w+)", line, re.IGNORECASE)
            if match:
                return match.group(1)
        return "MAIN"

    def _extract_program_name_fixed(self, lines: list[str]) -> str:
        """プログラム名を抽出（固定形式）."""
        # 固定形式ではプログラム名は通常外部で定義
        return "RPGPGM"

    def _extract_variables_free(self, lines: list[str]) -> list[dict[str, Any]]:
        """変数を抽出（自由形式）."""
        variables: list[dict[str, Any]] = []

        for line in lines:
            # DCL-S（スタンドアロン変数）
            match = re.match(r"DCL-S\s+(\w+)\s+(\w+)(?:\((\d+)(?::(\d+))?\))?", line, re.IGNORECASE)
            if match:
                rpg_type = match.group(2).upper()
                variables.append(
                    {
                        "name": match.group(1),
                        "type": self.TYPE_MAPPING.get(rpg_type, "unknown"),
                        "rpg_type": rpg_type,
                        "length": match.group(3),
                        "decimals": match.group(4),
                    }
                )

        return variables

    def _extract_variables_fixed(self, d_specs: list[str]) -> list[dict[str, Any]]:
        """変数を抽出（固定形式）."""
        variables: list[dict[str, Any]] = []

        for line in d_specs:
            if len(line) < 40:
                continue

            # D仕様書の解析（列位置固定）
            name = line[6:21].strip()
            if not name:
                continue

            data_type = line[39:40].upper() if len(line) > 39 else ""
            length = line[32:39].strip()

            variables.append(
                {
                    "name": name,
                    "type": self.TYPE_MAPPING.get(data_type, "unknown"),
                    "rpg_type": data_type,
                    "length": length,
                }
            )

        return variables

    def _extract_procedures_free(self, lines: list[str]) -> list[dict[str, Any]]:
        """プロシージャを抽出（自由形式）."""
        procedures: list[dict[str, Any]] = []

        for line in lines:
            # DCL-PROC
            match = re.match(r"DCL-PROC\s+(\w+)", line, re.IGNORECASE)
            if match:
                procedures.append(
                    {
                        "name": match.group(1),
                        "type": "procedure",
                    }
                )

            # BEGSR（サブルーチン）
            match = re.match(r"BEGSR\s+(\w+)", line, re.IGNORECASE)
            if match:
                procedures.append(
                    {
                        "name": match.group(1),
                        "type": "subroutine",
                    }
                )

        return procedures

    def _extract_files_free(self, lines: list[str]) -> list[dict[str, Any]]:
        """ファイル定義を抽出（自由形式）."""
        files: list[dict[str, Any]] = []

        for line in lines:
            match = re.match(r"DCL-F\s+(\w+)\s+(\w+)", line, re.IGNORECASE)
            if match:
                files.append(
                    {
                        "name": match.group(1),
                        "usage": match.group(2).upper(),
                    }
                )

        return files

    def _extract_files_fixed(self, f_specs: list[str]) -> list[dict[str, Any]]:
        """ファイル定義を抽出（固定形式）."""
        files: list[dict[str, Any]] = []

        for line in f_specs:
            if len(line) < 20:
                continue

            name = line[6:16].strip()
            file_type = line[16:17].upper() if len(line) > 16 else ""
            usage = self._map_file_usage(file_type)

            if name:
                files.append(
                    {
                        "name": name,
                        "type": file_type,
                        "usage": usage,
                    }
                )

        return files

    def _extract_control_options(self, lines: list[str]) -> list[str]:
        """制御オプションを抽出."""
        return [l for l in lines if l.upper().startswith("CTL-OPT")]

    def _extract_declarations_free(self, lines: list[str]) -> list[str]:
        """宣言を抽出（自由形式）."""
        return [l for l in lines if re.match(r"DCL-[SFDC]", l, re.IGNORECASE)]

    def _extract_calculations_free(self, lines: list[str]) -> list[str]:
        """計算部分を抽出（自由形式）."""
        exec_keywords = {
            "IF",
            "ELSE",
            "ELSEIF",
            "ENDIF",
            "DOW",
            "DOU",
            "FOR",
            "ENDDO",
            "ENDFOR",
            "SELECT",
            "WHEN",
            "OTHER",
            "ENDSL",
            "CALLP",
            "EXSR",
            "RETURN",
            "EVAL",
            "EVALR",
            "READ",
            "WRITE",
            "UPDATE",
            "DELETE",
            "CHAIN",
        }

        calculations: list[str] = []
        for line in lines:
            first_word = line.split("(")[0].split()[0].upper() if line.split() else ""
            if first_word in exec_keywords or "=" in line:
                calculations.append(line)

        return calculations

    def _map_file_usage(self, file_type: str) -> str:
        """ファイル使用法をマッピング."""
        if file_type == "I":
            return "INPUT"
        if file_type == "O":
            return "OUTPUT"
        return "UPDATE"
