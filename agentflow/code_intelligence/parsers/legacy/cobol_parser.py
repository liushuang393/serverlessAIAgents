"""COBOL Parser - COBOLパーサー.

COBOLソースコードを解析してUnifiedASTを生成します。

使用例:
    >>> parser = CobolParser()
    >>> result = parser.parse(cobol_source)
    >>> print(result.ast.root.name)  # PROGRAM-ID
"""

from __future__ import annotations

import logging
import re
import time
from typing import Any

from agentflow.code_intelligence.ast.unified_ast import (
    ASTNode,
    ASTNodeType,
    SymbolInfo,
    TypeInfo,
    UnifiedAST,
)
from agentflow.code_intelligence.parsers.base import (
    CodeParser,
    ParseContext,
    ParseError,
    ParseResult,
)


_logger = logging.getLogger(__name__)


class CobolParser(CodeParser):
    """COBOLパーサー.

    基本的なCOBOL構文を解析してUnifiedASTを生成します。

    サポート機能:
        - IDENTIFICATION DIVISION解析
        - DATA DIVISION解析（WORKING-STORAGE SECTION）
        - PROCEDURE DIVISION解析
        - 基本的な文の解析（MOVE, ADD, DISPLAY等）
        - 制御構造の解析（IF-THEN-ELSE, PERFORM）
    """

    @property
    def language(self) -> str:
        return "cobol"

    @property
    def supported_extensions(self) -> list[str]:
        return [".cob", ".cbl", ".cpy"]

    def parse(
        self,
        source_code: str,
        context: ParseContext | None = None,
    ) -> ParseResult:
        """COBOLソースコードを解析.

        Args:
            source_code: COBOLソースコード
            context: 解析コンテキスト

        Returns:
            解析結果
        """
        start_time = time.time()
        context = context or ParseContext()
        errors: list[ParseError] = []
        warnings: list[str] = []

        try:
            if not source_code or not source_code.strip():
                return ParseResult(
                    success=False,
                    errors=[ParseError("Source code cannot be empty")],
                )

            lines = source_code.split("\n")

            # プログラム情報を抽出
            program_id = self._extract_program_id(lines)
            if not program_id:
                errors.append(ParseError("PROGRAM-ID not found"))

            # DIVISION別に解析
            divisions = self._parse_divisions(lines)

            # 変数を抽出
            variables = self._extract_variables(divisions.get("DATA DIVISION", []))

            # プロシージャを抽出
            procedures = self._extract_procedures(divisions.get("PROCEDURE DIVISION", []))

            # シンボルテーブルを構築
            symbols: dict[str, SymbolInfo] = {}
            for var in variables:
                symbols[var["name"]] = SymbolInfo(
                    name=var["name"],
                    kind="variable",
                    type_info=TypeInfo(
                        name=var["type"],
                        source_type=var.get("pic", ""),
                    ),
                    location="DATA DIVISION",
                )

            for proc in procedures:
                symbols[proc["name"]] = SymbolInfo(
                    name=proc["name"],
                    kind="procedure",
                    location="PROCEDURE DIVISION",
                )

            # ASTルートを構築
            root = ASTNode(
                node_type=ASTNodeType.PROGRAM,
                name=program_id or "UNKNOWN",
                metadata={
                    "language": "cobol",
                    "divisions": list(divisions.keys()),
                },
            )

            # 変数ノードを追加
            for var in variables:
                var_node = ASTNode(
                    node_type=ASTNodeType.VARIABLE,
                    name=var["name"],
                    metadata={
                        "level": var["level"],
                        "pic": var.get("pic", ""),
                        "type": var["type"],
                    },
                )
                root.children.append(var_node)

            # プロシージャノードを追加
            for proc in procedures:
                proc_node = ASTNode(
                    node_type=ASTNodeType.FUNCTION,
                    name=proc["name"],
                    metadata={
                        "statements": proc.get("statements", []),
                    },
                )
                root.children.append(proc_node)

            # UnifiedASTを構築
            ast = UnifiedAST(
                source_language="cobol",
                root=root,
                symbols=symbols,
            )

            duration = (time.time() - start_time) * 1000

            return ParseResult(
                success=len(errors) == 0,
                ast=ast,
                errors=errors,
                warnings=warnings,
                metadata={
                    "program_id": program_id,
                    "total_lines": len(lines),
                    "variable_count": len(variables),
                    "procedure_count": len(procedures),
                    "duration_ms": duration,
                },
            )

        except Exception as e:
            _logger.exception(f"COBOL parsing failed: {e}")
            return ParseResult(
                success=False,
                errors=[ParseError(str(e))],
            )

    def _extract_program_id(self, lines: list[str]) -> str | None:
        """PROGRAM-IDを抽出."""
        for line in lines:
            if "PROGRAM-ID" in line.upper():
                match = re.search(r"PROGRAM-ID\.\s+(\S+)", line, re.IGNORECASE)
                if match:
                    return match.group(1).rstrip(".")
        return None

    def _parse_divisions(self, lines: list[str]) -> dict[str, list[str]]:
        """DIVISIONを解析."""
        divisions: dict[str, list[str]] = {
            "IDENTIFICATION DIVISION": [],
            "DATA DIVISION": [],
            "PROCEDURE DIVISION": [],
        }

        current_division: str | None = None

        for line in lines:
            stripped = line.strip()

            # 空行とコメント行をスキップ
            if not stripped or stripped.startswith("*"):
                continue

            # DIVISIONを検出
            upper = stripped.upper()
            if "IDENTIFICATION DIVISION" in upper:
                current_division = "IDENTIFICATION DIVISION"
            elif "DATA DIVISION" in upper:
                current_division = "DATA DIVISION"
            elif "PROCEDURE DIVISION" in upper:
                current_division = "PROCEDURE DIVISION"
            elif current_division:
                divisions[current_division].append(stripped)

        return divisions

    def _extract_variables(self, data_lines: list[str]) -> list[dict[str, Any]]:
        """変数を抽出."""
        variables: list[dict[str, Any]] = []

        for line in data_lines:
            # レベル番号付き変数を抽出
            match = re.match(
                r"(\d+)\s+(\S+)\s+PIC\s+([^\s.]+)",
                line,
                re.IGNORECASE,
            )
            if match:
                level = match.group(1)
                name = match.group(2)
                pic = match.group(3)
                cobol_type = self._infer_type_from_pic(pic)

                variables.append(
                    {
                        "level": level,
                        "name": name,
                        "pic": pic,
                        "type": cobol_type,
                    }
                )

        return variables

    def _infer_type_from_pic(self, pic: str) -> str:
        """PIC句から型を推定."""
        if "9" in pic and "V" in pic:
            return "decimal"
        if "9" in pic:
            return "numeric"
        if "X" in pic or "A" in pic:
            return "string"
        return "unknown"

    def _extract_procedures(self, proc_lines: list[str]) -> list[dict[str, Any]]:
        """プロシージャを抽出."""
        procedures: list[dict[str, Any]] = []
        current_proc: dict[str, Any] | None = None

        for line in proc_lines:
            # PARAGRAPH/SECTIONを検出
            if line.endswith(".") and not any(
                kw in line.upper() for kw in ["MOVE", "ADD", "DISPLAY", "IF", "PERFORM", "STOP"]
            ):
                # 前のプロシージャを保存
                if current_proc:
                    procedures.append(current_proc)

                proc_name = line.rstrip(".")
                current_proc = {
                    "name": proc_name,
                    "statements": [],
                }
            elif current_proc:
                # 文を追加
                current_proc["statements"].append(line)

        # 最後のプロシージャを保存
        if current_proc:
            procedures.append(current_proc)

        return procedures


# レジストリに登録
def _register() -> None:
    """パーサーをレジストリに登録."""
    try:
        from agentflow.code_intelligence.parsers.registry import register_parser

        register_parser("cobol", CobolParser)
    except ImportError:
        pass


_register()


__all__ = ["CobolParser"]
