# -*- coding: utf-8 -*-
"""COBOLParser MCP Tool.

このモジュールはCOBOLソースコードを解析してASTとメタデータを生成するMCP工具を提供します。

主な機能:
    - COBOL構文解析
    - AST生成
    - メタデータ抽出（変数、プロシージャ、ファイル制御）
    - エラー検出と警告
"""

import re
from typing import Any

from apps.code_migration_assistant.mcp_tools.base import MCPTool, MCPToolRequest, MCPToolResponse
from apps.code_migration_assistant.parsers import PLYCobolParser


class COBOLParser(MCPTool):
    """COBOLParser MCP Tool.

    COBOLソースコードを解析してAST（抽象構文木）とメタデータを生成します。

    Input:
        - cobol_code: COBOLソースコード（必須）
        - file_name: ファイル名（オプション）
        - encoding: エンコーディング（デフォルト: utf-8）
        - parse_options: 解析オプション
            - strict_mode: 厳格モード（デフォルト: false）
            - expand_copy: COPYステートメント展開（デフォルト: true）

    Output:
        - ast: 抽象構文木
            - program_id: プログラムID
            - divisions: 各DIVISIONの内容
        - metadata: メタデータ
            - variables: 変数リスト
            - procedures: プロシージャリスト
            - file_controls: ファイル制御リスト
        - errors: エラーリスト
        - warnings: 警告リスト
    """

    def __init__(self) -> None:
        """COBOLParserを初期化."""
        super().__init__(tool_name="cobol_parser", version="1.0.0")

    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """COBOLソースコードを解析.

        Args:
            request: MCP工具リクエスト

        Returns:
            MCP工具レスポンス
        """
        # 入力パラメータを取得
        cobol_code = request.input.get("cobol_code")
        file_name = request.input.get("file_name", "unknown.cob")
        encoding = request.input.get("encoding", "utf-8")
        parse_options = request.input.get("parse_options", {})

        # 必須パラメータチェック
        if not cobol_code:
            return MCPToolResponse(
                success=False,
                errors=["cobol_code is required"],
            )

        # COBOL解析実行
        try:
            ast, metadata, errors, warnings = self._parse_cobol(
                cobol_code=cobol_code,
                file_name=file_name,
                strict_mode=parse_options.get("strict_mode", False),
                expand_copy=parse_options.get("expand_copy", True),
            )

            return MCPToolResponse(
                success=len(errors) == 0,
                output={
                    "ast": ast,
                    "metadata": metadata,
                    "errors": errors,
                    "warnings": warnings,
                },
            )

        except Exception as e:
            return MCPToolResponse(
                success=False,
                errors=[f"Parse failed: {str(e)}"],
            )

    def _parse_cobol(
        self,
        cobol_code: str,
        file_name: str,
        strict_mode: bool,
        expand_copy: bool,
    ) -> tuple[dict[str, Any], dict[str, Any], list[str], list[str]]:
        """COBOLコードを解析（PLYパーサー使用）.

        Args:
            cobol_code: COBOLソースコード
            file_name: ファイル名
            strict_mode: 厳格モード
            expand_copy: COPYステートメント展開

        Returns:
            (AST, メタデータ, エラーリスト, 警告リスト)
        """
        errors: list[str] = []
        warnings: list[str] = []

        try:
            # PLYパーサーを使用
            parser = PLYCobolParser(cobol_code)
            ast = parser.parse()
            variables = parser.extract_variables(ast)

            # プログラムIDチェック
            if not ast.get("program_id"):
                errors.append("PROGRAM-ID not found")
                ast["program_id"] = "UNKNOWN"

            # 変数情報をメタデータに追加
            metadata = ast.get("metadata", {})
            metadata["variables"] = variables
            metadata["file_name"] = file_name
            metadata["strict_mode"] = strict_mode

            return ast, metadata, errors, warnings

        except Exception as e:
            errors.append(f"PLY parser error: {str(e)}")
            # フォールバック: 簡易パーサー
            return self._fallback_parse(cobol_code, file_name, strict_mode)

    def _fallback_parse(
        self,
        cobol_code: str,
        file_name: str,
        strict_mode: bool,
    ) -> tuple[dict[str, Any], dict[str, Any], list[str], list[str]]:
        """フォールバック簡易パーサー.

        Args:
            cobol_code: COBOLソースコード
            file_name: ファイル名
            strict_mode: 厳格モード

        Returns:
            (AST, メタデータ, エラーリスト, 警告リスト)
        """
        errors: list[str] = []
        warnings: list[str] = ["Using fallback parser"]

        # 行分割
        lines = cobol_code.split("\n")

        # PROGRAM-IDを抽出
        program_id = self._extract_program_id(lines)
        if not program_id:
            errors.append("PROGRAM-ID not found")
            program_id = "UNKNOWN"

        # DIVISIONを解析
        divisions = self._parse_divisions(lines)

        # 変数を抽出
        variables = self._extract_variables(divisions.get("DATA DIVISION", []))

        # プロシージャを抽出
        procedures = self._extract_procedures(divisions.get("PROCEDURE DIVISION", []))

        # ファイル制御を抽出
        file_controls = self._extract_file_controls(divisions.get("ENVIRONMENT DIVISION", []))

        # AST構築
        ast = {
            "program_id": program_id,
            "divisions": divisions,
        }

        # メタデータ構築
        metadata = {
            "file_name": file_name,
            "variables": variables,
            "procedures": procedures,
            "file_controls": file_controls,
            "line_count": len(lines),
        }

        return ast, metadata, errors, warnings

    def _extract_program_id(self, lines: list[str]) -> str | None:
        """PROGRAM-IDを抽出.

        Args:
            lines: COBOLコード行リスト

        Returns:
            PROGRAM-ID（見つからない場合はNone）
        """
        for line in lines:
            match = re.search(r"PROGRAM-ID\.\s+(\S+)", line, re.IGNORECASE)
            if match:
                return match.group(1).rstrip(".")
        return None

    def _parse_divisions(self, lines: list[str]) -> dict[str, list[str]]:
        """DIVISIONを解析.

        Args:
            lines: COBOLコード行リスト

        Returns:
            DIVISION名 → 行リストのマッピング
        """
        divisions: dict[str, list[str]] = {}
        current_division: str | None = None

        for line in lines:
            # DIVISION検出
            if "DIVISION" in line.upper():
                match = re.search(r"(\w+\s+DIVISION)", line, re.IGNORECASE)
                if match:
                    current_division = match.group(1).upper()
                    divisions[current_division] = []
                    continue

            # 現在のDIVISIONに行を追加
            if current_division:
                divisions[current_division].append(line)

        return divisions

    def _extract_variables(self, data_division_lines: list[str]) -> list[dict[str, Any]]:
        """変数を抽出.

        Args:
            data_division_lines: DATA DIVISIONの行リスト

        Returns:
            変数情報リスト
        """
        variables: list[dict[str, Any]] = []

        for line in data_division_lines:
            # レベル番号と変数名を抽出（例: 01 WS-NUM1 PIC 9(5).）
            match = re.search(r"(\d{2})\s+(\S+)\s+PIC\s+([^\s.]+)", line, re.IGNORECASE)
            if match:
                level = match.group(1)
                name = match.group(2)
                pic_clause = match.group(3)

                variables.append(
                    {
                        "level": level,
                        "name": name,
                        "pic_clause": pic_clause,
                        "type": self._infer_type_from_pic(pic_clause),
                    }
                )

        return variables

    def _infer_type_from_pic(self, pic_clause: str) -> str:
        """PIC句から型を推論.

        Args:
            pic_clause: PIC句（例: 9(5), X(10), 9(5)V9(2)）

        Returns:
            推論された型（numeric, string, decimal）
        """
        if "V" in pic_clause.upper():
            return "decimal"
        elif "9" in pic_clause:
            return "numeric"
        elif "X" in pic_clause.upper():
            return "string"
        else:
            return "unknown"

    def _extract_procedures(self, procedure_division_lines: list[str]) -> list[dict[str, Any]]:
        """プロシージャを抽出.

        Args:
            procedure_division_lines: PROCEDURE DIVISIONの行リスト

        Returns:
            プロシージャ情報リスト
        """
        procedures: list[dict[str, Any]] = []

        for line in procedure_division_lines:
            # パラグラフ名を抽出（例: MAIN-LOGIC.）
            match = re.search(r"^(\S+)\.", line)
            if match and not any(
                keyword in line.upper()
                for keyword in ["MOVE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "DISPLAY", "STOP"]
            ):
                procedures.append(
                    {
                        "name": match.group(1),
                        "type": "paragraph",
                    }
                )

        return procedures

    def _extract_file_controls(self, environment_division_lines: list[str]) -> list[dict[str, Any]]:
        """ファイル制御を抽出.

        Args:
            environment_division_lines: ENVIRONMENT DIVISIONの行リスト

        Returns:
            ファイル制御情報リスト
        """
        file_controls: list[dict[str, Any]] = []

        for line in environment_division_lines:
            # SELECT文を抽出（例: SELECT INPUT-FILE ASSIGN TO "input.dat".）
            match = re.search(r"SELECT\s+(\S+)\s+ASSIGN\s+TO\s+(.+)", line, re.IGNORECASE)
            if match:
                file_controls.append(
                    {
                        "file_name": match.group(1),
                        "assign_to": match.group(2).strip().rstrip(".").strip('"'),
                    }
                )

        return file_controls

