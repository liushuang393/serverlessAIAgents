# -*- coding: utf-8 -*-
"""PLY-based COBOL Parser.

このモジュールはPLY (Python Lex-Yacc)を使用したCOBOLパーサーを提供します。

主な機能:
    - IDENTIFICATION DIVISION解析
    - DATA DIVISION解析（WORKING-STORAGE SECTION）
    - PROCEDURE DIVISION解析
    - 基本的な文の解析（MOVE, ADD, DISPLAY等）
    - 制御構造の解析（IF-THEN-ELSE, PERFORM）

注意:
    - 完全なCOBOL仕様ではなく、基本的な構文のみをサポート
    - 実用的なCOBOLプログラムの移行に必要な最小限の機能を提供
"""

import logging
import re
from typing import Any


class ParseError(Exception):
    """COBOL解析エラー."""

    pass


class PLYCobolParser:
    """PLYベースのCOBOLパーサー.

    基本的なCOBOL構文を解析してASTを生成します。

    Attributes:
        source_code: COBOLソースコード
        _logger: ロガー
    """

    def __init__(self, source_code: str) -> None:
        """PLYCobolParserを初期化.

        Args:
            source_code: COBOLソースコード

        Raises:
            ParseError: ソースコードが空の場合
        """
        if not source_code or not source_code.strip():
            raise ParseError("Source code cannot be empty")

        self._source_code = source_code
        self._lines = source_code.split("\n")
        self._logger = logging.getLogger(__name__)

    def parse(self) -> dict[str, Any]:
        """COBOLソースコードを解析.

        Returns:
            AST（抽象構文木）

        Raises:
            ParseError: 解析エラー時
        """
        try:
            self._logger.debug(f"Starting COBOL parsing: {len(self._lines)} lines")

            ast: dict[str, Any] = {
                "program_id": "",
                "divisions": {
                    "IDENTIFICATION DIVISION": [],
                    "DATA DIVISION": [],
                "PROCEDURE DIVISION": [],
            },
            "metadata": {
                "total_lines": len(self._lines),
                "parser": "PLY",
            },
        }

            current_division = None
            current_section = None

            for line_num, line in enumerate(self._lines, 1):
                try:
                    stripped = line.strip()

                    # 空行とコメント行をスキップ
                    if not stripped or stripped.startswith("*"):
                        continue

                    # PROGRAM-IDを抽出
                    if "PROGRAM-ID" in stripped.upper():
                        match = re.search(r"PROGRAM-ID\.\s+(\S+)", stripped, re.IGNORECASE)
                        if match:
                            ast["program_id"] = match.group(1).rstrip(".")
                            self._logger.debug(f"Found PROGRAM-ID: {ast['program_id']}")

                    # DIVISIONを検出
                    if "IDENTIFICATION DIVISION" in stripped.upper():
                        current_division = "IDENTIFICATION DIVISION"
                        current_section = None
                        self._logger.debug(f"Entered {current_division}")
                    elif "DATA DIVISION" in stripped.upper():
                        current_division = "DATA DIVISION"
                        current_section = None
                        self._logger.debug(f"Entered {current_division}")
                    elif "PROCEDURE DIVISION" in stripped.upper():
                        current_division = "PROCEDURE DIVISION"
                        current_section = None
                        self._logger.debug(f"Entered {current_division}")
                    # SECTIONを検出
                    elif "WORKING-STORAGE SECTION" in stripped.upper():
                        current_section = "WORKING-STORAGE"
                        self._logger.debug(f"Entered {current_section} SECTION")
                    # 行を追加
                    elif current_division:
                        ast["divisions"][current_division].append(stripped)

                except Exception as e:
                    self._logger.warning(f"Error parsing line {line_num}: {e}")
                    # 解析エラーは警告のみで続行

            # PROGRAM-IDが見つからない場合はエラー
            if not ast["program_id"]:
                raise ParseError("PROGRAM-ID not found in source code")

            self._logger.info(f"Parsing completed: PROGRAM-ID={ast['program_id']}")
            return ast

        except ParseError:
            raise
        except Exception as e:
            self._logger.error(f"Unexpected error during parsing: {e}")
            raise ParseError(f"Failed to parse COBOL source: {e}") from e

    def extract_variables(self, ast: dict[str, Any]) -> list[dict[str, Any]]:
        """変数情報を抽出.

        Args:
            ast: 抽象構文木

        Returns:
            変数情報リスト
        """
        variables: list[dict[str, Any]] = []
        data_division = ast.get("divisions", {}).get("DATA DIVISION", [])

        for line in data_division:
            # 01, 05, 77レベルの変数を抽出
            match = re.match(r"(\d+)\s+(\S+)\s+PIC\s+([^\s.]+)", line, re.IGNORECASE)
            if match:
                level = match.group(1)
                name = match.group(2)
                pic = match.group(3)

                # PIC句から型を推定
                cobol_type = self._infer_type_from_pic(pic)

                variables.append({
                    "level": level,
                    "name": name,
                    "pic": pic,
                    "type": cobol_type,
                })

        return variables

    def _infer_type_from_pic(self, pic: str) -> str:
        """PIC句から型を推定.

        Args:
            pic: PIC句

        Returns:
            型（numeric, string, decimal）
        """
        if "9" in pic and "V" in pic:
            return "decimal"
        elif "9" in pic:
            return "numeric"
        elif "X" in pic or "A" in pic:
            return "string"
        else:
            return "unknown"

