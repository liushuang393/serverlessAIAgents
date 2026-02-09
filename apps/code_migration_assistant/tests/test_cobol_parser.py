"""Tests for COBOLParser MCP Tool."""

import pytest
from apps.code_migration_assistant.mcp_tools.cobol_parser import COBOLParser

from agentflow import MCPToolRequest


@pytest.mark.asyncio
async def test_cobol_parser_basic() -> None:
    """COBOLParser基本テスト."""
    parser = COBOLParser()

    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5).
       01 WS-NUM2 PIC 9(5).
       01 WS-RESULT PIC 9(10).

       PROCEDURE DIVISION.
           MOVE 100 TO WS-NUM1.
           MOVE 200 TO WS-NUM2.
           ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.
           DISPLAY "RESULT: " WS-RESULT.
           STOP RUN.
    """

    request = MCPToolRequest(
        tool="cobol_parser",
        version="1.0.0",
        input={
            "cobol_code": cobol_code,
            "file_name": "calculator.cob",
        },
    )

    response = await parser.handle_request(request)

    assert response.success is True
    assert response.output is not None
    assert "ast" in response.output
    assert "metadata" in response.output

    ast = response.output["ast"]
    assert ast["program_id"] == "CALCULATOR"
    assert "divisions" in ast

    metadata = response.output["metadata"]
    assert "variables" in metadata
    assert len(metadata["variables"]) == 3


@pytest.mark.asyncio
async def test_cobol_parser_invalid_input() -> None:
    """COBOLParser無効入力テスト."""
    parser = COBOLParser()

    request = MCPToolRequest(
        tool="cobol_parser",
        version="1.0.0",
        input={},
    )

    response = await parser.handle_request(request)

    assert response.success is False
    assert len(response.errors) > 0


@pytest.mark.asyncio
async def test_cobol_parser_empty_code() -> None:
    """COBOLParser空コードテスト."""
    parser = COBOLParser()

    request = MCPToolRequest(
        tool="cobol_parser",
        version="1.0.0",
        input={
            "cobol_code": "",
        },
    )

    response = await parser.handle_request(request)

    assert response.success is False
    assert len(response.errors) > 0


@pytest.mark.asyncio
async def test_cobol_parser_with_options() -> None:
    """COBOLParserオプション付きテスト."""
    parser = COBOLParser()

    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
    """

    request = MCPToolRequest(
        tool="cobol_parser",
        version="1.0.0",
        input={
            "cobol_code": cobol_code,
            "file_name": "test.cob",
            "encoding": "utf-8",
            "parse_options": {
                "strict_mode": False,
            },
        },
    )

    response = await parser.handle_request(request)

    assert response.success is True
    assert response.output is not None


@pytest.mark.asyncio
async def test_cobol_parser_metadata() -> None:
    """COBOLParserメタデータテスト."""
    parser = COBOLParser()

    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. METADATA-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(20).
       01 WS-AGE PIC 9(3).
       01 WS-SALARY PIC 9(7)V99.
    """

    request = MCPToolRequest(
        tool="cobol_parser",
        version="1.0.0",
        input={
            "cobol_code": cobol_code,
        },
    )

    response = await parser.handle_request(request)

    assert response.success is True
    metadata = response.output["metadata"]
    assert len(metadata["variables"]) == 3

    # 型チェック
    var_types = [var["type"] for var in metadata["variables"]]
    assert "string" in var_types
    assert "numeric" in var_types
    assert "decimal" in var_types

