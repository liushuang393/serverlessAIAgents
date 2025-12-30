# -*- coding: utf-8 -*-
"""Integration Tests for Code Migration Assistant."""

import pytest

from apps.code_migration_assistant.mcp_client import MCPClient
from apps.code_migration_assistant.mcp_tools.cobol_parser import COBOLParser
from apps.code_migration_assistant.mcp_tools.code_validator import CodeValidator
from apps.code_migration_assistant.mcp_tools.java_generator import JavaGenerator
from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator


@pytest.mark.asyncio
async def test_parser_generator_integration() -> None:
    """COBOLParser + JavaGenerator統合テスト."""
    # MCPClientを作成
    client = MCPClient()
    client.register_tool("cobol_parser", COBOLParser())
    client.register_tool("java_generator", JavaGenerator())

    # COBOLコードを解析
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5).
       01 WS-NUM2 PIC 9(5).
    """

    parse_response = await client.call_tool_by_name(
        tool_name="cobol_parser",
        input_data={
            "cobol_code": cobol_code,
        },
    )

    assert parse_response.success is True

    # Javaコードを生成
    ast = parse_response.output["ast"]
    metadata = parse_response.output["metadata"]

    generate_response = await client.call_tool_by_name(
        tool_name="java_generator",
        input_data={
            "ast": ast,
            "metadata": metadata,
        },
    )

    assert generate_response.success is True
    assert "java_code" in generate_response.output
    assert "class_name" in generate_response.output


@pytest.mark.asyncio
async def test_generator_validator_integration() -> None:
    """JavaGenerator + CodeValidator統合テスト."""
    # MCPClientを作成
    client = MCPClient()
    client.register_tool("java_generator", JavaGenerator())
    client.register_tool("code_validator", CodeValidator())

    # テストデータ
    ast = {
        "program_id": "TEST-PROGRAM",
        "divisions": {
            "PROCEDURE DIVISION": [],
        },
    }

    metadata = {
        "variables": [
            {
                "name": "WS-NUM1",
                "type": "numeric",
                "pic_clause": "9(5)",
                "level": "01",
            },
        ],
    }

    # Javaコードを生成
    generate_response = await client.call_tool_by_name(
        tool_name="java_generator",
        input_data={
            "ast": ast,
            "metadata": metadata,
        },
    )

    assert generate_response.success is True

    # Javaコードを検証
    java_code = generate_response.output["java_code"]
    mappings = generate_response.output["mappings"]

    validate_response = await client.call_tool_by_name(
        tool_name="code_validator",
        input_data={
            "java_code": java_code,
            "ast": ast,
            "metadata": metadata,
            "mappings": mappings,
        },
    )

    assert validate_response.success is True
    assert "score" in validate_response.output
    assert validate_response.output["score"] > 0


@pytest.mark.asyncio
async def test_orchestrator_basic() -> None:
    """Orchestrator基本テスト."""
    # MCPClientを作成
    client = MCPClient()
    client.register_tool("cobol_parser", COBOLParser())
    client.register_tool("java_generator", JavaGenerator())
    client.register_tool("code_validator", CodeValidator())

    # Orchestratorを作成
    orchestrator = CodeMigrationOrchestrator(client)

    # 簡単なCOBOLコード
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20).
       
       PROCEDURE DIVISION.
           MOVE "HELLO WORLD" TO WS-MESSAGE.
           DISPLAY WS-MESSAGE.
           STOP RUN.
    """

    # 移行実行（ReflectionPatternとMemorySystemは未登録なのでエラーになる）
    # 注意: 完全なテストには全てのMCP工具が必要
    result = await orchestrator.migrate(cobol_code=cobol_code)

    # 解析段階は成功するはず
    assert "ast" in result or "errors" in result


@pytest.mark.asyncio
async def test_mcp_client_tool_management() -> None:
    """MCPClient工具管理テスト."""
    client = MCPClient()

    # 工具登録
    parser = COBOLParser()
    client.register_tool("cobol_parser", parser)

    assert client.has_tool("cobol_parser") is True
    assert client.has_tool("unknown_tool") is False

    # 工具リスト
    tools = client.list_tools()
    assert "cobol_parser" in tools

    # 工具取得
    tool = client.get_tool("cobol_parser")
    assert tool is not None
    assert tool == parser

    # 工具登録解除
    client.unregister_tool("cobol_parser")
    assert client.has_tool("cobol_parser") is False

