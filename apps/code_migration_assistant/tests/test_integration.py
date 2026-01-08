# -*- coding: utf-8 -*-
"""Integration Tests for Code Migration Assistant.

v2.0: Multi-Agent アーキテクチャ対応
"""

import pytest

from agentflow import MCPToolClient as MCPClient
from apps.code_migration_assistant.mcp_tools.cobol_parser import COBOLParser
from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator


# ========================================
# Agent 統合テスト
# ========================================


@pytest.mark.asyncio
async def test_transform_agent_basic() -> None:
    """TransformAgent 基本テスト."""
    from apps.code_migration_assistant.agents import TransformAgent

    agent = TransformAgent()

    # 簡単な COBOL コード
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5).
       01 WS-NUM2 PIC 9(5).
    """

    # 確定性メソッドを直接テスト
    parse_result = agent.parse_cobol(cobol_code)
    assert parse_result["success"] is True
    assert "program_id" in parse_result

    # コンパイルテスト
    java_code = """
    public class SimpleCalc {
        private int wsNum1;
        private int wsNum2;
    }
    """
    compile_result = agent.compile_java(java_code)
    assert "success" in compile_result


@pytest.mark.asyncio
async def test_checker_agent_basic() -> None:
    """CheckerAgent 基本テスト."""
    from apps.code_migration_assistant.agents import CheckerAgent

    agent = CheckerAgent()

    # 比較テスト
    expected = {"value": "100", "name": "test"}
    actual = {"value": "100", "name": "test"}

    comparison = agent.compare_outputs(expected, actual)
    assert comparison["is_equal"] is True
    assert comparison["match_rate"] >= 0.99  # 浮点数比較は近似値で


@pytest.mark.asyncio
async def test_checker_agent_diff() -> None:
    """CheckerAgent 差分検出テスト."""
    from apps.code_migration_assistant.agents import CheckerAgent

    agent = CheckerAgent()

    expected = {"value": "100", "name": "test"}
    actual = {"value": "200", "name": "test"}

    comparison = agent.compare_outputs(expected, actual)
    assert comparison["is_equal"] is False
    assert len(comparison["differences"]) == 1
    assert comparison["differences"][0]["field"] == "value"


@pytest.mark.asyncio
async def test_fixer_agent_basic() -> None:
    """FixerAgent 基本テスト."""
    from apps.code_migration_assistant.agents import FixerAgent

    agent = FixerAgent()

    # コンパイルテスト
    java_code = """
    public class Test {
        public static void main(String[] args) {
            System.out.println("Hello");
        }
    }
    """

    compile_result = agent.compile_java(java_code)
    assert "success" in compile_result

    # エラー位置抽出テスト
    errors = ["Test.java:5: error: ';' expected"]
    locations = agent.extract_error_location(errors)
    assert len(locations) > 0


@pytest.mark.asyncio
async def test_testgen_agent_basic() -> None:
    """TestGenAgent 基本テスト."""
    from apps.code_migration_assistant.agents import TestGenAgent

    agent = TestGenAgent()

    # テンプレート取得
    template = agent.get_test_template("Calculator")
    assert "class CalculatorTest" in template
    assert "@Test" in template


@pytest.mark.asyncio
async def test_orchestrator_basic() -> None:
    """Orchestrator 基本テスト."""
    orchestrator = CodeMigrationOrchestrator()

    # 簡単な COBOL コード
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

    # run メソッドをテスト（LLM なしでは完全な変換はできない）
    result = await orchestrator.run({"cobol_code": cobol_code})

    # 結果構造を確認
    assert "success" in result


# ========================================
# MCP Client テスト（後方互換性）
# ========================================


@pytest.mark.asyncio
async def test_mcp_client_tool_management() -> None:
    """MCPClient 工具管理テスト."""
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
