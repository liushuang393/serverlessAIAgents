# -*- coding: utf-8 -*-
"""ToolDefinitionモデルのテスト.

統一ツール定義モデルのユニットテスト。
"""
import pytest
from pydantic import ValidationError


def test_tool_definition_creation():
    """基本的なToolDefinition作成のテスト."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    tool = ToolDefinition(
        uri="tool://builtin/calculator",
        name="calculator",
        description="算術計算を実行するツール",
        source=ToolSource.BUILTIN,
        input_schema={"type": "object", "properties": {"expression": {"type": "string"}}},
    )

    assert tool.uri == "tool://builtin/calculator"
    assert tool.name == "calculator"
    assert tool.source == ToolSource.BUILTIN
    assert tool.input_schema is not None


def test_tool_definition_uri_validation():
    """URI検証のテスト - tool://スキーム必須."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    with pytest.raises(ValidationError, match="uri"):
        ToolDefinition(
            uri="invalid-uri",  # tool:// スキームなし
            name="test",
            description="テスト",
            source=ToolSource.BUILTIN,
        )


def test_tool_definition_from_mcp():
    """MCPツール形式からToolDefinition作成のテスト."""
    from agentflow.core.tool_definition import ToolDefinition

    mcp_tool = {
        "name": "read_file",
        "description": "ファイル内容を読み取る",
        "inputSchema": {"type": "object", "properties": {"path": {"type": "string"}}},
    }

    tool = ToolDefinition.from_mcp(mcp_tool, server_name="filesystem")

    assert tool.uri == "tool://mcp/filesystem/read_file"
    assert tool.name == "read_file"
    assert tool.description == "ファイル内容を読み取る"


def test_tool_definition_from_skill():
    """Skill形式からToolDefinition作成のテスト."""
    from agentflow.core.tool_definition import ToolDefinition

    skill_data = {
        "name": "code_review",
        "description": "コードベストプラクティスをレビュー",
        "parameters": {"code": {"type": "string"}},
    }

    tool = ToolDefinition.from_skill(skill_data)

    assert tool.uri == "tool://skill/code_review"
    assert tool.name == "code_review"


def test_tool_definition_from_builtin():
    """@toolデコレータからToolDefinition作成のテスト."""
    from agentflow.core.tool_definition import ToolDefinition

    tool = ToolDefinition.from_builtin(
        name="search",
        description="ドキュメントを検索",
        input_schema={"type": "object", "properties": {"query": {"type": "string"}}},
    )

    assert tool.uri == "tool://builtin/search"
    assert tool.name == "search"


def test_tool_definition_to_mcp_format():
    """MCP形式への変換テスト."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    tool = ToolDefinition(
        uri="tool://builtin/search",
        name="search",
        description="ドキュメントを検索",
        source=ToolSource.BUILTIN,
        input_schema={"type": "object", "properties": {"query": {"type": "string"}}},
    )

    mcp_format = tool.to_mcp()

    assert mcp_format["name"] == "search"
    assert mcp_format["description"] == "ドキュメントを検索"
    assert "inputSchema" in mcp_format


def test_tool_definition_matches_query():
    """クエリマッチングのテスト."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    tool = ToolDefinition(
        uri="tool://builtin/calculator",
        name="calculator",
        description="算術計算を実行",
        source=ToolSource.BUILTIN,
        input_schema={"type": "object", "properties": {"expression": {"type": "string"}}},
    )

    # 名前にマッチ
    assert tool.matches("calculator") > 0.0
    # 説明にマッチ
    assert tool.matches("計算") > 0.0
    # マッチしない
    assert tool.matches("zzzzz") == 0.0


def test_tool_source_enum():
    """ToolSourceの列挙値テスト."""
    from agentflow.core.tool_definition import ToolSource

    assert ToolSource.BUILTIN.value == "builtin"
    assert ToolSource.MCP.value == "mcp"
    assert ToolSource.SKILL.value == "skill"
    assert ToolSource.DYNAMIC.value == "dynamic"
