# -*- coding: utf-8 -*-
"""UnifiedToolProvider 単体テスト."""

import pytest

from agentflow.providers.unified_tool import (
    BuiltinToolProvider,
    ToolDefinition,
    ToolResult,
    ToolStatus,
    ToolType,
    UnifiedToolProvider,
)


class TestToolResult:
    """ToolResult テストクラス."""

    def test_create_success_result(self) -> None:
        """成功結果を作成できること."""
        result = ToolResult(
            success=True,
            status=ToolStatus.SUCCESS,
            tool_uri="builtin://calculator",
            tool_type=ToolType.BUILTIN,
            output={"result": 42},
        )

        assert result.success is True
        assert result.status == ToolStatus.SUCCESS
        assert result.output == {"result": 42}

    def test_create_failure_result(self) -> None:
        """失敗結果を作成できること."""
        result = ToolResult(
            success=False,
            status=ToolStatus.FAILED,
            tool_uri="mcp://server/tool",
            tool_type=ToolType.MCP,
            error="接続エラー",
        )

        assert result.success is False
        assert result.error == "接続エラー"

    def test_to_dict(self) -> None:
        """辞書変換が正しいこと."""
        result = ToolResult(
            success=True,
            status=ToolStatus.SUCCESS,
            tool_uri="builtin://test",
            tool_type=ToolType.BUILTIN,
            output={"value": 1},
            duration_ms=100.5,
        )

        data = result.to_dict()
        assert data["success"] is True
        assert data["status"] == "success"
        assert data["tool_uri"] == "builtin://test"
        assert data["duration_ms"] == 100.5


class TestToolDefinition:
    """ToolDefinition テストクラス."""

    def test_create_definition(self) -> None:
        """ツール定義を作成できること."""
        definition = ToolDefinition(
            uri="builtin://calculator",
            name="calculator",
            description="計算を行う",
            tool_type=ToolType.BUILTIN,
            parameters={"expression": {"type": "string"}},
        )

        assert definition.uri == "builtin://calculator"
        assert definition.name == "calculator"
        assert definition.enabled is True


class TestBuiltinToolProvider:
    """BuiltinToolProvider テストクラス."""

    @pytest.fixture
    def provider(self) -> BuiltinToolProvider:
        """プロバイダーを作成."""
        return BuiltinToolProvider()

    def test_list_default_tools(self, provider: BuiltinToolProvider) -> None:
        """デフォルトツールが登録されていること."""
        tools = provider.list_tools()
        tool_names = [t.name for t in tools]

        assert "calculator" in tool_names
        assert "file_reader" in tool_names
        assert "json_parser" in tool_names
        assert "datetime" in tool_names

    @pytest.mark.asyncio
    async def test_call_calculator(self, provider: BuiltinToolProvider) -> None:
        """計算ツールが動作すること."""
        result = await provider.call("calculator", {"expression": "2 + 3 * 4"})

        assert result.success is True
        assert result.output["result"] == 14

    @pytest.mark.asyncio
    async def test_call_datetime(self, provider: BuiltinToolProvider) -> None:
        """日時ツールが動作すること."""
        result = await provider.call("datetime", {"format": "%Y-%m-%d"})

        assert result.success is True
        assert "formatted" in result.output
        assert "iso" in result.output

    @pytest.mark.asyncio
    async def test_call_json_parser(self, provider: BuiltinToolProvider) -> None:
        """JSON解析ツールが動作すること."""
        result = await provider.call(
            "json_parser",
            {"json_string": '{"name": "test", "value": 123}'},
        )

        assert result.success is True
        assert result.output["name"] == "test"
        assert result.output["value"] == 123

    @pytest.mark.asyncio
    async def test_call_unknown_tool(self, provider: BuiltinToolProvider) -> None:
        """不明なツールでエラーになること."""
        result = await provider.call("unknown_tool", {})

        assert result.success is False
        assert result.status == ToolStatus.NOT_FOUND

    def test_register_custom_tool(self, provider: BuiltinToolProvider) -> None:
        """カスタムツールを登録できること."""
        definition = ToolDefinition(
            uri="builtin://custom",
            name="custom",
            description="カスタムツール",
            tool_type=ToolType.BUILTIN,
        )

        async def handler(params: dict) -> dict:
            return {"custom": True}

        provider.register_tool(definition, handler)

        assert provider.get_tool("custom") is not None


class TestUnifiedToolProvider:
    """UnifiedToolProvider テストクラス."""

    @pytest.fixture
    def provider(self) -> UnifiedToolProvider:
        """プロバイダーを作成."""
        return UnifiedToolProvider()

    def test_parse_builtin_uri(self, provider: UnifiedToolProvider) -> None:
        """builtin URI を正しくパースできること."""
        tool_type, name = provider._parse_uri("builtin://calculator")
        assert tool_type == ToolType.BUILTIN
        assert name == "calculator"

    def test_parse_skill_uri(self, provider: UnifiedToolProvider) -> None:
        """skill URI を正しくパースできること."""
        tool_type, name = provider._parse_uri("skill://web_scraper")
        assert tool_type == ToolType.SKILL
        assert name == "web_scraper"

    def test_parse_mcp_uri(self, provider: UnifiedToolProvider) -> None:
        """MCP URI を正しくパースできること."""
        tool_type, name = provider._parse_uri("mcp://browser/scrape")
        assert tool_type == ToolType.MCP
        assert name == "mcp://browser/scrape"

    def test_parse_plain_name(self, provider: UnifiedToolProvider) -> None:
        """プレーン名を builtin として扱うこと."""
        tool_type, name = provider._parse_uri("calculator")
        assert tool_type == ToolType.BUILTIN
        assert name == "calculator"

    @pytest.mark.asyncio
    async def test_call_builtin_via_unified(self, provider: UnifiedToolProvider) -> None:
        """統一プロバイダー経由で内蔵ツールを呼べること."""
        result = await provider.call("builtin://calculator", {"expression": "10 / 2"})

        assert result.success is True
        assert result.output["result"] == 5.0

    def test_get_stats(self, provider: UnifiedToolProvider) -> None:
        """統計情報を取得できること."""
        stats = provider.get_stats()

        assert "initialized" in stats
        assert "builtin_tools" in stats
        assert stats["builtin_tools"] >= 4  # デフォルトツール数

    def test_get_tools_for_llm(self, provider: UnifiedToolProvider) -> None:
        """LLM用ツール定義を取得できること."""
        tools = provider.get_tools_for_llm()

        assert isinstance(tools, list)
        assert len(tools) > 0
        assert tools[0]["type"] == "function"
        assert "function" in tools[0]
