# -*- coding: utf-8 -*-
"""ToolProvider のユニットテスト.

このモジュールは、agentflow/providers/tool_provider.py の機能をテストします。
"""

import pytest

from agentflow.providers.tool_provider import (
    RegisteredTool,
    ToolProvider,
    _tool_registry,
    tool,
)


@pytest.fixture(autouse=True)
def clear_registry():
    """各テスト前後にツールレジストリをクリア."""
    _tool_registry.clear()
    ToolProvider._default_instance = None
    yield
    _tool_registry.clear()
    ToolProvider._default_instance = None


class TestRegisteredTool:
    """RegisteredTool のテスト."""

    def test_create_registered_tool(self):
        """RegisteredTool の作成."""
        def sample_func(x: int) -> int:
            return x * 2

        tool_info = RegisteredTool(
            name="sample_tool",
            description="サンプルツール",
            func=sample_func,
            provider_type="method",
            parameters={"x": {"type": "integer"}},
        )

        assert tool_info.name == "sample_tool"
        assert tool_info.description == "サンプルツール"
        assert tool_info.func is sample_func
        assert tool_info.provider_type == "method"
        assert tool_info.parameters == {"x": {"type": "integer"}}

    def test_default_values(self):
        """デフォルト値の確認."""
        def dummy():
            pass

        tool_info = RegisteredTool(name="test", func=dummy)

        assert tool_info.description == ""
        assert tool_info.provider_type == "method"
        assert tool_info.parameters == {}


class TestToolDecorator:
    """@tool デコレータのテスト."""

    def test_basic_decoration(self):
        """基本的なデコレーション."""
        # 装飾器を適用してレジストリに登録（括弧付きで呼び出し）
        def my_tool(query: str) -> str:
            """検索ツール"""
            return f"Result: {query}"

        tool()(my_tool)

        assert "my_tool" in _tool_registry
        assert _tool_registry["my_tool"].name == "my_tool"
        assert _tool_registry["my_tool"].description == "検索ツール"

    def test_custom_name(self):
        """カスタム名の指定."""
        def search(q: str) -> str:
            """検索"""
            return q

        tool(name="custom_search")(search)

        assert "custom_search" in _tool_registry
        assert "search" not in _tool_registry

    def test_custom_description(self):
        """カスタム説明の指定."""
        def another_tool():
            """元の説明"""
            pass

        tool(description="カスタム説明文")(another_tool)

        assert _tool_registry["another_tool"].description == "カスタム説明文"

    def test_provider_type(self):
        """プロバイダータイプの指定."""
        def mcp_tool():
            """MCPツール"""
            pass

        tool(provider="mcp")(mcp_tool)

        assert _tool_registry["mcp_tool"].provider_type == "mcp"

    def test_parameter_extraction_int(self):
        """整数パラメータの抽出."""
        def int_tool(count: int) -> int:
            return count

        tool()(int_tool)

        params = _tool_registry["int_tool"].parameters
        assert params["count"]["type"] == "integer"

    def test_parameter_extraction_float(self):
        """浮動小数点パラメータの抽出."""
        def float_tool(value: float) -> float:
            return value

        tool()(float_tool)

        params = _tool_registry["float_tool"].parameters
        assert params["value"]["type"] == "number"

    def test_parameter_extraction_bool(self):
        """ブールパラメータの抽出."""
        def bool_tool(flag: bool) -> bool:
            return flag

        tool()(bool_tool)

        params = _tool_registry["bool_tool"].parameters
        assert params["flag"]["type"] == "boolean"

    def test_parameter_extraction_list(self):
        """リストパラメータの抽出."""
        def list_tool(items: list) -> list:
            return items

        tool()(list_tool)

        params = _tool_registry["list_tool"].parameters
        assert params["items"]["type"] == "array"

    def test_parameter_extraction_dict(self):
        """辞書パラメータの抽出."""
        def dict_tool(data: dict) -> dict:
            return data

        tool()(dict_tool)

        params = _tool_registry["dict_tool"].parameters
        assert params["data"]["type"] == "object"

    def test_parameter_default_value(self):
        """デフォルト値の抽出."""
        def default_tool(name: str = "default") -> str:
            return name

        tool()(default_tool)

        params = _tool_registry["default_tool"].parameters
        assert params["name"]["default"] == "default"

    def test_skip_self_parameter(self):
        """self パラメータのスキップ."""
        class MyClass:
            def method(self, x: int) -> int:
                return x

        tool()(MyClass.method)

        params = _tool_registry["method"].parameters
        assert "self" not in params
        assert "x" in params


class TestToolProvider:
    """ToolProvider のテスト."""

    def test_discover_returns_instance(self):
        """discover がインスタンスを返す."""
        def test_tool():
            """テストツール"""
            pass

        tool()(test_tool)

        provider = ToolProvider.discover()
        assert isinstance(provider, ToolProvider)

    def test_discover_singleton(self):
        """discover がシングルトンを返す."""
        def test_tool2():
            """テストツール2"""
            pass

        tool()(test_tool2)

        provider1 = ToolProvider.discover()
        provider2 = ToolProvider.discover()
        assert provider1 is provider2

    def test_list_tools(self):
        """list_tools がツール一覧を返す."""
        def tool_a():
            """ツールA"""
            pass

        def tool_b():
            """ツールB"""
            pass

        tool()(tool_a)
        tool()(tool_b)

        provider = ToolProvider.discover()
        tools = provider.list_tools()

        assert len(tools) == 2
        names = [t.name for t in tools]
        assert "tool_a" in names
        assert "tool_b" in names

    def test_get_tool_found(self):
        """get_tool が存在するツールを返す."""
        def existing_tool():
            """既存ツール"""
            pass

        tool()(existing_tool)

        provider = ToolProvider.discover()
        result = provider.get_tool("existing_tool")

        assert result is not None
        assert result.name == "existing_tool"

    def test_get_tool_not_found(self):
        """get_tool が存在しないツールで None を返す."""
        provider = ToolProvider.discover()
        result = provider.get_tool("nonexistent_tool")

        assert result is None

    @pytest.mark.asyncio
    async def test_call_sync_tool(self):
        """同期ツールの呼び出し."""
        def sync_tool(x: int, y: int) -> int:
            return x + y

        tool()(sync_tool)

        provider = ToolProvider.discover()
        result = await provider.call("sync_tool", x=3, y=5)

        assert result == 8

    @pytest.mark.asyncio
    async def test_call_async_tool(self):
        """非同期ツールの呼び出し."""
        async def async_tool(message: str) -> str:
            return f"Received: {message}"

        tool()(async_tool)

        provider = ToolProvider.discover()
        result = await provider.call("async_tool", message="Hello")

        assert result == "Received: Hello"

    @pytest.mark.asyncio
    async def test_call_nonexistent_tool(self):
        """存在しないツールの呼び出しでエラー."""
        provider = ToolProvider.discover()

        with pytest.raises(ValueError, match="Tool not found"):
            await provider.call("nonexistent_tool")

    def test_register_tool_manually(self):
        """手動でツールを登録."""
        def manual_func():
            return "manual"

        registered = RegisteredTool(
            name="manual_tool",
            description="手動登録ツール",
            func=manual_func,
        )
        ToolProvider.register(registered)

        provider = ToolProvider.discover()
        tool_info = provider.get_tool("manual_tool")

        assert tool_info is not None
        assert tool_info.description == "手動登録ツール"

    def test_to_openai_tools(self):
        """OpenAI Function Calling 形式への変換."""
        def search(query: str) -> str:
            """検索を実行"""
            return query

        tool()(search)

        provider = ToolProvider.discover()
        openai_tools = provider.to_openai_tools()

        assert len(openai_tools) == 1
        assert openai_tools[0]["type"] == "function"
        assert openai_tools[0]["function"]["name"] == "search"
        assert openai_tools[0]["function"]["description"] == "検索を実行"
        assert "properties" in openai_tools[0]["function"]["parameters"]

    def test_to_mcp_tools(self):
        """MCP Tools 形式への変換."""
        def mcp_search(query: str) -> str:
            """MCP検索"""
            return query

        tool()(mcp_search)

        provider = ToolProvider.discover()
        mcp_tools = provider.to_mcp_tools()

        assert len(mcp_tools) == 1
        assert mcp_tools[0]["name"] == "mcp_search"
        assert mcp_tools[0]["description"] == "MCP検索"
        assert "inputSchema" in mcp_tools[0]
        assert mcp_tools[0]["inputSchema"]["type"] == "object"


class TestToolCache:
    """ツールキャッシュ機能のテスト."""

    @pytest.mark.asyncio
    async def test_cached_tool(self):
        """キャッシュ有効なツールの動作."""
        call_count = 0

        @tool(cache=True, ttl=3600)
        async def cached_tool(x: int) -> int:
            nonlocal call_count
            call_count += 1
            return x * 2

        # 最初の呼び出し
        result1 = await cached_tool(5)
        assert result1 == 10
        assert call_count == 1

        # 同じ引数での2回目の呼び出し（キャッシュから）
        result2 = await cached_tool(5)
        assert result2 == 10
        assert call_count == 1  # キャッシュヒットで増加しない

        # 異なる引数での呼び出し
        result3 = await cached_tool(10)
        assert result3 == 20
        assert call_count == 2

