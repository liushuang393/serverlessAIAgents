"""ToolRegistryインターフェースのテスト.

統一ツールレジストリのユニットテスト。
"""

import pytest


@pytest.fixture
def tool_registry():
    """新しいToolRegistryインスタンスを作成."""
    from agentflow.core.tool_registry import ToolRegistry

    return ToolRegistry()


@pytest.fixture
def sample_tool():
    """サンプルToolDefinitionを作成."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource

    return ToolDefinition(
        uri="tool://builtin/test_tool",
        name="test_tool",
        description="テスト用ツール",
        source=ToolSource.BUILTIN,
        input_schema={"type": "object", "properties": {"input": {"type": "string"}}},
    )


class TestToolRegistry:
    """ToolRegistryのテスト."""

    def test_register_and_get(self, tool_registry, sample_tool):
        """ツールの登録と取得のテスト."""
        tool_registry.register(sample_tool)

        retrieved = tool_registry.get("tool://builtin/test_tool")

        assert retrieved is not None
        assert retrieved.name == "test_tool"

    def test_get_nonexistent_returns_none(self, tool_registry):
        """存在しないツール取得でNoneを返すテスト."""
        result = tool_registry.get("tool://nonexistent/tool")
        assert result is None

    def test_list_all_tools(self, tool_registry, sample_tool):
        """全ツールリストのテスト."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        tool2 = ToolDefinition(
            uri="tool://mcp/server/tool2",
            name="tool2",
            description="別のツール",
            source=ToolSource.MCP,
        )

        tool_registry.register(sample_tool)
        tool_registry.register(tool2)

        all_tools = tool_registry.list_all()

        assert len(all_tools) == 2

    def test_search_by_query(self, tool_registry, sample_tool):
        """クエリ検索のテスト."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        calc_tool = ToolDefinition(
            uri="tool://builtin/calculator",
            name="calculator",
            description="算術計算を実行",
            source=ToolSource.BUILTIN,
        )

        tool_registry.register(sample_tool)
        tool_registry.register(calc_tool)

        results = tool_registry.search("calculator")

        assert len(results) > 0
        assert results[0].name == "calculator"

    def test_filter_by_source(self, tool_registry):
        """ソースでフィルタリングのテスト."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        builtin = ToolDefinition(
            uri="tool://builtin/b1",
            name="b1",
            description="ビルトイン",
            source=ToolSource.BUILTIN,
        )
        mcp = ToolDefinition(
            uri="tool://mcp/server/m1",
            name="m1",
            description="MCP",
            source=ToolSource.MCP,
        )

        tool_registry.register(builtin)
        tool_registry.register(mcp)

        builtin_tools = tool_registry.filter_by_source(ToolSource.BUILTIN)
        mcp_tools = tool_registry.filter_by_source(ToolSource.MCP)

        assert len(builtin_tools) == 1
        assert len(mcp_tools) == 1
        assert builtin_tools[0].name == "b1"

    def test_unregister(self, tool_registry, sample_tool):
        """ツール登録解除のテスト."""
        tool_registry.register(sample_tool)
        assert tool_registry.get(sample_tool.uri) is not None

        result = tool_registry.unregister(sample_tool.uri)

        assert result is True
        assert tool_registry.get(sample_tool.uri) is None

    def test_unregister_nonexistent_returns_false(self, tool_registry):
        """存在しないツールの登録解除でFalseを返すテスト."""
        result = tool_registry.unregister("tool://nonexistent/tool")
        assert result is False

    def test_register_duplicate_replaces(self, tool_registry, sample_tool):
        """重複URI登録で置換されるテスト."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        tool_registry.register(sample_tool)

        updated = ToolDefinition(
            uri=sample_tool.uri,
            name="updated_name",
            description="更新された説明",
            source=ToolSource.BUILTIN,
        )
        tool_registry.register(updated)

        retrieved = tool_registry.get(sample_tool.uri)
        assert retrieved.name == "updated_name"
        assert len(tool_registry.list_all()) == 1

    def test_contains(self, tool_registry, sample_tool):
        """containsチェックのテスト."""
        assert sample_tool.uri not in tool_registry

        tool_registry.register(sample_tool)

        assert sample_tool.uri in tool_registry

    def test_len(self, tool_registry, sample_tool):
        """長さ取得のテスト."""
        assert len(tool_registry) == 0

        tool_registry.register(sample_tool)

        assert len(tool_registry) == 1

    def test_search_empty_query_returns_all(self, tool_registry, sample_tool):
        """空クエリで全ツールを返すテスト."""
        tool_registry.register(sample_tool)

        results = tool_registry.search("")

        assert len(results) == 1

    def test_search_with_limit(self, tool_registry):
        """検索結果数制限のテスト."""
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        for i in range(10):
            tool = ToolDefinition(
                uri=f"tool://builtin/tool{i}",
                name=f"tool{i}",
                description="テスト",
                source=ToolSource.BUILTIN,
            )
            tool_registry.register(tool)

        results = tool_registry.search("tool", limit=5)

        assert len(results) == 5
