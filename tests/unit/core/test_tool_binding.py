"""ToolBinderインターフェースのテスト.

ランタイムツールバインディングのユニットテスト。
"""

from unittest.mock import MagicMock

import pytest


@pytest.fixture
def tool_registry():
    """ツールが登録されたToolRegistryを作成."""
    from agentflow.core.tool_definition import ToolDefinition, ToolSource
    from agentflow.core.tool_registry import ToolRegistry

    registry = ToolRegistry()
    registry.register(
        ToolDefinition(
            uri="tool://builtin/calculator",
            name="calculator",
            description="計算を実行",
            source=ToolSource.BUILTIN,
        )
    )
    registry.register(
        ToolDefinition(
            uri="tool://mcp/fs/read",
            name="read",
            description="ファイルを読み取る",
            source=ToolSource.MCP,
        )
    )
    return registry


@pytest.fixture
def tool_binder(tool_registry):
    """ToolBinderを作成."""
    from agentflow.core.tool_binding import ToolBinder

    return ToolBinder(tool_registry)


class TestToolBinder:
    """ToolBinderのテスト."""

    @pytest.mark.asyncio
    async def test_bind_tools_by_uri(self, tool_binder):
        """URIでツールをバインドするテスト."""
        mock_agent = MagicMock()
        mock_agent._tools = None

        uris = ["tool://builtin/calculator"]
        bound = await tool_binder.bind(mock_agent, tool_uris=uris)

        assert bound._tools is not None
        assert len(bound._tools) == 1

    @pytest.mark.asyncio
    async def test_bind_tools_by_capability(self, tool_binder, tool_registry):
        """能力でツールをバインドするテスト."""
        from agentflow.core.capability_spec import AgentCapabilitySpec

        mock_agent = MagicMock()
        mock_agent._tools = None

        cap = AgentCapabilitySpec(
            id="test",
            name="Test",
            description="Test",
            required_tools=["tool://builtin/calculator", "tool://mcp/fs/read"],
        )

        bound = await tool_binder.bind_for_capability(mock_agent, cap)

        assert len(bound._tools) == 2

    @pytest.mark.asyncio
    async def test_bind_validates_tool_exists(self, tool_binder):
        """存在しないツールはスキップするテスト."""
        mock_agent = MagicMock()
        mock_agent._tools = None

        uris = ["tool://nonexistent/tool"]
        bound = await tool_binder.bind(mock_agent, tool_uris=uris)

        assert len(bound._tools) == 0

    @pytest.mark.asyncio
    async def test_bound_tools_has_executor(self, tool_binder):
        """バインドされたツールにエグゼキュータがあるテスト."""
        mock_agent = MagicMock()
        mock_agent._tools = None

        uris = ["tool://builtin/calculator"]
        bound = await tool_binder.bind(mock_agent, tool_uris=uris)

        assert hasattr(bound, "_tool_executor")

    @pytest.mark.asyncio
    async def test_bind_empty_list(self, tool_binder):
        """空リストでバインドするテスト."""
        mock_agent = MagicMock()
        mock_agent._tools = None

        bound = await tool_binder.bind(mock_agent, tool_uris=[])

        assert len(bound._tools) == 0


class TestBoundTools:
    """BoundToolsのテスト."""

    def test_bound_tools_len(self):
        """BoundToolsの長さテスト."""
        from agentflow.core.tool_binding import BoundTools
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        tools = [
            ToolDefinition(
                uri="tool://builtin/t1",
                name="t1",
                description="",
                source=ToolSource.BUILTIN,
            ),
            ToolDefinition(
                uri="tool://builtin/t2",
                name="t2",
                description="",
                source=ToolSource.BUILTIN,
            ),
        ]

        bound = BoundTools(tools)

        assert len(bound) == 2

    def test_bound_tools_get(self):
        """BoundToolsの取得テスト."""
        from agentflow.core.tool_binding import BoundTools
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        tool = ToolDefinition(
            uri="tool://builtin/t1",
            name="t1",
            description="",
            source=ToolSource.BUILTIN,
        )

        bound = BoundTools([tool])

        assert bound.get("tool://builtin/t1") is not None
        assert bound.get("tool://nonexistent") is None

    def test_bound_tools_to_mcp_format(self):
        """BoundToolsのMCP形式変換テスト."""
        from agentflow.core.tool_binding import BoundTools
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        tool = ToolDefinition(
            uri="tool://builtin/calculator",
            name="calculator",
            description="計算を実行",
            source=ToolSource.BUILTIN,
            input_schema={"type": "object", "properties": {"expr": {"type": "string"}}},
        )

        bound = BoundTools([tool])
        mcp_format = bound.to_mcp_format()

        assert len(mcp_format) == 1
        assert mcp_format[0]["name"] == "calculator"
        assert "inputSchema" in mcp_format[0]
