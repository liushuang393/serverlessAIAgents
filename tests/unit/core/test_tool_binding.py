"""ToolBinderインターフェースのテスト.

ランタイムツールバインディングのユニットテスト。
"""

from __future__ import annotations

from typing import Any

import pytest

from kernel.tools.tool_binding import BoundTools, ToolExecutor


class _StubAgent:
    """ToolBinder テスト用スタブ Agent.

    set_bound_tools() を実装し、カプセル化されたAPI経由で
    ツールバインドが正しく行われることを検証する。
    """

    def __init__(self) -> None:
        self._bound_tools: BoundTools | None = None
        self._tool_executor: ToolExecutor | None = None

    def set_bound_tools(
        self,
        bound_tools: BoundTools,
        tool_executor: ToolExecutor,
    ) -> None:
        """パブリックAPIでツールを設定."""
        self._bound_tools = bound_tools
        self._tool_executor = tool_executor


@pytest.fixture
def tool_registry():
    """ツールが登録されたToolRegistryを作成."""
    from kernel.tools.tool_definition import ToolDefinition, ToolSource
    from kernel.tools.tool_registry import ToolRegistry

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
    from kernel.tools.tool_binding import ToolBinder

    return ToolBinder(tool_registry)


class TestToolBinder:
    """ToolBinderのテスト."""

    @pytest.mark.asyncio
    async def test_bind_tools_by_uri(self, tool_binder):
        """URIでツールをバインドするテスト."""
        agent = _StubAgent()

        uris = ["tool://builtin/calculator"]
        bound = await tool_binder.bind(agent, tool_uris=uris)

        assert bound._bound_tools is not None
        assert len(bound._bound_tools) == 1

    @pytest.mark.asyncio
    async def test_bind_tools_by_capability(self, tool_binder, tool_registry):
        """能力でツールをバインドするテスト."""
        from kernel.agents.capability_spec import AgentCapabilitySpec

        agent = _StubAgent()

        cap = AgentCapabilitySpec(
            id="test",
            name="Test",
            description="Test",
            required_tools=["tool://builtin/calculator", "tool://mcp/fs/read"],
        )

        bound = await tool_binder.bind_for_capability(agent, cap)

        assert len(bound._bound_tools) == 2

    @pytest.mark.asyncio
    async def test_bind_validates_tool_exists(self, tool_binder):
        """存在しないツールはスキップするテスト."""
        agent = _StubAgent()

        uris = ["tool://nonexistent/tool"]
        bound = await tool_binder.bind(agent, tool_uris=uris)

        assert len(bound._bound_tools) == 0

    @pytest.mark.asyncio
    async def test_bound_tools_has_executor(self, tool_binder):
        """バインドされたツールにエグゼキュータがあるテスト."""
        agent = _StubAgent()

        uris = ["tool://builtin/calculator"]
        bound = await tool_binder.bind(agent, tool_uris=uris)

        assert bound._tool_executor is not None
        assert isinstance(bound._tool_executor, ToolExecutor)

    @pytest.mark.asyncio
    async def test_bind_empty_list(self, tool_binder):
        """空リストでバインドするテスト."""
        agent = _StubAgent()

        bound = await tool_binder.bind(agent, tool_uris=[])

        assert len(bound._bound_tools) == 0

    @pytest.mark.asyncio
    async def test_bind_uses_set_bound_tools_api(self, tool_binder):
        """set_bound_tools() パブリックAPIが使用されることを検証."""
        agent = _StubAgent()

        uris = ["tool://builtin/calculator"]
        await tool_binder.bind(agent, tool_uris=uris)

        # set_bound_tools() 経由で設定されたことを確認
        assert agent._bound_tools is not None
        assert agent._tool_executor is not None


class TestBoundTools:
    """BoundToolsのテスト."""

    def test_bound_tools_len(self):
        """BoundToolsの長さテスト."""
        from kernel.tools.tool_binding import BoundTools
        from kernel.tools.tool_definition import ToolDefinition, ToolSource

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
        from kernel.tools.tool_binding import BoundTools
        from kernel.tools.tool_definition import ToolDefinition, ToolSource

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
        from kernel.tools.tool_binding import BoundTools
        from kernel.tools.tool_definition import ToolDefinition, ToolSource

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
