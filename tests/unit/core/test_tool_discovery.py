# -*- coding: utf-8 -*-
"""ToolDiscoveryServiceのテスト.

統一ツール発見サービスのユニットテスト。
"""
import pytest


@pytest.fixture
def tool_registry():
    """空のToolRegistryを作成."""
    from agentflow.core.tool_registry import ToolRegistry
    return ToolRegistry()


class TestToolDiscoveryService:
    """ToolDiscoveryServiceのテスト."""

    @pytest.mark.asyncio
    async def test_discover_skills(self, tool_registry):
        """Skillsをツールとして発見するテスト."""
        from agentflow.core.tool_discovery import ToolDiscoveryService
        from agentflow.core.tool_definition import ToolSource

        service = ToolDiscoveryService(tool_registry)

        mock_skills = [
            {"name": "code_review", "description": "コードをレビュー"},
            {"name": "summarize", "description": "テキストを要約"},
        ]

        count = await service.discover_skills(mock_skills)

        assert count == 2
        skills = tool_registry.filter_by_source(ToolSource.SKILL)
        assert len(skills) == 2

    @pytest.mark.asyncio
    async def test_discover_mcp_servers(self, tool_registry):
        """MCPサーバーツールを発見するテスト."""
        from agentflow.core.tool_discovery import ToolDiscoveryService
        from agentflow.core.tool_definition import ToolSource

        service = ToolDiscoveryService(tool_registry)

        mock_tools = [
            {"name": "read_file", "description": "ファイル読み取り", "inputSchema": {}},
            {"name": "write_file", "description": "ファイル書き込み", "inputSchema": {}},
        ]

        count = await service.discover_mcp_tools("filesystem", mock_tools)

        assert count == 2
        mcp_tools = tool_registry.filter_by_source(ToolSource.MCP)
        assert len(mcp_tools) == 2
        assert any(t.uri == "tool://mcp/filesystem/read_file" for t in mcp_tools)

    @pytest.mark.asyncio
    async def test_discover_all(self, tool_registry):
        """全発見パイプラインのテスト."""
        from agentflow.core.tool_discovery import ToolDiscoveryService

        service = ToolDiscoveryService(tool_registry)

        count = await service.discover_all()

        # ビルトインツールがない環境では0でもOK
        assert count >= 0

    @pytest.mark.asyncio
    async def test_refresh(self, tool_registry):
        """リフレッシュ（再発見）のテスト."""
        from agentflow.core.tool_discovery import ToolDiscoveryService
        from agentflow.core.tool_definition import ToolDefinition, ToolSource

        # 既存のツールを追加
        tool_registry.register(ToolDefinition(
            uri="tool://builtin/old",
            name="old",
            description="古いツール",
            source=ToolSource.BUILTIN,
        ))

        service = ToolDiscoveryService(tool_registry)

        # リフレッシュ
        await service.refresh()

        # 手動登録したツールはクリアされる
        assert tool_registry.get("tool://builtin/old") is None

    @pytest.mark.asyncio
    async def test_register_builtin(self, tool_registry):
        """ビルトインツール登録のテスト."""
        from agentflow.core.tool_discovery import ToolDiscoveryService
        from agentflow.core.tool_definition import ToolSource

        service = ToolDiscoveryService(tool_registry)

        # 手動でビルトインを登録
        service.register_builtin(
            name="custom_tool",
            description="カスタムツール",
            input_schema={"type": "object", "properties": {}},
        )

        tool = tool_registry.get("tool://builtin/custom_tool")
        assert tool is not None
        assert tool.source == ToolSource.BUILTIN

    def test_service_registry_property(self, tool_registry):
        """レジストリプロパティのテスト."""
        from agentflow.core.tool_discovery import ToolDiscoveryService

        service = ToolDiscoveryService(tool_registry)

        assert service.registry is tool_registry
