"""MCP クライアントのユニットテスト."""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.protocols.mcp_client import MCPClient
from agentflow.protocols.mcp_config import MCPConfig, MCPServerConfig


@pytest.fixture
def mock_session() -> MagicMock:
    """モック ClientSession を作成.

    Returns:
        モック ClientSession
    """
    session = MagicMock()
    session.initialize = AsyncMock()
    session.list_tools = AsyncMock()
    session.call_tool = AsyncMock()
    return session


@pytest.fixture
def sample_config() -> MCPConfig:
    """サンプル MCP 設定を作成.

    Returns:
        MCPConfig インスタンス
    """
    return MCPConfig(
        servers=[
            MCPServerConfig(
                name="test-server",
                command="python",
                args=["-m", "test_server"],
                env={"TEST": "true"},
                enabled=True,
            ),
            MCPServerConfig(
                name="disabled-server",
                command="python",
                args=["-m", "disabled_server"],
                enabled=False,
            ),
        ]
    )


class TestMCPClient:
    """MCP クライアントのテストスイート."""

    def test_client_initialization(self, sample_config: MCPConfig) -> None:
        """クライアントの初期化をテスト."""
        client = MCPClient(sample_config)
        assert len(client._config.servers) == 2

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_connect_success(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """サーバー接続の成功をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        # ツールリストのモック
        mock_tool = MagicMock()
        mock_tool.name = "test_tool"
        mock_tool.description = "A test tool"
        mock_tool.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        # クライアントを作成して接続
        client = MCPClient(sample_config)
        await client.connect()

        # 検証
        assert len(client._sessions) == 1
        assert "test-server" in client._sessions
        assert len(client._tools) == 1
        assert "mcp://test-server/test_tool" in client._tools

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_connect_with_disabled_server(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """無効なサーバーがスキップされることをテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tools_result = MagicMock()
        mock_tools_result.tools = []
        mock_session.list_tools.return_value = mock_tools_result

        # クライアントを作成して接続
        client = MCPClient(sample_config)
        await client.connect()

        # 無効なサーバーは接続されない
        assert "disabled-server" not in client._sessions

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_get_tool_definitions(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """ツール定義の取得をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "test_tool"
        mock_tool.description = "A test tool"
        mock_tool.inputSchema = {"type": "object", "properties": {}}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        # クライアントを作成して接続
        client = MCPClient(sample_config)
        await client.connect()

        # ツール定義を取得
        definitions = client.get_tool_definitions()

        # 検証
        assert len(definitions) == 1
        assert definitions[0]["type"] == "function"
        assert definitions[0]["function"]["name"] == "mcp://test-server/test_tool"
        assert definitions[0]["function"]["description"] == "A test tool"

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_call_tool_success(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """ツール呼び出しの成功をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "test_tool"
        mock_tool.description = "A test tool"
        mock_tool.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        # ツール呼び出し結果のモック
        mock_result = MagicMock()
        mock_result.content = [{"type": "text", "text": "Success"}]
        mock_session.call_tool.return_value = mock_result

        # クライアントを作成して接続
        client = MCPClient(sample_config)
        await client.connect()

        # ツールを呼び出す
        result = await client.call_tool(
            "mcp://test-server/test_tool", {"arg": "value"}
        )

        # 検証
        assert result["success"] is True
        assert result["tool"] == "test_tool"
        assert result["server"] == "test-server"

    async def test_call_tool_invalid_uri(self, sample_config: MCPConfig) -> None:
        """無効な URI でのツール呼び出しをテスト."""
        client = MCPClient(sample_config)

        with pytest.raises(ValueError, match="Invalid tool URI"):
            await client.call_tool("invalid://uri", {})

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_call_tool_not_found(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """存在しないツールの呼び出しをテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tools_result = MagicMock()
        mock_tools_result.tools = []
        mock_session.list_tools.return_value = mock_tools_result

        # クライアントを作成して接続
        client = MCPClient(sample_config)
        await client.connect()

        # 存在しないツールを呼び出す
        with pytest.raises(ValueError, match="Tool not found"):
            await client.call_tool("mcp://test-server/nonexistent", {})

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_list_tools(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """ツールリストの取得をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "test_tool"
        mock_tool.description = "A test tool"
        mock_tool.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        # クライアントを作成して接続
        client = MCPClient(sample_config)
        await client.connect()

        # ツールリストを取得
        tools = client.list_tools()

        # 検証
        assert len(tools) == 1
        assert "mcp://test-server/test_tool" in tools

    async def test_disconnect(self, sample_config: MCPConfig) -> None:
        """切断処理をテスト."""
        client = MCPClient(sample_config)
        await client.disconnect()

        # セッションとツールがクリアされることを確認
        assert len(client._sessions) == 0
        assert len(client._tools) == 0

