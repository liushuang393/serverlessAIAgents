"""MCP クライアントのユニットテスト."""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest

import kernel.protocols.mcp_client as _mcp_mod
from kernel.protocols.mcp_client import MCPClient
from kernel.protocols.mcp_config import MCPConfig, MCPServerConfig

# テスト環境で mcp パッケージの遅延インポートを事前に完了させる。
# これにより @patch でモジュール変数を差し替えた後、
# _ensure_mcp_imports() が本物のクラスで上書きすることを防止する。
_mcp_mod._ensure_mcp_imports()


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

        # クライアントを作成して接続（セキュリティ機能を無効化）
        client = MCPClient(sample_config, enable_security=False)
        await client.connect()

        # ツールを呼び出す
        result = await client.call_tool("mcp://test-server/test_tool", {"arg": "value"})

        # 検証
        assert result["success"] is True
        assert result["tool"] == "test_tool"
        assert result["server"] == "test-server"

    async def test_call_tool_invalid_uri(self, sample_config: MCPConfig) -> None:
        """無効な URI でのツール呼び出しをテスト."""
        client = MCPClient(sample_config, enable_security=False)

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

        # クライアントを作成して接続（セキュリティ機能を無効化）
        client = MCPClient(sample_config, enable_security=False)
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

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_disconnect_with_active_sessions(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """アクティブなセッションがある状態での切断をテスト."""
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

        # セッションが存在することを確認
        assert len(client._sessions) == 1

        # 切断
        await client.disconnect()

        # セッションとツールがクリアされることを確認
        assert len(client._sessions) == 0
        assert len(client._tools) == 0
        assert len(client._contexts) == 0
        mock_context.__aexit__.assert_called_once()

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_disconnect_with_error(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """切断時のエラーを処理することをテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock(side_effect=Exception("Disconnect error"))
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tools_result = MagicMock()
        mock_tools_result.tools = []
        mock_session.list_tools.return_value = mock_tools_result

        # クライアントを作成して接続
        client = MCPClient(sample_config)
        await client.connect()

        # 切断時のエラーは致命的ではない
        await client.disconnect()

        # セッションとツールがクリアされることを確認
        assert len(client._sessions) == 0
        assert len(client._tools) == 0

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_call_tool_with_exception(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """ツール呼び出し時の例外処理をテスト."""
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

        # ツール呼び出しで例外を発生させる
        mock_session.call_tool.side_effect = Exception("Tool execution failed")

        # クライアントを作成して接続（セキュリティ機能を無効化）
        client = MCPClient(sample_config, enable_security=False)
        await client.connect()

        # ツールを呼び出す
        result = await client.call_tool("mcp://test-server/test_tool", {"arg": "value"})

        # エラーレスポンスを確認（リトライ機能により詳細なエラーメッセージが含まれる）
        assert result["success"] is False
        assert "Tool call failed" in result["error"]
        assert "Tool execution failed" in result["error"]
        assert result["tool"] == "test_tool"
        assert result["server"] == "test-server"

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_call_tool_server_not_connected(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """サーバーが接続されていない状態でのツール呼び出しをテスト."""
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

        # クライアントを作成して接続（セキュリティ機能を無効化）
        client = MCPClient(sample_config, enable_security=False)
        await client.connect()

        # セッションを削除してサーバーが接続されていない状態をシミュレート
        client._sessions.clear()

        # ツールを呼び出す
        with pytest.raises(RuntimeError, match="Server not connected"):
            await client.call_tool("mcp://test-server/test_tool", {"arg": "value"})

    def test_get_tool_info_found(self, sample_config: MCPConfig) -> None:
        """ツール情報の取得をテスト."""
        client = MCPClient(sample_config)
        tool_info = {
            "name": "test_tool",
            "description": "A test tool",
            "input_schema": {"type": "object"},
            "server": "test-server",
        }
        client._tools["mcp://test-server/test_tool"] = tool_info

        info = client.get_tool_info("mcp://test-server/test_tool")
        assert info == tool_info

    def test_get_tool_info_not_found(self, sample_config: MCPConfig) -> None:
        """存在しないツールの情報取得をテスト."""
        client = MCPClient(sample_config)
        info = client.get_tool_info("mcp://test-server/nonexistent")
        assert info is None

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_context_manager(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """コンテキストマネージャーとしての使用をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tools_result = MagicMock()
        mock_tools_result.tools = []
        mock_session.list_tools.return_value = mock_tools_result

        # コンテキストマネージャーとして使用
        async with MCPClient(sample_config) as client:
            assert len(client._sessions) == 1

        # 終了後はセッションがクリアされる
        assert len(client._sessions) == 0

    @patch("agentflow.protocols.mcp_client.stdio_client")
    async def test_connect_server_failure(
        self,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
    ) -> None:
        """サーバー接続失敗時の処理をテスト."""
        # 接続失敗をシミュレート
        mock_stdio_client.side_effect = Exception("Connection failed")

        client = MCPClient(sample_config)
        # 接続失敗は致命的ではない
        await client.connect()

        # セッションは作成されない
        assert len(client._sessions) == 0

    def test_init_with_custom_logger(self, sample_config: MCPConfig) -> None:
        """カスタムロガーでの初期化をテスト."""
        import logging

        custom_logger = logging.getLogger("custom")
        client = MCPClient(sample_config, logger=custom_logger)
        assert client._logger == custom_logger

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_get_tool_definitions_with_none_description(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """description が None のツール定義をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "test_tool"
        mock_tool.description = None  # description が None
        mock_tool.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        # クライアントを作成して接続
        client = MCPClient(sample_config)
        await client.connect()

        # ツール定義を取得
        definitions = client.get_tool_definitions()

        # description が空文字列になることを確認
        assert definitions[0]["function"]["description"] == ""



class TestMCPLazyImport:
    """MCP遅延インポートのテスト."""

    def test_mcp_client_module_loads_without_mcp(self):
        """mcp_client モジュール自体が mcp 無しでもロード可能であることを検証.

        mcp パッケージのインポートは _connect_server 内でのみ行われるため、
        モジュールのインポート時点では mcp は不要。
        """
        import importlib
        import sys

        # mcp_client モジュールを再ロードしてインポートエラーが出ないことを確認
        # (mcp パッケージ自体はテスト環境にあるため、実際の ImportError は起きないが
        #  トップレベルに mcp のインポートがないことを間接的に検証)
        mod = importlib.import_module("agentflow.protocols.mcp_client")
        assert hasattr(mod, "MCPClient")

    def test_protocols_init_lazy_imports(self):
        """protocols __init__ から MCPClient が遅延インポートで取得できることを検証."""
        from kernel import protocols

        # __getattr__ 経由で取得
        client_cls = getattr(protocols, "MCPClient")
        assert client_cls.__name__ == "MCPClient"

    def test_protocols_init_lazy_imports_lazy_mcp_client(self):
        """LazyMCPClient が遅延インポートで取得できることを検証."""
        from kernel import protocols

        lazy_cls = getattr(protocols, "LazyMCPClient")
        assert lazy_cls.__name__ == "LazyMCPClient"

    def test_protocols_init_lazy_imports_tool_index_entry(self):
        """ToolIndexEntry/ToolSearchResult が遅延インポートで取得できることを検証."""
        from kernel import protocols

        entry_cls = getattr(protocols, "ToolIndexEntry")
        result_cls = getattr(protocols, "ToolSearchResult")
        assert entry_cls.__name__ == "ToolIndexEntry"
        assert result_cls.__name__ == "ToolSearchResult"

    def test_protocols_init_unknown_attr_raises(self):
        """存在しない属性はAttributeErrorを送出することを検証."""
        from kernel import protocols

        with pytest.raises(AttributeError, match="has no attribute"):
            getattr(protocols, "NonExistentClass")