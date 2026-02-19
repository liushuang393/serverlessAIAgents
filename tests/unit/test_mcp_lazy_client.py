"""MCP 懒加載クライアントのユニットテスト.

LazyMCPClient の懒加載機能をテストします。
"""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.protocols.mcp_config import LazyLoadingConfig, MCPConfig, MCPServerConfig
from agentflow.protocols.mcp_lazy_client import (
    LazyMCPClient,
    ToolIndexEntry,
    ToolSearchResult,
)


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
                description="テスト用サーバー",
            ),
        ],
        lazy_loading=LazyLoadingConfig(
            enabled=True,
            threshold=5,
        ),
    )


@pytest.fixture
def mock_session() -> MagicMock:
    """モック ClientSession を作成."""
    session = MagicMock()
    session.initialize = AsyncMock()
    session.list_tools = AsyncMock()
    session.call_tool = AsyncMock()
    return session


class TestToolIndexEntry:
    """ToolIndexEntry のテスト."""

    def test_keyword_extraction(self) -> None:
        """キーワード自動抽出をテスト."""
        entry = ToolIndexEntry(
            uri="mcp://server/read_file",
            name="read_file",
            description="Read a file from the filesystem",
            server="server",
        )

        # キーワードが自動生成される
        assert "read" in entry.keywords
        assert "file" in entry.keywords
        assert "filesystem" in entry.keywords

    def test_initial_state(self) -> None:
        """初期状態をテスト."""
        entry = ToolIndexEntry(
            uri="mcp://server/test_tool",
            name="test_tool",
            description="Test description",
            server="server",
        )

        assert entry.loaded is False
        assert entry.uri == "mcp://server/test_tool"


class TestToolSearchResult:
    """ToolSearchResult のテスト."""

    def test_empty_result(self) -> None:
        """空の検索結果をテスト."""
        result = ToolSearchResult(entries=[], query="test")

        assert len(result.entries) == 0
        assert result.query == "test"
        assert result.loaded_count == 0


class TestLazyMCPClient:
    """LazyMCPClient のテスト."""

    def test_init_default_values(self, sample_config: MCPConfig) -> None:
        """デフォルト値での初期化をテスト."""
        client = LazyMCPClient(sample_config)

        assert client._enable_lazy_loading is True
        assert client._lazy_threshold == 0.1
        assert len(client._tool_index) == 0
        assert len(client._loaded_tools) == 0

    def test_init_custom_values(self, sample_config: MCPConfig) -> None:
        """カスタム値での初期化をテスト."""
        client = LazyMCPClient(
            sample_config,
            lazy_threshold=0.2,
            enable_lazy_loading=False,
        )

        assert client._enable_lazy_loading is False
        assert client._lazy_threshold == 0.2

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_connect_builds_index(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """接続時にインデックスが構築されることをテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        # ツールリストのモック
        mock_tool1 = MagicMock()
        mock_tool1.name = "read_file"
        mock_tool1.description = "Read a file from disk"
        mock_tool1.inputSchema = {"type": "object", "properties": {"path": {"type": "string"}}}

        mock_tool2 = MagicMock()
        mock_tool2.name = "write_file"
        mock_tool2.description = "Write content to a file"
        mock_tool2.inputSchema = {"type": "object", "properties": {"path": {"type": "string"}}}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool1, mock_tool2]
        mock_session.list_tools.return_value = mock_tools_result

        # クライアントを作成して接続
        client = LazyMCPClient(sample_config)
        await client.connect()

        # インデックスが構築されていることを確認
        assert len(client._tool_index) == 2
        assert "mcp://test-server/read_file" in client._tool_index
        assert "mcp://test-server/write_file" in client._tool_index

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_get_tool_index(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """軽量ツールインデックスの取得をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "test_tool"
        mock_tool.description = "A test tool for testing"
        mock_tool.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        client = LazyMCPClient(sample_config)
        await client.connect()

        # 軽量インデックスを取得
        index = client.get_tool_index()

        assert len(index) == 1
        assert index[0]["uri"] == "mcp://test-server/test_tool"
        assert index[0]["name"] == "test_tool"
        # スキーマは含まれない（軽量）
        assert "parameters" not in index[0]

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_search_by_keywords(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """キーワード検索をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool1 = MagicMock()
        mock_tool1.name = "read_file"
        mock_tool1.description = "Read a file from disk"
        mock_tool1.inputSchema = {"type": "object"}

        mock_tool2 = MagicMock()
        mock_tool2.name = "create_issue"
        mock_tool2.description = "Create a GitHub issue"
        mock_tool2.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool1, mock_tool2]
        mock_session.list_tools.return_value = mock_tools_result

        client = LazyMCPClient(sample_config)
        await client.connect()

        # キーワード検索
        result = client.search_tools("file")

        assert len(result.entries) == 1
        assert result.entries[0].name == "read_file"

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_search_by_select(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """select:tool_name 形式の検索をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "create_issue"
        mock_tool.description = "Create a GitHub issue"
        mock_tool.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        client = LazyMCPClient(sample_config)
        await client.connect()

        # select: 形式で検索
        result = client.search_tools("select:create_issue")

        assert len(result.entries) == 1
        assert result.entries[0].name == "create_issue"

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_load_tools(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """ツール定義のロードをテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "test_tool"
        mock_tool.description = "Test tool"
        mock_tool.inputSchema = {"type": "object", "properties": {"arg": {"type": "string"}}}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        client = LazyMCPClient(sample_config)
        await client.connect()

        # 初期状態ではロード済みツールはない
        assert len(client._loaded_tools) == 0

        # ツールをロード
        loaded = client.load_tools(["mcp://test-server/test_tool"])

        assert len(loaded) == 1
        assert loaded[0]["type"] == "function"
        assert loaded[0]["function"]["name"] == "mcp://test-server/test_tool"
        assert "parameters" in loaded[0]["function"]

        # インデックスの loaded フラグが更新される
        assert client._tool_index["mcp://test-server/test_tool"].loaded is True

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_get_tool_definitions_lazy_mode(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """懒加載モードでの get_tool_definitions をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool1 = MagicMock()
        mock_tool1.name = "tool1"
        mock_tool1.description = "Tool 1"
        mock_tool1.inputSchema = {"type": "object"}

        mock_tool2 = MagicMock()
        mock_tool2.name = "tool2"
        mock_tool2.description = "Tool 2"
        mock_tool2.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool1, mock_tool2]
        mock_session.list_tools.return_value = mock_tools_result

        client = LazyMCPClient(sample_config, enable_lazy_loading=True)
        await client.connect()

        # 初期状態では空（懒加載モード）
        definitions = client.get_tool_definitions()
        assert len(definitions) == 0

        # 1つだけロード
        client.load_tools(["mcp://test-server/tool1"])

        # ロードしたツールのみ返される
        definitions = client.get_tool_definitions()
        assert len(definitions) == 1
        assert definitions[0]["function"]["name"] == "mcp://test-server/tool1"

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_get_tool_definitions_non_lazy_mode(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """非懒加載モードでの get_tool_definitions をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "tool1"
        mock_tool.description = "Tool 1"
        mock_tool.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        client = LazyMCPClient(sample_config, enable_lazy_loading=False)
        await client.connect()

        # 非懒加載モードでは全ツールが返される
        definitions = client.get_tool_definitions()
        assert len(definitions) == 1

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_get_stats(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """統計情報の取得をテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool1 = MagicMock()
        mock_tool1.name = "tool1"
        mock_tool1.description = "Tool 1"
        mock_tool1.inputSchema = {"type": "object"}

        mock_tool2 = MagicMock()
        mock_tool2.name = "tool2"
        mock_tool2.description = "Tool 2"
        mock_tool2.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool1, mock_tool2]
        mock_session.list_tools.return_value = mock_tools_result

        client = LazyMCPClient(sample_config)
        await client.connect()

        # 初期統計
        stats = client.get_stats()
        assert stats["total_tools"] == 2
        assert stats["loaded_tools"] == 0
        assert stats["load_ratio"] == 0.0
        assert stats["lazy_loading_enabled"] is True
        assert stats["estimated_token_saved"] == 400  # 2 * 200

        # 1つロード
        client.load_tools(["mcp://test-server/tool1"])

        stats = client.get_stats()
        assert stats["loaded_tools"] == 1
        assert stats["load_ratio"] == 0.5
        assert stats["estimated_token_saved"] == 200  # 1 * 200

    @patch("agentflow.protocols.mcp_client.stdio_client")
    @patch("agentflow.protocols.mcp_client.ClientSession")
    async def test_clear_loaded_tools(
        self,
        mock_session_class: MagicMock,
        mock_stdio_client: MagicMock,
        sample_config: MCPConfig,
        mock_session: MagicMock,
    ) -> None:
        """ロード済みツールのクリアをテスト."""
        # モックの設定
        mock_context = AsyncMock()
        mock_context.__aenter__ = AsyncMock(return_value=(MagicMock(), MagicMock()))
        mock_context.__aexit__ = AsyncMock()
        mock_stdio_client.return_value = mock_context
        mock_session_class.return_value = mock_session

        mock_tool = MagicMock()
        mock_tool.name = "tool1"
        mock_tool.description = "Tool 1"
        mock_tool.inputSchema = {"type": "object"}

        mock_tools_result = MagicMock()
        mock_tools_result.tools = [mock_tool]
        mock_session.list_tools.return_value = mock_tools_result

        client = LazyMCPClient(sample_config)
        await client.connect()

        # ロード
        client.load_tools(["mcp://test-server/tool1"])
        assert len(client._loaded_tools) == 1

        # クリア
        client.clear_loaded_tools()
        assert len(client._loaded_tools) == 0
        assert client._tool_index["mcp://test-server/tool1"].loaded is False

    def test_get_tool_index_prompt(self, sample_config: MCPConfig) -> None:
        """ツールインデックスプロンプトの生成をテスト."""
        client = LazyMCPClient(sample_config)

        # インデックスを手動追加
        client._tool_index["mcp://server/tool1"] = ToolIndexEntry(
            uri="mcp://server/tool1",
            name="tool1",
            description="First tool for testing",
            server="server",
        )

        prompt = client.get_tool_index_prompt()

        assert "利用可能な MCP ツール" in prompt
        assert "tool1" in prompt
        assert "MCPSearch" in prompt
