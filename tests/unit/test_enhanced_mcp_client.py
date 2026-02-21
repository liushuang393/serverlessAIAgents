"""Enhanced MCP Client のテスト."""

import asyncio
from unittest.mock import AsyncMock, Mock

import pytest

from agentflow.core.security import AuditLogger, ParameterValidator, ToolWhitelist
from agentflow.protocols.mcp_client import (
    MCPClient,
    ToolNotAllowedError,
    ToolValidationError,
)
from agentflow.protocols.mcp_config import MCPConfig, MCPServerConfig


@pytest.fixture
def mock_config():
    """テスト用の MCP 設定."""
    return MCPConfig(
        servers=[
            MCPServerConfig(
                name="test_server",
                command="test",
                args=[],
                enabled=True,
            )
        ]
    )


@pytest.fixture
def mock_whitelist():
    """テスト用のホワイトリスト."""
    whitelist = ToolWhitelist()
    whitelist.add_tool("mcp://test_server/allowed_tool")
    return whitelist


@pytest.fixture
def mock_audit_logger():
    """テスト用の審計ロガー."""
    return Mock(spec=AuditLogger)


@pytest.fixture
def mock_validator():
    """テスト用のバリデーター."""
    return Mock(spec=ParameterValidator)


@pytest.mark.asyncio
class TestEnhancedMCPClient:
    """Enhanced MCP Client のテスト."""

    async def test_init_with_security_components(self, mock_config):
        """セキュリティコンポーネント付きで初期化できる."""
        whitelist = ToolWhitelist()
        audit_logger = AuditLogger()
        validator = ParameterValidator()

        client = MCPClient(
            mock_config,
            whitelist=whitelist,
            audit_logger=audit_logger,
            validator=validator,
            max_retries=5,
            timeout=60.0,
        )

        assert client._whitelist is whitelist
        assert client._audit_logger is audit_logger
        assert client._validator is validator
        assert client._max_retries == 5
        assert client._timeout == 60.0

    async def test_init_with_default_security_components(self, mock_config):
        """デフォルトのセキュリティコンポーネントで初期化できる."""
        client = MCPClient(mock_config)

        assert isinstance(client._whitelist, ToolWhitelist)
        assert isinstance(client._audit_logger, AuditLogger)
        assert isinstance(client._validator, ParameterValidator)
        assert client._max_retries == 3
        assert client._timeout == 30.0

    async def test_call_tool_whitelist_check_success(
        self, mock_config, mock_whitelist, mock_audit_logger, mock_validator
    ):
        """ホワイトリストチェックが成功する."""
        client = MCPClient(
            mock_config,
            whitelist=mock_whitelist,
            audit_logger=mock_audit_logger,
            validator=mock_validator,
        )

        # ツール情報を設定
        client._tools = {
            "mcp://test_server/allowed_tool": {
                "name": "allowed_tool",
                "server": "test_server",
                "input_schema": {},
            }
        }

        # セッションをモック
        mock_session = AsyncMock()
        mock_result = Mock()
        mock_result.content = [{"type": "text", "text": "success"}]
        mock_session.call_tool.return_value = mock_result
        client._sessions = {"test_server": mock_session}

        # バリデーターをモック
        mock_validator.validate.return_value = (True, None)

        # ツールを呼び出す
        result = await client.call_tool("mcp://test_server/allowed_tool", {"arg": "value"})

        assert result["success"] is True
        assert result["tool"] == "allowed_tool"
        mock_audit_logger.log_tool_call.assert_called_once()

    async def test_call_tool_whitelist_check_failure(self, mock_config, mock_whitelist, mock_audit_logger):
        """ホワイトリストチェックが失敗する."""
        client = MCPClient(
            mock_config,
            whitelist=mock_whitelist,
            audit_logger=mock_audit_logger,
        )

        # ツール情報を設定（ホワイトリストにないツール）
        client._tools = {
            "mcp://test_server/forbidden_tool": {
                "name": "forbidden_tool",
                "server": "test_server",
            }
        }

        # ツールを呼び出す（エラーが発生するはず）
        with pytest.raises(ToolNotAllowedError):
            await client.call_tool("mcp://test_server/forbidden_tool", {"arg": "value"})

        # 審計ログが記録されているはず
        mock_audit_logger.log_tool_call.assert_called_once()
        call_args = mock_audit_logger.log_tool_call.call_args[1]
        assert call_args["success"] is False
        assert "not in whitelist" in call_args["error"]

    async def test_call_tool_parameter_validation_failure(
        self, mock_config, mock_whitelist, mock_audit_logger, mock_validator
    ):
        """パラメータ検証が失敗する."""
        client = MCPClient(
            mock_config,
            whitelist=mock_whitelist,
            audit_logger=mock_audit_logger,
            validator=mock_validator,
        )

        # ツール情報を設定
        client._tools = {
            "mcp://test_server/allowed_tool": {
                "name": "allowed_tool",
                "server": "test_server",
                "input_schema": {"required": ["arg"]},
            }
        }

        # バリデーターをモック（検証失敗）
        mock_validator.validate.return_value = (False, "Missing required field: arg")

        # ツールを呼び出す（エラーが発生するはず）
        with pytest.raises(ToolValidationError):
            await client.call_tool("mcp://test_server/allowed_tool", {})

        # 審計ログが記録されているはず
        mock_audit_logger.log_tool_call.assert_called_once()

    async def test_call_tool_with_retry_success_on_first_attempt(
        self, mock_config, mock_whitelist, mock_audit_logger, mock_validator
    ):
        """最初の試行で成功する."""
        client = MCPClient(
            mock_config,
            whitelist=mock_whitelist,
            audit_logger=mock_audit_logger,
            validator=mock_validator,
            max_retries=3,
        )

        # ツール情報を設定
        client._tools = {
            "mcp://test_server/allowed_tool": {
                "name": "allowed_tool",
                "server": "test_server",
                "input_schema": {},
            }
        }

        # セッションをモック
        mock_session = AsyncMock()
        mock_result = Mock()
        mock_result.content = [{"type": "text", "text": "success"}]
        mock_session.call_tool.return_value = mock_result
        client._sessions = {"test_server": mock_session}

        # バリデーターをモック
        mock_validator.validate.return_value = (True, None)

        # ツールを呼び出す
        result = await client.call_tool("mcp://test_server/allowed_tool", {"arg": "value"})

        assert result["success"] is True
        assert mock_session.call_tool.call_count == 1

    async def test_call_tool_with_retry_success_on_second_attempt(
        self, mock_config, mock_whitelist, mock_audit_logger, mock_validator
    ):
        """2回目の試行で成功する."""
        client = MCPClient(
            mock_config,
            whitelist=mock_whitelist,
            audit_logger=mock_audit_logger,
            validator=mock_validator,
            max_retries=3,
        )

        # ツール情報を設定
        client._tools = {
            "mcp://test_server/allowed_tool": {
                "name": "allowed_tool",
                "server": "test_server",
                "input_schema": {},
            }
        }

        # セッションをモック（1回目は失敗、2回目は成功）
        mock_session = AsyncMock()
        mock_result = Mock()
        mock_result.content = [{"type": "text", "text": "success"}]
        mock_session.call_tool.side_effect = [
            Exception("Network error"),
            mock_result,
        ]
        client._sessions = {"test_server": mock_session}

        # バリデーターをモック
        mock_validator.validate.return_value = (True, None)

        # ツールを呼び出す
        result = await client.call_tool("mcp://test_server/allowed_tool", {"arg": "value"})

        assert result["success"] is True
        assert mock_session.call_tool.call_count == 2

    async def test_call_tool_with_retry_all_attempts_fail(
        self, mock_config, mock_whitelist, mock_audit_logger, mock_validator
    ):
        """すべての試行が失敗する."""
        client = MCPClient(
            mock_config,
            whitelist=mock_whitelist,
            audit_logger=mock_audit_logger,
            validator=mock_validator,
            max_retries=3,
        )

        # ツール情報を設定
        client._tools = {
            "mcp://test_server/allowed_tool": {
                "name": "allowed_tool",
                "server": "test_server",
                "input_schema": {},
            }
        }

        # セッションをモック（すべて失敗）
        mock_session = AsyncMock()
        mock_session.call_tool.side_effect = Exception("Network error")
        client._sessions = {"test_server": mock_session}

        # バリデーターをモック
        mock_validator.validate.return_value = (True, None)

        # ツールを呼び出す
        result = await client.call_tool("mcp://test_server/allowed_tool", {"arg": "value"})

        assert result["success"] is False
        assert "failed after 3 attempts" in result["error"]
        assert mock_session.call_tool.call_count == 3

    async def test_call_tool_with_timeout(self, mock_config, mock_whitelist, mock_audit_logger, mock_validator):
        """タイムアウトが発生する."""
        client = MCPClient(
            mock_config,
            whitelist=mock_whitelist,
            audit_logger=mock_audit_logger,
            validator=mock_validator,
            max_retries=2,
            timeout=0.1,  # 短いタイムアウト
        )

        # ツール情報を設定
        client._tools = {
            "mcp://test_server/allowed_tool": {
                "name": "allowed_tool",
                "server": "test_server",
                "input_schema": {},
            }
        }

        # セッションをモック（遅延）
        async def slow_call(*args, **kwargs):
            await asyncio.sleep(1.0)  # タイムアウトより長い
            return Mock(content=[{"type": "text", "text": "success"}])

        mock_session = AsyncMock()
        mock_session.call_tool.side_effect = slow_call
        client._sessions = {"test_server": mock_session}

        # バリデーターをモック
        mock_validator.validate.return_value = (True, None)

        # ツールを呼び出す
        result = await client.call_tool("mcp://test_server/allowed_tool", {"arg": "value"})

        assert result["success"] is False
        assert "failed after 2 attempts" in result["error"]

    async def test_call_tool_with_security_disabled(
        self, mock_config, mock_whitelist, mock_audit_logger, mock_validator
    ):
        """セキュリティ機能を無効にできる."""
        client = MCPClient(
            mock_config,
            whitelist=mock_whitelist,
            audit_logger=mock_audit_logger,
            validator=mock_validator,
            enable_security=False,
        )

        # ツール情報を設定（ホワイトリストにないツール）
        client._tools = {
            "mcp://test_server/forbidden_tool": {
                "name": "forbidden_tool",
                "server": "test_server",
                "input_schema": {},
            }
        }

        # セッションをモック
        mock_session = AsyncMock()
        mock_result = Mock()
        mock_result.content = [{"type": "text", "text": "success"}]
        mock_session.call_tool.return_value = mock_result
        client._sessions = {"test_server": mock_session}

        # ツールを呼び出す（セキュリティ無効なのでエラーにならない）
        result = await client.call_tool("mcp://test_server/forbidden_tool", {"arg": "value"})

        assert result["success"] is True
        # バリデーターは呼ばれない
        mock_validator.validate.assert_not_called()
