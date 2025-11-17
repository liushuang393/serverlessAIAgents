"""AgentFlow セキュリティモジュールのユニットテスト."""

from __future__ import annotations

import pytest

from agentflow.core.security import (
    AuditLogger,
    CSRFProtection,
    ParameterValidator,
    ToolWhitelist,
)


class TestToolWhitelist:
    """ToolWhitelist のテスト."""

    def test_is_allowed_default_tools(self):
        """デフォルトで許可されている工具を確認."""
        assert ToolWhitelist.is_allowed("mcp://filesystem/read_file")
        assert ToolWhitelist.is_allowed("mcp://github/create_issue")

    def test_is_allowed_not_in_whitelist(self):
        """ホワイトリストにない工具を確認."""
        assert not ToolWhitelist.is_allowed("mcp://dangerous/delete_all")

    def test_add_tool(self):
        """工具をホワイトリストに追加."""
        tool_uri = "mcp://test/custom_tool"
        ToolWhitelist.add_tool(tool_uri)
        assert ToolWhitelist.is_allowed(tool_uri)
        # クリーンアップ
        ToolWhitelist.remove_tool(tool_uri)

    def test_remove_tool(self):
        """工具をホワイトリストから削除."""
        tool_uri = "mcp://test/temp_tool"
        ToolWhitelist.add_tool(tool_uri)
        assert ToolWhitelist.is_allowed(tool_uri)
        ToolWhitelist.remove_tool(tool_uri)
        assert not ToolWhitelist.is_allowed(tool_uri)


class TestAuditLogger:
    """AuditLogger のテスト."""

    def test_log_tool_call_success(self, caplog):
        """成功した工具調用をログに記録."""
        logger = AuditLogger()
        logger.log_tool_call(
            user_id="user123",
            tool_uri="mcp://filesystem/read_file",
            parameters={"path": "/test/file.txt"},
            result="file content",
            success=True,
        )
        assert "AUDIT:" in caplog.text
        assert "user123" in caplog.text
        assert "mcp://filesystem/read_file" in caplog.text

    def test_log_tool_call_failure(self, caplog):
        """失敗した工具調用をログに記録."""
        logger = AuditLogger()
        logger.log_tool_call(
            user_id="user456",
            tool_uri="mcp://filesystem/write_file",
            parameters={"path": "/test/file.txt", "content": "data"},
            result=None,
            success=False,
            error="Permission denied",
        )
        assert "AUDIT:" in caplog.text
        assert "user456" in caplog.text
        assert "Permission denied" in caplog.text

    def test_log_tool_call_long_result(self, caplog):
        """長い結果を切り詰めてログに記録."""
        logger = AuditLogger()
        long_result = "x" * 500
        logger.log_tool_call(
            user_id="user789",
            tool_uri="mcp://test/tool",
            parameters={},
            result=long_result,
            success=True,
        )
        assert "AUDIT:" in caplog.text
        # 結果は 200 文字に切り詰められる
        assert len(caplog.text) < len(long_result)


class TestCSRFProtection:
    """CSRFProtection のテスト."""

    def test_generate_token(self):
        """CSRF トークンを生成."""
        csrf = CSRFProtection()
        token = csrf.generate_token()
        assert isinstance(token, str)
        assert len(token) > 0

    def test_verify_token_valid(self):
        """有効な CSRF トークンを検証."""
        csrf = CSRFProtection()
        token = csrf.generate_token()
        assert csrf.verify_token(token)

    def test_verify_token_invalid(self):
        """無効な CSRF トークンを検証."""
        csrf = CSRFProtection()
        assert not csrf.verify_token("invalid_token")

    def test_verify_token_once_only(self):
        """CSRF トークンは一度だけ使用可能."""
        csrf = CSRFProtection()
        token = csrf.generate_token()
        assert csrf.verify_token(token)
        # 2回目は失敗
        assert not csrf.verify_token(token)

    def test_clear_tokens(self):
        """すべてのトークンをクリア."""
        csrf = CSRFProtection()
        token1 = csrf.generate_token()
        token2 = csrf.generate_token()
        csrf.clear_tokens()
        assert not csrf.verify_token(token1)
        assert not csrf.verify_token(token2)


class TestParameterValidator:
    """ParameterValidator のテスト."""

    def test_validate_success(self):
        """有効なパラメータを検証."""
        schema = {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "age": {"type": "integer"},
            },
            "required": ["name"],
        }
        parameters = {"name": "Alice", "age": 30}
        valid, error = ParameterValidator.validate(schema, parameters)
        assert valid
        assert error is None

    def test_validate_missing_required_field(self):
        """必須フィールドが欠けている場合."""
        schema = {
            "type": "object",
            "properties": {"name": {"type": "string"}},
            "required": ["name"],
        }
        parameters = {}
        valid, error = ParameterValidator.validate(schema, parameters)
        assert not valid
        assert "Missing required field" in error

    def test_validate_wrong_type(self):
        """型が間違っている場合."""
        schema = {
            "type": "object",
            "properties": {"age": {"type": "integer"}},
        }
        parameters = {"age": "not_an_integer"}
        valid, error = ParameterValidator.validate(schema, parameters)
        assert not valid
        assert "must be integer" in error

    def test_validate_optional_field(self):
        """オプションフィールドがない場合."""
        schema = {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "email": {"type": "string"},
            },
            "required": ["name"],
        }
        parameters = {"name": "Bob"}
        valid, error = ParameterValidator.validate(schema, parameters)
        assert valid
        assert error is None

