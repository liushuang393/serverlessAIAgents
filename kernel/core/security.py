"""AgentFlow セキュリティモジュール — kernel 層.

ツール呼び出しのセキュリティ、監査ログ、CSRF 保護を提供。
agentflow/core/security.py から移行。
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime
from secrets import token_urlsafe
from typing import Any


class ToolWhitelist:
    """ツールホワイトリスト管理."""

    ALLOWED_TOOLS: set[str] = {
        "mcp://filesystem/read_file",
        "mcp://filesystem/write_file",
        "mcp://filesystem/list_directory",
        "mcp://github/create_issue",
        "mcp://github/list_issues",
    }

    @classmethod
    def is_allowed(cls, tool_uri: str) -> bool:
        """ツールが許可されているか確認.

        Args:
            tool_uri: ツール URI

        Returns:
            許可されている場合 True
        """
        return tool_uri in cls.ALLOWED_TOOLS

    @classmethod
    def add_tool(cls, tool_uri: str) -> None:
        """ツールをホワイトリストに追加.

        Args:
            tool_uri: ツール URI
        """
        cls.ALLOWED_TOOLS.add(tool_uri)

    @classmethod
    def remove_tool(cls, tool_uri: str) -> None:
        """ツールをホワイトリストから削除.

        Args:
            tool_uri: ツール URI
        """
        cls.ALLOWED_TOOLS.discard(tool_uri)


class AuditLogger:
    """ツール呼び出しの監査ログ."""

    def __init__(self, logger: logging.Logger | None = None) -> None:
        """監査ロガーを初期化.

        Args:
            logger: ロガーインスタンス
        """
        self._logger = logger or logging.getLogger(__name__)

    def log_tool_call(
        self,
        user_id: str,
        tool_uri: str,
        parameters: dict[str, Any],
        result: Any,
        success: bool,
        error: str | None = None,
    ) -> None:
        """ツール呼び出しをログ記録.

        Args:
            user_id: ユーザー ID
            tool_uri: ツール URI
            parameters: パラメータ
            result: 結果
            success: 成功フラグ
            error: エラーメッセージ
        """
        log_entry = {
            "timestamp": datetime.now(UTC).isoformat(),
            "user_id": user_id,
            "tool_uri": tool_uri,
            "parameters": parameters,
            "result": str(result)[:200] if result else None,
            "success": success,
            "error": error,
        }
        self._logger.warning(f"AUDIT: {log_entry}")


class CSRFProtection:
    """CSRF トークン検証."""

    def __init__(self) -> None:
        """CSRF 保護を初期化."""
        self._tokens: set[str] = set()

    def generate_token(self) -> str:
        """CSRF トークンを生成.

        Returns:
            生成されたトークン
        """
        token = token_urlsafe(32)
        self._tokens.add(token)
        return token

    def verify_token(self, token: str) -> bool:
        """CSRF トークンを検証.

        Args:
            token: 検証するトークン

        Returns:
            有効な場合 True
        """
        if token in self._tokens:
            self._tokens.remove(token)
            return True
        return False

    def clear_tokens(self) -> None:
        """すべてのトークンをクリア."""
        self._tokens.clear()


class ParameterValidator:
    """ツールパラメータの検証."""

    @staticmethod
    def validate(schema: dict[str, Any], parameters: dict[str, Any]) -> tuple[bool, str | None]:
        """パラメータを検証.

        Args:
            schema: JSON Schema
            parameters: 検証するパラメータ

        Returns:
            (検証成功, エラーメッセージ)
        """
        try:
            required = schema.get("required", [])
            properties = schema.get("properties", {})
            for field in required:
                if field not in parameters:
                    return False, f"Missing required field: {field}"
            for field, value in parameters.items():
                if field in properties:
                    expected_type = properties[field].get("type")
                    if expected_type == "string" and not isinstance(value, str):
                        return False, f"Field {field} must be string"
                    if expected_type == "integer" and not isinstance(value, int):
                        return False, f"Field {field} must be integer"
                    if expected_type == "boolean" and not isinstance(value, bool):
                        return False, f"Field {field} must be boolean"
            return True, None
        except Exception as e:
            return False, str(e)

