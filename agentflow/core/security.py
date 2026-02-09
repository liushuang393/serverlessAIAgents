"""AgentFlow セキュリティモジュール.

工具調用のセキュリティ、審計ログ、CSRF 保護などを提供します。
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime
from secrets import token_urlsafe
from typing import Any


class ToolWhitelist:
    """工具ホワイトリスト管理.

    許可された工具のみ調用を許可します。
    """

    # デフォルトで許可される工具
    ALLOWED_TOOLS: set[str] = {
        "mcp://filesystem/read_file",
        "mcp://filesystem/write_file",
        "mcp://filesystem/list_directory",
        "mcp://github/create_issue",
        "mcp://github/list_issues",
    }

    @classmethod
    def is_allowed(cls, tool_uri: str) -> bool:
        """工具が許可されているか確認.

        Args:
            tool_uri: 工具 URI (例: "mcp://filesystem/read_file")

        Returns:
            許可されている場合 True
        """
        return tool_uri in cls.ALLOWED_TOOLS

    @classmethod
    def add_tool(cls, tool_uri: str) -> None:
        """工具をホワイトリストに追加.

        Args:
            tool_uri: 工具 URI
        """
        cls.ALLOWED_TOOLS.add(tool_uri)

    @classmethod
    def remove_tool(cls, tool_uri: str) -> None:
        """工具をホワイトリストから削除.

        Args:
            tool_uri: 工具 URI
        """
        cls.ALLOWED_TOOLS.discard(tool_uri)


class AuditLogger:
    """工具調用の審計ログ.

    すべての工具調用を記録し、セキュリティ監査を可能にします。
    """

    def __init__(self, logger: logging.Logger | None = None) -> None:
        """審計ロガーを初期化.

        Args:
            logger: ロガーインスタンス (オプション)
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
        """工具調用をログに記録.

        Args:
            user_id: ユーザー ID
            tool_uri: 工具 URI
            parameters: 工具パラメータ
            result: 実行結果
            success: 成功したかどうか
            error: エラーメッセージ (失敗時)
        """
        log_entry = {
            "timestamp": datetime.now(UTC).isoformat(),
            "user_id": user_id,
            "tool_uri": tool_uri,
            "parameters": parameters,
            "result": str(result)[:200] if result else None,  # 長すぎる結果は切り詰め
            "success": success,
            "error": error,
        }
        # WARNING レベルで記録して確実にキャプチャされるようにする
        self._logger.warning(f"AUDIT: {log_entry}")


class CSRFProtection:
    """CSRF トークン検証.

    CSRF 攻撃を防ぐためのトークン生成と検証を提供します。
    """

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
            トークンが有効な場合 True
        """
        if token in self._tokens:
            self._tokens.remove(token)  # 一度だけ使用可能
            return True
        return False

    def clear_tokens(self) -> None:
        """すべてのトークンをクリア."""
        self._tokens.clear()


class ParameterValidator:
    """工具パラメータの検証.

    Pydantic を使用してパラメータを検証します。
    """

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
            # 簡易的な検証（実際には jsonschema ライブラリを使用すべき）
            required = schema.get("required", [])
            properties = schema.get("properties", {})

            # 必須フィールドのチェック
            for field in required:
                if field not in parameters:
                    return False, f"Missing required field: {field}"

            # 型チェック
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

