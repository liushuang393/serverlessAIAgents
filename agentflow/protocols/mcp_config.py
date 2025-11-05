"""MCP サーバー設定モデル.

このモジュールは MCP サーバーの設定を管理するための Pydantic モデルを提供します。

Note:
    MCP ライブラリは Pydantic 1.x を使用しているため、
    Pydantic 2.x の v1 互換層を使用します。
"""

from typing import Any


# MCP ライブラリとの互換性のため Pydantic v1 を使用
try:
    from pydantic.v1 import BaseModel, Field
except ImportError:
    # Pydantic 1.x の場合は通常の import
    from pydantic import BaseModel, Field  # type: ignore[assignment]


class MCPServerConfig(BaseModel):
    """MCP サーバーの設定.

    Attributes:
        name: サーバーの一意識別子
        command: 実行するコマンド
        args: コマンドライン引数のリスト
        env: 環境変数の辞書
        enabled: サーバーが有効かどうか
    """

    name: str = Field(..., description="サーバーの一意識別子")
    command: str = Field(..., description="実行するコマンド")
    args: list[str] = Field(default_factory=list, description="コマンドライン引数")
    env: dict[str, str] = Field(default_factory=dict, description="環境変数")
    enabled: bool = Field(default=True, description="サーバーが有効かどうか")


class MCPConfig(BaseModel):
    """MCP 設定のルートモデル.

    Attributes:
        servers: MCP サーバー設定のリスト
    """

    servers: list[MCPServerConfig] = Field(
        default_factory=list, description="MCP サーバー設定のリスト"
    )

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "MCPConfig":
        """辞書から MCPConfig を作成.

        Args:
            data: 設定データの辞書

        Returns:
            MCPConfig インスタンス
        """
        return cls(**data)
