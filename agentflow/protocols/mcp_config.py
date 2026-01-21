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
        description: サーバーの説明（懒加載時の検索に使用）
    """

    name: str = Field(..., description="サーバーの一意識別子")
    command: str = Field(..., description="実行するコマンド")
    args: list[str] = Field(default_factory=list, description="コマンドライン引数")
    env: dict[str, str] = Field(default_factory=dict, description="環境変数")
    enabled: bool = Field(default=True, description="サーバーが有効かどうか")
    description: str = Field(default="", description="サーバーの説明（懒加載検索用）")


class LazyLoadingConfig(BaseModel):
    """懒加載（Lazy Loading）の設定.

    Claude Code の MCP Tool Search 機能を参考にした設定。
    ツール定義を必要時のみロードし、上下文 token 消費を削減。

    Attributes:
        enabled: 懒加載を有効にするか
        threshold: 懒加載を自動有効化する閾値（ツール数）
        auto_load_on_call: ツール呼び出し時に自動ロードするか
        cache_session: セッション内でロード済みツールをキャッシュするか
    """

    enabled: bool = Field(default=True, description="懒加載を有効にするか")
    threshold: int = Field(
        default=10,
        description="この数以上のツールがある場合に懒加載を自動有効化",
    )
    auto_load_on_call: bool = Field(
        default=True,
        description="ツール呼び出し時に未ロードツールを自動ロードするか",
    )
    cache_session: bool = Field(
        default=True,
        description="セッション内でロード済みツールをキャッシュするか",
    )


class MCPConfig(BaseModel):
    """MCP 設定のルートモデル.

    Attributes:
        servers: MCP サーバー設定のリスト
        lazy_loading: 懒加載設定
    """

    servers: list[MCPServerConfig] = Field(
        default_factory=list, description="MCP サーバー設定のリスト"
    )
    lazy_loading: LazyLoadingConfig = Field(
        default_factory=LazyLoadingConfig,
        description="懒加載（Lazy Loading）の設定",
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

    def should_enable_lazy_loading(self) -> bool:
        """懒加載を有効にすべきかどうかを判定.

        設定と有効サーバー数に基づいて判定。

        Returns:
            懒加載を有効にすべきかどうか
        """
        if not self.lazy_loading.enabled:
            return False

        # 有効サーバー数をカウント
        enabled_servers = sum(1 for s in self.servers if s.enabled)

        # 閾値以上の場合は懒加載を推奨
        return enabled_servers >= 1  # サーバーがあれば懒加載推奨
