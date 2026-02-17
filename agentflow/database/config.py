"""データベース設定モデル.

目的:
    アプリケーションの DB 接続設定を Pydantic モデルで一元管理する。
    各アプリは DatabaseConfig を定義するだけで、エンジン作成・セッション管理は
    フレームワークに委譲できる。

使用例:
    >>> from agentflow.database import DatabaseConfig
    >>> config = DatabaseConfig(url="sqlite+aiosqlite:///./app.db")
    >>> config = DatabaseConfig(
    ...     url="postgresql+asyncpg://user:pass@host/db",
    ...     pool_size=10,
    ...     max_overflow=20,
    ... )
"""

from __future__ import annotations

import os
from typing import Any

from pydantic import BaseModel, Field, field_validator


class DatabaseConfig(BaseModel):
    """データベース接続設定.

    Attributes:
        url: SQLAlchemy 接続 URL（環境変数で上書き可能）
        url_env_key: URL を上書きする環境変数名
        echo: SQL ログ出力の有無
        echo_env_key: echo を上書きする環境変数名
        pool_size: コネクションプールサイズ（PostgreSQL 等）
        max_overflow: プール最大オーバーフロー数
        pool_pre_ping: 接続前のヘルスチェック
        expire_on_commit: コミット後のオブジェクト失効
        connect_args: ドライバ固有の接続引数
    """

    url: str = Field(
        default="sqlite+aiosqlite:///./app.db",
        description="SQLAlchemy 接続 URL",
    )
    url_env_key: str = Field(
        default="DATABASE_URL",
        description="URL を上書きする環境変数名",
    )
    echo: bool = Field(
        default=False,
        description="SQL ログ出力",
    )
    echo_env_key: str = Field(
        default="DB_ECHO",
        description="echo を上書きする環境変数名",
    )
    pool_size: int = Field(default=5, ge=1, le=100)
    max_overflow: int = Field(default=10, ge=0, le=200)
    pool_pre_ping: bool = Field(default=True)
    expire_on_commit: bool = Field(default=False)
    connect_args: dict[str, Any] = Field(default_factory=dict)

    @field_validator("url")
    @classmethod
    def _validate_url(cls, v: str) -> str:
        """URL の基本バリデーション."""
        if "://" not in v:
            msg = f"無効な DB URL: {v!r}（'://' が必要）"
            raise ValueError(msg)
        return v

    def resolve_url(self) -> str:
        """環境変数を考慮した最終 URL を取得.

        Returns:
            解決済み DB URL
        """
        return os.getenv(self.url_env_key, self.url)

    def resolve_echo(self) -> bool:
        """環境変数を考慮した echo 設定を取得.

        Returns:
            echo 有効/無効
        """
        env_val = os.getenv(self.echo_env_key, "")
        if env_val:
            return env_val.lower() in {"1", "true", "yes", "on"}
        return self.echo
