# -*- coding: utf-8 -*-
"""{{ app_name }} 設定管理モジュール.

環境変数と YAML 設定ファイルから設定を読み込む。
"""
from functools import lru_cache
from pathlib import Path
from typing import Any

import yaml
from pydantic import Field
from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    """アプリケーション設定.
    
    環境変数から読み込み、YAML 設定でオーバーライド可能。
    """
    
    # アプリケーション基本設定
    app_name: str = Field(default="{{ app_name }}", description="アプリケーション名")
    app_env: str = Field(default="development", description="環境 (development/staging/production)")
    debug: bool = Field(default=True, description="デバッグモード")
    
    # データベース設定
    database_url: str = Field(
        default="postgresql+asyncpg://{{ db_user }}:{{ db_password }}@localhost:{{ db_port }}/{{ db_name }}",
        description="メイン DB 接続 URL"
    )
{% if use_history_db %}
    history_database_url: str = Field(
        default="postgresql+asyncpg://{{ db_user }}:{{ db_password }}@localhost:{{ db_port + 1 }}/{{ db_name }}_history",
        description="履歴 DB 接続 URL"
    )
{% endif %}
    
{% if redis_enabled %}
    # Redis 設定
    redis_url: str = Field(
        default="redis://localhost:6379/0",
        description="Redis 接続 URL"
    )
    cache_ttl: int = Field(default=3600, description="キャッシュ TTL (秒)")
{% endif %}
    
    # API 設定
    api_host: str = Field(default="0.0.0.0", description="API ホスト")
    api_port: int = Field(default={{ api_port }}, description="API ポート")
    
    model_config = {
        "env_file": ".env",
        "env_file_encoding": "utf-8",
        "extra": "ignore",
    }


@lru_cache
def get_settings() -> Settings:
    """設定インスタンスを取得（キャッシュ付き）."""
    return Settings()


def reload_settings() -> Settings:
    """設定を再読み込み."""
    get_settings.cache_clear()
    return get_settings()


def load_yaml_config(config_path: Path | str = "config/settings.yaml") -> dict[str, Any]:
    """YAML 設定ファイルを読み込む.
    
    Args:
        config_path: 設定ファイルパス
        
    Returns:
        設定辞書
    """
    path = Path(config_path)
    if not path.exists():
        return {}
    
    with path.open("r", encoding="utf-8") as f:
        return yaml.safe_load(f) or {}

