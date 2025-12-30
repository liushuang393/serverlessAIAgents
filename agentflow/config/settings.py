# -*- coding: utf-8 -*-
"""AgentFlow設定.

このモジュールは、AgentFlowフレームワークの設定を管理します。

使用例:
    ```python
    from agentflow.config import get_settings

    settings = get_settings()
    print(settings.llm_provider)  # "openai"
    print(settings.llm_model)  # "gpt-4"
    ```

環境変数:
    - AGENTFLOW_LLM_PROVIDER: LLMプロバイダー（openai/anthropic/google/local）
    - AGENTFLOW_LLM_API_KEY: LLM APIキー
    - AGENTFLOW_LLM_MODEL: LLMモデル名
    - AGENTFLOW_MEMORY_BACKEND: メモリバックエンド（local/redis/postgres）
    - AGENTFLOW_REDIS_URL: Redis URL
    - AGENTFLOW_POSTGRES_URL: PostgreSQL URL
    - AGENTFLOW_LOG_LEVEL: ログレベル（DEBUG/INFO/WARNING/ERROR）
"""

import logging
from functools import lru_cache

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class AgentFlowSettings(BaseSettings):
    """AgentFlow設定.

    環境変数または.envファイルから設定を読み込みます。

    Attributes:
        llm_provider: LLMプロバイダー
        llm_api_key: LLM APIキー
        llm_model: LLMモデル名
        llm_temperature: LLM温度パラメータ
        llm_max_tokens: LLM最大トークン数
        memory_backend: メモリバックエンド
        redis_url: Redis URL
        postgres_url: PostgreSQL URL
        log_level: ログレベル
        debug: デバッグモード
    """

    # LLM設定
    llm_provider: str = Field(
        default="openai",
        description="LLMプロバイダー（openai/anthropic/google/local）",
    )
    llm_api_key: str | None = Field(default=None, description="LLM APIキー")
    llm_model: str = Field(default="gpt-4", description="LLMモデル名")
    llm_temperature: float = Field(
        default=0.7, ge=0.0, le=2.0, description="LLM温度パラメータ"
    )
    llm_max_tokens: int = Field(default=2000, gt=0, description="LLM最大トークン数")

    # メモリ設定
    memory_backend: str = Field(
        default="local", description="メモリバックエンド（local/redis/postgres）"
    )
    redis_url: str | None = Field(default=None, description="Redis URL")
    postgres_url: str | None = Field(default=None, description="PostgreSQL URL")

    # 外部API設定
    news_api_key: str | None = Field(default=None, description="NewsAPI APIキー")

    # ログ設定
    log_level: str = Field(default="INFO", description="ログレベル")
    debug: bool = Field(default=False, description="デバッグモード")

    # Pydantic設定
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        env_prefix="AGENTFLOW_",
        case_sensitive=False,
    )

    def configure_logging(self) -> None:
        """ログ設定を適用."""
        logging.basicConfig(
            level=getattr(logging, self.log_level.upper()),
            format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        )

    def get_llm_config(self) -> dict[str, any]:
        """LLM設定を取得.

        Returns:
            LLM設定辞書
        """
        return {
            "provider": self.llm_provider,
            "api_key": self.llm_api_key,
            "model": self.llm_model,
            "temperature": self.llm_temperature,
            "max_tokens": self.llm_max_tokens,
        }

    def get_memory_config(self) -> dict[str, any]:
        """メモリ設定を取得.

        Returns:
            メモリ設定辞書
        """
        return {
            "backend": self.memory_backend,
            "redis_url": self.redis_url,
            "postgres_url": self.postgres_url,
        }


@lru_cache
def get_settings() -> AgentFlowSettings:
    """設定シングルトンを取得.

    この関数は設定をキャッシュし、アプリケーション全体で同じインスタンスを返します。

    Returns:
        AgentFlow設定

    使用例:
        ```python
        from agentflow.config import get_settings

        settings = get_settings()
        print(settings.llm_provider)
        ```
    """
    settings = AgentFlowSettings()
    settings.configure_logging()
    return settings

