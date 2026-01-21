# -*- coding: utf-8 -*-
"""AgentFlow設定.

このモジュールは、AgentFlowフレームワークの設定を管理します。

設定優先順位（高い順）:
    1. 環境変数（直接設定）
    2. .env ファイル
    3. config.py デフォルト値

使用例:
    ```python
    from agentflow.config import get_settings

    settings = get_settings()
    print(settings.openai_model)  # "gpt-5.2"
    print(settings.get_active_llm_config())  # 有効なLLM設定
    ```

対応環境変数:
    LLM関連:
        - OPENAI_API_KEY, OPENAI_MODEL
        - ANTHROPIC_API_KEY, ANTHROPIC_MODEL
        - GOOGLE_API_KEY, GOOGLE_MODEL
        - DEEPSEEK_API_KEY, DEEPSEEK_MODEL
        - OLLAMA_BASE_URL, OLLAMA_MODEL
    DB関連:
        - DATABASE_URL, SUPABASE_URL, TURSO_URL
    VectorDB関連:
        - CHROMA_PERSIST_DIR, CHROMA_COLLECTION
        - PINECONE_API_KEY, PINECONE_INDEX
        - QDRANT_URL
    Embedding:
        - OPENAI_EMBEDDING_MODEL
"""

import logging
import os
from functools import lru_cache
from typing import Any

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class AgentFlowSettings(BaseSettings):
    """AgentFlow設定.

    環境変数または.envファイルから設定を読み込みます。
    優先順位: 環境変数 > .env > デフォルト値
    """

    # ========================================
    # OpenAI
    # ========================================
    openai_api_key: str | None = Field(default=None, description="OpenAI APIキー")
    openai_model: str = Field(default="gpt-5.2", description="OpenAI モデル名")
    openai_embedding_model: str = Field(
        default="text-embedding-3-small", description="OpenAI Embedding モデル"
    )

    # ========================================
    # Anthropic
    # ========================================
    anthropic_api_key: str | None = Field(default=None, description="Anthropic APIキー")
    anthropic_model: str = Field(
        default="claude-sonnet-4-20250514", description="Anthropic モデル名"
    )

    # ========================================
    # Google
    # ========================================
    google_api_key: str | None = Field(default=None, description="Google APIキー")
    google_model: str = Field(default="gemini-2.0-flash", description="Google モデル名")

    # ========================================
    # DeepSeek
    # ========================================
    deepseek_api_key: str | None = Field(default=None, description="DeepSeek APIキー")
    deepseek_model: str = Field(default="deepseek-chat", description="DeepSeek モデル名")

    # ========================================
    # Ollama（ローカル）
    # ========================================
    ollama_base_url: str | None = Field(default=None, description="Ollama URL")
    ollama_model: str = Field(default="llama3.3:70b", description="Ollama モデル名")

    # ========================================
    # データベース
    # ========================================
    database_url: str | None = Field(default=None, description="汎用 Database URL")
    supabase_url: str | None = Field(default=None, description="Supabase URL")
    supabase_key: str | None = Field(default=None, description="Supabase Key")
    turso_url: str | None = Field(default=None, description="Turso URL")

    # ========================================
    # VectorDB
    # ========================================
    chroma_persist_dir: str | None = Field(default=None, description="ChromaDB 永続化ディレクトリ")
    chroma_collection: str = Field(default="default", description="ChromaDB コレクション名")
    pinecone_api_key: str | None = Field(default=None, description="Pinecone APIキー")
    pinecone_index: str = Field(default="default", description="Pinecone インデックス名")
    qdrant_url: str | None = Field(default=None, description="Qdrant URL")

    # ========================================
    # LLM共通設定
    # ========================================
    llm_temperature: float = Field(default=0.7, ge=0.0, le=2.0, description="LLM温度")
    llm_max_tokens: int = Field(default=4096, gt=0, description="最大トークン数")
    # LLM APIタイムアウト（複雑な分析タスクには180秒以上推奨）
    llm_timeout: int = Field(default=180, gt=0, description="LLM APIタイムアウト（秒）")

    # ========================================
    # メモリ設定
    # ========================================
    memory_backend: str = Field(default="local", description="メモリバックエンド")
    redis_url: str | None = Field(default=None, description="Redis URL")

    # ========================================
    # 知識ストア設定（長期記憶）
    # ========================================
    knowledge_storage_path: str = Field(
        default="memory/knowledge",
        description="知識ストレージのパス"
    )
    knowledge_backend: str = Field(
        default="auto",
        description="知識ストアバックエンド (auto/memvid/memory)"
    )
    knowledge_auto_persist: bool = Field(
        default=True,
        description="知識の自動永続化を有効化"
    )

    # ========================================
    # ログ設定
    # ========================================
    log_level: str = Field(default="INFO", description="ログレベル")
    debug: bool = Field(default=False, description="デバッグモード")

    # Pydantic設定（環境変数プレフィックスなし＝直接読み取り）
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=False,
        extra="ignore",
    )

    def get_active_provider(self) -> str:
        """有効なLLMプロバイダーを検出.

        Returns:
            プロバイダー名（openai/anthropic/google/deepseek/ollama/mock）
        """
        if self.openai_api_key:
            return "openai"
        if self.anthropic_api_key:
            return "anthropic"
        if self.google_api_key:
            return "google"
        if self.deepseek_api_key:
            return "deepseek"
        if self.ollama_base_url:
            return "ollama"
        return "mock"

    def get_active_model(self) -> str:
        """有効なモデル名を取得.

        Returns:
            モデル名
        """
        provider = self.get_active_provider()
        model_map = {
            "openai": self.openai_model,
            "anthropic": self.anthropic_model,
            "google": self.google_model,
            "deepseek": self.deepseek_model,
            "ollama": self.ollama_model,
            "mock": "mock",
        }
        return model_map.get(provider, "unknown")

    def get_active_llm_config(self) -> dict[str, Any]:
        """有効なLLM設定を取得.

        Returns:
            LLM設定辞書（provider, model, api_key, base_url, timeout等）
        """
        provider = self.get_active_provider()
        config: dict[str, Any] = {
            "provider": provider,
            "model": self.get_active_model(),
            "temperature": self.llm_temperature,
            "max_tokens": self.llm_max_tokens,
            "timeout": self.llm_timeout,
        }

        if provider == "openai":
            config["api_key"] = self.openai_api_key
        elif provider == "anthropic":
            config["api_key"] = self.anthropic_api_key
        elif provider == "google":
            config["api_key"] = self.google_api_key
        elif provider == "deepseek":
            config["api_key"] = self.deepseek_api_key
            config["base_url"] = "https://api.deepseek.com"
        elif provider == "ollama":
            config["base_url"] = self.ollama_base_url

        return config

    def get_db_config(self) -> dict[str, Any]:
        """DB設定を取得.

        Returns:
            DB設定辞書（backend, url）
        """
        if self.supabase_url:
            return {"backend": "supabase", "url": self.supabase_url, "key": self.supabase_key}
        if self.turso_url:
            return {"backend": "turso", "url": self.turso_url}
        if self.database_url:
            backend = "postgresql" if "postgres" in self.database_url else "sqlite"
            return {"backend": backend, "url": self.database_url}
        return {"backend": "memory", "url": None}

    def get_vectordb_config(self) -> dict[str, Any]:
        """VectorDB設定を取得.

        Returns:
            VectorDB設定辞書
        """
        if self.pinecone_api_key:
            return {"backend": "pinecone", "api_key": self.pinecone_api_key, "index": self.pinecone_index}
        if self.qdrant_url:
            return {"backend": "qdrant", "url": self.qdrant_url}
        if self.chroma_persist_dir:
            return {"backend": "chroma", "persist_dir": self.chroma_persist_dir, "collection": self.chroma_collection}
        return {"backend": "memory"}

    def get_knowledge_config(self) -> dict[str, Any]:
        """知識ストア設定を取得.

        Returns:
            知識ストア設定辞書（backend, storage_path, auto_persist）
        """
        return {
            "backend": self.knowledge_backend,
            "storage_path": self.knowledge_storage_path,
            "auto_persist": self.knowledge_auto_persist,
        }

    def configure_logging(self) -> None:
        """ログ設定を適用."""
        logging.basicConfig(
            level=getattr(logging, self.log_level.upper()),
            format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        )


@lru_cache
def get_settings() -> AgentFlowSettings:
    """設定シングルトンを取得.

    この関数は設定をキャッシュし、アプリケーション全体で同じインスタンスを返します。
    優先順位: 環境変数 > .env > デフォルト値

    Returns:
        AgentFlow設定

    使用例:
        ```python
        from agentflow.config import get_settings

        settings = get_settings()
        print(settings.get_active_provider())  # "openai"
        print(settings.get_active_model())  # "gpt-5.2"
        ```
    """
    settings = AgentFlowSettings()
    settings.configure_logging()
    return settings


def clear_settings_cache() -> None:
    """設定キャッシュをクリア（テスト用）."""
    get_settings.cache_clear()

