"""Platform プロビジョニング関連スキーマ."""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any, Literal

from pydantic import BaseModel, Field, field_validator, model_validator


_APP_NAME_PATTERN = re.compile(r"^[a-z][a-z0-9_]*$")
_ENV_KEY_PATTERN = re.compile(r"^[A-Z_][A-Z0-9_]*$")


class AgentBlueprintInput(BaseModel):
    """App 作成時の Agent 入力."""

    name: str = Field(..., min_length=1, max_length=100, description="Agent 名")
    role: str = Field(default="specialist", description="Agent ロール")
    prompt: str = Field(default="", description="Agent 個別プロンプト")
    capabilities: list[str] = Field(default_factory=list, description="能力タグ")
    business_base: str | None = Field(default=None, description="業務基盤分類")
    pattern: str | None = Field(default=None, description="Agent pattern")


class PluginBindingInput(BaseModel):
    """App 作成時のプラグインバインド入力."""

    id: str = Field(..., min_length=1, max_length=120, description="プラグインID")
    version: str = Field(..., min_length=1, max_length=50, description="要求バージョン")
    config: dict[str, Any] = Field(default_factory=dict, description="プラグイン設定")


class AppCreateRequest(BaseModel):
    """新規 App 作成リクエスト."""

    name: str = Field(..., min_length=1, max_length=50, description="App 識別子")
    display_name: str = Field(..., min_length=1, max_length=100, description="表示名")
    description: str = Field(default="", max_length=500, description="説明")
    icon: str = Field(default="📦", max_length=10, description="アイコン")
    business_base: Literal[
        "platform",
        "knowledge",
        "reasoning",
        "interaction",
        "integration",
        "operations",
        "governance",
        "media",
        "custom",
    ] = Field(default="custom", description="業務基盤分類")
    product_line: Literal["migration", "faq", "assistant", "framework"] = Field(
        ...,
        description="製品主線",
    )
    surface_profile: Literal["business", "developer", "operator"] = Field(
        ...,
        description="表示/操作プロファイル",
    )
    audit_profile: Literal["business", "developer"] = Field(
        ...,
        description="監査プロファイル",
    )
    security_mode: Literal["read_only", "approval_required", "autonomous"] | None = Field(
        default=None,
        description="セキュリティ実行モード（assistant 向け）",
    )
    plugin_bindings: list[PluginBindingInput] = Field(
        ...,
        description="バインド済みプラグイン",
    )
    template: str | None = Field(default=None, description="Business 作成向けテンプレートID")
    data_sources: list[str] = Field(
        default_factory=list, description="Business 作成向けデータソース"
    )
    permission_scopes: list[str] = Field(
        default_factory=list,
        description="Business 作成向け権限スコープ",
    )
    risk_level: Literal["low", "medium", "high"] | None = Field(
        default=None,
        description="Business 作成向けリスクレベル",
    )

    engine_pattern: Literal[
        "simple",
        "flow",
        "pipeline",
        "coordinator",
        "deep_agent",
    ] = Field(default="flow", description="AgentFlow エンジンパターン")
    flow_pattern: str | None = Field(default=None, description="フローパターン名")
    system_prompt: str = Field(default="", description="システムプロンプト")

    database: Literal["none", "sqlite", "postgresql"] = Field(
        default="postgresql",
        description="データベース種別",
    )
    vector_database: Literal[
        "none",
        "qdrant",
        "pinecone",
        "weaviate",
        "pgvector",
        "milvus",
    ] = Field(default="none", description="ベクトルDB種別")
    frontend_enabled: bool = Field(default=True, description="Frontend を生成")
    redis_enabled: bool = Field(default=False, description="Redis を有効化")

    rag_enabled: bool = Field(default=False, description="RAG を有効化")
    default_skills: list[str] = Field(default_factory=list, description="初期 Skills")
    mcp_servers: list[str] = Field(default_factory=list, description="利用 MCP サーバー名")

    llm_provider: Literal[
        "auto",
        "openai",
        "anthropic",
        "gemini",
        "azure_openai",
        "ollama",
        "openrouter",
        "custom",
    ] = Field(default="auto", description="LLM プロバイダー")
    default_model: str | None = Field(default=None, description="既定モデル")
    llm_base_url: str | None = Field(default=None, description="LLM ベース URL")
    llm_api_key: str | None = Field(default=None, description="LLM API キー")
    llm_api_key_env: str | None = Field(default=None, description="LLM API キーの env 名")

    vector_db_url: str | None = Field(default=None, description="VectorDB URL")
    vector_db_collection: str | None = Field(default=None, description="VectorDB 既定コレクション")
    vector_db_api_key: str | None = Field(default=None, description="VectorDB API キー")
    vector_db_api_key_env: str | None = Field(
        default=None, description="VectorDB API キーの env 名"
    )

    write_framework_env: bool = Field(
        default=True,
        description="フレームワークのローカル env ファイルへ反映",
    )
    framework_env_file: str = Field(
        default=".env",
        description="反映先 env ファイル（リポジトリルート相対）",
    )

    tenant_visibility_mode: Literal[
        "private",
        "public",
        "tenant_allowlist",
    ] = Field(default="private", description="可視性モード")
    tenant_ids: list[str] = Field(default_factory=list, description="許可テナント ID")

    agents: list[AgentBlueprintInput] = Field(default_factory=list, description="初期 Agent 構成")

    @field_validator("name")
    @classmethod
    def validate_name(cls, value: str) -> str:
        """App 名が snake_case であることを検証."""
        if not _APP_NAME_PATTERN.match(value):
            msg = f"App 名は snake_case (^[a-z][a-z0-9_]*$): '{value}'"
            raise ValueError(msg)
        return value

    @field_validator("product_line", "surface_profile", "audit_profile", mode="before")
    @classmethod
    def normalize_profile_fields(cls, value: str) -> str:
        """分類フィールドを小文字へ正規化する."""
        return str(value).strip().lower()

    @field_validator("llm_api_key_env", "vector_db_api_key_env")
    @classmethod
    def validate_env_key(cls, value: str | None) -> str | None:
        """env キー名を検証."""
        if value is None or value == "":
            return None
        upper = value.strip().upper()
        if not _ENV_KEY_PATTERN.match(upper):
            msg = f"env キーは ^[A-Z_][A-Z0-9_]*$ に一致する必要があります: '{value}'"
            raise ValueError(msg)
        return upper

    @field_validator("framework_env_file")
    @classmethod
    def validate_env_file(cls, value: str) -> str:
        """env ファイルパスを検証."""
        trimmed = value.strip() or ".env"
        p = Path(trimmed)
        if p.is_absolute():
            msg = "framework_env_file は相対パスで指定してください"
            raise ValueError(msg)
        if ".." in p.parts:
            msg = "framework_env_file に '..' は使用できません"
            raise ValueError(msg)
        return trimmed

    @model_validator(mode="after")
    def validate_vector_rag_relation(self) -> AppCreateRequest:
        """RAG と vector DB の整合性を検証."""
        if self.rag_enabled and self.vector_database == "none":
            msg = "rag_enabled=true の場合は vector_database を選択してください"
            raise ValueError(msg)
        if self.product_line == "assistant" and self.security_mode is None:
            msg = "product_line='assistant' の App 作成では security_mode が必須です"
            raise ValueError(msg)
        return self


class PortConflictItem(BaseModel):
    """ポート重複の 1 件."""

    port_type: Literal["api", "frontend", "db", "redis"]
    port: int
    apps: list[str]


class PortConflictReport(BaseModel):
    """ポート重複レポート."""

    has_conflicts: bool
    conflicts: list[PortConflictItem] = Field(default_factory=list)


class MCPServerUpsertRequest(BaseModel):
    """MCP サーバー追加・更新リクエスト."""

    name: str = Field(..., min_length=1, max_length=100, description="サーバー名")
    command: str = Field(..., min_length=1, description="起動コマンド")
    args: list[str] = Field(default_factory=list, description="引数")
    env: dict[str, str] = Field(default_factory=dict, description="環境変数")
    enabled: bool = Field(default=True, description="有効化")
    description: str = Field(default="", description="説明")


class MCPLazyLoadingPatchRequest(BaseModel):
    """MCP lazy_loading 更新リクエスト."""

    enabled: bool | None = Field(default=None, description="lazy loading 有効化")
    threshold: int | None = Field(default=None, ge=1, description="閾値")
    auto_load_on_call: bool | None = Field(default=None, description="自動ロード")
    cache_session: bool | None = Field(default=None, description="セッションキャッシュ")


class AppCreateResponse(BaseModel):
    """新規 App 作成レスポンス."""

    success: bool
    app_name: str
    app_dir: str
    config_path: str
    ports: dict[str, int | None]
    files_created: list[str]
    framework_env_file: str | None = None
    framework_env_updated_keys: list[str] = Field(default_factory=list)
    app_config: dict[str, Any]
