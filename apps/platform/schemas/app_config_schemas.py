# -*- coding: utf-8 -*-
"""app_config.json Pydantic スキーマ.

全 App 共通マニフェストの型定義とバリデーション。
Platform の AppDiscoveryService がこのスキーマを使って
apps/*/app_config.json を検証する。

使用例:
    >>> from apps.platform.schemas.app_config_schemas import AppConfig
    >>> config = AppConfig.model_validate_json(path.read_text())
"""

from __future__ import annotations

import re
from typing import Any, Literal

from pydantic import BaseModel, Field, field_validator, model_validator


# 正規表現: snake_case の App 名（先頭は英小文字）
_APP_NAME_PATTERN = re.compile(r"^[a-z][a-z0-9_]*$")

# 正規表現: SemVer (MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD])
_SEMVER_PATTERN = re.compile(
    r"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)"
    r"(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?$"
)

# ポート番号の有効範囲
_PORT_MIN = 1024
_PORT_MAX = 65535


class AgentInfo(BaseModel):
    """Agent メタデータ.

    Attributes:
        name: Agent 名（App 内で一意）
        module: Python モジュールパス（省略可）
        capabilities: 能力タグ一覧
    """

    name: str = Field(..., min_length=1, max_length=100, description="Agent 名")
    module: str | None = Field(default=None, description="Python モジュールパス")
    capabilities: list[str] = Field(
        default_factory=list, description="能力タグ"
    )


class PortsConfig(BaseModel):
    """ポート設定.

    Attributes:
        api: API サーバーポート
        frontend: フロントエンドポート
        db: データベースポート
        redis: Redis ポート
    """

    api: int | None = Field(default=None, description="API ポート")
    frontend: int | None = Field(default=None, description="フロントエンドポート")
    db: int | None = Field(default=None, description="DB ポート")
    redis: int | None = Field(default=None, description="Redis ポート")

    @field_validator("api", "frontend", "db", "redis", mode="before")
    @classmethod
    def validate_port_range(cls, v: int | None) -> int | None:
        """ポート番号が有効範囲内か検証."""
        if v is not None and not (_PORT_MIN <= v <= _PORT_MAX):
            msg = f"ポート番号は {_PORT_MIN}〜{_PORT_MAX} の範囲: {v}"
            raise ValueError(msg)
        return v


class EntryPointsConfig(BaseModel):
    """エントリーポイント設定.

    Attributes:
        api_module: FastAPI モジュールパス（例: "apps.faq_system.main:app"）
        health: ヘルスチェックパス
    """

    api_module: str | None = Field(
        default=None, description="FastAPI モジュールパス"
    )
    health: str | None = Field(default="/health", description="ヘルスチェックパス")


class DependenciesConfig(BaseModel):
    """依存設定.

    Attributes:
        database: DB 種別（postgresql / sqlite / None）
        redis: Redis 使用有無
        external: 外部依存サービス名
    """

    database: str | None = Field(default=None, description="DB 種別")
    redis: bool = Field(default=False, description="Redis 使用有無")
    external: list[str] = Field(
        default_factory=list, description="外部依存サービス"
    )


class AuthContractConfig(BaseModel):
    """認証契約設定."""

    enabled: bool = Field(default=False, description="認証機能を有効化")
    providers: list[str] = Field(default_factory=list, description="認証プロバイダ")
    allow_anonymous: bool = Field(default=True, description="匿名アクセス許可")
    required_scopes: list[str] = Field(default_factory=list, description="必須スコープ")
    session_ttl_minutes: int = Field(
        default=60,
        ge=5,
        le=10080,
        description="セッション有効期限（分）",
    )


class RAGContractConfig(BaseModel):
    """RAG 契約設定."""

    enabled: bool = Field(default=False, description="RAG機能を有効化")
    pattern: str | None = Field(default=None, description="RAG 設定パターン")
    provider: str | None = Field(default=None, description="RAGプロバイダ")
    collections: list[str] = Field(default_factory=list, description="既定コレクション")
    data_sources: list[dict[str, Any]] = Field(default_factory=list, description="データソース定義")
    chunk_strategy: str = Field(default="recursive", description="チャンク分割方式")
    chunk_size: int = Field(default=800, ge=100, le=8000, description="チャンクサイズ")
    chunk_overlap: int = Field(default=120, ge=0, le=2000, description="チャンク重複サイズ")
    retrieval_method: str = Field(default="hybrid", description="取得方式")
    embedding_model: str | None = Field(default=None, description="埋め込みモデル")
    rerank_model: str | None = Field(default=None, description="リランクモデル")
    default_top_k: int = Field(default=5, ge=1, le=100, description="既定TopK")
    score_threshold: float | None = Field(default=None, ge=0.0, le=1.0, description="スコア閾値")
    indexing_schedule: str | None = Field(default=None, description="インデックス更新スケジュール")


class SkillsContractConfig(BaseModel):
    """Skills 契約設定."""

    auto_install: bool = Field(default=False, description="起動時の自動インストール")
    hot_reload: bool = Field(default=True, description="ホットリロード有効化")
    allowed_sources: list[str] = Field(default_factory=list, description="許可ソース")
    default_skills: list[str] = Field(default_factory=list, description="既定スキル")


class AgentBlueprintConfig(BaseModel):
    """Agent 設計メモ（作成ウィザード用）."""

    name: str = Field(..., min_length=1, max_length=100, description="Agent 名")
    role: str = Field(default="specialist", description="Agent ロール")
    prompt: str = Field(default="", description="Agent 個別プロンプト")
    capabilities: list[str] = Field(default_factory=list, description="能力タグ")


class BlueprintConfig(BaseModel):
    """AgentFlow 設計ブループリント.

    App 作成時の設計意図（Engine パターン、初期プロンプト等）を保持する。
    """

    engine_pattern: str = Field(
        default="flow",
        description="エンジンパターン (simple / flow / pipeline / coordinator / deep_agent)",
    )
    flow_pattern: str | None = Field(default=None, description="フローパターン名")
    system_prompt: str = Field(default="", description="システムプロンプト")
    llm_provider: str | None = Field(default=None, description="LLM プロバイダー")
    llm_base_url: str | None = Field(default=None, description="LLM Base URL")
    llm_api_key_env: str | None = Field(default=None, description="LLM API キー env 名")
    default_model: str | None = Field(default=None, description="既定モデル")
    default_skills: list[str] = Field(default_factory=list, description="既定スキル")
    vector_db_provider: str | None = Field(default=None, description="VectorDB プロバイダー")
    vector_db_url: str | None = Field(default=None, description="VectorDB URL")
    vector_db_collection: str | None = Field(default=None, description="VectorDB 既定コレクション")
    vector_db_api_key_env: str | None = Field(default=None, description="VectorDB API キー env 名")
    mcp_servers: list[str] = Field(default_factory=list, description="利用するMCPサーバー名")
    agents: list[AgentBlueprintConfig] = Field(
        default_factory=list,
        description="Agent 設計メモ",
    )


class VisibilityConfig(BaseModel):
    """テナント可視性設定."""

    mode: Literal["private", "public", "tenant_allowlist"] = Field(
        default="private",
        description="公開範囲",
    )
    tenants: list[str] = Field(default_factory=list, description="許可テナントID")


class ReleaseContractConfig(BaseModel):
    """リリース契約設定."""

    strategy: str = Field(default="manual", description="リリース戦略")
    targets: list[str] = Field(default_factory=list, description="配備ターゲット")
    environments: list[str] = Field(
        default_factory=lambda: ["dev"],
        description="配備環境",
    )
    require_approval: bool = Field(default=True, description="承認必須")


class ContractsConfig(BaseModel):
    """プラットフォーム契約セクション."""

    auth: AuthContractConfig = Field(default_factory=AuthContractConfig)
    rag: RAGContractConfig = Field(default_factory=RAGContractConfig)
    skills: SkillsContractConfig = Field(default_factory=SkillsContractConfig)
    release: ReleaseContractConfig = Field(default_factory=ReleaseContractConfig)


class AppConfig(BaseModel):
    """app_config.json のルートスキーマ.

    各 App ディレクトリに配置するマニフェストファイルの型定義。

    Attributes:
        name: App 識別子（snake_case、ディレクトリ名と一致）
        display_name: UI 表示用の名前
        description: App の説明文
        version: セマンティックバージョニング
        icon: 絵文字アイコン
        ports: ポート設定
        entry_points: エントリーポイント設定
        agents: Agent メタデータ一覧
        services: 利用サービス情報（自由形式）
        dependencies: 依存設定
        contracts: プラットフォーム契約設定
        tags: 検索用タグ
    """

    name: str = Field(
        ..., min_length=1, max_length=50, description="App 識別子"
    )
    display_name: str = Field(
        ..., min_length=1, max_length=100, description="表示名"
    )
    description: str = Field(default="", max_length=500, description="説明文")
    version: str = Field(default="1.0.0", description="バージョン")
    icon: str = Field(default="📦", max_length=10, description="絵文字アイコン")
    ports: PortsConfig = Field(
        default_factory=PortsConfig, description="ポート設定"
    )
    entry_points: EntryPointsConfig = Field(
        default_factory=EntryPointsConfig, description="エントリーポイント"
    )
    agents: list[AgentInfo] = Field(
        default_factory=list, description="Agent 一覧"
    )
    services: dict[str, Any] = Field(
        default_factory=dict, description="利用サービス情報"
    )
    dependencies: DependenciesConfig = Field(
        default_factory=DependenciesConfig, description="依存設定"
    )
    contracts: ContractsConfig = Field(
        default_factory=ContractsConfig,
        description="プラットフォーム契約設定",
    )
    blueprint: BlueprintConfig = Field(
        default_factory=BlueprintConfig,
        description="AgentFlow 設計ブループリント",
    )
    visibility: VisibilityConfig = Field(
        default_factory=VisibilityConfig,
        description="テナント可視性設定",
    )
    tags: list[str] = Field(default_factory=list, description="検索用タグ")

    @field_validator("name")
    @classmethod
    def validate_app_name(cls, v: str) -> str:
        """App 名が snake_case であることを検証."""
        if not _APP_NAME_PATTERN.match(v):
            msg = f"App 名は snake_case (^[a-z][a-z0-9_]*$): '{v}'"
            raise ValueError(msg)
        return v

    @field_validator("version")
    @classmethod
    def validate_version(cls, v: str) -> str:
        """バージョンが SemVer 形式であることを検証."""
        if not _SEMVER_PATTERN.match(v):
            msg = f"version は SemVer 形式である必要があります: '{v}'"
            raise ValueError(msg)
        return v

    @model_validator(mode="after")
    def validate_unique_agent_names(self) -> AppConfig:
        """Agent 名の重複を検証."""
        seen: set[str] = set()
        duplicates: set[str] = set()
        for agent in self.agents:
            if agent.name in seen:
                duplicates.add(agent.name)
            seen.add(agent.name)

        if duplicates:
            dup = ", ".join(sorted(duplicates))
            msg = f"agents[].name は App 内で一意である必要があります: {dup}"
            raise ValueError(msg)

        return self
