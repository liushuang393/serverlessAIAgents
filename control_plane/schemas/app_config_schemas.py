"""app_config.json Pydantic スキーマ.

全 App 共通マニフェストの型定義とバリデーション。
Platform の AppDiscoveryService がこのスキーマを使って
apps/*/app_config.json を検証する。

使用例:
    >>> from control_plane.schemas.app_config_schemas import AppConfig
    >>> config = AppConfig.model_validate_json(path.read_text())
"""

from __future__ import annotations

import re
from typing import Annotated, Any, Literal

from pydantic import BaseModel, Field, field_validator, model_validator

from infrastructure.llm.contracts import LLMContractsConfig
from control_plane.schemas.capability_schemas import CapabilitySpec


# CapabilitySpec または レガシーフラット文字列の Union 型
CapabilityItem = Annotated[CapabilitySpec | str, Field(union_mode="left_to_right")]


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
        class_name: Agent 実装クラス名（module と組で executable contract を構成）
        init_kwargs: Agent 初期化引数
        capabilities: 能力タグ一覧
        business_base: Agent の業務基盤分類（省略時は推論）
        agent_type: Agent タイプ分類（省略時は推論）
        pattern: Agent パターン分類（互換フィールド、省略時は推論）
    """

    name: str = Field(..., min_length=1, max_length=100, description="Agent 名")
    module: str | None = Field(default=None, description="Python モジュールパス")
    class_name: str | None = Field(default=None, description="Python クラス名")
    init_kwargs: dict[str, Any] = Field(default_factory=dict, description="Agent 初期化引数")
    capabilities: list[CapabilityItem] = Field(
        default_factory=list,
        description=('能力タグ。レガシーフラット文字列（"rag"）または 3層構造 CapabilitySpec オブジェクトを混在可能。'),
    )
    business_base: str | None = Field(
        default=None,
        description="業務基盤分類（knowledge / governance など）",
    )
    agent_type: str | None = Field(
        default=None,
        description="Agent タイプ分類（specialist / planner など）",
    )
    pattern: str | None = Field(
        default=None,
        description="Agent パターン分類（互換）",
    )

    @field_validator("business_base", "agent_type", "pattern", mode="before")
    @classmethod
    def normalize_optional_label(cls, v: str | None) -> str | None:
        """任意分類文字列を正規化（空文字は None）."""
        if v is None:
            return None
        text = str(v).strip().lower()
        return text or None

    @field_validator("class_name", mode="before")
    @classmethod
    def normalize_class_name(cls, v: str | None) -> str | None:
        """クラス名を正規化（空文字は None）."""
        if v is None:
            return None
        text = str(v).strip()
        return text or None


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

    api_module: str | None = Field(default=None, description="FastAPI モジュールパス")
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
    external: list[str] = Field(default_factory=list, description="外部依存サービス")


class RuntimeUrlsConfig(BaseModel):
    """ランタイムURL設定."""

    backend: str | None = Field(default=None, description="バックエンドURL")
    frontend: str | None = Field(default=None, description="フロントエンドURL")
    health: str | None = Field(default=None, description="ヘルスチェックURL")
    database: str | None = Field(default=None, description="DB 接続URL（表示用）")


class RuntimeHostsConfig(BaseModel):
    """ランタイム bind host 設定."""

    backend: str | None = Field(default=None, description="バックエンド bind host")
    frontend: str | None = Field(default=None, description="フロントエンド bind host")


class RuntimeDatabaseConfig(BaseModel):
    """ランタイムDB接続設定."""

    kind: str | None = Field(default=None, description="DB 種別")
    url: str | None = Field(default=None, description="接続URL")
    host: str | None = Field(default=None, description="ホスト")
    port: int | None = Field(default=None, description="ポート")
    name: str | None = Field(default=None, description="DB 名")
    user: str | None = Field(default=None, description="ユーザー名")
    password: str | None = Field(default=None, description="パスワード（ローカル開発用のみ）")
    password_env: str | None = Field(default=None, description="パスワード環境変数名")
    note: str | None = Field(default=None, description="補足")

    @field_validator("port", mode="before")
    @classmethod
    def validate_port_range(cls, v: int | None) -> int | None:
        """ポート番号が有効範囲内か検証."""
        if v is not None and not (_PORT_MIN <= v <= _PORT_MAX):
            msg = f"ポート番号は {_PORT_MIN}〜{_PORT_MAX} の範囲: {v}"
            raise ValueError(msg)
        return v


class RuntimeCommandsConfig(BaseModel):
    """ランタイムコマンド設定."""

    backend_dev: str | None = Field(default=None, description="バックエンド開発起動コマンド")
    frontend_dev: str | None = Field(default=None, description="フロントエンド開発起動コマンド")
    publish: str | None = Field(default=None, description="publish コマンド")
    start: str | None = Field(default=None, description="start コマンド")
    stop: str | None = Field(default=None, description="stop コマンド")


class RuntimeCLIAuthConfig(BaseModel):
    """CLI 認証設定."""

    status: list[str] | None = Field(default=None, description="認証状態確認コマンド")
    api_key_env: str | None = Field(default=None, description="API キー環境変数名")
    api_key_login: list[str] | None = Field(default=None, description="API キーでログインするコマンド")
    interactive_login: list[str] | None = Field(default=None, description="手動ログインコマンド")


class RuntimeCLIToolConfig(BaseModel):
    """CLI ツール設定."""

    executable: str | None = Field(default=None, description="CLI 実行ファイル名")
    install_commands: list[list[str]] = Field(default_factory=list, description="インストールコマンド配列")
    auth: RuntimeCLIAuthConfig = Field(default_factory=RuntimeCLIAuthConfig, description="認証設定")
    diagnostic_mode: Literal["read_only", "plan"] = Field(default="read_only", description="診断実行モード")
    diagnostic_command: list[str] | None = Field(default=None, description="診断実行コマンドテンプレート")


class RuntimeCLIConfig(BaseModel):
    """Runtime CLI 設定."""

    preferred: list[Literal["codex", "claude"]] = Field(
        default_factory=lambda: ["codex", "claude"],
        description="診断時に優先する CLI 順序",
    )
    codex: RuntimeCLIToolConfig = Field(default_factory=RuntimeCLIToolConfig, description="Codex CLI 設定")
    claude: RuntimeCLIToolConfig = Field(default_factory=RuntimeCLIToolConfig, description="Claude Code CLI 設定")


class RuntimeConfig(BaseModel):
    """ランタイム設定."""

    urls: RuntimeUrlsConfig = Field(default_factory=RuntimeUrlsConfig, description="URL 設定")
    hosts: RuntimeHostsConfig = Field(default_factory=RuntimeHostsConfig, description="bind host 設定")
    database: RuntimeDatabaseConfig = Field(
        default_factory=RuntimeDatabaseConfig,
        description="DB 接続設定",
    )
    commands: RuntimeCommandsConfig = Field(
        default_factory=RuntimeCommandsConfig,
        description="起動/停止コマンド設定",
    )
    cli: RuntimeCLIConfig = Field(
        default_factory=RuntimeCLIConfig,
        description="CLI 自動セットアップ/診断設定",
    )


class AuthContractConfig(BaseModel):
    """認証契約設定."""

    enabled: bool = Field(default=False, description="認証機能を有効化")
    providers: list[str] = Field(default_factory=list, description="認証プロバイダ")
    allow_anonymous: bool = Field(default=True, description="匿名アクセス許可")
    required_scopes: list[str] = Field(default_factory=list, description="必須スコープ")
    mode: str = Field(default="tenant_sso", description="認証モード（tenant_sso / enterprise_isolated）")
    tenant_claim_key: str = Field(default="tenant_id", description="JWT テナント claim キー")
    allow_same_tenant_sso: bool = Field(default=False, description="同一テナント内の跨 app SSO を許可")
    token_policy: dict[str, Any] = Field(default_factory=dict, description="トークンポリシー詳細")
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
    agent_type: str | None = Field(default=None, description="Agent タイプ分類")
    prompt: str = Field(default="", description="Agent 個別プロンプト")
    capabilities: list[str] = Field(default_factory=list, description="能力タグ")

    @field_validator("role", mode="before")
    @classmethod
    def normalize_role_field(cls, v: str | None) -> str:
        """role を正規化."""
        if v is None:
            return "specialist"
        text = str(v).strip().lower()
        return text or "specialist"

    @field_validator("agent_type", mode="before")
    @classmethod
    def normalize_agent_type_field(cls, v: str | None) -> str | None:
        """agent_type を正規化."""
        if v is None:
            return None
        text = str(v).strip().lower()
        return text or None


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
    app_template: str | None = Field(default=None, description="アプリテンプレート ID")
    agents: list[AgentBlueprintConfig] = Field(
        default_factory=list,
        description="Agent 設計メモ",
    )

    @field_validator("flow_pattern", "llm_provider", "app_template", mode="before")
    @classmethod
    def normalize_blueprint_labels(cls, v: str | None) -> str | None:
        """Blueprint ラベル系フィールドを正規化."""
        if v is None:
            return None
        text = str(v).strip().lower()
        return text or None


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


class PluginBindingConfig(BaseModel):
    """プラグインバインディング設定."""

    id: str = Field(..., min_length=1, max_length=120, description="プラグインID")
    version: str = Field(..., min_length=1, max_length=50, description="要求バージョン")
    config: dict[str, Any] = Field(default_factory=dict, description="プラグイン設定")

    @field_validator("id", mode="before")
    @classmethod
    def normalize_plugin_id(cls, v: str) -> str:
        """plugin id を正規化（空白除去・小文字化）."""
        text = str(v).strip().lower()
        if not text:
            msg = "plugin id は空文字を許可しません"
            raise ValueError(msg)
        return text

    @field_validator("version")
    @classmethod
    def validate_plugin_version(cls, v: str) -> str:
        """plugin version が SemVer 形式であることを検証."""
        if not _SEMVER_PATTERN.match(v):
            msg = f"plugin version は SemVer 形式である必要があります: '{v}'"
            raise ValueError(msg)
        return v


class EvolutionValidatorQueueConfig(BaseModel):
    """Evolution validator queue settings."""

    backend: Literal["redis_stream", "none"] = Field(
        default="redis_stream",
        description="バリデーターキュー backend",
    )
    redis_url: str | None = Field(default=None, description="Redis 接続URL")
    stream_key: str = Field(default="evolution:validate:stream", description="Redis stream key")
    consumer_group: str = Field(
        default="evolution-validator-v1",
        description="Redis consumer group",
    )
    max_retries: int = Field(default=5, ge=0, le=20, description="再試行上限")


class EvolutionRetrievalConfig(BaseModel):
    """Evolution retrieval decision thresholds."""

    high_confidence_skip_threshold: float = Field(default=0.82, ge=0.0, le=1.0)
    high_complexity_threshold: float = Field(default=0.70, ge=0.0, le=1.0)
    low_confidence_threshold: float = Field(default=0.55, ge=0.0, le=1.0)


class EvolutionSuspicionConfig(BaseModel):
    """Evolution suspicion trigger settings."""

    max_age_days: int = Field(default=30, ge=1, le=3650)
    failure_streak_threshold: int = Field(default=2, ge=1, le=20)
    performance_drop_ratio: float = Field(default=0.2, ge=0.01, le=1.0)


class EvolutionConfig(BaseModel):
    """Evolution V2 app-level config."""

    enabled: bool = Field(default=True, description="進化ループを有効化")
    strategy_service_url: str | None = Field(default=None, description="Strategy Service URL")
    validator_queue: EvolutionValidatorQueueConfig = Field(default_factory=EvolutionValidatorQueueConfig)
    scope_policy: list[Literal["tenant_app", "tenant_product_line", "global_verified"]] = Field(
        default_factory=lambda: ["tenant_app", "tenant_product_line", "global_verified"],
        description="戦略検索スコープ順序",
    )
    retrieval: EvolutionRetrievalConfig = Field(default_factory=EvolutionRetrievalConfig)
    suspicion: EvolutionSuspicionConfig = Field(default_factory=EvolutionSuspicionConfig)


class ContractsConfig(BaseModel):
    """プラットフォーム契約セクション."""

    auth: AuthContractConfig = Field(default_factory=AuthContractConfig)
    llm: LLMContractsConfig = Field(default_factory=LLMContractsConfig)
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
        runtime: ランタイムURL/DB/コマンド設定
        contracts: プラットフォーム契約設定
        product_line: 製品主線（migration / faq / assistant / framework）
        surface_profile: UI/操作面プロファイル（business / developer / operator）
        audit_profile: 監査プロファイル（business / developer）
        evolution: Evolution V2 設定
        plugin_bindings: バインド済みプラグイン一覧
        security_mode: セキュリティ動作モード（assistant 向け）
        tags: 検索用タグ
    """

    name: str = Field(..., min_length=1, max_length=50, description="App 識別子")
    display_name: str = Field(..., min_length=1, max_length=100, description="表示名")
    description: str = Field(default="", max_length=500, description="説明文")
    business_base: str | None = Field(
        default=None,
        description="App の業務基盤分類（省略時は推論）",
    )
    version: str = Field(default="1.0.0", description="バージョン")
    icon: str = Field(default="📦", max_length=10, description="絵文字アイコン")
    ports: PortsConfig = Field(default_factory=PortsConfig, description="ポート設定")
    entry_points: EntryPointsConfig = Field(default_factory=EntryPointsConfig, description="エントリーポイント")
    agents: list[AgentInfo] = Field(default_factory=list, description="Agent 一覧")
    services: dict[str, Any] = Field(default_factory=dict, description="利用サービス情報")
    dependencies: DependenciesConfig = Field(default_factory=DependenciesConfig, description="依存設定")
    runtime: RuntimeConfig = Field(
        default_factory=RuntimeConfig,
        description="ランタイム設定",
    )
    contracts: ContractsConfig = Field(
        default_factory=ContractsConfig,
        description="プラットフォーム契約設定",
    )
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
    evolution: EvolutionConfig = Field(
        default_factory=EvolutionConfig,
        description="Evolution V2 設定",
    )
    plugin_bindings: list[PluginBindingConfig] = Field(
        ...,
        description="バインド済みプラグイン",
    )
    security_mode: Literal["read_only", "approval_required", "autonomous"] | None = Field(
        default=None,
        description="セキュリティ実行モード（assistant 向け）",
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

    @field_validator("business_base", mode="before")
    @classmethod
    def normalize_business_base(cls, v: str | None) -> str | None:
        """business_base を正規化（空文字は None）."""
        if v is None:
            return None
        text = str(v).strip().lower()
        return text or None

    @field_validator("product_line", mode="before")
    @classmethod
    def normalize_product_line(cls, v: str) -> str:
        """product_line を小文字へ正規化する."""
        text = str(v).strip().lower()
        if not text:
            msg = "product_line は空文字を許可しません"
            raise ValueError(msg)
        return text

    @field_validator("surface_profile", "audit_profile", mode="before")
    @classmethod
    def normalize_profiles(cls, v: str) -> str:
        """surface/audit profile を小文字へ正規化する."""
        text = str(v).strip().lower()
        if not text:
            msg = "surface_profile / audit_profile は空文字を許可しません"
            raise ValueError(msg)
        return text

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
        """Agent 名重複と assistant の security_mode を検証."""
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

        plugin_seen: set[str] = set()
        plugin_duplicates: set[str] = set()
        for binding in self.plugin_bindings:
            if binding.id in plugin_seen:
                plugin_duplicates.add(binding.id)
            plugin_seen.add(binding.id)
        if plugin_duplicates:
            dup = ", ".join(sorted(plugin_duplicates))
            msg = f"plugin_bindings[].id は App 内で一意である必要があります: {dup}"
            raise ValueError(msg)

        if self.product_line == "assistant" and self.security_mode is None:
            msg = "product_line='assistant' の App では security_mode が必須です"
            raise ValueError(msg)

        return self
