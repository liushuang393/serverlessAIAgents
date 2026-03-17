"""A2A AgentCard モデル.

このモジュールは A2A プロトコルの AgentCard を管理するための Pydantic モデルを提供します。
python_a2a SDK の AgentCard 仕様に準拠したフィールドを持ちます。
"""

from typing import Any

from pydantic import BaseModel, Field


class AgentSkill(BaseModel):
    """エージェントのスキル定義.

    Attributes:
        id: スキル ID（一意識別子）
        name: スキル名
        description: スキルの説明
        input_schema: 入力スキーマ (JSON Schema)
        output_schema: 出力スキーマ (JSON Schema)
        tags: 検索用タグ
        examples: 入出力の使用例
    """

    id: str | None = Field(default=None, description="スキル ID（省略時は name を使用）")
    name: str = Field(..., description="スキル名")
    description: str = Field(..., description="スキルの説明")
    input_schema: dict[str, Any] = Field(default_factory=dict, description="入力スキーマ")
    output_schema: dict[str, Any] = Field(default_factory=dict, description="出力スキーマ")
    tags: list[str] = Field(default_factory=list, description="検索用タグ")
    examples: list[dict[str, Any]] | None = Field(default=None, description="使用例")

    @property
    def effective_id(self) -> str:
        """実効 ID（id が未設定なら name を返す）."""
        return self.id if self.id else self.name


class AgentCapabilities(BaseModel):
    """エージェントの能力宣言.

    クライアントが Agent の機能を事前に判断するための情報。
    """

    streaming: bool = Field(default=False, description="ストリーミング対応")
    push_notifications: bool = Field(default=False, description="プッシュ通知対応")
    state_transition_history: bool = Field(default=False, description="状態遷移履歴対応")


class AgentProvider(BaseModel):
    """エージェントの提供者情報."""

    organization: str = Field(..., description="組織名")
    url: str | None = Field(default=None, description="組織 URL")


class AgentCard(BaseModel):
    """A2A AgentCard.

    python_a2a SDK の AgentCard 仕様に準拠。
    既存フィールドはそのまま維持し、新規フィールドは全て Optional。
    """

    # 既存フィールド（後方互換）
    name: str = Field(..., description="エージェント名")
    description: str = Field(..., description="エージェントの説明")
    version: str = Field(default="1.0.0", description="エージェントのバージョン")
    author: str | None = Field(default=None, description="エージェントの作成者")
    skills: list[AgentSkill] = Field(default_factory=list, description="エージェントのスキルリスト")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加のメタデータ")

    # A2A SDK 準拠フィールド（新規、全て Optional）
    url: str | None = Field(default=None, description="エージェントエンドポイント URL")
    capabilities: AgentCapabilities = Field(
        default_factory=AgentCapabilities,
        description="エージェントの能力宣言",
    )
    default_input_modes: list[str] = Field(
        default_factory=lambda: ["application/json"],
        description="デフォルト入力 MIME タイプ",
    )
    default_output_modes: list[str] = Field(
        default_factory=lambda: ["application/json"],
        description="デフォルト出力 MIME タイプ",
    )
    protocol_version: str = Field(default="0.3.0", description="A2A プロトコルバージョン")
    provider: AgentProvider | None = Field(default=None, description="提供者情報")
    preferred_transport: str | None = Field(default=None, description="優先トランスポート")
    security_schemes: dict[str, Any] | None = Field(
        default=None,
        description="セキュリティスキーム定義",
    )
    documentation_url: str | None = Field(default=None, description="ドキュメント URL")

    @classmethod
    def from_yaml(cls, yaml_data: dict[str, Any]) -> "AgentCard":
        """YAML データから AgentCard を作成.

        Args:
            yaml_data: agent.yaml から読み込んだデータ

        Returns:
            AgentCard インスタンス
        """
        skills = [
            AgentSkill(
                id=skill.get("id"),
                name=skill.get("name", ""),
                description=skill.get("description", ""),
                input_schema=skill.get("input_schema", {}),
                output_schema=skill.get("output_schema", {}),
                tags=skill.get("tags", []),
                examples=skill.get("examples"),
            )
            for skill in yaml_data.get("skills", [])
        ]

        # capabilities の解析
        caps_data = yaml_data.get("capabilities", {})
        capabilities = AgentCapabilities(**caps_data) if caps_data else AgentCapabilities()

        # provider の解析
        provider_data = yaml_data.get("provider")
        provider = AgentProvider(**provider_data) if provider_data else None

        return cls(
            name=yaml_data.get("name", ""),
            description=yaml_data.get("description", ""),
            version=yaml_data.get("version", "1.0.0"),
            author=yaml_data.get("author"),
            skills=skills,
            metadata=yaml_data.get("metadata", {}),
            url=yaml_data.get("url"),
            capabilities=capabilities,
            default_input_modes=yaml_data.get("default_input_modes", ["application/json"]),
            default_output_modes=yaml_data.get("default_output_modes", ["application/json"]),
            protocol_version=yaml_data.get("protocol_version", "0.3.0"),
            provider=provider,
            preferred_transport=yaml_data.get("preferred_transport"),
            security_schemes=yaml_data.get("security_schemes"),
            documentation_url=yaml_data.get("documentation_url"),
        )

    def to_a2a_format(self) -> dict[str, Any]:
        """A2A プロトコル形式に変換.

        Returns:
            A2A AgentCard 形式の辞書
        """
        result: dict[str, Any] = {
            "name": self.name,
            "description": self.description,
            "version": self.version,
            "protocolVersion": self.protocol_version,
            "capabilities": {
                "streaming": self.capabilities.streaming,
                "pushNotifications": self.capabilities.push_notifications,
                "stateTransitionHistory": self.capabilities.state_transition_history,
            },
            "defaultInputModes": self.default_input_modes,
            "defaultOutputModes": self.default_output_modes,
            "skills": [
                {
                    "id": skill.effective_id,
                    "name": skill.name,
                    "description": skill.description,
                    "inputSchema": skill.input_schema,
                    "outputSchema": skill.output_schema,
                    "tags": skill.tags,
                }
                for skill in self.skills
            ],
            "metadata": self.metadata,
        }

        if self.url:
            result["url"] = self.url
        if self.author:
            result["author"] = self.author
        if self.provider:
            result["provider"] = {
                "organization": self.provider.organization,
                "url": self.provider.url,
            }
        if self.preferred_transport:
            result["preferredTransport"] = self.preferred_transport
        if self.security_schemes:
            result["securitySchemes"] = self.security_schemes
        if self.documentation_url:
            result["documentationUrl"] = self.documentation_url

        return result
