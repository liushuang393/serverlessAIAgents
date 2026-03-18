"""エージェントメタデータスキーマ定義 — kernel 層.

agent.yaml ファイルのスキーマを Pydantic モデルとして定義する。
legacy core surface/metadata.py から移行。
"""

from typing import Any

from pydantic import BaseModel, Field


class MetaInfo(BaseModel):
    """エージェントのメタ情報.

    Attributes:
        id: 一意識別子 (kebab-case)
        name: 表示名
        version: セマンティックバージョン
        author: 作成者名
        icon: 絵文字またはアイコン名
        category: マーケットプレイスカテゴリ
        description: エージェントの説明
    """

    id: str = Field(..., description="一意識別子 (kebab-case)", pattern=r"^[a-z0-9-]+$")
    name: str = Field(..., description="表示名", min_length=1)
    version: str = Field(..., description="セマンティックバージョン", pattern=r"^\d+\.\d+\.\d+$")
    author: str = Field(..., description="作成者名", min_length=1)
    icon: str = Field(..., description="絵文字またはアイコン名", min_length=1)
    category: str = Field(..., description="マーケットプレイスカテゴリ", min_length=1)
    description: str = Field(..., description="エージェントの説明", min_length=1)


class InputField(BaseModel):
    """入力フィールド定義."""

    name: str = Field(..., description="フィールド名", min_length=1)
    type: str = Field(..., description="データ型 (file, string, number, enum, etc.)", min_length=1)
    required: bool = Field(True, description="必須フィールドかどうか")
    description: str = Field(..., description="フィールドの説明", min_length=1)
    accept: list[str] | None = Field(None, description="ファイルタイプの場合の許可拡張子リスト")
    options: list[str] | None = Field(None, description="enum タイプの場合の選択肢")
    default: Any | None = Field(None, description="デフォルト値")


class OutputField(BaseModel):
    """出力フィールド定義."""

    name: str = Field(..., description="フィールド名", min_length=1)
    type: str = Field(..., description="データ型", min_length=1)
    output_schema: dict[str, Any] | None = Field(None, alias="schema", description="複雑な型の場合の JSON スキーマ")


class InterfaceDefinition(BaseModel):
    """インターフェース定義."""

    inputs: list[InputField] = Field(..., description="入力フィールドリスト")
    outputs: list[OutputField] = Field(..., description="出力フィールドリスト")


class MCPConfig(BaseModel):
    """MCP プロトコル設定."""

    tools: list[str] = Field(default_factory=list, description="ツール URI リスト")
    resources: list[str] = Field(default_factory=list, description="リソース URI リスト")


class A2AConfig(BaseModel):
    """A2A プロトコル設定."""

    enabled: bool = Field(True, description="A2A プロトコルを有効にするか")
    skills: list[str] = Field(default_factory=list, description="スキル名リスト")
    card_path: str | None = Field(None, description="AgentCard YAML ファイルパス")


class AGUIConfig(BaseModel):
    """AG-UI プロトコル設定."""

    enabled: bool = Field(True, description="AG-UI プロトコルを有効にするか")
    events: list[str] = Field(default_factory=list, description="発行するイベントタイプリスト")


class ProtocolConfig(BaseModel):
    """プロトコル設定."""

    mcp: MCPConfig | None = Field(None, description="MCP プロトコル設定")
    a2a: A2AConfig | None = Field(None, description="A2A プロトコル設定")
    agui: AGUIConfig | None = Field(None, description="AG-UI プロトコル設定")


class DependencySpec(BaseModel):
    """依存関係仕様."""

    agents: list[str] = Field(default_factory=list, description="依存する他のエージェント ID リスト")
    tools: list[str] = Field(default_factory=list, description="依存する MCP ツール URI リスト")
    packages: list[str] = Field(default_factory=list, description="依存する Python パッケージリスト")


class PocketFlowConfig(BaseModel):
    """PocketFlow 設定."""

    entry: str = Field(..., description="モジュールパス", min_length=1)
    shared_schema: str = Field(..., description="スキーマパス", min_length=1)


class VisualConfig(BaseModel):
    """ビジュアル設定."""

    color: str = Field(..., description="16進数カラーコード", pattern=r"^#[0-9A-Fa-f]{6}$")
    size: str = Field("medium", description="サイズ", pattern=r"^(small|medium|large)$")
    ports: dict[str, Any] = Field(default_factory=dict, description="入出力ポート位置設定")


class AgentMetadata(BaseModel):
    """エージェントメタデータ — agent.yaml の完全なスキーマ."""

    meta: MetaInfo = Field(..., description="メタ情報")
    interfaces: InterfaceDefinition = Field(..., description="インターフェース定義")
    protocols: ProtocolConfig = Field(..., description="プロトコル設定")
    dependencies: DependencySpec = Field(..., description="依存関係仕様")
    pocketflow: PocketFlowConfig = Field(..., description="PocketFlow 設定")
    visual: VisualConfig = Field(..., description="ビジュアル設定")

