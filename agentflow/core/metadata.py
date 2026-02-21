"""エージェントメタデータスキーマ定義.

このモジュールは agent.yaml ファイルのスキーマを Pydantic モデルとして定義します。
"""

from typing import Any

from pydantic import BaseModel, Field


class MetaInfo(BaseModel):
    """エージェントのメタ情報.

    エージェントの基本的な識別情報と説明を含みます。
    """

    id: str = Field(..., description="一意識別子 (kebab-case)", pattern=r"^[a-z0-9-]+$")
    name: str = Field(..., description="表示名", min_length=1)
    version: str = Field(..., description="セマンティックバージョン", pattern=r"^\d+\.\d+\.\d+$")
    author: str = Field(..., description="作成者名", min_length=1)
    icon: str = Field(..., description="絵文字またはアイコン名", min_length=1)
    category: str = Field(..., description="マーケットプレイスカテゴリ", min_length=1)
    description: str = Field(..., description="エージェントの説明", min_length=1)


class InputField(BaseModel):
    """入力フィールド定義.

    エージェントが受け取る入力パラメータの定義です。
    """

    name: str = Field(..., description="フィールド名", min_length=1)
    type: str = Field(
        ...,
        description="データ型 (file, string, number, enum, etc.)",
        min_length=1,
    )
    required: bool = Field(True, description="必須フィールドかどうか")
    description: str = Field(..., description="フィールドの説明", min_length=1)
    accept: list[str] | None = Field(None, description="ファイルタイプの場合の許可拡張子リスト")
    options: list[str] | None = Field(None, description="enum タイプの場合の選択肢")
    default: Any | None = Field(None, description="デフォルト値")


class OutputField(BaseModel):
    """出力フィールド定義.

    エージェントが返す出力データの定義です。
    """

    name: str = Field(..., description="フィールド名", min_length=1)
    type: str = Field(..., description="データ型", min_length=1)
    output_schema: dict[str, Any] | None = Field(None, alias="schema", description="複雑な型の場合の JSON スキーマ")


class InterfaceDefinition(BaseModel):
    """インターフェース定義.

    エージェントの入出力インターフェースを定義します。
    """

    inputs: list[InputField] = Field(..., description="入力フィールドリスト")
    outputs: list[OutputField] = Field(..., description="出力フィールドリスト")


class MCPConfig(BaseModel):
    """MCP プロトコル設定.

    Model Context Protocol の設定を定義します。
    """

    tools: list[str] = Field(default_factory=list, description="ツール URI リスト (mcp://server/tool)")
    resources: list[str] = Field(default_factory=list, description="リソース URI リスト")


class A2AConfig(BaseModel):
    """A2A プロトコル設定.

    Agent-to-Agent プロトコルの設定を定義します。
    """

    enabled: bool = Field(True, description="A2A プロトコルを有効にするか")
    skills: list[str] = Field(default_factory=list, description="スキル名リスト")
    card_path: str | None = Field(None, description="AgentCard YAML ファイルパス")


class AGUIConfig(BaseModel):
    """AG-UI プロトコル設定.

    AG-UI プロトコルの設定を定義します。
    """

    enabled: bool = Field(True, description="AG-UI プロトコルを有効にするか")
    events: list[str] = Field(default_factory=list, description="発行するイベントタイプリスト")


class ProtocolConfig(BaseModel):
    """プロトコル設定.

    エージェントが使用する各プロトコルの設定を定義します。
    """

    mcp: MCPConfig | None = Field(None, description="MCP プロトコル設定")
    a2a: A2AConfig | None = Field(None, description="A2A プロトコル設定")
    agui: AGUIConfig | None = Field(None, description="AG-UI プロトコル設定")


class DependencySpec(BaseModel):
    """依存関係仕様.

    エージェントが依存する他のコンポーネントを定義します。
    """

    agents: list[str] = Field(default_factory=list, description="依存する他のエージェント ID リスト")
    tools: list[str] = Field(default_factory=list, description="依存する MCP ツール URI リスト")
    packages: list[str] = Field(default_factory=list, description="依存する Python パッケージリスト")


class PocketFlowConfig(BaseModel):
    """PocketFlow 設定.

    PocketFlow ワークフローのエントリーポイントとスキーマを定義します。
    """

    entry: str = Field(..., description="モジュールパス (例: 'flow.py:create_flow')", min_length=1)
    shared_schema: str = Field(..., description="スキーマパス (例: 'schemas.py:MySchema')", min_length=1)


class VisualConfig(BaseModel):
    """ビジュアル設定.

    エージェントの UI 表示設定を定義します。
    """

    color: str = Field(..., description="16進数カラーコード", pattern=r"^#[0-9A-Fa-f]{6}$")
    size: str = Field("medium", description="サイズ (small/medium/large)", pattern=r"^(small|medium|large)$")
    ports: dict[str, Any] = Field(default_factory=dict, description="入出力ポート位置設定")


class AgentMetadata(BaseModel):
    """エージェントメタデータ.

    agent.yaml ファイルの完全なスキーマを定義します。
    """

    meta: MetaInfo = Field(..., description="メタ情報")
    interfaces: InterfaceDefinition = Field(..., description="インターフェース定義")
    protocols: ProtocolConfig = Field(..., description="プロトコル設定")
    dependencies: DependencySpec = Field(..., description="依存関係仕様")
    pocketflow: PocketFlowConfig = Field(..., description="PocketFlow 設定")
    visual: VisualConfig = Field(..., description="ビジュアル設定")
