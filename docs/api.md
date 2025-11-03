# AgentFlow API リファレンス

AgentFlow の主要 API の詳細なリファレンスです。

## Core API

### AgentBlock

すべてのエージェントの基底クラス。

```python
from agentflow.core.agent_block import AgentBlock
```

#### コンストラクタ

```python
def __init__(
    self,
    metadata_path: str | Path | None = None,
    engine: AgentFlowEngine | None = None
) -> None
```

**パラメータ:**
- `metadata_path` (str | Path | None): agent.yaml ファイルのパス。デフォルトは `./agent.yaml`
- `engine` (AgentFlowEngine | None): カスタムエンジンインスタンス。デフォルトは新規作成

#### プロパティ

##### `metadata`

```python
@property
def metadata(self) -> AgentMetadata
```

エージェントのメタデータを取得。

**戻り値:** `AgentMetadata` - エージェントメタデータ

**例外:** `ValueError` - メタデータが未ロードの場合

##### `engine`

```python
@property
def engine(self) -> AgentFlowEngine
```

ワークフローエンジンを取得。

**戻り値:** `AgentFlowEngine` - エンジンインスタンス

##### `is_initialized`

```python
@property
def is_initialized(self) -> bool
```

初期化状態を取得。

**戻り値:** `bool` - 初期化済みの場合 True

#### メソッド

##### `load_metadata`

```python
def load_metadata(
    self,
    metadata_path: str | Path | None = None
) -> AgentMetadata
```

メタデータファイルを読み込む。

**パラメータ:**
- `metadata_path` (str | Path | None): agent.yaml ファイルのパス

**戻り値:** `AgentMetadata` - 読み込んだメタデータ

**例外:** `FileNotFoundError` - ファイルが存在しない場合

##### `initialize`

```python
async def initialize(self) -> None
```

エージェントを初期化。サブクラスでオーバーライド可能。

**例:**

```python
async def initialize(self) -> None:
    await super().initialize()
    # カスタム初期化処理
    self.db = await connect_database()
```

##### `run`

```python
@abstractmethod
async def run(self, input_data: dict[str, Any]) -> dict[str, Any]
```

エージェントのメイン処理。**必須オーバーライド**。

**パラメータ:**
- `input_data` (dict[str, Any]): 入力データ

**戻り値:** `dict[str, Any]` - 処理結果

**例:**

```python
async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
    text = input_data.get("text", "")
    result = text.upper()
    return {"result": result}
```

##### `cleanup`

```python
async def cleanup(self) -> None
```

エージェントをクリーンアップ。サブクラスでオーバーライド可能。

**例:**

```python
async def cleanup(self) -> None:
    # カスタムクリーンアップ処理
    await self.db.close()
    await super().cleanup()
```

##### `get_mcp_tools`

```python
def get_mcp_tools(self) -> list[dict[str, Any]]
```

MCP ツール定義を取得（`@auto_adapt` により自動注入）。

**戻り値:** `list[dict[str, Any]]` - MCP ツール定義のリスト

##### `get_a2a_card`

```python
def get_a2a_card(self) -> AgentCard
```

A2A エージェントカードを取得（`@auto_adapt` により自動注入）。

**戻り値:** `AgentCard` - A2A エージェントカード

##### `create_agui_emitter`

```python
def create_agui_emitter(self, engine: Any) -> AGUIEventEmitter
```

AG-UI イベントエミッターを作成（`@auto_adapt` により自動注入）。

**パラメータ:**
- `engine` (Any): ワークフローエンジン

**戻り値:** `AGUIEventEmitter` - イベントエミッター

##### `get_metadata`

```python
def get_metadata(self) -> AgentMetadata
```

メタデータを取得（`@auto_adapt` により自動注入）。

**戻り値:** `AgentMetadata` - エージェントメタデータ

#### コンテキストマネージャー

```python
async with AgentBlock(metadata_path="agent.yaml") as agent:
    result = await agent.run({"input": "data"})
```

自動的に `initialize()` と `cleanup()` を呼び出します。

---

## Engine API

### AgentFlowEngine

PocketFlow ワークフローエンジンのラッパー。

```python
from agentflow.core.engine import AgentFlowEngine
```

#### コンストラクタ

```python
def __init__(self) -> None
```

#### メソッド

##### `register_workflow`

```python
def register_workflow(
    self,
    workflow_id: str,
    flow: AsyncFlow | Flow
) -> None
```

ワークフローを登録。

**パラメータ:**
- `workflow_id` (str): ワークフロー ID
- `flow` (AsyncFlow | Flow): ワークフローインスタンス

##### `execute_workflow`

```python
async def execute_workflow(
    self,
    workflow_id: str,
    input_data: dict[str, Any]
) -> dict[str, Any]
```

ワークフローを実行。

**パラメータ:**
- `workflow_id` (str): ワークフロー ID
- `input_data` (dict[str, Any]): 入力データ

**戻り値:** `dict[str, Any]` - 実行結果

**例外:** `ValueError` - ワークフローが未登録の場合

##### `list_workflows`

```python
def list_workflows(self) -> list[str]
```

登録済みワークフロー一覧を取得。

**戻り値:** `list[str]` - ワークフロー ID のリスト

---

## Metadata API

### AgentMetadata

エージェントメタデータのデータモデル。

```python
from agentflow.core.metadata import AgentMetadata
```

#### フィールド

- `meta` (MetaInfo): 基本情報
- `protocols` (ProtocolConfig): プロトコル設定
- `inputs` (list[InputField]): 入力フィールド
- `outputs` (list[OutputField]): 出力フィールド
- `skills` (list[Skill]): スキル定義

### MetaInfo

エージェントの基本情報。

#### フィールド

- `id` (str): エージェント ID（kebab-case）
- `name` (str): 表示名
- `version` (str): バージョン（semver）
- `description` (str): 説明
- `author` (str): 作者
- `license` (str): ライセンス
- `icon` (str): アイコン（絵文字）
- `category` (str): カテゴリ

### ProtocolConfig

プロトコル設定。

#### フィールド

- `mcp` (bool): MCP 有効化
- `a2a` (A2AConfig | None): A2A 設定
- `agui` (bool): AG-UI 有効化

### InputField

入力フィールド定義。

#### フィールド

- `name` (str): フィールド名
- `type` (str): データ型
- `description` (str): 説明
- `required` (bool): 必須フラグ
- `default` (Any | None): デフォルト値
- `options` (list[str] | None): 選択肢（enum）
- `accept` (str | None): ファイル受け入れ形式

### OutputField

出力フィールド定義。

#### フィールド

- `name` (str): フィールド名
- `type` (str): データ型
- `description` (str): 説明
- `schema` (dict[str, Any] | None): JSON Schema

### Skill

スキル定義。

#### フィールド

- `name` (str): スキル名
- `description` (str): 説明
- `inputs` (list[str]): 入力フィールド名
- `outputs` (list[str]): 出力フィールド名

### SchemaLoader

メタデータの読み込み・保存。

```python
from agentflow.core.schemas import SchemaLoader
```

#### メソッド

##### `load_from_file`

```python
def load_from_file(self, path: str | Path) -> AgentMetadata
```

YAML ファイルからメタデータを読み込む。

**パラメータ:**
- `path` (str | Path): YAML ファイルパス

**戻り値:** `AgentMetadata` - メタデータ

**例外:** `FileNotFoundError`, `ValidationError`

##### `save_to_file`

```python
def save_to_file(
    self,
    metadata: AgentMetadata,
    path: str | Path
) -> None
```

メタデータを YAML ファイルに保存。

**パラメータ:**
- `metadata` (AgentMetadata): メタデータ
- `path` (str | Path): 保存先パス

---

## Protocol API

### MCP Client

```python
from agentflow.protocols.mcp_client import MCPClient
```

詳細は [プロトコルガイド](protocols.md#mcp) を参照。

### A2A Server

```python
from agentflow.protocols.a2a_server import A2AServer
```

詳細は [プロトコルガイド](protocols.md#a2a) を参照。

### AG-UI Emitter

```python
from agentflow.protocols.agui_emitter import AGUIEventEmitter
```

詳細は [プロトコルガイド](protocols.md#ag-ui) を参照。

---

## Decorator API

### @auto_adapt

プロトコルアダプターを自動適用するデコレーター。

```python
from agentflow.decorators import auto_adapt
```

#### 使用方法

```python
@auto_adapt(protocols=["mcp", "a2a", "agui"])
class MyAgent:
    def __init__(self, metadata_path: str):
        self.metadata_path = metadata_path
```

**パラメータ:**
- `protocols` (list[str] | None): 有効化するプロトコル。None の場合は自動判定
- `metadata_path` (str | None): メタデータファイルパス

**注入されるメソッド:**
- `get_mcp_tools()` - MCP ツール定義を取得
- `get_a2a_card()` - A2A カードを取得
- `create_agui_emitter(engine)` - AG-UI エミッターを作成
- `get_metadata()` - メタデータを取得

---

## 次のステップ

- [プロトコルガイド](protocols.md) - MCP/A2A/AG-UI の詳細
- [CLI リファレンス](cli.md) - CLI コマンドの詳細
- [サンプル集](../examples/) - 実装例

