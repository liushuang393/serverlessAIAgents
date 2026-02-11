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
    metadata_path: str | Path = "agent.yaml",
    engine: AgentFlowEngine | None = None
) -> None
```

**パラメータ:**

- `metadata_path` (str | Path): agent.yaml ファイルのパス。デフォルトは `agent.yaml`
- `engine` (AgentFlowEngine | None): カスタムエンジンインスタンス。デフォルトは新規作成

#### プロパティ

##### `metadata`

```python
@property
def metadata(self) -> AgentMetadata | None
```

エージェントのメタデータを取得。

**戻り値:** `AgentMetadata | None` - エージェントメタデータ（未ロードの場合は None）

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

**戻り値:** `AgentCard | None` - A2A エージェントカード（未設定の場合は None）

##### `create_agui_emitter`

```python
def create_agui_emitter(self, engine: Any) -> AGUIEventEmitter
```

AG-UI イベントエミッターを作成（`@auto_adapt` により自動注入）。

**パラメータ:**

- `engine` (Any): ワークフローエンジン

**戻り値:** `AGUIEventEmitter` - イベントエミッター
**例外:** `ValueError` - メタデータが未ロードの場合（`@auto_adapt` の挙動）

##### `get_metadata`

```python
def get_metadata(self) -> AgentMetadata | None
```

メタデータを取得（`@auto_adapt` により自動注入）。

**戻り値:** `AgentMetadata | None` - エージェントメタデータ（未ロードの場合は None）

#### コンテキストマネージャー

```python
class MyAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return {"result": "ok"}

async with MyAgent(metadata_path="agent.yaml") as agent:
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
def __init__(self, *, logger: logging.Logger | None = None) -> None
```

#### メソッド

##### `register_workflow`

```python
def register_workflow(
    self,
    workflow: WorkflowConfig
) -> None
```

ワークフローを登録。

**パラメータ:**

- `workflow` (WorkflowConfig): ワークフロー設定

##### `execute`

```python
async def execute(
    self,
    workflow_id: str,
    inputs: dict[str, Any]
) -> ExecutionResult
```

ワークフローを実行。

**パラメータ:**

- `workflow_id` (str): ワークフロー ID
- `inputs` (dict[str, Any]): 入力データ

**戻り値:** `ExecutionResult` - 実行結果（status/output/error など）

**例外:** `WorkflowNotFoundError` - ワークフローが未登録の場合

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
- `interfaces` (InterfaceDefinition): 入出力インターフェース定義
- `protocols` (ProtocolConfig): プロトコル設定
- `dependencies` (DependencySpec): 依存関係
- `pocketflow` (PocketFlowConfig): PocketFlow 設定
- `visual` (VisualConfig): UI 表示設定

### MetaInfo

エージェントの基本情報。

#### フィールド

- `id` (str): エージェント ID（kebab-case）
- `name` (str): 表示名
- `version` (str): バージョン（semver）
- `author` (str): 作者
- `icon` (str): アイコン（絵文字）
- `category` (str): カテゴリ
- `description` (str): 説明

### InterfaceDefinition

入出力インターフェース定義。

#### フィールド

- `inputs` (list[InputField]): 入力フィールド定義
- `outputs` (list[OutputField]): 出力フィールド定義

### ProtocolConfig

プロトコル設定。

#### フィールド

- `mcp` (MCPConfig | None): MCP 設定
- `a2a` (A2AConfig | None): A2A 設定
- `agui` (AGUIConfig | None): AG-UI 設定

### MCPConfig

MCP 設定。

#### フィールド

- `tools` (list[str]): ツール URI リスト（例: `mcp://server/tool`）
- `resources` (list[str]): リソース URI リスト

### A2AConfig

A2A 設定。

#### フィールド

- `enabled` (bool): A2A を有効化
- `skills` (list[str]): 公開スキル名リスト
- `card_path` (str | None): AgentCard YAML パス（任意）

### AGUIConfig

AG-UI 設定。

#### フィールド

- `enabled` (bool): AG-UI を有効化
- `events` (list[str]): 発行するイベントタイプ（例: `flow.start`）

### InputField

入力フィールド定義。

#### フィールド

- `name` (str): フィールド名
- `type` (str): データ型
- `description` (str): 説明
- `required` (bool): 必須フラグ
- `default` (Any | None): デフォルト値
- `options` (list[str] | None): 選択肢（enum）
- `accept` (list[str] | None): ファイル受け入れ拡張子（fileタイプ用）

### OutputField

出力フィールド定義。

#### フィールド

- `name` (str): フィールド名
- `type` (str): データ型
- `description` (str): 説明
- `schema` (dict[str, Any] | None): JSON Schema（複雑型の場合、任意）

### DependencySpec

依存関係。

#### フィールド

- `agents` (list[str]): 依存する Agent ID
- `tools` (list[str]): 依存するツール URI
- `packages` (list[str]): 追加で必要な Python パッケージ

### PocketFlowConfig

PocketFlow 設定。

#### フィールド

- `entry` (str): エントリーポイント（例: `main.py:flow`）
- `shared_schema` (str): 共有スキーマ（例: `schemas.py:SharedSchema`）

### VisualConfig

UI 表示設定。

#### フィールド

- `color` (str): 16進数カラーコード（例: `#3B82F6`）
- `size` (str): `small` / `medium` / `large`
- `ports` (dict[str, Any]): 入出力ポート位置

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

**例外:** `FileNotFoundError`, `SchemaValidationError`

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
- `metadata_path` (str | Path): メタデータファイルパス

**注入されるメソッド:**

- `get_mcp_tools()` - MCP ツール定義を取得
- `get_a2a_card()` - A2A カードを取得
- `create_agui_emitter(engine)` - AG-UI エミッターを作成
- `get_metadata()` - メタデータを取得

---

---

## Service API（NEW）

### AgentService

Agent 実行の統一サービス。

```python
from agentflow.services import AgentService
```

#### 実行メソッド

##### `execute`

```python
async def execute(
    self,
    *,
    agent_id: str = "",
    agent_path: str | Path | None = None,
    input_data: dict[str, Any] | None = None,
    **kwargs: Any,
) -> ServiceResult
```

Agent を実行して結果を返す（API向け）。

**パラメータ:**

- `agent_id` (str): Agent ID（@agent デコレータ名）
- `agent_path` (str | Path | None): Agent パス
- `input_data` (dict[str, Any] | None): 入力データ

**戻り値:** `ServiceResult` - 実行結果

##### `execute_stream`

```python
async def execute_stream(
    self,
    **kwargs: Any,
) -> AsyncIterator[ServiceEvent]
```

イベントストリームを返す（WebSocket/SSE向け）。

**Yields:** `ServiceEvent` - 実行イベント

##### `execute_with_callback`

```python
async def execute_with_callback(
    self,
    on_event: EventCallback | None = None,
    on_progress: ProgressCallback | None = None,
    **kwargs: Any,
) -> ServiceResult
```

コールバック付き実行（CLI向け）。

**パラメータ:**

- `on_event`: 全イベント受信コールバック
- `on_progress`: 進捗のみ受信コールバック `(progress%, message) -> None`

**例:**

```python
service = AgentService()

# API向け
result = await service.execute(agent_id="MyAgent", input_data={"text": "hello"})

# CLI向け
def on_progress(pct, msg):
    print(f"[{pct:.1f}%] {msg}")

result = await service.execute_with_callback(
    agent_id="MyAgent",
    input_data={"text": "hello"},
    on_progress=on_progress,
)

# WebSocket向け
async for event in service.execute_stream(agent_id="MyAgent"):
    await ws.send(event.to_json())
```

### WorkflowService

Workflow 実行の統一サービス。

```python
from agentflow.services import WorkflowService
```

**使用例:**

```python
service = WorkflowService()

result = await service.execute(
    workflow_type="deep_agent",  # "deep_agent" | "pipeline" | "reflection"
    task="市場分析レポート作成",
    input_data={"context": "..."},
)
```

### ServiceEvent

統一イベントモデル。

```python
from agentflow.services import ServiceEvent, ProgressEvent, ResultEvent
```

**イベントタイプ:**

| タイプ | 説明 |
|--------|------|
| `start` | 実行開始 |
| `progress` | 進捗更新 |
| `complete` | 実行完了 |
| `error` | エラー発生 |
| `agent.start` | Agent開始 |
| `agent.complete` | Agent完了 |
| `approval.required` | HITL承認待ち |

---

## Tool API（NEW）

### ToolExecutor

並行ツール実行（OpenAI Function Calling 互換）。

```python
from agentflow.providers import ToolExecutor, ToolCall
```

#### 基本使用

```python
executor = ToolExecutor(tool_provider=my_tools)

# 単一実行
result = await executor.execute(ToolCall.create("search", {"q": "AI"}))

# 並行実行（OpenAI互換）
results = await executor.execute_parallel([
    ToolCall.create("search", {"q": "AI"}),
    ToolCall.create("fetch", {"url": "..."}),
])

# フォールバック付き
result = await executor.execute_with_fallback(tool_call)
```

#### ToolCall

```python
class ToolCall(BaseModel):
    id: str           # 呼び出しID
    type: str         # "function"
    function: FunctionCall
```

#### ToolResult

```python
class ToolResult(BaseModel):
    tool_call_id: str    # 対応するToolCall ID
    role: str            # "tool"
    content: str         # 実行結果
    name: str            # ツール名
    status: ToolCallStatus
```

---

## VectorStore API（NEW）

### VectorStore

ベクトル検索抽象インターフェース（LlamaIndex/LangChain 互換）。

```python
from agentflow.memory import VectorStore, Document, InMemoryVectorStore
```

#### 基本使用

```python
store = InMemoryVectorStore()

# ドキュメント追加
await store.add_documents([
    Document(page_content="AgentFlowはAgent框架です"),
])

# 類似度検索
results = await store.similarity_search("Agentフレームワーク", k=5)
for r in results:
    print(f"Score: {r.score}, Content: {r.document.page_content}")

# MMR検索（多様性考慮）
results = await store.max_marginal_relevance_search("Agent", k=5)
```

---

## ErrorResponse API（NEW）

### ErrorResponse

RFC 7807 Problem Details 互換のエラーレスポンス。

```python
from agentflow.core import ErrorCode, ErrorResponse, create_error_response
```

#### 基本使用

```python
# エラーレスポンス作成
error = create_error_response(
    code=ErrorCode.AGENT_NOT_FOUND,
    detail="Agent 'MyAgent' was not found",
)

# JSON形式
print(error.to_dict())
# {
#   "type": "https://agentflow.dev/errors/agent_not_found",
#   "title": "Agent Not Found",
#   "status": 404,
#   "detail": "Agent 'MyAgent' was not found",
#   "code": "agent_not_found",
#   "trace_id": "trace_abc123..."
# }
```

#### エラーコード

| コード | HTTP Status | 説明 |
|--------|-------------|------|
| `AGENT_NOT_FOUND` | 404 | Agent未発見 |
| `WORKFLOW_NOT_FOUND` | 404 | Workflow未発見 |
| `VALIDATION_ERROR` | 400 | バリデーションエラー |
| `LLM_TIMEOUT` | 408 | LLMタイムアウト |
| `AGENT_EXECUTION_ERROR` | 500 | Agent実行エラー |

---

## Context Engineering API（NEW）

### ContextEngineer

上下文エンジニアリングの統合インターフェース。

```python
from agentflow import ContextEngineer, ContextConfig
```

#### 基本使用

```python
engineer = ContextEngineer()
await engineer.start()

# メッセージ追加
engineer.add_message("user", "APIの仕様を教えて")

# コンテキスト構築
context = await engineer.build_context(
    query="決済APIの仕様",
    base_prompt="技術アシスタント",
    available_tools=tools,
    rag_search_func=rag.search,
)

# 結果
# context.system_prompt  -> 予算内プロンプト
# context.tools          -> Top-K関連ツール
# context.rag_results    -> 検索結果（必要時のみ）
# context.messages       -> 圧縮済み履歴
```

### TokenBudgetManager

Token予算管理。

```python
from agentflow import TokenBudgetManager, BudgetConfig

config = BudgetConfig(
    system_prompt_budget=500,
    tools_budget=300,
    rag_context_budget=2000,
)
manager = TokenBudgetManager(config=config)

# プロンプト配分
allocation = manager.allocate_system_prompt(long_prompt, skills)
print(f"Tokens: {allocation.token_count}, Truncated: {allocation.truncated}")
```

### ToolRelevanceSelector

クエリベースのツール選択。

```python
from agentflow import ToolRelevanceSelector

selector = ToolRelevanceSelector()
selected = await selector.select_relevant_tools(
    query="データベース検索",
    all_tools=all_tools,
    max_tools=7,
)
```

### RetrievalGate

RAG検索必要性判定。

```python
from agentflow import RetrievalGate

gate = RetrievalGate()
decision = await gate.should_retrieve("文書の内容を教えて")

if decision.should_retrieve:
    results = await rag.search(decision.suggested_query)
```

### KeyNotesStore

重要情報の永続化。

```python
from agentflow import KeyNotesStore, NoteImportance

store = KeyNotesStore()
store.add_note("予算は100万円", importance=NoteImportance.HIGH)
await store.extract_and_store("私は田中です", source="user")

context_str = store.to_context_string(max_tokens=500)
```

### TurnBasedCompressor

ターン数ベースの会話圧縮。

```python
from agentflow import TurnBasedCompressor, TurnConfig

compressor = TurnBasedCompressor(
    config=TurnConfig(turn_threshold=10),
)

for msg in messages:
    compressor.add_message(msg["role"], msg["content"])

if compressor.should_compress():
    result = await compressor.compress()
    print(f"圧縮: {result.original_count} -> {result.compressed_count}")
```

### ResultSummarizer

子Agent結果フィルター。

```python
from agentflow.patterns.deep_agent import ResultSummarizer

summarizer = ResultSummarizer()
summarized = await summarizer.summarize_results(results)
# debug_info, intermediate_steps 等は自動除去
```

詳細は [Context Engineering ガイド](context-engineering.md) を参照。

---

## 次のステップ

- [Context Engineering ガイド](context-engineering.md) - 上下文予算管理の詳細
- [パターンガイド](PATTERNS_GUIDE.md) - DeepAgent/Reflection/Pipeline の詳細
- [プロトコルガイド](protocols.md) - MCP/A2A/AG-UI の詳細
- [CLI リファレンス](cli.md) - CLI コマンドの詳細
- [サンプル集](../examples/) - 実装例
