# AgentFlow フレームワーク アーキテクチャ再設計

> **更新日**: 2024-12-30
> **バージョン**: 0.2.0
> **ステータス**: 実装完了

---

## 実装状況サマリー

| コンポーネント | ステータス | ファイル |
|---------------|-----------|----------|
| `@agent` デコレータ | ✅ 完了 | `agentflow/agent_decorator.py` |
| `@tool` デコレータ | ✅ 完了 | `agentflow/providers/tool_provider.py` |
| `AgentClient` | ✅ 完了 | `agentflow/agent_decorator.py` |
| `LLMProvider` | ✅ 完了 | `agentflow/providers/llm_provider.py` |
| `ToolProvider` | ✅ 完了 | `agentflow/providers/tool_provider.py` |
| `DataProvider` | ✅ 完了 | `agentflow/providers/data_provider.py` |
| `EventProvider` | ✅ 完了 | `agentflow/providers/event_provider.py` |
| `AgentRouter` (FastAPI) | ✅ 完了 | `agentflow/integrations/fastapi_integration.py` |
| `create_flow` | ✅ 既存 | `agentflow/quick.py` |
| `Flow.run/run_stream` | ✅ 既存 | `agentflow/quick.py` |
| React SDK | ⏳ 将来 | - |

---

## 1. 現状の課題

### 1.1 開発者体験の問題

| 問題 | 現状 | 理想 |
|------|------|------|
| 学習曲線 | AgentBlock, Workflow, MCP, A2UI, AG-UI, Skills... | `@agent` 一つで始められる |
| 設定の分散 | YAML + Python + Frontend各自独立 | 約定優先、設定任意 |
| 呼び出し方法 | 固定パターンのみ | 同期/SSE/WS/部分更新を選択 |
| ツール統合 | MCP/Method/Skill別々に設定 | 自動発見、統一インターフェース |

### 1.2 フレームワーク構造の問題

```
現状の問題点:
┌─────────────────────────────────────────────────────────────┐
│  Application                                                 │
│    ↓ 複雑な継承関係                                          │
│  AgentBlock → Workflow → SharedContext → ...                 │
│    ↓ 手動設定必須                                            │
│  MCP設定 + LLM設定 + DB設定 + ...                           │
│    ↓ 固定パターン                                            │
│  SSE進捗のみ                                                 │
└─────────────────────────────────────────────────────────────┘

目標:
┌─────────────────────────────────────────────────────────────┐
│  Application                                                 │
│    ↓ デコレータ一つ                                          │
│  @agent + @tool                                              │
│    ↓ 自動設定（約定優先）                                    │
│  統一Provider（デフォルト値あり）                            │
│    ↓ 柔軟な選択                                              │
│  同期 / SSE / WebSocket / 部分更新                          │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. 新アーキテクチャ設計

### 2.1 レイヤー構成

```
┌─────────────────────────────────────────────────────────────────┐
│                     APPLICATION LAYER                            │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  従来のWebアプリ + AI能力 = スマートアプリ                 │   │
│  │  - FastAPI/Flask Controller                               │   │
│  │  - Serviceを呼ぶようにAgentを呼ぶ                         │   │
│  └──────────────────────────────────────────────────────────┘   │
├─────────────────────────────────────────────────────────────────┤
│                     AGENTFLOW SDK LAYER                          │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  デコレータ API                                          │    │
│  │  @agent     - Agent定義（設定不要）                       │    │
│  │  @workflow  - ワークフロー定義                            │    │
│  │  @tool      - ツール定義（自動登録）                      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌────────────────────────┐  │
│  │ AgentClient │  │FlowClient   │  │ ToolClient             │  │
│  │ .invoke()   │  │ .run()      │  │ .call()                │  │
│  │ .stream()   │  │ .stream()   │  │ .discover()            │  │
│  │ .async()    │  │ .partial()  │  │                        │  │
│  └─────────────┘  └─────────────┘  └────────────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│                     PROVIDER LAYER (統一抽象)                    │
│                                                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  LLMProvider      - 統一LLM呼び出し（デフォルト: OpenAI） │   │
│  │  ToolProvider     - 統一ツール呼び出し（自動発見）        │   │
│  │  DataProvider     - 統一データアクセス（SQL/Vector/Cache）│   │
│  │  EventProvider    - 統一イベント通知（SSE/WS/Callback）   │   │
│  └──────────────────────────────────────────────────────────┘   │
├─────────────────────────────────────────────────────────────────┤
│                     INFRASTRUCTURE LAYER                         │
│                                                                  │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌──────────┐  │
│  │ OpenAI  │ │Anthropic│ │PostgreSQL│ │ Redis  │ │ MCP Svr  │  │
│  │ Claude  │ │  Azure  │ │ SQLite  │ │ChromaDB │ │ Skills   │  │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘ └──────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 設計原則

1. **約定優先（Convention over Configuration）**
   - 設定なしでも動作する
   - 必要な時だけ設定を追加

2. **漸進的複雑度（Progressive Complexity）**
   - 簡単なシナリオは簡単に
   - 複雑なシナリオは拡張可能

3. **統一入口（Unified Entry Point）**
   - 一つのデコレータ、一つのHook、一つのコンポーネント

4. **従来フレンドリー（Traditional Friendly）**
   - Springのように使える
   - 既存の知識が活かせる

---

## 3. バックエンドAPI設計

### 3.1 Agent定義（デコレータ方式）

```python
from agentflow import agent, tool, workflow

# ===================================
# 最も簡単な定義 - 設定ゼロ
# ===================================
@agent
class QAAgent:
    """質問応答Agent - クラス名がそのままAgent名"""
    
    system_prompt = "あなたは親切なアシスタントです"
    
    # ツールは自動発見・自動登録
    @tool
    def search_knowledge(self, query: str) -> list[dict]:
        """ナレッジ検索"""
        return self.db.search(query)  # dbは自動注入


# ===================================
# カスタマイズが必要な場合
# ===================================
@agent(
    name="カスタマーサポート",
    llm="gpt-4",  # デフォルトはgpt-3.5-turbo
    temperature=0.7,
    max_tokens=2000,
)
class CustomerSupportAgent:
    system_prompt = "..."
    
    @tool(cache=True, ttl=3600)  # キャッシュ有効
    def get_order_status(self, order_id: str) -> dict:
        ...


# ===================================
# ワークフロー定義
# ===================================
@workflow
class DecisionFlow:
    """複数Agentの協調"""
    
    agents = ["GatekeeperAgent", "DaoAgent", "FaAgent", "ShuAgent"]
    
    @step(after="GatekeeperAgent")
    def on_gatekeeper_complete(self, ctx, result):
        if result.is_acceptable:
            return "DaoAgent"  # 次へ
        else:
            return None  # 終了
```

### 3.2 Agent呼び出し（クライアント方式）

```python
from agentflow import AgentClient, FlowClient

# ===================================
# 同期呼び出し（従来のAPI風）
# ===================================
async def handle_question(question: str):
    client = AgentClient.get("QAAgent")
    result = await client.invoke(question)
    return result

# ===================================
# ストリーミング呼び出し
# ===================================
async def handle_chat(question: str):
    client = AgentClient.get("QAAgent")
    async for chunk in client.stream(question):
        yield chunk

# ===================================
# 非同期呼び出し（長時間処理）
# ===================================
async def handle_analysis(data: dict):
    client = AgentClient.get("AnalysisAgent")
    task_id = await client.invoke_async(
        data,
        callback_url="https://myapp.com/callback"
    )
    return {"task_id": task_id}

# ===================================
# ワークフロー呼び出し
# ===================================
async def handle_decision(question: str):
    flow = FlowClient.get("DecisionFlow")
    
    # 全体実行
    result = await flow.run({"question": question})
    
    # または、ストリーミング（進捗付き）
    async for event in flow.stream({"question": question}):
        yield event
    
    # または、部分実行（特定Agentのみ）
    dao_result = await flow.run_agent("DaoAgent", {"question": question})
```

### 3.3 FastAPI統合

```python
from fastapi import FastAPI
from agentflow.integrations.fastapi import AgentRouter

app = FastAPI()

# 一行でAgent APIを生成
app.include_router(
    AgentRouter(
        agents=["QAAgent", "CustomerSupportAgent"],
        workflows=["DecisionFlow"],
    ),
    prefix="/api"
)

# 自動生成されるエンドポイント:
# POST /api/agents/{agent_id}/invoke    - 同期呼び出し
# GET  /api/agents/{agent_id}/stream    - SSEストリーム
# POST /api/agents/{agent_id}/async     - 非同期呼び出し
# POST /api/workflows/{flow_id}/run     - ワークフロー実行
# GET  /api/workflows/{flow_id}/stream  - ワークフローストリーム
```

---

## 4. フロントエンド統合設計

### 4.1 React SDK

```tsx
// @agentflow/react

// ===================================
// Hook方式（最も柔軟）
// ===================================
import { useAgent, useWorkflow } from '@agentflow/react';

function ChatComponent() {
  const { 
    invoke, 
    stream, 
    loading, 
    result, 
    error 
  } = useAgent('QAAgent');
  
  const handleSubmit = async () => {
    // 方式1: 同期（結果待ち）
    const answer = await invoke({ question: userInput });
    
    // 方式2: ストリーム（リアルタイム）
    for await (const chunk of stream({ question: userInput })) {
      setMessages(prev => [...prev, chunk]);
    }
  };
  
  return (
    <div>
      <input onChange={e => setUserInput(e.target.value)} />
      <button onClick={handleSubmit} disabled={loading}>送信</button>
      {result && <Answer data={result} />}
      {error && <Error message={error} />}
    </div>
  );
}


// ===================================
// コンポーネント方式（最も簡単）
// ===================================
import { AgentChat, AgentView } from '@agentflow/react';

function SimpleChatPage() {
  // 一行でチャットUI
  return <AgentChat agent="QAAgent" />;
}

function DashboardPage() {
  // 宣言的にAgentビューを配置
  return (
    <div className="grid grid-cols-2">
      <AgentView 
        id="analysis" 
        agent="AnalysisAgent"
        trigger="onLoad"
        input={{ data: dashboardData }}
      />
      <AgentView 
        id="recommendation" 
        agent="RecommendAgent"
        trigger="analysis.onComplete"  // 依存関係
      />
    </div>
  );
}


// ===================================
// ワークフロー方式（複数Agent協調）
// ===================================
import { useWorkflow, WorkflowProgress } from '@agentflow/react';

function DecisionPage() {
  const { 
    run, 
    progress,      // 各Agentの進捗
    currentAgent,  // 現在実行中
    results,       // 各Agentの結果
    isComplete 
  } = useWorkflow('DecisionFlow');
  
  return (
    <div>
      <button onClick={() => run({ question })}>分析開始</button>
      
      {/* 進捗表示（自動更新） */}
      <WorkflowProgress 
        agents={progress} 
        current={currentAgent} 
      />
      
      {/* 結果表示（部分更新） */}
      {results.dao && <DaoResult data={results.dao} />}
      {results.fa && <FaResult data={results.fa} />}
      {/* ... */}
    </div>
  );
}


// ===================================
// 部分更新方式（最も効率的）
// ===================================
import { AgentPartial } from '@agentflow/react';

function ComplexDashboard() {
  return (
    <div className="grid">
      {/* 各領域が独立して更新 */}
      <AgentPartial 
        id="chart-area" 
        agent="ChartAgent"
        updateMode="replace"  // replace | append | merge
      />
      
      <AgentPartial 
        id="table-area" 
        agent="TableAgent"
        updateMode="merge"
        dependencies={["chart-area"]}  // chart完了後に実行
      />
    </div>
  );
}
```

### 4.2 通信モード

```
┌─────────────────────────────────────────────────────────────────┐
│                     Frontend Communication Modes                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Mode 1: AJAX (同期)                                            │
│  ┌─────────────────┐                                            │
│  │ POST /invoke    │ → JSON結果                                 │
│  └─────────────────┘                                            │
│  用途: 簡単なクエリ、結果待ちOKな場合                           │
│                                                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Mode 2: SSE (Server-Sent Events)                               │
│  ┌─────────────────┐                                            │
│  │ GET /stream     │ → Event Stream                             │
│  └─────────────────┘                                            │
│  用途: リアルタイム進捗、チャット                                │
│                                                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Mode 3: WebSocket (双方向)                                     │
│  ┌─────────────────┐                                            │
│  │ WS /realtime    │ ← → 双方向通信                             │
│  └─────────────────┘                                            │
│  用途: 対話型、複雑なインタラクション                            │
│                                                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Mode 4: Partial Update (部分更新)                              │
│  ┌─────────────────┐                                            │
│  │ GET /partial/id │ → 特定コンポーネントのみ更新               │
│  └─────────────────┘                                            │
│  用途: ダッシュボード、複雑なUI                                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## 5. 統一Provider設計

### 5.1 LLMProvider

```python
from agentflow.providers import LLMProvider

# 設定なしで使用（デフォルト: OpenAI gpt-3.5-turbo）
llm = LLMProvider.default()
response = await llm.chat([{"role": "user", "content": "こんにちは"}])

# カスタム設定
llm = LLMProvider.create(
    provider="anthropic",  # openai | anthropic | azure | local
    model="claude-3-opus",
    api_key="...",  # 環境変数から自動取得も可
)

# Agent内では自動注入
@agent
class MyAgent:
    async def process(self, input):
        # self.llmは自動注入
        response = await self.llm.chat([...])
```

### 5.2 ToolProvider（統一ツール呼び出し）

```python
from agentflow.providers import ToolProvider

# ===================================
# 自動発見モード（推奨）
# ===================================
# @toolデコレータ付きメソッドを自動発見
tools = ToolProvider.discover()

# ===================================
# 統一呼び出し
# ===================================
# ToolProviderが内部でMCP/Method/Skillを自動選択
result = await tools.call("search_products", query="laptop")

# ===================================
# ツール定義（開発者向け）
# ===================================
@tool  # 自動でMethodツールとして登録
def search_products(query: str) -> list:
    return db.products.search(query)

@tool(provider="mcp", server="database")  # MCPを使用
def complex_query(sql: str) -> list:
    pass

@tool(provider="skill")  # Skillsエンジンを使用
def analyze_sentiment(text: str) -> dict:
    pass
```

### 5.3 DataProvider

```python
from agentflow.providers import DataProvider

# ===================================
# デフォルト接続（設定不要）
# ===================================
# 開発: SQLite + ChromaDB + Memory Cache
# 本番: 環境変数から自動検出

sql = DataProvider.sql()        # SQL DB
vector = DataProvider.vector()  # Vector DB
cache = DataProvider.cache()    # Cache

# ===================================
# Agent内では自動注入
# ===================================
@agent
class MyAgent:
    async def process(self, input):
        # 自動注入されたProvider
        data = await self.db.query("SELECT * FROM users")
        similar = await self.vector.search(input.text, top_k=5)
        cached = await self.cache.get(f"user:{input.user_id}")
```

### 5.4 EventProvider

```python
from agentflow.providers import EventProvider

# ===================================
# イベント発火（Agent内で自動使用）
# ===================================
@agent
class MyAgent:
    async def process(self, input):
        # 進捗通知（SSE/WS自動選択）
        await self.events.emit("progress", {"step": 1, "total": 3})
        
        # 部分更新
        await self.events.emit("partial_update", {
            "target_id": "result-area",
            "content": {"status": "processing"}
        })
        
        # 完了通知
        await self.events.emit("complete", {"result": result})
```

---

## 6. デフォルト設定

### 6.1 約定優先（Convention over Configuration）

```yaml
# agentflow.yaml - すべて任意、デフォルト値あり

# LLM設定
llm:
  provider: openai        # デフォルト
  model: gpt-3.5-turbo    # デフォルト
  api_key: ${OPENAI_API_KEY}  # 環境変数から

# データベース設定
database:
  sql:
    url: sqlite:///./data.db  # 開発デフォルト
    # url: ${DATABASE_URL}    # 本番は環境変数
  vector:
    provider: chromadb        # デフォルト
    # provider: pgvector      # 本番用
  cache:
    provider: memory          # 開発デフォルト
    # url: ${REDIS_URL}       # 本番用

# ツール設定
tools:
  discovery: auto    # @toolを自動発見
  mcp_servers: []    # 追加MCPサーバー（任意）

# API設定
api:
  prefix: /api       # デフォルト
  cors: true         # 開発用
  docs: true         # Swagger UI有効
```

### 6.2 環境別設定

```python
# 開発環境（設定ファイルなしで動作）
# - SQLite
# - ChromaDB（ローカル）
# - Memory Cache
# - OpenAI gpt-3.5-turbo

# 本番環境（環境変数で自動切り替え）
# DATABASE_URL=postgresql://...
# REDIS_URL=redis://...
# OPENAI_API_KEY=...
# AGENTFLOW_ENV=production
```

---

## 7. 実装ロードマップ

### Phase 1: 核心リファクタリング（2週間）

1. **Provider Layer実装**
   - LLMProvider（デフォルトOpenAI）
   - ToolProvider（自動発見）
   - DataProvider（SQL/Vector/Cache統一）
   - EventProvider（SSE/WS/Callback統一）

2. **デコレータAPI**
   - @agent（ゼロ設定Agent定義）
   - @tool（自動登録）
   - @workflow（宣言的ワークフロー）

### Phase 2: SDK Layer（2週間）

1. **Python SDK**
   - AgentClient（invoke/stream/async）
   - FlowClient（run/stream/partial）
   - 依存性注入

2. **FastAPI統合**
   - AgentRouter
   - 自動API生成
   - OpenAPI文書

### Phase 3: Frontend SDK（2週間）

1. **@agentflow/core**
   - 統一通信層
   - イベント標準化

2. **@agentflow/react**
   - useAgent / useWorkflow
   - AgentView / WorkflowProgress
   - 部分更新対応

### Phase 4: ドキュメント・移行（1週間）

1. **開発者ドキュメント**
   - 5分クイックスタート
   - 従来アプリからの移行ガイド
   - ベストプラクティス

2. **サンプルアプリ**
   - シンプルチャット
   - ダッシュボード
   - 意思決定システム

---

## 8. 移行例

### 8.1 現在のコード → 新コード

**Before（現在）:**
```python
# 複雑な継承とYAML設定が必要
class MyAgent(AgentBlock):
    def prep(self, shared): ...
    def exec(self, prep_res): ...
    def post(self, shared, prep_res, exec_res): ...

# agent.yaml
agents:
  MyAgent:
    llm:
      provider: openai
      model: gpt-4
    tools:
      - name: search
        type: mcp
        ...
```

**After（新）:**
```python
# デコレータ一つで完結
@agent(llm="gpt-4")  # オプション
class MyAgent:
    system_prompt = "..."
    
    @tool
    def search(self, query: str) -> list:
        return self.db.search(query)
```

### 8.2 API呼び出しの変更

**Before:**
```python
# 手動で複雑な設定
workflow = DecisionEngine(llm_client=..., enable_rag=True)
result = await workflow.process(request)
```

**After:**
```python
# シンプルな呼び出し
result = await AgentClient.get("DecisionFlow").run(request)
```

### 8.3 フロントエンドの変更

**Before:**
```tsx
// 手動でSSE接続管理
const eventSource = new EventSource('/api/stream?...');
eventSource.onmessage = (e) => { ... };
```

**After:**
```tsx
// Hook一つで完結
const { stream, loading } = useAgent('MyAgent');
for await (const chunk of stream(input)) { ... }
```

---

## 付録: 設計比較

| 観点 | Spring Boot | AgentFlow (現在) | AgentFlow (新) |
|------|-------------|-----------------|----------------|
| 設定 | 約定優先 | 設定必須 | 約定優先 |
| 定義 | @Service | class継承 | @agent |
| 呼び出し | @Autowired | 手動生成 | AgentClient |
| 通信 | REST | SSE固定 | 選択可能 |
| ツール | 自動発見 | 手動設定 | 自動発見 |

