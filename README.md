# AgentFlow

<div align="center">

**軽量 AI エージェント開発フレームワーク**

_PocketFlow ベースの統一プロトコルインターフェース_

[![Python 3.13+](https://img.shields.io/badge/python-3.13+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-434%20passed-brightgreen.svg)](tests/)
[![Coverage](https://img.shields.io/badge/coverage-92.46%25-brightgreen.svg)](htmlcov/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Code style: ruff](https://img.shields.io/badge/code%20style-ruff-000000.svg)](https://github.com/astral-sh/ruff)

[ドキュメント](docs/) | [サンプル](examples/) | [貢献ガイド](CONTRIBUTING.md)

**Languages**: [English](README_EN.md) | [简体中文](README_ZH.md) | 日本語

</div>

---

## ⚠️ プロジェクトステータス

> **注意**: このプロジェクトは現在開発中です。
>
> - ✅ **自動テスト**: 434 テスト、92.46% カバレッジ
> - 🚧 **本番環境**: 使用前に十分なテストを実施してください

---

## 🎯 AgentFlow とは

**MCP / A2A / AG-UI / A2UI** の 4 プロトコルを統一インターフェースで提供する軽量 AI エージェントフレームワーク。

### 🏗️ フレームワークアーキテクチャ

AgentFlow は **8層アーキテクチャ** で構成され、各層が明確に分離されています。

```mermaid
graph TB
    subgraph L1["📱 アプリケーション層"]
        direction LR
        A1[decision_governance_engine]
        A2[market_trend_monitor]
        A3[code_migration_assistant]
        A4[Your Custom App]
    end

    subgraph L2["🎨 UI層"]
        direction LR
        U1[Studio UI<br/>ビジュアルエディタ]
        U2[A2UI<br/>宣言式UI]
        U3[AG-UI<br/>リアルタイムイベント]
    end

    subgraph L3["🔄 フロー層 - 3つの開発方式"]
        direction TB
        F1["方式1: @agent<br/>最も簡単・推奨"]
        F2["方式2: create_flow<br/>宣言的協調"]
        F3["方式3: AgentCoordinator<br/>完全制御"]
    end

    subgraph L4["🤖 Agent層"]
        direction LR
        AG1[AgentBlock<br/>基底クラス]
        AG2[@agent<br/>デコレータ]
        AG3[Custom Agent<br/>カスタム実装]
    end

    subgraph L5["🛠️ ツール層"]
        direction TB
        T1[@tool<br/>メソッドツール]
        T2[MCP Tools<br/>外部ツール統合]
        T3[Skills Engine<br/>自動進化システム]
        T4[Built-in Skills<br/>DB/決済/認証/デプロイ]
    end

    subgraph L6["🔌 Provider層 - 統一アクセス"]
        direction LR
        P1[LLMProvider<br/>OpenAI/Anthropic]
        P2[DataProvider<br/>SQL/Vector/Cache]
        P3[EventProvider<br/>SSE/WebSocket]
        P4[ToolProvider<br/>統一ツール呼び出し]
    end

    subgraph L7["🌐 プロトコル層 - 4プロトコル"]
        direction LR
        PR1[MCP]
        PR2[A2A]
        PR3[AG-UI]
        PR4[A2UI]
    end

    subgraph L8["💾 インフラ層"]
        direction LR
        I1[LLM Services<br/>OpenAI/Anthropic/Google/Ollama]
        I2[データベース<br/>Supabase/PostgreSQL/Turso]
        I3[Vector DB<br/>Pinecone/Qdrant]
        I4[キャッシュ<br/>Redis/Memory]
    end

    A1 --> F1
    A2 --> F2
    A3 --> F3
    A4 --> F1

    U1 --> F2
    U2 --> AG2
    U3 --> F3

    F1 --> AG2
    F2 --> AG1
    F3 --> AG3

    AG1 --> T1
    AG2 --> T2
    AG3 --> T3
    AG3 --> T4

    T1 --> P4
    T2 --> PR1
    T3 --> P4
    T4 --> P4

    AG1 --> P1
    AG2 --> P2
    AG3 --> P3

    P1 --> I1
    P2 --> I2
    P2 --> I3
    P2 --> I4

    P4 --> PR1
    P3 --> PR3
    U2 --> PR4
    F3 --> PR2

    style L1 fill:#e3f2fd
    style L2 fill:#fff3e0
    style L3 fill:#e8f5e9
    style L4 fill:#f3e5f5
    style L5 fill:#fff9c4
    style L6 fill:#e0f2f1
    style L7 fill:#fce4ec
    style L8 fill:#f5f5f5
```

**レイヤー詳細説明**:

| レイヤー | 主要コンポーネント | 役割 | 選択基準 |
|---------|-----------------|------|---------|
| 📱 **アプリケーション層** | decision_governance_engine, market_trend_monitor, code_migration_assistant | 実際のビジネスアプリケーション | ビジネス要件に応じて選択 |
| 🎨 **UI層** | Studio UI, A2UI, AG-UI | ビジュアルエディタ、宣言式UI生成、リアルタイム進捗表示 | ユーザー体験要件に応じて選択 |
| 🔄 **フロー層** | @agent, create_flow, AgentCoordinator | **3つの開発方式**（簡単→複雑） | 複雑度に応じて選択 |
| 🤖 **Agent層** | AgentBlock, @agent, Custom Agent | Agent実装（基底クラス/デコレータ/カスタム） | 実装方式に応じて選択 |
| 🛠️ **ツール層** | @tool, MCP Tools, Skills, Built-in | ツール統合（メソッド/MCP/自動進化/内蔵） | 機能要件に応じて選択 |
| 🔌 **Provider層** | LLMProvider, DataProvider, EventProvider, ToolProvider | **統一アクセスインターフェース**（約定優先） | 自動選択（デフォルト値あり） |
| 🌐 **プロトコル層** | MCP, A2A, AG-UI, A2UI | 4つの標準プロトコル | 統合要件に応じて自動適用 |
| 💾 **インフラ層** | LLM Services, DB, Vector DB, Cache | 外部サービス・データストア | 環境変数で自動検出 |

**データフロー例**:

```
ユーザーリクエスト
  ↓
アプリケーション層（decision_governance_engine）
  ↓
フロー層（create_flow）
  ↓
Agent層（GatekeeperAgent → DaoAgent → ...）
  ↓
ツール層（@tool / MCP / Skills）
  ↓
Provider層（LLMProvider / DataProvider）
  ↓
インフラ層（OpenAI / PostgreSQL）
  ↓
結果を返す
```

### 🔧 Core Interface Layer（v0.4.0 NEW）

AgentFlow v0.4.0 では、安定した **Core Interface Layer** を導入しました。
Studio / CLI / SDK / API 全ての消費者が統一されたインターフェースを通じてコア機能にアクセスします。

```
┌─────────────────────────────────────────────────────────────────┐
│                    Consumer Layer（消費者層）                    │
│  ┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────┐     │
│  │ Studio   │   │   CLI    │   │   SDK    │   │   API    │     │
│  │  (UI)    │   │(Command) │   │   (TS)   │   │ (REST)   │     │
│  └────┬─────┘   └────┬─────┘   └────┬─────┘   └────┬─────┘     │
└───────┼──────────────┼──────────────┼──────────────┼───────────┘
        │              │              │              │
        └──────────────┴──────┬───────┴──────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Services Layer（サービス層）                  │
│                    agentflow/services/                          │
│  ┌────────────────────────┐   ┌────────────────────────┐       │
│  │    PublishService      │   │    PreviewService      │       │
│  │ ・generate_code()      │   │ ・run()                │       │
│  │ ・preview_code()       │   │ ・run_stream()         │       │
│  │ ・export_zip()         │   │ ・run_debug()          │       │
│  │ ・deploy()             │   │ ・validate()           │       │
│  │ ・publish()            │   │                        │       │
│  └────────────────────────┘   └────────────────────────┘       │
└─────────────────────────────────┬───────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│                Core Interface Layer（核心接口層・安定）          │
│                agentflow/core/interfaces/                       │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐    │
│  │ ICodeGenerator │  │IDeployExecutor │  │IWorkflowRunner │    │
│  │   Protocol     │  │   Protocol     │  │   Protocol     │    │
│  └────────────────┘  └────────────────┘  └────────────────┘    │
│  ┌────────────────┐  ┌────────────────┐                        │
│  │ IConfigManager │  │ Types/Enums    │                        │
│  │   Protocol     │  │                │                        │
│  └────────────────┘  └────────────────┘                        │
└─────────────────────────────────┬───────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│                Implementation Layer（実装層）                    │
│  ┌─────────────┐   ┌─────────────┐   ┌─────────────┐          │
│  │  codegen/   │   │   deploy/   │   │    flow/    │          │
│  │ CodeGenerator│   │DeployExecutor│   │FlowExecutor │          │
│  └─────────────┘   └─────────────┘   └─────────────┘          │
└─────────────────────────────────────────────────────────────────┘
```

#### 核心インターフェース一覧

| インターフェース | 場所 | 説明 | 主要メソッド |
|-----------------|------|------|-------------|
| **ICodeGenerator** | `core/interfaces/code_generator.py` | ワークフロー→コード生成 | `generate()`, `preview()`, `export_zip()` |
| **IDeployExecutor** | `core/interfaces/deploy_executor.py` | コード→デプロイ実行 | `deploy()`, `deploy_sync()`, `validate_config()` |
| **IConfigManager** | `core/interfaces/config_manager.py` | 設定テンプレート管理 | `get_template()`, `get_required_fields()`, `validate()` |
| **IWorkflowRunner** | `core/interfaces/workflow_runner.py` | ワークフロー実行 | `run()`, `run_stream()`, `run_debug()` |

#### 核心データ型

| 型 | 説明 |
|----|------|
| `WorkflowDefinition` | Studio保存形式のワークフロー定義 |
| `CodeOutputType` | 出力タイプ（FRONTEND / BACKEND / FULLSTACK） |
| `DeployTarget` | デプロイ先（VERCEL / DOCKER / AWS_LAMBDA / GITHUB_ACTIONS） |
| `GeneratedCode` | 生成されたコード（files, entry_point, commands） |
| `DeployConfig` | デプロイ設定（credentials, settings, env_vars） |
| `ConfigField` | UI用設定フィールド（name, type, required, options） |

#### 実装クラス一覧

| 実装クラス | 場所 | 実装インターフェース | 説明 |
|-----------|------|---------------------|------|
| **CodeGenerator** | `agentflow/codegen/generator.py` | `ICodeGenerator` | コード生成器 |
| **BackendBuilder** | `agentflow/codegen/builders/backend.py` | - | FastAPI バックエンド生成 |
| **FrontendBuilder** | `agentflow/codegen/builders/frontend.py` | - | React フロントエンド生成 |
| **FullstackBuilder** | `agentflow/codegen/builders/fullstack.py` | - | 全栈アプリ生成 |
| **DeployExecutor** | `agentflow/deploy/executor.py` | `IDeployExecutor` | デプロイ統合実行器 |
| **VercelTarget** | `agentflow/deploy/targets/vercel.py` | `BaseDeployTarget` | Vercel デプロイ |
| **DockerTarget** | `agentflow/deploy/targets/docker.py` | `BaseDeployTarget` | Docker ビルド＆プッシュ |
| **AWSLambdaTarget** | `agentflow/deploy/targets/aws_lambda.py` | `BaseDeployTarget` | AWS Lambda デプロイ |
| **GitHubActionsTarget** | `agentflow/deploy/targets/github_actions.py` | `BaseDeployTarget` | CI/CD ワークフロー生成 |
| **ConfigManager** | `agentflow/deploy/config/manager.py` | `IConfigManager` | 設定テンプレート管理 |
| **PreviewService** | `agentflow/services/preview_service.py` | `IWorkflowRunner` | プレビュー実行サービス |
| **PublishService** | `agentflow/services/publish_service.py` | - | 統一発布サービス |

#### 使用例

```python
from agentflow.services import PublishService, PreviewService
from agentflow.core.interfaces import CodeOutputType, DeployTarget

# PublishService - コード生成＆デプロイ
publish = PublishService()

# コード生成
code = await publish.generate_code(workflow, CodeOutputType.FULLSTACK)

# ZIP エクスポート
zip_buffer = await publish.export_zip(workflow, CodeOutputType.BACKEND)

# デプロイ（ストリーム）
async for event in publish.deploy(code, DeployTarget.VERCEL, config):
    print(f"{event.progress}%: {event.message}")

# PreviewService - ワークフロー実行
preview = PreviewService()

# ストリーム実行
async for event in preview.run_stream(workflow, inputs):
    print(f"{event.type}: {event.message}")

# デバッグ実行
async for event in preview.run_debug(workflow, inputs, breakpoints=["node-1"]):
    if event.breakpoint_hit:
        print(f"Variables: {event.variables}")
```

#### Studio API エンドポイント

| エンドポイント | メソッド | 説明 |
|---------------|---------|------|
| `/api/preview/run` | POST | ワークフロー同期実行 |
| `/api/preview/stream` | POST | ワークフローSSE実行 |
| `/api/publish/export` | POST | ZIP エクスポート |
| `/api/publish/preview` | POST | 生成コードプレビュー |
| `/api/publish/deploy` | POST | 同期デプロイ |
| `/api/publish/deploy/stream` | POST | SSE デプロイ |
| `/api/publish/targets` | GET | サポートターゲット一覧 |
| `/api/publish/config-fields/{target}` | GET | 動的設定フィールド |

---

### ✨ 主な特徴

| 特徴 | 説明 |
|------|------|
| 🚀 **軽量** | コアコード ~500 行 |
| 🎯 **@agent デコレータ** | 1行でAgent定義、設定ゼロ (v0.2.0 NEW) |
| 🔧 **統一Provider** | LLM/Tool/Data/Eventの統一アクセス (v0.2.0 NEW) |
| 🔌 **4 プロトコル** | MCP / A2A / AG-UI / A2UI 統合 |
| 🎨 **自動アダプター** | `@auto_adapt` でプロトコル自動変換 |
| 🧠 **Skills 自動進化** | 越用越厉害 - 使うほど強くなる |
| 📦 **CLI** | `agentflow init/run/create` |
| 🔒 **型安全** | 100% 型アノテーション |
| ⚡ **非同期** | 完全非同期 I/O |

### 🎯 Skills 自動進化システム（NEW）

Claude Code Skills 完全互換の自動進化能力システム：

```
用户需求 → 技能匹配 → 存在なら実行
                   → 不在なら自動生成 → 検証 → 固化
= 越用越厉害（使うほど強くなる）
```

```python
from agentflow.skills import SkillEngine

engine = SkillEngine(auto_learn=True)
result = await engine.resolve("PDFからテキストを抽出")

if result.generated:
    print(f"🆕 新スキル自動生成: {result.skill.name}")
```

詳細は [Skills ガイド](docs/guide-skills.md) を参照。

### 🤖 LLM プロバイダー（松耦合設計）

**設計原則**: Agent/サービスは具体的なプロバイダー・モデルを知る必要がありません。
環境変数からAPIキーを設定するだけで自動検出されます。

| プロバイダー | 環境変数 | 対応モデル |
|-------------|---------|-----------|
| **OpenAI** | `OPENAI_API_KEY` | GPT-4o, o1, o3-mini, GPT-4o-realtime（音声） |
| **Anthropic** | `ANTHROPIC_API_KEY` | Claude 3.5 Sonnet/Haiku, Claude 4（予定） |
| **Google** | `GOOGLE_API_KEY` | Gemini 2.0 Flash, Gemini 1.5 Pro（200万トークン） |
| **DeepSeek** | `DEEPSEEK_API_KEY` | DeepSeek V3, DeepSeek R1（推論） |
| **Ollama** | `OLLAMA_BASE_URL` | Llama 3.3, Qwen 2.5, Mistral Large（ローカル） |
| **LocalAI** | `LOCALAI_BASE_URL` | 任意のGGUF/GGML（ローカル・デフォルト） |

```python
# ✅ 推奨: get_llm() 松耦合 API
from agentflow import get_llm

# プロバイダー/モデル不明でOK - 環境変数から自動検出
llm = get_llm()
response = await llm.chat([{"role": "user", "content": "Hello!"}])
print(response["content"])

# Agent 内での使用例
class MyAgent(AgentBlock):
    async def run(self, input_data):
        llm = get_llm(temperature=0.3)  # 分析タスク向け低温度
        result = await llm.chat([{"role": "user", "content": "..."}])
        return result["content"]

# ストリーミング
llm = get_llm()
async for chunk in llm.stream([{"role": "user", "content": "..."}]):
    print(chunk, end="", flush=True)

# 高度なルーティング（コスト最適化・フォールバック）
from agentflow.llm import create_router_from_env
router = create_router_from_env()  # 複数プロバイダー自動管理
```

### 🗄️ データベース（松耦合設計）

**設計原則**: Agent/サービスは具体的なDB実装を知る必要がありません。

| データベース | 環境変数 | 特徴 |
|-------------|---------|------|
| **Supabase** | `SUPABASE_URL` + `SUPABASE_KEY` | RLS、リアルタイム、500MB無料 |
| **PostgreSQL** | `DATABASE_URL` | 汎用、SSL対応 |
| **Turso** | `TURSO_URL` + `TURSO_AUTH_TOKEN` | エッジ対応、9GB無料 |

```python
# ✅ 推奨: get_db() 松耦合 API
from agentflow import get_db

# プロバイダー/接続情報不明でOK - 環境変数から自動検出
db = get_db()
await db.connect()

# CRUD 操作
users = await db.select("users", filters={"status": "active"})
new_user = await db.insert("users", {"email": "test@example.com"})
await db.update("users", {"name": "Updated"}, filters={"id": 1})
await db.delete("users", filters={"id": 1})
```

### 🔍 ベクトルDB & Embedding（松耦合設計）

| サービス | 環境変数 | 用途 |
|----------|---------|------|
| **Pinecone** | `PINECONE_API_KEY` | クラウドベクトルDB |
| **Qdrant** | `QDRANT_URL` | セルフホストベクトルDB |
| **ChromaDB** | `CHROMA_PERSIST_DIR` | ローカルベクトルDB |
| **OpenAI Embedding** | `OPENAI_API_KEY` | text-embedding-3-small |
| **SentenceTransformer** | `USE_LOCAL_EMBEDDING` | ローカル埋め込み |

```python
# ✅ 推奨: get_vectordb() / get_embedding() 松耦合 API
from agentflow import get_vectordb, get_embedding

# VectorDB（Pinecone/Qdrant/ChromaDB 自動検出）
vdb = get_vectordb()
await vdb.connect()
await vdb.add(["doc1", "doc2"], ids=["1", "2"])
results = await vdb.search("query text", top_k=5)

# Embedding（OpenAI/SentenceTransformer 自動検出）
emb = get_embedding()
vector = await emb.embed_text("Hello world")
vectors = await emb.embed_batch(["text1", "text2"])
```

### 🏗️ 内蔵 Production-Ready Skills

| スキル | 説明 | 対応サービス |
|--------|------|------------|
| 🗄️ **database-manager** | DB統合管理、CRUD、RLS | Supabase / Turso / PostgreSQL |
| 💳 **stripe-payment** | 決済・サブスク管理 | Stripe Checkout / Billing |
| 🚀 **deployment-manager** | デプロイ・環境管理 | Vercel / Cloudflare Pages |
| 🔐 **auth-provider** | 認証・セッション管理 | Supabase Auth / Clerk |
| 🔄 **model-router** | 複数LLM切替・コスト最適化 | 全プロバイダー対応 |

詳細は [内蔵 Skills ガイド](docs/guide-builtin-skills.md) を参照。

### 🧠 協調パターン

| パターン | 説明 |
|---------|------|
| **Supervisor** | 監督者が動的にワーカー選択 |
| **Hierarchical** | 階層的タスク分解 |
| **Sequential/Concurrent** | 順次/並行実行 |

## 📦 インストール

```bash
# Conda 環境
conda env create -f environment.yml
conda activate agentflow

# または pip
pip install -e .
```

---

## 🚀 クイックスタート

AgentFlow は **3つの開発方式** を提供します。用途に応じて最適な方法を選択してください。

### 方式1: @agent デコレータ（最も簡単・推奨）

**特徴**: 設定ゼロ、1行でAgent定義、すぐに使える

```python
from agentflow import agent, tool, AgentClient

@agent  # デコレータ一つでAgent定義
class QAAgent:
    """質問応答Agent - 設定ゼロで動作"""
    
    system_prompt = "あなたは親切なアシスタントです"
    
    @tool  # ツールを自動登録
    def search_database(self, query: str) -> list:
        """DBを検索"""
        return []  # 実際のDB検索

# 呼び出し（同期）
result = await AgentClient.get("QAAgent").invoke({"question": "..."})

# ストリーム（SSE）
async for chunk in AgentClient.get("QAAgent").stream({"question": "..."}):
    print(chunk)
```

**適用シーン**: 
- ✅ 単一Agentの簡単なタスク
- ✅ プロトタイプ開発
- ✅ クイックスタート

---

### 方式2: create_flow（複数Agent協調 - チェーンビルダー）

**特徴**: 宣言的チェーンAPI、Gate/Review/並行実行、REVISE回退、進捗追跡

```python
from agentflow import create_flow

# 複雑なワークフロー（Gate + Review + REVISE）
flow = (
    create_flow("decision-engine")
    .gate(GatekeeperAgent, check=lambda r: r["is_acceptable"])  # 条件分岐
    .then(ClarificationAgent)                                     # 順次実行
    .then(DaoAgent, FaAgent, ShuAgent, QiAgent, ids=["dao", "fa", "shu", "qi"])
    .review(
        ReviewAgent,
        retry_from="dao",      # REVISE時はdaoに回退
        max_revisions=2,       # 最大2回修正
        on_pass=lambda ctx: generate_report(ctx)
    )
    .build()
)

# 同期実行
result = await flow.run({"question": "新規事業AとBのどちらに投資すべきか"})

# SSEストリーム（進捗付き）
async for event in flow.run_stream({"question": "..."}):
    print(f"{event['type']}: {event.get('node_name', '')}")
    # flow_start → node_start → node_complete → ... → flow_complete

# 記憶システム
flow.memory.remember("key", "value")
value = flow.memory.recall("key")
```

**適用シーン**:
- ✅ 複数Agentの協調処理（Gate→Agent→Review）
- ✅ 条件分岐が必要なワークフロー
- ✅ 審査・回退ロジックが必要な場合
- ✅ SSE進捗表示が必要な場合

---

### 方式3: AgentCoordinator（完全制御）

**特徴**: 最大の柔軟性、カスタムロジック、高度な制御

```python
from agentflow.patterns.multi_agent import AgentCoordinator, SharedContext
from agentflow.patterns.supervisor import SupervisorCoordinator

# Sequential協調
coordinator = AgentCoordinator(
    agents=[Agent1(), Agent2(), Agent3()],
    pattern="sequential",
    shared_context=SharedContext(enable_memory=True)
)
result = await coordinator.execute({"task": "..."})

# Supervisorパターン（動的選択）
supervisor = SupervisorCoordinator(
    supervisor=SupervisorAgent(),
    workers={
        "research": ResearchAgent(),
        "write": WriteAgent(),
        "review": ReviewAgent(),
    },
    max_iterations=10
)
result = await supervisor.execute("タスク")
```

**適用シーン**:
- ✅ 複雑なビジネスロジック
- ✅ カスタム協調パターン
- ✅ エンタープライズ級アプリケーション

---

### 方式4: Engine Pattern（配置即用・NEW）

**特徴**: 4種類の予定義パターン、配置だけで使用可能、90%のAIアプリシーンをカバー

```mermaid
graph LR
    subgraph Engines["⚙️ Engine Pattern（4種類）"]
        E1["SimpleEngine<br/>単一Agent問答"]
        E2["GateEngine<br/>Gate + Agent"]
        E3["PipelineEngine<br/>多Agent + Review"]
        E4["RAGEngine<br/>RAG増強"]
    end

    subgraph Apps["🏢 アプリケーション"]
        A1["カスタマーBot"]
        A2["承認フロー"]
        A3["決策エンジン"]
        A4["ナレッジベース"]
    end

    A1 --> E1
    A2 --> E2
    A3 --> E3
    A4 --> E4

    style Engines fill:#fff3e0
    style Apps fill:#e3f2fd
```

#### 4種類のEngine Pattern

| Pattern | 適用シーン | フロー構造 |
|---------|-----------|-----------|
| **SimpleEngine** | 単一Agent問答、ChatBot | Agent → Response |
| **GateEngine** | 権限チェック、コンプライアンス | Gate → Agent → Response |
| **PipelineEngine** | 多ステップ審査、決策フロー | Gate → [Agents] → Review → Report |
| **RAGEngine** | ナレッジベース、文書QA | RAG検索 → Agent → Response |

#### 使用例

```python
from agentflow.engines import SimpleEngine, GateEngine, PipelineEngine, RAGEngine

# 1. SimpleEngine - 最もシンプル
engine = SimpleEngine(agent=QAAgent)
result = await engine.run({"question": "こんにちは"})

# 2. GateEngine - 前置チェック付き
engine = GateEngine(
    gate_agent=ComplianceChecker,
    main_agent=ApprovalAgent,
    gate_check=lambda r: r.get("compliant", False),
)

# 3. PipelineEngine - 複雑なフロー
engine = PipelineEngine(
    stages=[
        {"name": "gate", "agent": GatekeeperAgent, "gate": True},
        {"name": "analysis", "agents": [DaoAgent, FaAgent, ShuAgent], "parallel": True},
        {"name": "review", "agent": ReviewAgent, "review": True, "retry_from": "analysis"},
    ],
    max_revisions=2,
    report_generator=my_report_generator,
)

# 4. RAGEngine - ナレッジベース増強
engine = RAGEngine(
    agent=KnowledgeAgent,
    vector_store="company_docs",
    top_k=5,
)

# 共通API: run() / run_stream()
result = await engine.run(inputs)
async for event in engine.run_stream(inputs):
    print(event)  # AG-UI イベント
```

**適用シーン**:
- ✅ 高速プロトタイプ開発
- ✅ 標準パターンの再利用
- ✅ 配置ベースのAIアプリ構築

---

詳細は [クイックスタート](docs/quickstart.md) を参照。

---

## 🎨 使用シナリオ

AgentFlow は3つの操作方法を提供します。用途に応じて最適な方法を選択してください。

### 1. 🖱️ Studio UI（ビジュアルエディタ）

**コードを書かずに、ブラウザ上でドラッグ&ドロップでワークフローを作成**

- ✅ **初心者向け**: プログラミング知識不要
- ✅ **視覚的**: ワークフローを視覚的に理解・編集
- ✅ **迅速**: 数分でワークフローを作成

**使用例**: 複数のエージェントを接続して複雑な処理フローを構築

📖 [Studio UI 操作ガイド](docs/guide-studio-ui.md) - インストール、使用、注意事項、ベストプラクティス

---

### 2. ⚡ CLI（コマンドライン）

**ターミナルから素早くエージェントを実行・管理**

- ✅ **高速**: GUI なしで高速に操作
- ✅ **自動化**: スクリプト化・バッチ処理に最適
- ✅ **シンプル**: コマンド1つで実行

**使用例**: バッチ処理、CI/CD パイプライン、サーバー環境での実行

📖 [CLI 操作ガイド](docs/guide-cli.md) - インストール、使用、注意事項、ベストプラクティス

---

### 3. 🐍 コーディング（Python）

**Python コードでエージェントを開発・カスタマイズ**

- ✅ **柔軟性**: 完全なカスタマイズが可能
- ✅ **型安全**: 100% 型アノテーション対応
- ✅ **拡張性**: プロトコル統合・協調パターンが利用可能

**使用例**: カスタムエージェントの開発、複雑なロジックの実装、プロトコル統合

📖 [コーディングガイド](docs/guide-coding.md) - インストール、使用、注意事項、ベストプラクティス

---

## 📚 ドキュメント

| ドキュメント | 説明 |
|------------|------|
| [Studio UI 操作ガイド](docs/guide-studio-ui.md) | ビジュアルエディタでの操作 |
| [CLI 操作ガイド](docs/guide-cli.md) | コマンドラインでの操作 |
| [コーディングガイド](docs/guide-coding.md) | Python コードでの開発 |
| [Skills ガイド](docs/guide-skills.md) | 自動進化システム |
| [内蔵 Skills ガイド](docs/guide-builtin-skills.md) | DB/決済/認証/デプロイ（NEW） |
| [LLM ルーター](docs/guide-llm-router.md) | マルチモデル切替（NEW） |
| [アーキテクチャ](docs/architecture.md) | 設計思想・構成 |
| [プロトコル](docs/protocols.md) | MCP/A2A/AG-UI/A2UI |
| [API](docs/api.md) | API リファレンス |
| [CLI](docs/cli.md) | コマンド一覧 |
| [クイックスタート](docs/quickstart.md) | 入門ガイド |
| [開発規範](docs/DEVELOPMENT_STANDARDS_JA.md) | コーディング規約 |

---

## 🤝 貢献

AgentFlow への貢献を歓迎します！

- [貢献ガイドライン](CONTRIBUTING.md) - ローカル開発環境のセットアップ、テスト提出、プルリクエストの手順
- [変更履歴](CHANGELOG.md)

---
## その他
　他の優秀なAIエージェント開発フレームワーク
    [agno](https://github.com/agno-agi/agno)

| フレームワーク | 説明 |
## 📄 ライセンス

[MIT License](LICENSE)

---

<div align="center">

**AgentFlow で AI エージェント開発を加速！**

Made with ❤️ by the AgentFlow Team

</div>
