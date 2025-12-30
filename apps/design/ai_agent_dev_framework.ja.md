````markdown
# AgentFlowフレームワーク v2.0 - オープン標準統合版

> PocketFlow + 3つのオープンプロトコル (MCP/A2A/AG-UI) に基づくエンタープライズ向けAIエージェント開発フレームワーク
> スローガン: **ブロックを組み立てるように開発、プロンプト駆動、オープン標準で相互接続**

---

## 🎯 コア設計理念

### 三層プロトコルスタックアーキテクチャ

```
┌─────────────────────────────────────────┐
│         AG-UI Layer (フロントエンド対話層)     │  ユーザー ↔ エージェント
│  • リアルタイムストリーミングUI更新            │
│  • マルチモーダル対話(テキスト/音声/フォーム)   │
│  • エージェント状態の可視化                    │
└─────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────┐
│       A2A Layer (エージェント協調層)           │  エージェント ↔ エージェント
│  • フレームワーク横断のエージェント通信        │
│  • タスク委譲と調整                            │
│  • 分散マルチエージェントのオーケストレーション│
└─────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────┐
│      PocketFlow Core (実行エンジン)            │
│  • Node-Flowワークフローエンジン               │
│  • Shared Storeによるデータ管理                │
│  • リトライ/フォールバック/並行制御             │
└─────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────┐
│        MCP Layer (ツール接続層)               │  エージェント ↔ ツール/データ
│  • 標準化されたツール呼び出し                  │
│  • データソース接続                            │
│  • コンテキスト管理                            │
└─────────────────────────────────────────┘
```

### ブロック化設計の4原則

#### 1️⃣ **標準インターフェースで疎結合**

各エージェントは独立した「ブロック」として、標準プロトコルで相互接続されます:

```python
class AgentBlock:
    # A2Aインターフェース - エージェント間通信
    def get_agent_card(self) -> AgentCard: ...

    # AG-UIインターフェース - ユーザー対話
    def emit_ui_event(self, event: UIEvent): ...

    # MCPインターフェース - ツール利用
    def list_tools(self) -> List[Tool]: ...

    # PocketFlowインターフェース - ワークフロー実行
    def get_flow(self) -> Flow: ...
```

#### 2️⃣ **ホットプラグ & 組み合わせ可能**

レゴのように自由に組み合わせ可能:

```yaml
# 製品設定 = エージェントブロックの組合せ
product:
  name: "ドキュメント処理アシスタント"
  agents:
    - id: pdf_analyzer
      source: "local://agents/pdf_analyzer" # ローカルAgent
    - id: translator
      source: "a2a://translation-service.com" # リモートA2A Agent
    - id: email_sender
      source: "marketplace://email-pro" # マーケットAgent

  workflow:
    - when: user_upload_pdf
      call: pdf_analyzer
    - then: translator
      if: needs_translation
    - finally: email_sender
```

#### 3️⃣ **テンプレート化で再利用可能**

PocketFlowのテンプレート思想に従う:

```
templates/
├── basic/              # 基本シングルAgentテンプレート
├── rag/                # RAGパターンテンプレート
├── mapreduce/          # MapReduceパターンテンプレート
├── agent-orchestrator/ # マルチエージェントオーケストレーションテンプレ
└── custom/            # カスタムテンプレート
    └── pdf-to-report/  # 具体的シナリオテンプレート(再利用可能)
        ├── design.md
        ├── agent.yaml
        ├── nodes.py
        └── prompts/
```

#### 4️⃣ **可視化 & ノーコード**

将来の可視化プラットフォーム向けインターフェースを用意:

```json
// agent.visual.json - 可視化メタデータ
{
  "ui": {
    "icon": "📄",
    "color": "#3B82F6",
    "category": "ドキュメント処理",
    "inputs": [
      {"name": "pdf_path", "type": "file", "accept": ".pdf"}
    ],
    "outputs": [
      {"name": "summary", "type": "text", "preview": true}
    ]
  },
  "a2a_card": {...},
  "ag_ui_events": [...]
}
```

---

## 1️⃣ ユーザーストーリー (User Stories)

### US-001: ワンクリック起動、3プロトコル対応

**役割**: 開発者
**目的**: 作成したエージェントが自動的に MCP/A2A/AG-UI の3プロトコルをサポートするようにしたい
**理由**: AIエコシステムへシームレスに接続するため

**受け入れ基準**:

```bash
agentflow init my-agent --protocols mcp,a2a,ag-ui
# 生成されるAgentには以下が自動で含まれる:
# - MCPツール定義(tools.mcp.json)
# - A2A AgentCard(agent.a2a.json)
# - AG-UIイベントマッピング(agent.agui.json)
```

### US-002: ブロックマーケット、検索してすぐ使える

**役割**: 開発者
**目的**: 既製のエージェントブロックを検索してインポートしたい
**理由**: 製品プロトタイプを迅速に組み立てるため

**受け入れ基準**:

```bash
# エージェントブロックを検索
agentflow search "PDF解析"
> 3つのブロックが見つかる:
> 1. pdf-parser-basic (ローカル) - 基本PDFテキスト抽出
> 2. pdf-analyzer-pro (A2A) - 高度なPDF解析、表検出対応
> 3. pdf-ocr-agent (A2A) - OCR認識

# インストールして自動設定
agentflow install pdf-analyzer-pro
> ✅ A2A AgentCard をダウンロード
> ✅ 認証情報を設定
> ✅ ローカルアダプタを生成
> ✅ プロジェクト依存に追加

# コード内で直接使用
from agentflow import get_agent
pdf_agent = get_agent("pdf-analyzer-pro")
result = pdf_agent.run({"pdf_path": "test.pdf"})
```

### US-003: 可視化オーケストレーション、ノーコード構築

**役割**: プロダクトマネージャー
**目的**: ドラッグ&ドロップの画面でエージェントワークフローを設計したい
**理由**: コードを書かずにAI製品を構築するため

**受け入れ基準**:

- ✅ Webオーケストレータを開く: `agentflow studio`
- ✅ エージェントブロックをキャンバスにドラッグ&ドロップ
- ✅ エージェント間のデータフローを接続
- ✅ AG-UI対話インターフェースをプレビュー
- ✅ ワンクリックでデプロイ可能なコードを生成

### US-004: テンプレート再利用、プロンプトだけ変更

**役割**: 開発者
**目的**: 既存のシナリオテンプレートを使いたい
**理由**: プロンプトや設定を変えるだけで迅速にローンチできるため

**受け入れ基準**:

```bash
# テンプレートから作成
agentflow create from-template invoice-processor
> 📋 テンプレート使用: 請求書処理システム
> 📝 以下の質問に答えてAgentをカスタマイズ:
>
> 1. 請求書フォーマット? [PDF/画像/Excel] PDF
> 2. 抽出するフィールド? 請求書番号, 金額, 日付, 供給元
> 3. 処理後のアクション? [DBへ保存/メール送信/レポート生成] DBへ保存
>
> ✅ 生成完了! prompts/extract.txt の抽出ルールを調整するだけ
+```

---

## 2️⃣ フレームワーク要件 (Framework Requirements)

### コアアーキテクチャ図

```mermaid
graph TB
    subgraph "ユーザインターフェース層"
        WebUI[Webインターフェース<br/>AG-UIプロトコル]
        MobileUI[モバイル端末<br/>AG-UIプロトコル]
        VSCODE[VS Codeプラグイン<br/>AG-UIプロトコル]
    end

    subgraph "AgentFlowコア"
        Studio[可視化オーケストレーター]
        Registry[Agentブロックマーケット]
        CLI[コマンドラインツール]

        subgraph "ランタイムエンジン"
            A2AServer[A2A Server<br/>エージェント協調]
            AGUIEmitter[AG-UI Emitter<br/>UIイベントストリーム]
            FlowEngine[PocketFlow Engine<br/>ワークフロー実行]
            MCPClient[MCP Client<br/>ツール呼び出し]
        end
    end

    subgraph "エージェントブロックライブラリ"
        LocalAgent[ローカルAgentブロック]
        RemoteAgent[リモートA2A Agent]
        MarketAgent[マーケットAgentブロック]
    end

    subgraph "ツール & データ層"
        MCPTools[MCPツールサーバ]
        Database[(データベース)]
        APIs[外部API]
    end

    WebUI --> AGUIEmitter
    MobileUI --> AGUIEmitter
    VSCODE --> AGUIEmitter

    Studio --> Registry
    CLI --> Registry
    Registry --> LocalAgent
    Registry --> RemoteAgent
    Registry --> MarketAgent

    AGUIEmitter --> FlowEngine
    A2AServer --> FlowEngine
    FlowEngine --> MCPClient

    RemoteAgent -.A2Aプロトコル.-> A2AServer
    FlowEngine --> LocalAgent
    MCPClient --> MCPTools
    MCPClient --> Database
    MCPClient --> APIs
```

### 技術スタック選定

| レイヤー | 技術選択 | 理由 |
| -------- | -------- | ---- |
| **ワークフローエンジン** | PocketFlow (100行) | シンプル、拡張容易、AIに親和性あり |
| **A2A通信** | google-a2a SDK | 公式SDKでエコシステムのサポートが良い |
| **AG-UI実装** | AG-UI Python SDK | SSEストリーミング出力をサポート |
| **MCPクライアント** | mcp Python SDK | Anthropic公式 |
| **可視化** | React Flow + shadcn/ui | ドラッグ&ドロップでの編成 |
| **CLI** | Click + Rich | きれいなCLI表示 |
| **設定管理** | YAML + Pydantic | 型安全 |

### プロジェクト構成 v2.0

```
agentflow-project/
├── .agentflow/                 # フレームワーク設定ディレクトリ
│   ├── config.yaml             # プロジェクト全体設定
│   ├── protocols/              # プロトコル設定
│   │   ├── mcp.json            # MCPツール一覧
│   │   ├── a2a.json            # A2A AgentCard
│   │   └── agui.json           # AG-UIイベント定義
│   ├── prompts/                # AIアシスタント用プロンプト集
│   │   ├── design.prompt
│   │   ├── implement.prompt
│   │   └── optimize.prompt
│   └── registry.yaml           # インストール済みAgent一覧
│
├── agents/                     # エージェントブロックライブラリ
│   └── my_agent/               # 単一エージェントブロック
│       ├── agent.yaml  Yeah.         # エージェントメタデータ(ブロックの説明)
│       ├── design.md           # 設計ドキュメント
│       ├── nodes.py            # PocketFlowのノード
│       ├── flow.py             # PocketFlowのフロー
│       ├── a2a_adapter.py      # A2Aアダプタ
│       ├── agui_events.py      # AG-UIイベント定義
│       └── visual.json         # 可視化メタデータ
│
├── tools/                      # MCPツール集
│   ├── mcp_server.py           # MCPサーバ
│   ├── llm_tools.py
│   ├── file_tools.py
│   └── custom_tools.py
│
├── workflows/                  # 製品レベルのワークフロー(ブロックの組合せ)
│   └── document_processor/
│       ├── workflow.yaml       # ワークフロー定義
│       ├── orchestrator.py     # オーケストレーションロジック
│       └── ui_config.yaml      # AG-UIインターフェース設定
│
├── tests/                      # テスト
│   ├── test_agents.py
│   ├── test_tools.py
│   └── test_protocols.py       # プロトコル互換性テスト
│
├── main.py                     # エントリポイント
├── requirements.txt
└── README.md
```

### エージェントブロック仕様 (agent.yaml)

```yaml
# agents/pdf_analyzer/agent.yaml
meta:
  id: pdf-analyzer-v1
  name: "PDFアナライザー"
  version: "1.0.0"
  author: "your-team"
  icon: "📄"
  category: "ドキュメント処理"
  description: "高度なPDF解析。テキスト抽出、表検出、要約生成をサポート"

# ブロックインターフェース定義
interfaces:
  # 入力インターフェース(ソケット)
  inputs:
    - name: pdf_path
      type: file
      required: true
      accept: [".pdf"]
      description: "解析対象のPDFファイルパス"
    - name: analysis_type
      type: enum
      options: ["extract", "summarize", "qa"]
      default: "extract"

  # 出力インターフェース(プラグ)
  outputs:
    - name: result
      type: object
      schema:
        text: string
        tables: array
        metadata: object

# プロトコルサポート
protocols:
  mcp:
    tools: ["read_pdf", "extract_tables"]
    resources: ["pdf_content"]

  a2a:
    enabled: true
    card_path: "./a2a_card.json"
    skills: ["document_analysis", "table_extraction"]

  agui:
    enabled: true
    events:
      - type: "TEXT_MESSAGE_CONTENT" # ストリーミングで解析進捗を出力
      - type: "TOOL_CALL_START" # ツール呼び出しを表示
      - type: "STATE_DELTA" # 解析状態の差分を更新

# 依存関係
dependencies:
  agents: [] # 他Agentへの依存なし
  tools: # MCPツール依存
    - mcp://file-tools/read_pdf
    - mcp://llm-tools/summarize
  packages: # Pythonパッケージ依存
    - PyPDF2>=3.0.0
    - pdfplumber>=0.9.0

# PocketFlow設定
pocketflow:
  entry: "flow.py:create_pdf_analysis_flow"
  shared_schema: "schemas.py:PDFAnalysisSchema"

# 可視化設定
visual:
  color: "#3B82F6"
  size: "medium" # small/medium/large
  ports:
    input: ["left"]
    output: ["right"]
```

---

## 3️⃣ 詳細機能設計

### 機能1: 3プロトコル自動アダプタ

**設計目標**: エージェント開発者はビジネスロジックだけに注力すればよく、プロトコル層は自動生成される

```python
# agentflow/adapters.py
from agentflow import AgentBlock, auto_adapt

@auto_adapt(protocols=["mcp", "a2a", "agui"])
class PDFAnalyzerAgent(AgentBlock):
    """コアビジネスロジックのみ実装すれば、プロトコル適合は自動化される"""

    def __init__(self):
        self.metadata = self.load_metadata("agent.yaml")
        self.flow = self.create_flow()

    # コアビジネスロジック - PocketFlow
    def create_flow(self) -> Flow:
        read_node = ReadPDFNode()
        analyze_node = AnalyzeNode()
        output_node = OutputNode()

        read_node >> analyze_node >> output_node
        return Flow(start=read_node)

    # 以下のメソッドは @auto_adapt デコレータにより自動生成される:

    # def get_mcp_tools(self) -> List[Tool]:
    #     """agent.yaml の protocols.mcp から自動生成"""
    #     ...

    # def get_a2a_card(self) -> AgentCard:
    #     """agent.yaml の protocols.a2a から自動生成"""
    #     ...

    # def emit_agui_events(self, flow_state):
    #     """Flow実行状態をAG-UIイベントへ変換して出力"""
    #     ...
```

**自動アダプタ実装ロジック**:

```python
# agentflow/adapters/auto_adapter.py
class ProtocolAdapter:
    @staticmethod
    def generate_mcp_tools(agent_meta: dict) -> List[Tool]:
        """agent.yaml からMCPツール定義を自動生成"""
        tools = []
        for tool_uri in agent_meta["protocols"]["mcp"]["tools"]:
            # mcp://category/tool_name を解析
            category, name = parse_mcp_uri(tool_uri)
            tool_def = load_tool_definition(category, name)
            tools.append(tool_def)
        return tools

    @staticmethod
    def generate_a2a_card(agent_meta: dict) -> AgentCard:
        """A2A AgentCard を生成"""
        return AgentCard(
            name=agent_meta["meta"]["name"],
            description=agent_meta["meta"]["description"],
            version=agent_meta["meta"]["version"],
            skills=[
                Skill(
                    name=skill,
                    input_schema=agent_meta["interfaces"]["inputs"],
                    output_schema=agent_meta["interfaces"]["outputs"]
                )
                for skill in agent_meta["protocols"]["a2a"]["skills"]
            ],
            endpoint=f"http://localhost:8000/agents/{agent_meta['meta']['id']}"
        )

    @staticmethod
    def wrap_flow_with_agui(flow: Flow, agent_meta: dict):
        """PocketFlow の実行を AG-UI イベントストリームにラップする"""

        # Flow 実行前
        def on_flow_start(shared):
            emit_agui_event({
                "type": "RUN_STARTED",
                "data": {"agent": agent_meta["meta"]["name"]}
            })

        # ノード実行時
        def on_node_exec(node, input_data):
            emit_agui_event({
                "type": "TOOL_CALL_START",
                "data": {"tool": node.__class__.__name__, "input": input_data}
            })

        # LLMストリーミング出力時
        def on_llm_stream(token):
            emit_agui_event({
                "type": "TEXT_MESSAGE_CONTENT",
                "data": {"delta": token}
            })

        # 状態変化時
        def on_state_change(old_state, new_state):
            emit_agui_event({
                "type": "STATE_DELTA",
                "data": {"patch": diff(old_state, new_state)}
            })

        # Flow にフックを注入
        flow.register_hooks({
            "on_start": on_flow_start,
            "on_node_exec": on_node_exec,
            "on_llm_token": on_llm_stream,
            "on_state_change": on_state_change
        })
```

### 機能2: エージェントブロックマーケット

**ローカルマーケット** (`.agentflow/marketplace/`)

```
marketplace/
├── index.yaml              # マーケットインデックス
├── agents/
│   ├── pdf-analyzer/
│   │   ├── agent.yaml
│   │   ├── package.tar.gz
│   │   └── README.md
│   └── email-sender/
└── templates/
    └── invoice-processor/  # 一式テンプレート
```

**リモートマーケットAPI**:

```python
# agentflow/marketplace.py
class AgentMarketplace:
    def search(self, query: str, filters: dict = None):
        """エージェントブロックを検索する"""
        # タグ、カテゴリ、プロトコル対応などでフィルタ可能
        return [
            {
                "id": "pdf-analyzer-pro",
                "name": "PDF高度解析器",
                "author": "acme-corp",
                "version": "2.1.0",
                "protocols": ["mcp", "a2a", "agui"],
                "rating": 4.8,
                "downloads": 15234,
                "price": "free",
                "source": "a2a://marketplace.agentflow.ai/pdf-analyzer-pro"
            }
        ]

    def install(self, agent_id: str):
        """エージェントブロックをインストールする"""
        # 1. AgentパッケージをダウンロードまたはA2Aエンドポイント取得
        # 2. 依存関係を解析
        # 3. 必要なツールや他Agentをインストール
        # 4. ローカルアダプタ生成
        # 5. registry.yaml を更新
        pass

    def publish(self, agent_path: str):
        """自身のエージェントをマーケットへ公開する"""
        # 1. agent.yaml の完全性を検証
        # 2. プロトコル互換性テストを実行
        # 3. パッケージ化してアップロード
        # 4. (リモートAgentの場合) A2Aエンドポイントを生成
        pass
```

### 機能3: 可視化オーケストレーター (AgentFlow Studio)

**起動方法**:

```bash
agentflow studio
> 🎨 AgentFlow Studio が起動しました
> 🌐 アクセス: http://localhost:3000
```

**コア機能**:

1. **キャンバス上のオーケストレーション**

```
┌────────────────────────────────────────────────────┐
│  [File] [Edit] [View] [Agents]  [🔍Search Agents]  │
├────────────────────────────────────────────────────┤
│                                                    │
│   📤 User Upload                                   │
│        │                                           │
│        ↓                                           │
│   [📄 PDF Analyzer]  ←─ マーケットからドラッグしたAgentブロック │
│        │                                           │
│        ├─→ {text}                                  │
│        ├─→ {tables}                                │
│        │                                           │
│        ↓                                           │
│   [🌐 Translator]                                  │
│        │                                           │
│        ↓                                           │
│   [📧 Email Sender]                                │
│        │                                           │
│        ↓                                           │
│   ✅ Done                                          │
│                                                    │
├────────────────────────────────────────────────────┤
│  Properties:          Preview:                     │
│  Agent: PDF Analyzer  ┌─────────────────┐         │
│  ID: pdf-analyzer     │  [Test Run] 🔄  │         │
│  Inputs:              │                 │         │
│    • pdf_path: file   │  📄 分析中...   │         │
│  Outputs:             │  ▓▓▓▓░░░░ 50%   │         │
│    • text: string     │                 │         │
│    • tables: array    └─────────────────┘         │
└────────────────────────────────────────────────────┘
```

2. **リアルタイムプレビュー**
   - 「Test Run」でワークフローをテスト可能
   - AG-UIプロトコルを通じてエージェント状態をリアルタイム表示
   - ブレークポイントによるデバッグをサポート

3. **ワンクリックコード生成**

```python
# studio が自動生成するワークフローコード
# workflows/my_workflow/orchestrator.py

from agentflow import get_agent, Workflow

class MyWorkflow(Workflow):
    def __init__(self):
        # キャンバス上のAgentブロックを読み込む
        self.pdf_analyzer = get_agent("pdf-analyzer")
        self.translator = get_agent("translator")
        self.email_sender = get_agent("email-sender")

    async def run(self, user_input):
        # キャンバスの接続順に実行
        pdf_result = await self.pdf_analyzer.run({
            "pdf_path": user_input["file"]
        })

        translated = await self.translator.run({
            "text": pdf_result["text"],
            "target_lang": "en"
        })

        await self.email_sender.run({
            "to": user_input["email"],
            "content": translated["text"]
        })

        return {"status": "success"}
```

### 機能4: テンプレートシステム強化

**シナリオテンプレートライブラリ**:

```
templates/
├── scenarios/                  # 具体的業務シナリオテンプレ
│   ├── invoice-processor/      # 請求書処理
│   │   ├── README.md
│   │   ├── template.yaml       # テンプレートメタデータ
│   │   ├── agents/             # 事前設定されたAgent群
│   │   ├── prompts/            # カスタマイズ可能なプロンプト
│   │   └── config.example.yaml # 設定サンプル
│   
│   ├── customer-service-bot/   # カスタマーサポートボット
│   ├── data-analysis-pipeline/ # データ分析パイプライン
│   └── email-automation/       # メール自動化
│
└── patterns/                   # 設計パターンテンプレ
    ├── rag/
    ├── mapreduce/
    └── multi-agent-debate/
```

**テンプレートメタデータ** (template.yaml):

```yaml
meta:
  id: invoice-processor-v1
  name: "請求書処理システム"
  description: "PDF/画像の請求書を自動処理し、情報を抽出してDBに保存"
  author: "agentflow-team"
  version: "1.0.0"
  difficulty: "beginner" # beginner/intermediate/advanced

# テンプレートに含まれるブロック
includes:
  agents:
    - pdf-extractor
    - invoice-parser
    - database-writer
  tools:
    - pdf-reader
    - ocr-tool
    - db-connector

# カスタマイズ可能なパラメータ(ウィザード形式)
customization:
  questions:
    - id: invoice_format
      prompt: "請求書の形式は?"
      type: select
      options: ["PDF", "画像", "Excel"]
      default: "PDF"

    - id: extract_fields
      prompt: "どのフィールドを抽出しますか?"
      type: multiselect
      options: ["請求書番号", "金額", "日付", "供給者", "税額"]
      default: ["請求書番号", "金額", "日付"]

    - id: output_action
      prompt: "処理後のアクションは?"
      type: select
      options: ["DBへ保存", "メール送信", "レポート生成"]
      default: "DBへ保存"

  # ユーザ回答に基づき生成される設定
  config_template: |
    invoice:
      format: {{ invoice_format }}
      fields: {{ extract_fields | join(', ') }}
    output:
      action: {{ output_action }}

# 編集が必要なプロンプトファイル
prompts_to_edit:
  - path: "prompts/extract.txt"
    description: "請求書フィールド抽出ルールを編集"
    example: |
      請求書から以下を抽出:
      - 請求書番号: 通常右上
      - 金額: "合計"または"総計"を検索
      ...

# デプロイ設定
deployment:
  type: "standalone" # standalone/cloud/edge
  dependencies:
    - PostgreSQL >= 12
  environment:
    - DATABASE_URL
    - OCR_API_KEY
```

**テンプレート利用コマンド**:

```bash
# 対話形式で作成
agentflow create from-template invoice-processor

# 出力:
📋 テンプレート使用: 請求書処理システム v1.0.0
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📝 以下の質問に答えてAgentをカスタマイズ:

❓ 請求書フォーマット?
   1) PDF  2) 画像  3) Excel
👉 選択 [1]: 1

❓ 抽出するフィールド? (複数選択, スペース区切り)
   1) 請求書番号  2) 金額  3) 日付  4) 供給者  5) 税額
👉 選択 [1 2 3]: 1 2 3 4

❓ 処理後のアクション?
   1) DBへ保存  2) メール送信  3) レポート生成
👉 選択 [1]: 1

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ プロジェクト生成完了!

📁 プロジェクト構成:
   invoice-processor/
   ├── agents/
   │   ├── pdf-extractor/
   │   ├── invoice-parser/
   │   └── database-writer/
   ├── prompts/
   │   └── extract.txt  ⚠️  要微調整
   └── config.yaml

🎯 次の手順:
   1. cd invoice-processor
   2. prompts/extract.txt を編集して抽出ルールを調整
   3. .env を設定 (DATABASE_URL等)
   4. agentflow run --test  # テスト実行
   5. agentflow deploy      # 本番デプロイ

💡 AIアシスタントが支援可能です。Cursorでプロジェクトを開くと
   AIはテンプレートを自動理解して迅速にカスタマイズを支援します。
```

---

## 4️⃣ 実装タスクリスト

### フェーズ1: コアフレームワーク + プロトコル統合 (Week 1-3)

#### Task 1.1: PocketFlowコアラッパー

**優先度**: P0  
**工数**: 2日

```python
# agentflow/core/flow_engine.py
from pocketflow import Node, Flow as PocketFlow

class AgentFlowEngine:
    """PocketFlowをラップし、プロトコル用フックを追加する"""

    def __init__(self, flow: PocketFlow):
        self.flow = flow
        self.hooks = {
            "on_start": [],
            "on_node_exec": [],
            "on_node_complete": [],
            "on_state_change": [],
            "on_finish": []
        }

    def register_hook(self, event: str, callback):
        """イベントフックを登録する"""
        self.hooks[event].append(callback)

    def run(self, shared: dict):
        """Flowを実行し、フックを呼び出す"""
        # 開始フックを呼び出す
        for hook in self.hooks["on_start"]:
            hook(shared)

        # PocketFlow を実行 (Node を monkey patch する必要あり)
        result = self.flow.run(shared)

        # 完了フックを呼び出す
        for hook in self.hooks["on_finish"]:
            hook(shared, result)

        return result
```

#### Task 1.2: MCPクライアント統合

**優先度**: P0  
**工数**: 3日

```python
# agentflow/protocols/mcp_client.py
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

class MCPToolManager:
    """MCPツール管理器"""

    def __init__(self, mcp_config_path: str):
        self.config = load_yaml(mcp_config_path)
        self.sessions = {}  # 各MCPサーバ毎のセッション

    async def connect_all_servers(self):
        """全てのMCPサーバへ接続する"""
        for server in self.config["servers"]:
            session = await self._connect_server(server)
            self.sessions[server["name"]] = session

    async def _connect_server(self, server_config):
        """単一のMCPサーバへ接続"""
        server_params = StdioServerParameters(
            command=server_config["command"],
            args=server_config.get("args", []),
            env=server_config.get("env", {})
        )

        async with stdio_client(server_params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()

                # 利用可能ツールを列挙
                tools = await session.list_tools()
                print(f"✅ MCPサーバに接続しました: {server_config['name']}")
                print(f"   利用可能ツール: {[t.name for t in tools]}")

                return session

    async def call_tool(self, tool_uri: str, arguments: dict):
        """MCPツールを呼び出す

        Args:
            tool_uri: 形式 mcp://server_name/tool_name
            arguments: ツール引数
        """
        server_name, tool_name = self._parse_uri(tool_uri)
        session = self.sessions[server_name]

        result = await session.call_tool(tool_name, arguments)
        return result

    def generate_tool_definitions(self):
        """LLM用にツール定義を生成する"""
        tools = []
        for server_name, session in self.sessions.items():
            for tool in session.list_tools():
                tools.append({
                    "name": f"{server_name}/{tool.name}",
                    "description": tool.description,
                    "parameters": tool.inputSchema
                })
        return tools
```

**MCP設定例** (.agentflow/protocols/mcp.yaml):

```yaml
servers:
  - name: file-tools
    command: npx
    args: ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/data"]

  - name: database-tools
    command: npx
    args: ["-y", "@modelcontextprotocol/server-postgres"]
    env:
      DATABASE_URL: "postgresql://user:pass@localhost/db"

  - name: custom-tools
    command: python
    args: ["tools/mcp_server.py"]
```

#### Task 1.3: A2A Server/Client 実装

**優先度**: P0  
**工数**: 4日

```python
# agentflow/protocols/a2a_server.py
from google_a2a import A2AServer, AgentCard, Skill

class AgentFlowA2AServer:
    """A2Aサーバ。ローカルAgentをリモート呼び出し可能にする"""

    def __init__(self, port: int = 8000):
        self.port = port
        self.agents = {}  # 登録されたAgent
        self.server = A2AServer()

    def register_agent(self, agent_block):
        """AgentをA2Aサーバに登録する"""
        # agent.yaml から AgentCard を生成
        card = self._generate_card(agent_block)

        # ハンドラ登録
        @self.server.task_handler(card.name)
        async def handle_task(task):
            # Agent の PocketFlow 実行を呼ぶ
            result = await agent_block.run(task.input)
            return result

        self.agents[card.name] = agent_block
        print(f"✅ Agent を A2A に登録しました: {card.name}")

    def _generate_card(self, agent_block) -> AgentCard:
        """agent.yaml から AgentCard を生成する"""
        meta = agent_block.metadata

        return AgentCard(
            name=meta["meta"]["name"],
            description=meta["meta"]["description"],
            version=meta["meta"]["version"],
            endpoint=f"http://localhost:{self.port}/agents/{meta['meta']['id']}",
            skills=[
                Skill(
                    name=skill,
                    description=f"{meta['meta']['name']} - {skill}",
                    input_schema=meta["interfaces"]["inputs"],
                    output_schema=meta["interfaces"]["outputs"]
                )
                for skill in meta["protocols"]["a2a"]["skills"]
            ]
        )

    def start(self):
        """A2Aサーバを起動する"""
        print(f"🚀 A2A Server 起動: http://localhost:{self.port}")
        self.server.run(port=self.port)


# agentflow/protocols/a2a_client.py
class AgentFlowA2AClient:
    """A2Aクライアント: リモートAgentを呼び出す"""

    def __init__(self):
        self.remote_agents = {}  # リモートAgent の AgentCard をキャッシュ

    async def discover_agent(self, endpoint: str):
        """リモートAgentを発見する"""
        # AgentCard を取得
        card = await fetch_agent_card(endpoint)
        self.remote_agents[card.name] = {
            "card": card,
            "endpoint": endpoint
        }
        return card

    async def call_remote_agent(self, agent_name: str, input_data: dict):
        """リモートAgentを呼び出す"""
        agent_info = self.remote_agents[agent_name]

        # A2Aタスクを送信
        result = await send_a2a_task(
            endpoint=agent_info["endpoint"],
            skill=agent_info["card"].skills[0].name,  # 単純化: 最初のskillを使う
            input=input_data
        )

        return result
```

#### Task 1.4: AG-UI イベントストリーム実装

**優先度**: P1  
**工数**: 3日

```python
# agentflow/protocols/agui_emitter.py
from ag_ui import AGUIServer, Event

class AGUIEventEmitter:
    """AG-UI イベント発行器"""

    def __init__(self):
        self.server = AGUIServer()
        self.current_run_id = None

    def attach_to_flow(self, flow_engine: AgentFlowEngine):
        """Flow実行にAG-UIイベントをバインドする"""

        # フック登録
        flow_engine.register_hook("on_start", self._on_flow_start)
        flow_engine.register_hook("on_node_exec", self._on_node_exec)
        flow_engine.register_hook("on_state_change", self._on_state_change)
        flow_engine.register_hook("on_finish", self._on_flow_finish)

    def _on_flow_start(self, shared):
        """Flow開始時処理"""
        self.current_run_id = generate_id()
        self.emit({
            "type": "RUN_STARTED",
            "run_id": self.current_run_id,
            "timestamp": now()
        })

    def _on_node_exec(self, node, input_data):
        """ノード実行時処理"""
        self.emit({
            "type": "TOOL_CALL_START",
            "run_id": self.current_run_id,
            "data": {
                "tool_name": node.__class__.__name__,
                "arguments": input_data
            }
        })

    def _on_state_change(self, old_state, new_state):
        """状態変化時処理"""
        delta = diff_dict(old_state, new_state)
        self.emit({
            "type": "STATE_DELTA",
            "run_id": self.current_run_id,
            "data": {"patch": delta}
        })

    def _on_flow_finish(self, shared, result):
        """Flow完了時処理"""
        self.emit({
            "type": "RUN_FINISHED",
            "run_id": self.current_run_id,
            "data": {"result": result}
        })

    def emit(self, event: dict):
        """AG-UIイベントを送信(SSE)する"""
        self.server.send_event(Event(**event))

    def stream_llm_tokens(self, token_generator):
        """LLMトークンをストリーミング出力"""
        for token in token_generator:
            self.emit({
                "type": "TEXT_MESSAGE_CONTENT",
                "run_id": self.current_run_id,
                "data": {"delta": token}
            })
```

### フェーズ2: CLIツール + マーケット (Week 4-5)

#### Task 2.1: CLIコマンド実装

```bash
agentflow --help

Commands:
  init              新規プロジェクト作成
  create            Agent/Tool/Workflow 作成
  install           マーケットからAgentをインストール
  search            Agentマーケットを検索
  list              ローカルリソース一覧
  run               Agent/Workflow を実行
  test              Agentをテスト
  deploy            本番環境へデプロイ
  studio            可視化オーケストレーターを起動
  protocols         プロトコル管理(test/validate)
```

#### Task 2.2: AgentマーケットAPI

```python
# 検索
agentflow search "PDF処理" --filter protocols=a2a,mcp

# インストール
agentflow install pdf-analyzer-pro
> 📦 インストール中: pdf-analyzer-pro@2.1.0
> ⬇️  AgentCard をダウンロード...
> 🔍  依存解析: PyPDF2, pdfplumber
> ✅ インストール完了!
>
> 利用方法:
>   from agentflow import get_agent
>   agent = get_agent("pdf-analyzer-pro")

# 公開
agentflow publish ./agents/my-agent --price free
```

### フェーズ3: 可視化Studio (Week 6-8)

#### Task 3.1: React Flow キャンバス

- Agentブロックのドラッグ&ドロップ
- データフロー接続
- プロパティ編集
- リアルタイムプレビュー

#### Task 3.2: AG-UI フロントエンド統合

```typescript
// studio フロントエンドは AG-UI 経由でバックエンドに接続
import { useAGUI } from '@ag-ui/react'

function AgentPreview({ agentId }) {
  const { events, sendMessage } = useAGUI(`http://localhost:8000/agents/${agentId}`)

  return (
    <div>
      {events.map(event => {
        switch(event.type) {
          case 'TEXT_MESSAGE_CONTENT':
            return <StreamingText delta={event.data.delta} />
          case 'TOOL_CALL_START':
            return <ToolIndicator tool={event.data.tool_name} />
          case 'STATE_DELTA':
            return <StateViewer patch={event.data.patch} />
        }
      })}
    </div>
  )
}
```

### フェーズ4: テンプレートとドキュメント (Week 9-10)

#### Task 4.1: 10個のシナリオテンプレート作成

1. 請求書処理システム ✓
2. カスタマーサポートチャットボット
3. データ分析パイプライン
4. メール自動化
5. ドキュメント翻訳サービス
6. コードレビューアシスタント
7. 会議要約生成
8. ソーシャルメディアコンテンツ作成
9. 履歴書スクリーニングシステム
10. スマートQAシステム

#### Task 4.2: 完全なドキュメント

- クイックスタートガイド
- プロトコル統合チュートリアル
- Agent開発ガイド
- ブロックマーケットの使い方
- 可視化オーケストレーションチュートリアル
- ベストプラクティス
- APIリファレンス

---

## 5️⃣ Cursor/Windsurf AIアシスタント用プロンプト

### プロンプトテンプレート集

#### 1. プロジェクト初期化プロンプト

```
我要使用AgentFlow框架创建一个新的AI Agent项目。

项目需求:
- 项目名称: [项目名]
- 主要功能: [描述功能]
- 协议支持: MCP, A2A, AG-UI 全部启用
- 设计模式: [RAG/MapReduce/Agent/Workflow]

请帮我:
1. 运行 agentflow init 命令创建项目结构
2. 生成 agent.yaml 配置文件
3. 创建符合三协议标准的适配器代码
4. 生成 design.md 设计文档

AgentFlow框架规范:
- 项目结构参考 AgentFlow v2.0 文档
- agent.yaml必须包含 protocols 配置
- 自动生成MCP/A2A/AG-UI适配器

请逐步执行,等待我确认每一步。
```

（注: 上記の中国語プロンプトは原文のまま示しています。必要に応じて日本語化します。）

#### 2. エージェント開発プロンプト

```
基于AgentFlow框架,开发一个[Agent名称]。

功能需求:
{详细描述功能}

技术约束:
- 必须支持MCP/A2A/AG-UI三协议
- 使用PocketFlow作为工作流引擎
- 输入:[描述输入]
- 输出:[描述输出]

开发步骤:
1. 先完成 design.md 设计文档
   - 选择合适的设计模式
   - 设计Node和Flow结构
   - 定义Shared Store Schema
   - 识别需要的MCP工具

2. 实现 agent.yaml 元数据
   - 定义inputs/outputs接口
   - 配置protocols支持
   - 声明dependencies

3. 实现PocketFlow代码
   - nodes.py: 实现所有Node
   - flow.py: 连接Flow
   - schemas.py: Pydantic数据模型

4. 协议适配
   - 使用 @auto_adapt 装饰器
   - 确保MCP工具正确注册
   - 生成A2A AgentCard
   - 配置AG-UI事件映射

5. 测试
   - 单元测试
   - 协议兼容性测试
   - 端到端测试

请从设计文档开始,每完成一步暂停等我确认。
```

#### 3. ブロック組合プロンプト

```
我要组合多个Agent积木创建一个产品级工作流。

产品名称: [产品名]
业务场景: [描述场景]

需要的Agent积木:
1. [Agent1] - 功能: [描述]
2. [Agent2] - 功能: [描述]
3. [Agent3] - 功能: [描述]

数据流:
User Input → Agent1 → Agent2 → Agent3 → Output

请帮我:
1. 检查这些Agent是否在市场存在
   agentflow search [关键词]

2. 安装需要的Agent
   agentflow install [agent-id]

3. 创建工作流编排代码 workflows/[product-name]/orchestrator.py
   - 使用A2A协议连接远程Agent
   - 处理Agent间データ传達
   - 実装エラーハンドリングとリトライ

4. 配置AG-UI界面 workflows/[product-name]/ui_config.yaml
   - 设计用户输入表单
   - 配置实时状态显示

5. 编写测试用例

AgentFlow规范:
- 工作流使用 AgentFlow.Workflow 基类
- 优先使用A2A协议调用远程Agent
- 数据伝達は標準化されたSchemaを使用

开始实现,遇到问题及时询问我。
```

#### 4. テンプレートカスタマイズプロンプト

```
使用AgentFlow模板快速创建项目。

模板名称: invoice-processor
定制需求:
- 发票格式: PDF
- 提取字段: [字段列表]
- 输出方式: 保存到PostgreSQL

步骤:
1. agentflow create from-template invoice-processor

2. 根据定制需求修改配置:
   - config.yaml: 更新数据库连接
   - prompts/extract.txt: 调整字段提取规则

3. 修改agent.yaml适应我的需求:
   - 更新inputs/outputs定义
   - 调整dependencies

4. 测试运行:
   agentflow run --test

5. 如有问题,使用AG-UI调试界面查看实时日志

请帮我执行这些步骤,并解释每个配置的作用。
```

---

## 6️⃣ まとめとロードマップ

### コアバリュー

```
┌─────────────────────────────────────────────────────────┐
│         AgentFlow = シンプル + オープン + ブロック化         │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  🎯 PocketFlowコア      → 約100行, AIフレンドリー        │
│  🌐 3プロトコル対応     → MCP/A2A/AG-UI を網羅            │
│  🧩 ブロック化設計      → ブロックを組み合わせて開発     │
│  📦 テンプレートシステム → プロンプト変更だけで利用可能  │
│  🎨 可視化オーケストレーション → ノーコード構築         │
│  🤖 AIアシスタント親和性  → Cursor/Windsurf でネイティブ対応│
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### 他フレームワークとの比較

| 特性 | AgentFlow | LangChain | CrewAI | AutoGen |
| ---- | --------- | --------- | ------ | ------ |
| **コア行数** | ~500行 | ~50K行 | ~10K行 | ~20K行 |
| **MCP対応** | ✅ ネイティブ | ⚠️ プラグイン必要 | ❌ | ❌ |
| **A2A対応** | ✅ ネイティブ | ❌ | ⚠️ 実験的 | ❌ |
| **AG-UI対応** | ✅ ネイティブ | ❌ | ✅ | ❌ |
| **ブロック化設計** | ✅ | ⚠️ 複雑 | ⚠️ 部分的 | ❌ |
| **可視化オーケストレーション** | ✅ | ❌ | ❌ | ❌ |
| **テンプレートシステム** | ✅ | ⚠️ 簡素 | ⚠️ 簡素 | ❌ |
| **AIアシスタント親和性** | ✅ 非常に良い | ⚠️ 普通 | ✅ | ⚠️ |

### 開発ロードマップ

**Q1 2026: MVP**

- ✅ PocketFlow統合
- ✅ MCP/A2A/AG-UI 基本サポート
- ✅ CLIツール
- ✅ 3つのシナリオテンプレート
- ✅ 基本ドキュメント

**Q2 2026: マーケット版**

- 🔄 エージェントブロックマーケット
- 🔄 可視化Studio (Beta)
- 🔄 10のシナリオテンプレート
- 🔄 コミュニティ貢献仕組み

**Q3 2026: エンタープライズ版**

- 📅 企業向けセキュリティ認証
- 📅 マルチテナント対応
- 📅 パフォーマンス監視
- 📅 SaaS展開

**Q4 2026: エコシステム版**

- 📅 サードパーティ統合マーケット
- 📅 AIアシスタント深い統合
- 📅 モバイル対応
- 📅 国際化

---

## 付録: クイックリファレンス

### よく使うコマンド

```bash
# プロジェクト管理
agentflow init <name>                 # プロジェクト作成
agentflow create agent <name>         # Agent作成
agentflow create from-template <tpl>  # テンプレートから作成

# マーケット操作
agentflow search <query>              # Agent検索
agentflow install <agent-id>          # Agentインストール
agentflow publish <path>              # Agent公開

# 実行とテスト
agentflow run <agent-name>            # Agent実行
agentflow test                        # テスト
agentflow studio                      # 可視化起動

# プロトコル管理
agentflow protocols test              # プロトコル互換性テスト
agentflow protocols validate          # 設定検証
```

### agent.yaml 簡易テンプレ

```yaml
meta: { id, name, version, author, icon, category, description }
interfaces:
  inputs: [{ name, type, required, description }]
  outputs: [{ name, type, schema }]
protocols:
  mcp: { tools, resources }
  a2a: { enabled, skills }
  agui: { enabled, events }
dependencies: { agents, tools, packages }
pocketflow: { entry, shared_schema }
visual: { color, size, ports }
```

### プロトコルドキュメントリンク

- **MCP**: <https://modelcontextprotocol.io>
- **A2A**: <https://github.com/google-a2a/A2A>
- **AG-UI**: <https://docs.ag-ui.com>

---

**AgentFlow: AIエージェント開発をレゴのように簡単にする!** 🚀

````
