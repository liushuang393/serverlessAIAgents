# AgentFlow アーキテクチャ

AgentFlow の設計思想とアーキテクチャの詳細を説明します。

## 設計思想

AgentFlow は以下の原則に基づいて設計されています：

1. **軽量性**: コアコードは約500行、PocketFlow エンジンは100行
2. **モジュラー性**: プロトコル、ツール、エージェントを独立して開発可能
3. **プロトコル中立**: MCP、A2A、AG-UI の3つのプロトコルを統一的にサポート
4. **開発者体験**: 10分で最初のエージェントを作成可能
5. **型安全性**: Python 3.13+ の型ヒントを完全活用

## システムアーキテクチャ

AgentFlow は4層のアーキテクチャで構成されています：

```
┌─────────────────────────────────────────────────────────┐
│                    UI Layer (Optional)                   │
│                  Visual Studio (React)                   │
└─────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────┐
│                    Protocol Layer                        │
│  ┌──────────┐  ┌──────────┐  ┌──────────────────────┐  │
│  │   MCP    │  │   A2A    │  │       AG-UI          │  │
│  │  Client  │  │  Server  │  │  Event Emitter       │  │
│  └──────────┘  └──────────┘  └──────────────────────┘  │
└─────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────┐
│                    Engine Layer                          │
│              AgentFlowEngine (PocketFlow)                │
│  ┌──────────────────────────────────────────────────┐   │
│  │  AsyncFlow / Flow / AsyncNode / Node             │   │
│  └──────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────┐
│                    Tool Layer                            │
│  ┌──────────┐  ┌──────────┐  ┌──────────────────────┐  │
│  │   LLM    │  │ Database │  │    External API      │  │
│  │  Tools   │  │  Tools   │  │       Tools          │  │
│  └──────────┘  └──────────┘  └──────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### 1. UI Layer (オプション)

Visual Studio は React + TypeScript で実装されたビジュアルワークフローエディタです。

**主要コンポーネント:**
- **Canvas**: React Flow ベースのビジュアルエディタ
- **AgentNode**: ドラッグ&ドロップ可能なエージェントノード
- **PropertiesPanel**: ノードプロパティエディタ
- **Sidebar**: エージェント一覧とマーケットプレイス

**技術スタック:**
- React 18.3.1
- TypeScript 5.7.2
- React Flow 11.11.4
- Zustand 4.5.5 (状態管理)
- Tailwind CSS 3.4.15

### 2. Protocol Layer

3つのオープンプロトコルを統一的にサポートします。

#### MCP (Model Context Protocol)

LLM ツールとの接続を提供します。

**主要クラス:**
- `MCPClient`: MCP サーバーへの接続
- `MCPConfig`: MCP 設定管理

**使用例:**
```python
from agentflow.protocols.mcp_client import MCPClient

async with MCPClient("mcp://localhost:3000") as client:
    tools = await client.list_tools()
    result = await client.call_tool("search", {"query": "AgentFlow"})
```

#### A2A (Agent-to-Agent)

エージェント間の協調を提供します。

**主要クラス:**
- `A2AServer`: A2A サーバー実装
- `A2AClient`: A2A クライアント実装
- `A2ACard`: エージェント情報カード

**使用例:**
```python
from agentflow.protocols.a2a_server import A2AServer

server = A2AServer(agent, host="0.0.0.0", port=8000)
await server.start()
```

#### AG-UI (Agent-UI)

フロントエンドへのイベントストリーミングを提供します。

**主要クラス:**
- `AGUIEmitter`: イベント送信
- `AGUIEvent`: イベントデータ

**使用例:**
```python
from agentflow.protocols.agui_emitter import AGUIEmitter

emitter = AGUIEmitter()
await emitter.emit_progress(50, "処理中...")
await emitter.emit_result({"status": "success"})
```

### 3. Engine Layer

PocketFlow ベースの軽量ワークフローエンジンです。

**主要クラス:**
- `AgentFlowEngine`: エンジン本体
- `AsyncFlow` / `Flow`: ワークフロー定義
- `AsyncNode` / `Node`: ノード定義

**特徴:**
- 非同期実行サポート
- ノード間のデータフロー管理
- エラーハンドリング
- ライフサイクル管理

**使用例:**
```python
from agentflow.core.engine import AgentFlowEngine, AsyncFlow, AsyncNode

async def process_node(input_data):
    return {"result": input_data["value"] * 2}

flow = AsyncFlow([
    AsyncNode("process", process_node),
])

engine = AgentFlowEngine()
result = await engine.run_async(flow, {"value": 10})
```

### 4. Tool Layer

外部ツールとの統合を提供します。

**ツールカテゴリ:**
- **LLM Tools**: OpenAI、Anthropic、Google などの LLM API
- **Database Tools**: PostgreSQL、MySQL、MongoDB などのデータベース
- **External API Tools**: REST API、GraphQL などの外部 API

## コアコンポーネント

### AgentBlock

すべてのエージェントの基底クラスです。

**主要機能:**
- メタデータ自動読み込み
- プロトコルアダプター自動適用
- ライフサイクル管理 (initialize / run / cleanup)
- コンテキストマネージャーサポート

**実装例:**
```python
from agentflow.core.agent_block import AgentBlock

class MyAgent(AgentBlock):
    async def initialize(self) -> None:
        """初期化処理."""
        await super().initialize()
        # カスタム初期化

    async def run(self, input_data: dict) -> dict:
        """エージェント実行."""
        # ビジネスロジック
        return {"result": "success"}

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        # カスタムクリーンアップ
        await super().cleanup()
```

### Protocol Adapter

プロトコルメソッドを自動生成します。

**主要クラス:**
- `ProtocolAdapter`: アダプター生成
- `@auto_adapt`: デコレーター

**使用例:**
```python
from agentflow.decorators import auto_adapt

@auto_adapt()
class MyAgent:
    def process(self, data: str) -> str:
        return data.upper()

# プロトコルメソッドが自動注入される
agent = MyAgent()
mcp_tools = agent.get_mcp_tools()
a2a_card = agent.get_a2a_card()
emitter = agent.create_agui_emitter()
```

### Marketplace

エージェントの検索とインストールを提供します。

**主要クラス:**
- `MarketplaceClient`: マーケットプレイス API クライアント
- `LocalRegistry`: ローカルエージェント管理

**使用例:**
```bash
# エージェント検索
agentflow marketplace search chatbot

# エージェントインストール
agentflow marketplace install chatbot-agent

# インストール済みエージェント一覧
agentflow marketplace list
```

### Template System

プロジェクトテンプレートを提供します。

**主要クラス:**
- `TemplateManager`: テンプレート管理
- `TemplateMetadata`: テンプレートメタデータ

**使用例:**
```bash
# テンプレート一覧
agentflow template list

# テンプレートから生成
agentflow template generate chatbot my-chatbot -i
```

## データフロー

エージェント実行時のデータフローを示します：

```
1. ユーザー入力
   ↓
2. AgentBlock.run() 呼び出し
   ↓
3. Engine でワークフロー実行
   ↓
4. ノードごとに処理
   ├─ Tool 呼び出し (MCP)
   ├─ 他エージェント呼び出し (A2A)
   └─ イベント送信 (AG-UI)
   ↓
5. 結果を返却
   ↓
6. ユーザーに出力
```

## セキュリティ

AgentFlow は以下のセキュリティ対策を実装しています：

1. **入力検証**: Pydantic による型検証
2. **認証**: A2A プロトコルでの認証サポート
3. **サンドボックス**: ツール実行の分離
4. **ログ**: 監査ログの記録

## パフォーマンス

AgentFlow は以下のパフォーマンス最適化を実装しています：

1. **非同期実行**: asyncio による並行処理
2. **キャッシング**: メタデータとツール情報のキャッシュ
3. **遅延読み込み**: 必要なモジュールのみ読み込み
4. **軽量設計**: コアコードは約500行

## 拡張性

AgentFlow は以下の拡張ポイントを提供しています：

1. **カスタムプロトコル**: 新しいプロトコルの追加
2. **カスタムツール**: 新しいツールの追加
3. **カスタムエンジン**: 独自のワークフローエンジン
4. **カスタムテンプレート**: 独自のプロジェクトテンプレート

## 次のステップ

- [API リファレンス](api.md) - 詳細な API ドキュメント
- [プロトコルガイド](protocols.md) - プロトコルの詳細
- [CLI リファレンス](cli.md) - CLI コマンドの詳細

