# AgentFlow プロトコルガイド

> バージョン: 2.1.0
> 更新日: 2026-01-13

AgentFlow は **5つのプロトコル** をサポートしています：

| プロトコル | 役割 | 通信方式 |
|-----------|------|----------|
| **MCP** | ツール接続 | stdio |
| **A2A** | Agent間通信 | HTTP/REST |
| **AG-UI** | UIイベント配信 | SSE |
| **A2UI** | 生成式UI規範 | AG-UI経由 |
| **WebSocket** | 双方向通信 | WS（NEW） |

## プロトコルスタック概要

```
┌─────────────────────────────────────────────────────────────────┐
│                        プロトコルスタック                       │
├─────────────────────────────────────────────────────────────────┤
│  👤 ユーザー層                                                  │
│     └── フロントエンドアプリ (Web/Mobile/Desktop)              │
│                                                                 │
│  🎨 UI 交互層                                                   │
│     ├── WebSocket: 双方向リアルタイム（HITL対応）              │
│     ├── AG-UI: 通信プロトコル (イベントストリーム/SSE)         │
│     └── A2UI: UI 規範 (宣言式コンポーネント)                   │
│                                                                 │
│  🤖 Agent 層                                                    │
│     └── Agent コア + Skills                                    │
│                                                                 │
│  🔗 Agent 協調層                                                │
│     └── A2A: Agent 間通信 (発見・協調・委譲)                   │
│                                                                 │
│  🔧 ツール層                                                    │
│     └── MCP: ツール接続 (データソース・API・外部サービス)      │
└─────────────────────────────────────────────────────────────────┘
```

**通信プロトコル比較：**

| 比較 | AG-UI (SSE) | WebSocket |
|------|-------------|-----------|
| **方向** | 単方向（Server→Client） | 双方向 |
| **用途** | イベント配信、ログ | HITL、リアルタイム操作 |
| **類比** | Server Push | Chat |
| **推奨シーン** | 長時間タスク進捗 | 承認待ち、対話 |

**AG-UI と A2UI の関係：**
- AG-UI = 通信パイプ（HTTP のような役割）
- A2UI = コンテンツフォーマット（HTML のような役割）

## MCP (Model Context Protocol)

### 概要

MCP は AI モデルとツールを接続するためのプロトコルです。

- **Python バージョン**: 3.10+
- **通信方式**: stdio ベース
- **用途**: 外部ツール（ファイルシステム、データベース、API など）への接続

### 設定

`agent.yaml` で MCP を有効化：

```yaml
protocols:
  mcp: true
```

### MCP Client の使用

#### 基本的な使い方

```python
from agentflow.protocols.mcp_client import MCPClient

# クライアントを作成
client = MCPClient()

# サーバーに接続
await client.connect("filesystem")

# ツール定義を取得
tools = await client.get_tool_definitions()
print(tools)

# ツールを呼び出し
result = await client.call_tool(
    "mcp://filesystem/read_file",
    {"path": "/path/to/file.txt"}
)
print(result)

# 切断
await client.disconnect("filesystem")
```

#### 設定ファイル

`~/.agentflow/mcp_config.yaml` で MCP サーバーを設定：

```yaml
servers:
  filesystem:
    command: npx
    args:
      - -y
      - "@modelcontextprotocol/server-filesystem"
      - /path/to/allowed/directory
    enabled: true

  github:
    command: npx
    args:
      - -y
      - "@modelcontextprotocol/server-github"
    env:
      GITHUB_TOKEN: ${GITHUB_TOKEN}
    enabled: true
```

#### エージェントから使用

```python
from agentflow.core.agent_block import AgentBlock

class FileProcessorAgent(AgentBlock):
    async def initialize(self) -> None:
        await super().initialize()
        # MCP クライアントを初期化
        self.mcp = MCPClient()
        await self.mcp.connect("filesystem")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # ファイルを読み込む
        content = await self.mcp.call_tool(
            "mcp://filesystem/read_file",
            {"path": input_data["file_path"]}
        )

        # 処理
        result = content.upper()

        return {"result": result}

    async def cleanup(self) -> None:
        await self.mcp.disconnect("filesystem")
        await super().cleanup()
```

#### MCP ツール定義の自動生成

`@auto_adapt` デコレーターにより、エージェントのスキルが自動的に MCP ツールとして公開されます：

```python
# agent.yaml
skills:
  - name: process_text
    description: テキストを処理する
    inputs:
      - text
    outputs:
      - result

# Python コード
tools = agent.get_mcp_tools()
# [
#   {
#     "name": "my-agent.process_text",
#     "description": "テキストを処理する",
#     "inputSchema": {
#       "type": "object",
#       "properties": {
#         "text": {"type": "string", "description": "..."}
#       },
#       "required": ["text"]
#     }
#   }
# ]
```

---

## A2A (Agent-to-Agent)

### 概要

A2A はエージェント間の協調を実現するプロトコルです。

- **Python バージョン**: 3.9+
- **通信方式**: HTTP/REST
- **用途**: エージェント間のタスク委譲、マルチエージェントシステム

### 設定

`agent.yaml` で A2A を有効化：

```yaml
protocols:
  a2a:
    enabled: true
    endpoint: http://localhost:8000
```

### A2A Server の使用

#### サーバーの起動

```python
from agentflow.protocols.a2a_server import A2AServer

# サーバーを作成
server = A2AServer()

# エージェントを登録
card = agent.get_a2a_card()
handlers = {
    "process_text": lambda inputs: agent.run(inputs),
    "analyze": lambda inputs: agent.analyze(inputs),
}
server.register_agent(card, handlers)

# タスクを処理
result = await server.handle_task(
    "My Agent",  # エージェント名
    "process_text",  # スキル名
    {"text": "hello"}  # 入力
)
print(result)
# {
#   "status": "success",
#   "result": {"result": "HELLO"},
#   "agent": "My Agent",
#   "skill": "process_text"
# }
```

#### エージェントカードの取得

```python
# エージェントカードを取得
card = agent.get_a2a_card()
print(card.model_dump())
# {
#   "name": "My Agent",
#   "description": "...",
#   "version": "1.0.0",
#   "skills": [
#     {
#       "name": "process_text",
#       "description": "テキストを処理する",
#       "inputs": [...],
#       "outputs": [...]
#     }
#   ],
#   "metadata": {...}
# }
```

### A2A Client の使用

#### リモートエージェントの呼び出し

```python
from agentflow.protocols.a2a_client import A2AClient

# クライアントを作成
client = A2AClient()

# エージェントを発見
card = await client.discover_agent("http://localhost:8000")
print(f"発見: {card.name}")

# リモートエージェントを呼び出し
result = await client.call_remote_agent(
    "http://localhost:8000",
    "process_text",
    {"text": "hello"}
)
print(result)
```

#### キャッシュ管理

```python
# キャッシュをクリア
client.clear_cache("http://localhost:8000")

# 全キャッシュをクリア
client.clear_all_cache()

# キャッシュされたエンドポイントを取得
endpoints = client.get_cached_endpoints()
```

#### マルチエージェントワークフロー

```python
class OrchestratorAgent(AgentBlock):
    async def initialize(self) -> None:
        await super().initialize()
        self.a2a_client = A2AClient()

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # エージェント 1: テキスト処理
        result1 = await self.a2a_client.call_remote_agent(
            "http://localhost:8001",
            "process_text",
            {"text": input_data["text"]}
        )

        # エージェント 2: 分析
        result2 = await self.a2a_client.call_remote_agent(
            "http://localhost:8002",
            "analyze",
            {"text": result1["result"]}
        )

        return {"final_result": result2}
```

---

## AG-UI (Agent-UI)

### 概要

AG-UI はエージェントとフロントエンドをリアルタイムで接続するプロトコルです。

- **Python バージョン**: 3.13+ **必須**
- **通信方式**: SSE (Server-Sent Events)
- **用途**: リアルタイムログ、進捗表示、ストリーミング

### 設定

`agent.yaml` で AG-UI を有効化：

```yaml
protocols:
  agui: true
```

### AG-UI Emitter の使用

#### 基本的な使い方

```python
from agentflow.protocols.agui_emitter import AGUIEventEmitter

# エミッターを作成
emitter = agent.create_agui_emitter(agent.engine)

# フローにアタッチ
await emitter.attach_to_flow("my-flow")

# ログを送信
await emitter.emit_log("info", "処理を開始します", "agent")
await emitter.emit_log("debug", "データを読み込み中...", "agent")
await emitter.emit_log("success", "処理完了！", "agent")

# デタッチ
await emitter.detach_from_flow("my-flow")
```

#### イベントストリーミング

```python
# イベントをストリーミング
async for event in emitter.stream_events():
    print(f"[{event.event_type.value}] {event.data}")

    if event.event_type == AGUIEventType.FLOW_COMPLETE:
        break
```

#### エージェントから使用

```python
class StreamingAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # エミッターを作成
        emitter = self.create_agui_emitter(self.engine)
        await emitter.attach_to_flow("processing")

        # 処理開始
        await emitter.emit_log("info", "処理を開始します", "agent")

        # ステップ 1
        await emitter.emit_log("info", "ステップ 1: データ読み込み", "agent")
        data = await self.load_data(input_data["source"])

        # ステップ 2
        await emitter.emit_log("info", "ステップ 2: データ処理", "agent")
        result = await self.process_data(data)

        # 完了
        await emitter.emit_log("success", "処理完了！", "agent")
        await emitter.detach_from_flow("processing")

        return {"result": result}
```

#### イベントタイプ

```python
from agentflow.protocols.agui_events import AGUIEventType

# 利用可能なイベントタイプ
AGUIEventType.FLOW_START      # フロー開始
AGUIEventType.FLOW_COMPLETE   # フロー完了
AGUIEventType.FLOW_ERROR      # フローエラー
AGUIEventType.FLOW_CANCEL     # フローキャンセル
AGUIEventType.NODE_START      # ノード開始
AGUIEventType.NODE_COMPLETE   # ノード完了
AGUIEventType.LOG             # ログメッセージ
```

---

## A2UI (Agent-to-UI)

### 概要

A2UI は Google が提唱する生成式 UI プロトコルです。Agent が宣言式コンポーネントを生成し、クライアントがネイティブ UI として描画します。

- **バージョン**: v0.8 (Public Preview)
- **通信方式**: AG-UI を通じて配信
- **用途**: Agent による動的 UI 生成

### AG-UI との関係

| 比較 | AG-UI | A2UI |
|------|-------|------|
| **種別** | 通信プロトコル | UI 規範 |
| **役割** | メッセージ配信 | UI 記述 |
| **類比** | HTTP | HTML |

### 基本的な使い方

```python
from agentflow.protocols.a2ui import (
    TextComponent, ButtonComponent, CardComponent,
    A2UIEmitter
)

# コンポーネントを作成
card = CardComponent(
    title="検索結果",
    children=[
        TextComponent("5件の結果が見つかりました"),
        ButtonComponent("詳細を見る", action="view_details"),
    ]
)

# A2UI エミッターで配信
emitter = A2UIEmitter(agui_emitter)
await emitter.emit_component(card, surface_id="main")
```

### 利用可能なコンポーネント

| コンポーネント | 説明 | 主なプロパティ |
|--------------|------|---------------|
| `TextComponent` | テキスト | `content` |
| `ButtonComponent` | ボタン | `label`, `action` |
| `InputComponent` | 入力フィールド | `name`, `type`, `placeholder` |
| `CardComponent` | カード | `title`, `children` |
| `ListComponent` | リスト | `children` |
| `ImageComponent` | 画像 | `src`, `alt` |
| `FormComponent` | フォーム | `action`, `children` |

### カスタムコンポーネント

```python
from agentflow.protocols.a2ui import A2UIComponent, ComponentType, ComponentRegistry

# カスタムコンポーネントを定義
class ChartComponent(A2UIComponent):
    def __init__(self, data: list, chart_type: str = "bar"):
        super().__init__(
            component_type=ComponentType.CUSTOM,
            props={"data": data, "chart_type": chart_type, "custom_type": "chart"}
        )

# レジストリに登録
registry = ComponentRegistry()
registry.register("chart", ChartComponent)
```

### シリアライズ

```python
# コンポーネントを JSON 形式に変換
card_dict = card.to_dict()
print(card_dict)
# {
#   "type": "card",
#   "props": {"title": "検索結果"},
#   "children": [...]
# }

# JSON からコンポーネントを復元
restored = A2UIComponent.from_dict(card_dict)
```

---

## プロトコルの組み合わせ

### 全プロトコルを有効化

```yaml
protocols:
  mcp: true
  a2a:
    enabled: true
    endpoint: http://localhost:8000
  agui: true
```

### 統合例

```python
class AdvancedAgent(AgentBlock):
    async def initialize(self) -> None:
        await super().initialize()
        # MCP クライアント
        self.mcp = MCPClient()
        await self.mcp.connect("filesystem")

        # A2A クライアント
        self.a2a = A2AClient()

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # AG-UI エミッター
        emitter = self.create_agui_emitter(self.engine)
        await emitter.attach_to_flow("advanced-flow")

        # ステップ 1: MCP でファイル読み込み
        await emitter.emit_log("info", "ファイルを読み込み中...", "agent")
        content = await self.mcp.call_tool(
            "mcp://filesystem/read_file",
            {"path": input_data["file"]}
        )

        # ステップ 2: A2A でリモート処理
        await emitter.emit_log("info", "リモート処理中...", "agent")
        result = await self.a2a.call_remote_agent(
            "http://localhost:8001",
            "process",
            {"content": content}
        )

        # 完了
        await emitter.emit_log("success", "処理完了！", "agent")
        await emitter.detach_from_flow("advanced-flow")

        return result
```

---

## WebSocket（双方向通信）

### 概要

WebSocket は Agent とフロントエンドの双方向リアルタイム通信を実現します。

- **通信方式**: WebSocket (ws://, wss://)
- **用途**: HITL（Human-in-the-Loop）、リアルタイム対話、承認待ち
- **互換性**: FastAPI WebSocket、Socket.IO風イベント駆動

### AG-UI との違い

| 項目 | AG-UI (SSE) | WebSocket |
|------|-------------|-----------|
| 通信方向 | Server → Client | 双方向 |
| クライアント操作 | 不可 | 可能 |
| 接続維持 | 自動再接続 | 手動管理 |
| 推奨用途 | ログ配信、進捗 | HITL、承認、対話 |

### 基本使用（FastAPI統合）

```python
from fastapi import FastAPI, WebSocket
from agentflow.integrations import WebSocketManager, WSEvent, WSEventType

app = FastAPI()
ws_manager = WebSocketManager()

# WebSocket エンドポイント
@app.websocket("/ws/{session_id}")
async def websocket_endpoint(websocket: WebSocket, session_id: str):
    await ws_manager.handle_connection(websocket, session_id)

# Agent からイベント送信
async def notify_progress(session_id: str, progress: float):
    await ws_manager.send(session_id, WSEvent(
        type=WSEventType.PROGRESS,
        data={"progress": progress, "message": "処理中..."},
    ))

# 全員にブロードキャスト
async def broadcast_update():
    await ws_manager.broadcast(WSEvent(
        type=WSEventType.STATE_UPDATE,
        data={"status": "updated"},
    ))
```

### HITL（承認待ち）

```python
from agentflow.integrations import WebSocketManager, WSEvent, WSEventType, WSCommand

ws_manager = WebSocketManager()

# コマンドハンドラー登録
def handle_approval(command: WSCommand):
    if command.payload.get("approved"):
        print(f"承認されました: {command.flow_id}")
    else:
        print(f"拒否されました: {command.flow_id}")

ws_manager.register_command_handler("approval.response", handle_approval)

# 承認リクエスト送信
async def request_approval(session_id: str, flow_id: str, action: str):
    await ws_manager.send(session_id, WSEvent(
        type=WSEventType.APPROVAL_REQUIRED,
        flow_id=flow_id,
        data={
            "action": action,
            "description": "この操作を承認しますか？",
            "options": ["approve", "reject"],
        },
    ))

    # 応答待ち
    command = await ws_manager.get_next_command(timeout=300)
    return command.payload.get("approved", False) if command else False
```

### イベントタイプ

```python
from agentflow.integrations import WSEventType

# 接続管理
WSEventType.CONNECT        # 接続成功
WSEventType.DISCONNECT     # 切断
WSEventType.PING           # ハートビート
WSEventType.PONG           # ハートビート応答

# Agent イベント（AG-UI互換）
WSEventType.FLOW_START     # フロー開始
WSEventType.FLOW_COMPLETE  # フロー完了
WSEventType.FLOW_ERROR     # エラー発生
WSEventType.NODE_START     # ノード開始
WSEventType.NODE_COMPLETE  # ノード完了
WSEventType.PROGRESS       # 進捗更新

# HITL イベント
WSEventType.APPROVAL_REQUIRED   # 承認待ち
WSEventType.APPROVAL_RESPONSE   # 承認応答

# カスタム
WSEventType.MESSAGE        # メッセージ
WSEventType.COMMAND        # コマンド
WSEventType.STATE_UPDATE   # 状態更新
```

### ルーター作成（簡易方法）

```python
from agentflow.integrations import create_websocket_router, WebSocketManager

# WebSocketManager 共有
manager = WebSocketManager()

# ルーター作成
ws_router = create_websocket_router(manager=manager, path="/ws/{session_id}")

# FastAPI に登録
app.include_router(ws_router)
```

### クライアント側（JavaScript）

```javascript
// 接続
const ws = new WebSocket(`ws://localhost:8000/ws/${sessionId}`);

// イベント受信
ws.onmessage = (event) => {
    const data = JSON.parse(event.data);
    
    switch (data.type) {
        case 'progress':
            updateProgressBar(data.data.progress);
            break;
        case 'approval.required':
            showApprovalDialog(data);
            break;
        case 'flow.complete':
            handleCompletion(data);
            break;
    }
};

// 承認応答送信
function approve(flowId) {
    ws.send(JSON.stringify({
        type: 'approval.response',
        flow_id: flowId,
        payload: { approved: true }
    }));
}

// 切断
ws.onclose = () => {
    console.log('Disconnected');
    // 再接続ロジック
};
```

### 統計・監視

```python
# 接続統計
stats = ws_manager.get_stats()
print(f"接続数: {stats['total_connections']}")
print(f"セッション: {stats['active_sessions']}")

# ハートビート開始（自動切断検知）
await ws_manager.start_heartbeat()

# 停止
await ws_manager.stop_heartbeat()
```

---

## プロトコル選択ガイド

| シナリオ | 推奨プロトコル |
|---------|---------------|
| 外部ツール呼び出し | MCP |
| 別 Agent への委譲 | A2A |
| 進捗・ログ配信 | AG-UI (SSE) |
| 動的 UI 生成 | A2UI (AG-UI経由) |
| 承認待ち・HITL | WebSocket |
| リアルタイム対話 | WebSocket |

---

## 次のステップ

- [パターンガイド](PATTERNS_GUIDE.md) - DeepAgent/Reflection/Pipeline の詳細
- [API リファレンス](api.md) - 詳細な API ドキュメント
- [CLI リファレンス](cli.md) - CLI コマンドの詳細
- [サンプル集](../examples/) - 実装例
