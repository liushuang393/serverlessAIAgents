# AgentFlow フレームワーク改善 v0.2.0

> **更新日**: 2024-12-30  
> **バージョン**: 0.2.0

---

## 📋 改善サマリー

フレームワークの3つの開発方式（@agent / create_flow / AgentCoordinator）に対応し、**CLI** と **Studio UI** も更新しました。

---

## 🎯 フレームワーク層の改善

### 1. Decorator API（新規）

| コンポーネント | ファイル | 説明 |
|---------------|---------|------|
| `@agent` | `agentflow/agent_decorator.py` | ゼロ設定Agent定義 |
| `@tool` | `agentflow/providers/tool_provider.py` | ツール自動登録 |
| `AgentClient` | `agentflow/agent_decorator.py` | 統一呼び出しインターフェース |

### 2. Provider Layer（新規）

| Provider | ファイル | 説明 |
|----------|---------|------|
| `LLMProvider` | `agentflow/providers/llm_provider.py` | LLM統一アクセス（デフォルト: OpenAI） |
| `ToolProvider` | `agentflow/providers/tool_provider.py` | ツール統一アクセス（自動発見） |
| `DataProvider` | `agentflow/providers/data_provider.py` | データ統一アクセス（SQL/Vector/Cache） |
| `EventProvider` | `agentflow/providers/event_provider.py` | イベント統一アクセス（SSE/WS） |

### 3. FastAPI統合（新規）

| コンポーネント | ファイル | 説明 |
|---------------|---------|------|
| `AgentRouter` | `agentflow/integrations/fastapi_integration.py` | 自動API生成 |
| `create_app` | `agentflow/integrations/fastapi_integration.py` | アプリケーション生成 |

---

## 🔧 CLI 改善

### 更新ファイル

- `agentflow/cli/main.py`

### 新機能

#### 1. `run` コマンド拡張

**従来（agent.yaml ベース）:**
```bash
agentflow run my-agent --input '{"text": "Hello"}'
```

**新規（@agent デコレータ）:**
```bash
agentflow run . --agent-name QAAgent --input '{"question": "Hello"}'
```

**ストリームモード:**
```bash
agentflow run my-agent --input data.json --stream
```

#### 2. `list` コマンド改善

**@agent デコレータで定義されたAgentも表示:**
```bash
agentflow list
```

出力例:
```
┌─────────────────────────────────────────┐
│         Installed Agents                 │
├──────────────┬──────────┬───────────────┤
│ ID           │ Type     │ Description   │
├──────────────┼──────────┼───────────────┤
│ QAAgent      │ @agent   │ Decorator...  │
│ MyAgent      │ @agent   │ Decorator...  │
└──────────────┴──────────┴───────────────┘
```

#### 3. バージョン更新

- CLI バージョン: `0.1.0` → `0.2.0`

---

## 🎨 Studio UI 改善

### 更新ファイル

- `agentflow/studio/api.py`

### 新機能

#### 1. Agent一覧API拡張

**エンドポイント**: `GET /api/agents`

**変更点**:
- `@agent` デコレータで定義されたAgentも含む
- `type` フィールドで区別（`"yaml"` / `"decorator"`）

**レスポンス例**:
```json
[
  {
    "id": "my-agent",
    "name": "My Agent",
    "type": "yaml",
    ...
  },
  {
    "id": "QAAgent",
    "name": "QAAgent",
    "type": "decorator",
    ...
  }
]
```

#### 2. Agent実行API拡張

**エンドポイント**: `POST /api/agents/{agent_id}/run`

**変更点**:
- `@agent` デコレータで定義されたAgentも実行可能
- `AgentClient.invoke()` を使用

**実行フロー**:
```
1. @agent デコレータAgentかチェック
   → 見つかった場合: AgentClient.invoke() で実行
2. agent.yaml ベースAgentかチェック
   → 見つかった場合: 従来通り実行
3. エラー: Agent not found
```

#### 3. ストリームAPI拡張

**エンドポイント**: `GET /api/agents/{agent_id}/events`

**変更点**:
- `AgentClient.stream()` をサポート
- `create_flow().run_stream()` をサポート

**ストリームフロー**:
```
1. @agent デコレータAgentかチェック
   → AgentClient.stream() でストリーム
2. create_flow の run_stream かチェック
   → flow.run_stream() でストリーム
3. フォールバック: ダミーイベント
```

---

## 📊 対応状況

| 機能 | フレームワーク | CLI | Studio UI |
|------|--------------|-----|-----------|
| `@agent` デコレータ | ✅ | ✅ | ✅ |
| `AgentClient` | ✅ | ✅ | ✅ |
| `create_flow` | ✅ | ✅ | ✅ |
| `AgentCoordinator` | ✅ | ✅ | ✅ |
| `LLMProvider` | ✅ | - | - |
| `ToolProvider` | ✅ | - | - |
| `DataProvider` | ✅ | - | - |
| `EventProvider` | ✅ | - | - |
| `AgentRouter` | ✅ | - | - |

---

## 🚀 使用例

### CLI使用例

```bash
# 1. @agent デコレータAgentを実行
agentflow run . --agent-name QAAgent --input '{"question": "Hello"}'

# 2. ストリームモード
agentflow run my-agent --input data.json --stream

# 3. Agent一覧
agentflow list
```

### Studio UI使用例

```javascript
// 1. Agent一覧取得（@agent含む）
const agents = await fetch('/api/agents').then(r => r.json());

// 2. @agent デコレータAgentを実行
const result = await fetch('/api/agents/QAAgent/run', {
  method: 'POST',
  body: JSON.stringify({ input_data: { question: "Hello" } })
}).then(r => r.json());

// 3. ストリームイベント受信
const es = new EventSource('/api/agents/QAAgent/events');
es.onmessage = (e) => {
  const event = JSON.parse(e.data);
  console.log(event.type, event.data);
};
```

---

## 📝 移行ガイド

### 既存コードからの移行

**変更不要**:
- `agent.yaml` ベースのAgentは従来通り動作
- `create_flow` は従来通り動作
- `AgentCoordinator` は従来通り動作

**新機能利用**:
- `@agent` デコレータで新しいAgentを定義
- `AgentClient` で統一呼び出し
- Provider層で統一アクセス

---

## 🔗 関連ドキュメント

- [ARCHITECTURE_REDESIGN.md](ARCHITECTURE_REDESIGN.md) - アーキテクチャ再設計
- [README.md](README.md) - フレームワーク概要
- [AI_PROMPT_TEMPLATE.md](AI_PROMPT_TEMPLATE.md) - AI開発ガイド

