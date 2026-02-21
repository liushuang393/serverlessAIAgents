# @agentflow/ui

> AgentFlow Frontend SDK: AI Agent アプリ向けの React Hooks / UI コンポーネント集

## 🎯 目的

AI アプリ開発で繰り返し実装しがちな共通部品を SDK として提供し、開発者が業務ロジックに集中できるようにします。

- SSE ストリーム接続管理（再接続、タイムアウト、エラーハンドリング）
- 状態の永続化（LocalStorage、履歴）
- API クライアント（リトライ、キャンセル、エラー分類）
- 共通 UI（進捗、通知、エラーバウンダリ）

## 📦 インストール

```bash
npm install @agentflow/ui
# or
pnpm add @agentflow/ui
```

## 🚀 クイックスタート

### 1. SSE ストリーム処理

```tsx
import { useAgentStream, AgentProgress } from "@agentflow/ui";

function MyApp() {
  const { agents, isConnected, start, isComplete, result } = useAgentStream({
    endpoint: "/api/my-agent/stream",
    agents: [
      { id: "analyzer", name: "分析", label: "データ分析" },
      { id: "recommender", name: "提案", label: "提案生成" },
    ],
    onComplete: (result) => console.log("完了:", result),
  });

  return (
    <div>
      <button onClick={() => start({ question: "..." })}>開始</button>
      <AgentProgress agents={agents} />
      {isComplete && <Result data={result} />}
    </div>
  );
}
```

### 2. Store（状態管理）

```tsx
import { createAgentStore } from "@agentflow/ui";

// 業務状態だけを定義（履歴/永続化などは SDK 側が付与）
interface MyAppState {
  question: string;
  options: string[];
}

const useMyAppStore = createAgentStore<MyAppState>({
  name: "my-app",
  initialState: {
    question: "",
    options: [],
  },
});

function InputPage() {
  const { data, setData } = useMyAppStore();

  return (
    <input
      value={data.question}
      onChange={(e) => setData({ question: e.target.value })}
    />
  );
}
```

### 3. API クライアント

```tsx
import { AgentApiClient, AgentApiError } from "@agentflow/ui";

const api = new AgentApiClient({
  baseUrl: "/api",
  retry: { maxRetries: 3 },
});

try {
  const result = await api.post("/process", { question: "..." });
  console.log(result);
} catch (err) {
  if (err instanceof AgentApiError) {
    if (err.isRetryable) {
      // ネットワーク/サーバーなど（リトライ可能）
    } else {
      // バリデーション/権限など（リトライ不可）
    }
  }
}
```

### 4. 通知システム

```tsx
import { NotificationProvider, useNotification } from "@agentflow/ui";

function App() {
  return (
    <NotificationProvider>
      <MyContent />
    </NotificationProvider>
  );
}

function MyContent() {
  const { notify } = useNotification();

  const handleSave = async () => {
    try {
      await api.post("/save", data);
      notify.success("保存に成功しました");
    } catch {
      notify.error("保存に失敗しました");
    }
  };
}
```

### 5. エラーバウンダリ

```tsx
import { ErrorBoundary, withErrorBoundary } from "@agentflow/ui";

function App() {
  return (
    <ErrorBoundary onError={(error) => logError(error)}>
      <MyApp />
    </ErrorBoundary>
  );
}

const SafeComponent = withErrorBoundary(MyComponent, {
  onError: (error) => logError(error),
});
```

## 📖 API リファレンス

### Hooks

| Hook | 説明 |
|------|------|
| `useAgentStream` | SSE ストリーム処理（自動再接続/タイムアウト） |
| `useNotification` | 通知 Hook |

### Store

| 関数 | 説明 |
|------|------|
| `createAgentStore` | Zustand Store 工場（履歴/永続化などを追加） |

### Components

| コンポーネント | 説明 |
|------|------|
| `AgentProgress` | Agent 進捗表示 |
| `Notification` | 通知 |
| `NotificationProvider` | 通知コンテキスト |
| `ErrorBoundary` | エラーバウンダリ |

### API Client

| クラス | 説明 |
|------|------|
| `AgentApiClient` | REST API クライアント（自動リトライ） |
| `AgentApiError` | API エラー型 |

## 🎨 スタイル（Tailwind 互換）

```tsx
<AgentProgress agents={agents} className="bg-gray-100 rounded-xl p-4" />
```

## 📄 型定義

```ts
import type {
  AGUIEvent,
  FlowStartEvent,
  FlowCompleteEvent,
  NodeStartEvent,
} from "@agentflow/ui";
```

## 🛠️ 開発（ローカル）

```bash
cd agentflow/sdk/frontend
npm install
npm run dev
```

テスト/静的チェック:

```bash
cd agentflow/sdk/frontend
npm run test
npm run lint
npm run type-check
```

ビルド:

```bash
cd agentflow/sdk/frontend
npm run build
```

## 📦 本番リリース（npm）

```bash
cd agentflow/sdk/frontend
npm run build
npm publish
```

