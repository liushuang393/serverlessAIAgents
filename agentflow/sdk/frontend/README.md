# @agentflow/ui

> AgentFlow Frontend SDK - React Hooks & Components for AI Agent Applications

## 🎯 目标

让 AI 应用开发者**只关注业务逻辑**，不需要重复实现：

- SSE 连接管理（自动重连、超时、错误处理）
- 状态持久化（LocalStorage、历史记录）
- API 客户端（重试、取消、错误分类）
- 通用 UI 组件（进度、通知、错误边界）

## 📦 安装

```bash
npm install @agentflow/ui
# or
pnpm add @agentflow/ui
```

## 🚀 快速开始

### 1. SSE 流处理

```tsx
import { useAgentStream, AgentProgress } from '@agentflow/ui';

function MyApp() {
  const { agents, isConnected, start, isComplete, result } = useAgentStream({
    endpoint: '/api/my-agent/stream',
    agents: [
      { id: 'analyzer', name: '分析', label: '数据分析' },
      { id: 'recommender', name: '推荐', label: '生成建议' },
    ],
    onComplete: (result) => console.log('完成:', result),
  });

  return (
    <div>
      <button onClick={() => start({ question: '...' })}>
        开始分析
      </button>
      <AgentProgress agents={agents} />
      {isComplete && <Result data={result} />}
    </div>
  );
}
```

### 2. Store 状态管理

```tsx
import { createAgentStore } from '@agentflow/ui';

// 只定义业务状态
interface MyAppState {
  question: string;
  options: string[];
}

const useMyAppStore = createAgentStore<MyAppState>({
  name: 'my-app',
  initialState: {
    question: '',
    options: [],
  },
  // 框架自动提供：history, persistence, reset, setPage, setError
});

function InputPage() {
  const { data, setData, history, addToHistory } = useMyAppStore();
  
  return (
    <input 
      value={data.question}
      onChange={(e) => setData({ question: e.target.value })}
    />
  );
}
```

### 3. API 客户端

```tsx
import { AgentApiClient, AgentApiError } from '@agentflow/ui';

const api = new AgentApiClient({
  baseUrl: '/api',
  retry: { maxRetries: 3 },
});

// 自动重试、错误分类
try {
  const result = await api.post('/process', { question: '...' });
} catch (err) {
  if (err instanceof AgentApiError) {
    if (err.isRetryable) {
      // 可重试的错误（网络、服务器错误）
    } else {
      // 不可重试的错误（验证、权限）
    }
  }
}
```

### 4. 通知系统

```tsx
import { NotificationProvider, useNotification } from '@agentflow/ui';

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
      await api.post('/save', data);
      notify.success('保存成功');
    } catch (err) {
      notify.error('保存失败');
    }
  };
}
```

### 5. 错误边界

```tsx
import { ErrorBoundary, withErrorBoundary } from '@agentflow/ui';

// 方式1：直接使用
function App() {
  return (
    <ErrorBoundary onError={(error) => logError(error)}>
      <MyApp />
    </ErrorBoundary>
  );
}

// 方式2：HOC
const SafeComponent = withErrorBoundary(MyComponent, {
  onError: (error) => logError(error),
});
```

## 📖 API 参考

### Hooks

| Hook | 说明 |
|------|------|
| `useAgentStream` | SSE 流处理，自动重连、超时 |
| `useNotification` | 通知系统 Hook |

### Store

| 函数 | 说明 |
|------|------|
| `createAgentStore` | Zustand Store 工厂，自动添加历史、持久化 |

### Components

| 组件 | 说明 |
|------|------|
| `AgentProgress` | Agent 进度展示 |
| `Notification` | 单个通知 |
| `NotificationProvider` | 通知上下文提供者 |
| `ErrorBoundary` | 错误边界 |

### API Client

| 类 | 说明 |
|------|------|
| `AgentApiClient` | REST API 客户端，自动重试 |
| `AgentApiError` | API 错误类 |

## 🎨 自定义样式

所有组件都支持 `className` 属性，兼容 Tailwind CSS：

```tsx
<AgentProgress
  agents={agents}
  className="bg-gray-100 rounded-xl p-4"
/>

<Notification
  type="success"
  message="成功"
  className="shadow-xl"
/>
```

## 📄 类型定义

```typescript
// AG-UI 事件类型
import type {
  AGUIEvent,
  FlowStartEvent,
  FlowCompleteEvent,
  NodeStartEvent,
  // ...
} from '@agentflow/ui';

// Store 类型
import type {
  BaseAgentState,
  BaseAgentActions,
  HistoryItem,
} from '@agentflow/ui';
```

## 🔧 配置选项

### useAgentStream

```typescript
interface UseAgentStreamConfig<TResult> {
  endpoint: string;           // SSE 端点
  agents: AgentDefinition[];  // Agent 列表
  baseUrl?: string;           // 基础 URL
  autoReconnect?: boolean;    // 自动重连 (默认: true)
  maxReconnectAttempts?: number; // 最大重连次数 (默认: 3)
  connectionTimeout?: number; // 连接超时 ms (默认: 30000)
  onComplete?: (result: TResult) => void;
  onError?: (error: string) => void;
}
```

### createAgentStore

```typescript
interface CreateAgentStoreConfig<T> {
  name: string;               // Store 名称
  initialState: T;            // 初始业务状态
  actions?: CustomActions;    // 自定义 actions
  maxHistoryItems?: number;   // 最大历史条数 (默认: 10)
  persistFields?: string[];   // 持久化字段
  enableDevtools?: boolean;   // DevTools (默认: true)
}
```

### AgentApiClient

```typescript
interface AgentApiClientConfig {
  baseUrl: string;
  retry?: {
    maxRetries: number;    // 默认: 3
    baseDelay: number;     // 默认: 1000ms
    maxDelay: number;      // 默认: 10000ms
  };
  headers?: Record<string, string>;
  timeout?: number;        // 默认: 30000ms
}
```

## 📝 License

MIT

