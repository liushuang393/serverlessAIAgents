# AgentFlow 框架抽象改进计划

> **目标**: 让各个 App 只关注业务逻辑，技术实现由框架统一提供

---

## 1. 当前状态分析

### 1.1 框架层 vs 应用层责任混淆

通过对 `decision_governance_engine` 前端的改进，发现以下功能**本应由框架提供**，但目前由各应用**重复实现**：

```
┌─────────────────────────────────────────────────────────────────┐
│                    当前重复实现的功能                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  📱 前端层 (每个App都在重复)                                     │
│  ├── API客户端 (重试/取消/错误处理)                              │
│  ├── SSE Hook (自动重连/超时)                                   │
│  ├── 状态持久化 (LocalStorage策略)                              │
│  ├── 通知组件 (Toast/Alert)                                     │
│  ├── 加载状态管理                                               │
│  ├── 错误边界组件                                               │
│  └── 历史记录管理                                               │
│                                                                 │
│  🔌 后端层 (每个App都在重复)                                     │
│  ├── FastAPI路由模板 (health/stream/export)                     │
│  ├── SSE生成器                                                  │
│  ├── CORS配置                                                   │
│  └── 错误响应格式                                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 1.2 理想架构

```
┌─────────────────────────────────────────────────────────────────┐
│                        App 只关注                                │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  • 业务 Schema (Input/Output)                             │  │
│  │  • Agent 提示词 (Prompts)                                 │  │
│  │  • 业务组件 (自定义UI)                                     │  │
│  │  • 业务逻辑 (Workflow)                                    │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     AgentFlow 框架提供                           │
│  ┌────────────────────┐  ┌────────────────────┐               │
│  │   @agentflow/ui    │  │   @agentflow/api   │               │
│  ├────────────────────┤  ├────────────────────┤               │
│  │ • useAgentStream   │  │ • createAgentAPI   │               │
│  │ • useAgentStore    │  │ • createSSERouter  │               │
│  │ • AgentProgress    │  │ • createHealthAPI  │               │
│  │ • Notification     │  │ • AgentApiError    │               │
│  │ • ErrorBoundary    │  │ • corsMiddleware   │               │
│  └────────────────────┘  └────────────────────┘               │
└─────────────────────────────────────────────────────────────────┘
```

---

## 2. 需要抽象的模块

### 2.1 前端 SDK (`@agentflow/ui`)

#### 模块结构

```
agentflow/
└── sdk/
    └── frontend/           # 新增：前端 SDK
        ├── package.json
        ├── src/
        │   ├── index.ts
        │   │
        │   ├── hooks/              # React Hooks
        │   │   ├── useAgentStream.ts      # SSE 流处理
        │   │   ├── useAgentStore.ts       # Zustand store 工厂
        │   │   ├── useAgentApi.ts         # API 客户端
        │   │   └── useAgentHistory.ts     # 历史记录
        │   │
        │   ├── components/         # 通用组件
        │   │   ├── AgentProgress/         # 进度展示
        │   │   ├── Notification/          # 通知
        │   │   ├── ErrorBoundary/         # 错误边界
        │   │   ├── LoadingOverlay/        # 加载遮罩
        │   │   └── SignaturePanel/        # 签名面板
        │   │
        │   ├── api/                # API 工具
        │   │   ├── AgentApiClient.ts      # 通用 API 客户端
        │   │   ├── retry.ts               # 重试策略
        │   │   └── errors.ts              # 错误类型
        │   │
        │   ├── store/              # 状态管理
        │   │   ├── createAgentStore.ts    # Store 工厂
        │   │   └── persistence.ts         # 持久化策略
        │   │
        │   └── types/              # 类型定义
        │       ├── events.ts              # AG-UI 事件类型
        │       └── store.ts               # Store 类型
        │
        └── README.md
```

#### 核心 API 设计

```typescript
// ========================================
// 1. useAgentStream - SSE 流处理 Hook
// ========================================

import { useAgentStream } from '@agentflow/ui';

// App 只需调用，不关心重连/超时/错误处理
const { 
  agents,           // Agent 进度列表
  isConnected,      // 连接状态
  isComplete,       // 完成状态
  error,            // 错误信息
  result,           // 最终结果
  start,            // 开始流
  stop,             // 停止流
  retry,            // 重试
} = useAgentStream({
  endpoint: '/api/decision/stream',
  agents: ['dao', 'fa', 'shu', 'qi', 'review'],  // 业务定义
  onComplete: (result) => { /* 业务逻辑 */ },
});


// ========================================
// 2. createAgentStore - Store 工厂
// ========================================

import { createAgentStore } from '@agentflow/ui';

// App 只定义业务状态
interface DecisionState {
  question: string;
  constraints: ConstraintSet;
  // ... 业务字段
}

// 框架自动添加：history, persistence, devtools
const useDecisionStore = createAgentStore<DecisionState>({
  name: 'decision',
  initialState: { question: '', constraints: {} },
  
  // 业务 actions
  actions: (set, get) => ({
    setQuestion: (q: string) => set({ question: q }),
    // ...
  }),
  
  // 框架自动提供
  // - history (最近10条记录)
  // - persistence (LocalStorage)
  // - devtools (开发工具)
});


// ========================================
// 3. AgentApiClient - API 客户端
// ========================================

import { AgentApiClient } from '@agentflow/ui';

// App 只配置 endpoint，不关心重试/取消/错误处理
const api = new AgentApiClient({
  baseUrl: '/api',
  retry: { maxRetries: 3 },  // 可选覆盖
});

// 自动处理：重试、取消、错误分类
const result = await api.post('/decision', payload);
const blob = await api.download('/report/123/pdf');


// ========================================
// 4. 通用组件
// ========================================

import { 
  AgentProgress,    // 进度展示
  Notification,     // 通知
  ErrorBoundary,    // 错误边界
  SignaturePanel,   // 签名面板
} from '@agentflow/ui';

// App 只传业务数据
<AgentProgress 
  agents={[
    { id: 'dao', name: '道', label: '本質分析' },
    { id: 'fa', name: '法', label: '戦略選定' },
  ]}
  status={agentStatus}
/>

<Notification 
  type="success" 
  message="処理完了" 
  autoClose={5000}
/>

<SignaturePanel 
  reportId="123"
  onSign={handleSign}
  onExport={handleExport}
/>
```

### 2.2 后端 SDK (`agentflow.sdk.api`)

#### 模块结构

```
agentflow/
└── sdk/
    └── api/                # 新增：后端 API SDK
        ├── __init__.py
        ├── router.py              # 路由工厂
        ├── sse.py                 # SSE 生成器
        ├── errors.py              # 错误处理
        ├── middleware.py          # 中间件
        └── schemas.py             # 通用 Schema
```

#### 核心 API 设计

```python
# ========================================
# 1. 路由工厂 - 自动生成标准端点
# ========================================

from agentflow.sdk.api import create_agent_router

# App 只定义业务 Engine
router = create_agent_router(
    engine=DecisionEngine,
    prefix="/api",
    
    # 自动生成端点：
    # - GET /api/health
    # - GET /api/agents
    # - POST /api/decision
    # - GET /api/decision/stream (SSE)
    # - GET /api/report/{id}/pdf
    # - GET /api/report/{id}/components
)

# App 只需添加到 FastAPI
app.include_router(router)


# ========================================
# 2. SSE 生成器 - 统一事件格式
# ========================================

from agentflow.sdk.api import create_sse_response

async def stream_endpoint(request: Request):
    async def event_generator():
        async for event in engine.process_with_events(req):
            yield event
    
    # 框架统一处理：格式化、错误处理、连接管理
    return create_sse_response(event_generator())


# ========================================
# 3. 错误处理 - 统一错误格式
# ========================================

from agentflow.sdk.api import AgentApiException, error_handler

@app.exception_handler(AgentApiException)
async def handle_agent_error(request, exc):
    return error_handler(exc)
    # 返回统一格式：
    # {
    #   "error": "VALIDATION_ERROR",
    #   "message": "入力が不正です",
    #   "details": {...},
    #   "retryable": false
    # }
```

---

## 3. 改进后的 App 代码对比

### 3.1 改进前 (当前状态)

```
apps/decision_governance_engine/
├── frontend/
│   ├── src/
│   │   ├── api/
│   │   │   └── client.ts          # 168行 - 重复实现
│   │   ├── hooks/
│   │   │   └── useDecisionStream.ts  # 191行 - 重复实现
│   │   ├── store/
│   │   │   └── useDecisionStore.ts   # 134行 - 部分重复
│   │   ├── components/
│   │   │   ├── DecisionInputPage.tsx   # 303行
│   │   │   ├── ProcessingPage.tsx      # 250行
│   │   │   └── ReportPage.tsx          # 530行
│   │   └── types/
│   │       └── index.ts                # 177行
│   └── ...
└── api.py                              # 418行 - 大量重复
```

### 3.2 改进后 (使用框架 SDK)

```
apps/decision_governance_engine/
├── frontend/
│   ├── src/
│   │   ├── store/
│   │   │   └── useDecisionStore.ts   # 50行 - 只有业务状态
│   │   ├── components/
│   │   │   ├── DecisionInputPage.tsx   # 200行 - 只有业务UI
│   │   │   ├── ProcessingPage.tsx      # 80行  - 使用框架组件
│   │   │   └── ReportPage.tsx          # 300行 - 只有业务UI
│   │   └── types/
│   │       └── index.ts                # 100行 - 只有业务类型
│   └── package.json  # 依赖 @agentflow/ui
│
├── api.py                              # 50行 - 使用路由工厂
└── agent.yaml                          # 业务配置
```

**代码量减少约 60%，且无重复逻辑！**

---

## 4. 实施路线图

### Phase 1: 前端 SDK (2周)

```
Week 1:
├── [ ] 创建 @agentflow/ui 包结构
├── [ ] 实现 useAgentStream hook
├── [ ] 实现 AgentApiClient
└── [ ] 实现 createAgentStore

Week 2:
├── [ ] 实现通用组件 (Progress, Notification, ErrorBoundary)
├── [ ] 编写单元测试
├── [ ] 编写文档
└── [ ] 迁移 decision_governance_engine
```

### Phase 2: 后端 SDK (1周)

```
Week 3:
├── [ ] 实现 create_agent_router
├── [ ] 实现 SSE 生成器
├── [ ] 实现统一错误处理
├── [ ] 迁移 decision_governance_engine
└── [ ] 编写文档
```

### Phase 3: 模板更新 (1周)

```
Week 4:
├── [ ] 更新项目模板
├── [ ] 更新 CLI 脚手架
├── [ ] 编写快速入门教程
└── [ ] 更新其他 Apps
```

---

## 5. 快速学习指南

### 5.1 AI 学习路径 (给 LLM 的提示)

```markdown
# AgentFlow 框架理解指南

## 核心概念层次

1. **道 (Why)** - 理解目标
   - AgentFlow 让 AI 应用开发标准化
   - App 只关注业务，技术由框架处理
   
2. **法 (What)** - 理解结构
   - 后端: agentflow.core (引擎) + agentflow.protocols (协议)
   - 前端: @agentflow/ui (组件 + Hooks)
   - 协议: AG-UI (事件) + A2UI (组件) + A2A (通信)
   
3. **术 (How)** - 理解用法
   - 创建 Agent: @agent 装饰器
   - 创建 Flow: create_flow() 或 YAML
   - 前端连接: useAgentStream + AgentProgress
   
4. **器 (Tools)** - 理解工具
   - CLI: agentflow create/run/test
   - Studio: 可视化编辑器
   - SDK: 前后端统一 SDK
```

### 5.2 人类学习路径

#### 5分钟快速入门

```bash
# 1. 创建项目
agentflow create my-agent --template decision

# 2. 定义业务 (agent.yaml)
cat > agent.yaml << 'EOF'
name: my-agent
agents:
  analyzer:
    prompt: "分析用户输入..."
  recommender:
    prompt: "基于分析给出建议..."
workflow:
  - analyzer
  - recommender
EOF

# 3. 启动
agentflow run
```

#### 30分钟进阶

```typescript
// 1. 自定义前端 (使用框架组件)
import { useAgentStream, AgentProgress } from '@agentflow/ui';

function MyApp() {
  const { agents, start, isComplete, result } = useAgentStream({
    endpoint: '/api/my-agent/stream',
    agents: ['analyzer', 'recommender'],
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

```python
# 2. 自定义后端 (使用路由工厂)
from agentflow.sdk.api import create_agent_router
from my_agent.engine import MyAgentEngine

router = create_agent_router(
    engine=MyAgentEngine,
    prefix="/api/my-agent",
)

app.include_router(router)
```

### 5.3 速查表

```
┌─────────────────────────────────────────────────────────────────┐
│                    AgentFlow 速查表                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  创建项目      agentflow create <name>                          │
│  运行项目      agentflow run                                    │
│  测试项目      agentflow test                                   │
│  打开 Studio   agentflow studio                                 │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  后端 Agent    @agent class MyAgent: ...                        │
│  后端 Tool     @tool def search(): ...                          │
│  后端 Flow     flow = create_flow([Agent1, Agent2])             │
│  后端 SSE      async for event in flow.run_stream(): ...        │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  前端 Stream   useAgentStream({ endpoint, agents })             │
│  前端 Store    createAgentStore({ name, initialState })         │
│  前端 API      new AgentApiClient({ baseUrl })                  │
│  前端 Progress <AgentProgress agents={[...]} />                 │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  AG-UI 事件    flow.start / node.start / progress / complete    │
│  A2UI 组件     Text / Button / Card / List / Form               │
│  A2A 通信      AgentCard / AgentSkill / A2AClient               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 6. 预期收益

| 指标 | 改进前 | 改进后 | 提升 |
|------|--------|--------|------|
| 新 App 开发时间 | 2-3周 | 3-5天 | **70%↓** |
| 前端代码量 | ~1500行 | ~500行 | **67%↓** |
| 后端代码量 | ~500行 | ~100行 | **80%↓** |
| 重复代码 | 高 | 近零 | **95%↓** |
| 学习曲线 | 陡峭 | 平缓 | **显著** |
| 维护成本 | 每 App 独立 | 框架统一 | **80%↓** |

---

## 7. 附录：详细 API 规范

### 7.1 AG-UI 事件类型

| 事件 | 触发时机 | 前端处理 |
|------|---------|---------|
| `flow.start` | 流开始 | 显示连接状态 |
| `node.start` | Agent 开始 | 更新进度为"处理中" |
| `progress` | 进度更新 | 更新百分比 |
| `node.complete` | Agent 完成 | 显示结果预览 |
| `node.error` | Agent 错误 | 显示错误信息 |
| `flow.complete` | 流完成 | 跳转结果页 |
| `flow.error` | 流错误 | 显示错误+重试按钮 |

### 7.2 Store 状态结构

```typescript
interface AgentStoreState<T> {
  // 业务状态 (App 定义)
  data: T;
  
  // 框架自动管理
  currentPage: 'input' | 'processing' | 'result';
  isLoading: boolean;
  error: string | null;
  history: HistoryItem[];
  
  // 框架自动提供的 actions
  setData: (data: Partial<T>) => void;
  setPage: (page: string) => void;
  setError: (error: string | null) => void;
  addToHistory: (item: Omit<HistoryItem, 'id' | 'createdAt'>) => void;
  reset: () => void;
}
```

### 7.3 API 错误码

| 错误码 | 含义 | 可重试 |
|-------|------|-------|
| `VALIDATION_ERROR` | 输入验证失败 | 否 |
| `AUTHENTICATION_ERROR` | 认证失败 | 否 |
| `AUTHORIZATION_ERROR` | 权限不足 | 否 |
| `NOT_FOUND` | 资源不存在 | 否 |
| `RATE_LIMITED` | 请求过多 | 是 |
| `SERVER_ERROR` | 服务器错误 | 是 |
| `TIMEOUT` | 超时 | 是 |
| `NETWORK_ERROR` | 网络错误 | 是 |

---

*文档版本: v1.0 | 更新日期: 2026-01-03*

