# AgentFlow 优化总结

> 优化原则：接口标准化 + 自研实现 + 可替换性

本次优化按照 P1/P2 优先级实施，设计标准接口但完全自研实现，保证将来可无缝替换为成熟框架。

---

## 🏗️ 核心架构：统一服务层

### 设计目标
- **后端通用**：不针对特定前端定制
- **三模式统一**：API / CLI / Studio 共用同一服务层

### 架构图

```
┌─────────────────────────────────────────────────────────────┐
│                    Frontend Layer                            │
├─────────────┬─────────────┬─────────────┬─────────────────────┤
│    CLI      │    API      │   Studio    │   WebSocket         │
│   (Click)   │  (FastAPI)  │   (React)   │   (Realtime)        │
└──────┬──────┴──────┬──────┴──────┬──────┴──────┬──────────────┘
       │             │             │             │
       └─────────────┴──────┬──────┴─────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                  Service Layer（统一服务层）                  │
│                  agentflow/services/                        │
├─────────────────────────────────────────────────────────────┤
│  AgentService      WorkflowService      ToolService         │
│  - execute()       - execute()          - execute()         │
│  - execute_stream()  - execute_stream()   - list()          │
│  - execute_with_callback()               - call()           │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                    Core Layer                                │
│  AgentBlock / LLMClient / ToolProvider / MemoryManager      │
└─────────────────────────────────────────────────────────────┘
```

### 三种调用模式

```python
from agentflow.services import AgentService

service = AgentService()

# ============================================
# 1. API 模式 - 返回结果
# ============================================
result = await service.execute(
    agent_id="MyAgent",
    input_data={"text": "Hello"},
)
# result: ServiceResult(success=True, data={...})

# ============================================
# 2. CLI 模式 - 回调进度
# ============================================
def on_progress(pct, msg):
    print(f"[{pct:5.1f}%] {msg}")

result = await service.execute_with_callback(
    agent_id="MyAgent",
    input_data={"text": "Hello"},
    on_progress=on_progress,
)
# 输出:
# [ 10.0%] Loading agent...
# [ 30.0%] Executing agent...
# [100.0%] Completed

# ============================================
# 3. Studio/WebSocket 模式 - 事件流
# ============================================
async for event in service.execute_stream(
    agent_id="MyAgent",
    input_data={"text": "Hello"},
):
    await websocket.send(event.to_json())
    # 或 SSE: yield event.to_sse()
```

### 事件类型（通用）

```python
class ServiceEventType(Enum):
    # 生命周期
    START = "start"
    COMPLETE = "complete"
    ERROR = "error"
    
    # 进度
    PROGRESS = "progress"
    PHASE = "phase"
    
    # Agent/Workflow
    AGENT_START = "agent.start"
    AGENT_COMPLETE = "agent.complete"
    TOOL_CALL = "tool.call"
    
    # HITL
    APPROVAL_REQUIRED = "approval.required"
```

---

## 新增模块概览

| 模块 | 接口参考 | 文件位置 | 状态 |
|------|----------|----------|------|
| 并行工具执行器 | OpenAI Function Calling | `providers/tool_executor.py` | ✅ 完成 |
| 向量存储 | LlamaIndex/LangChain VectorStore | `memory/vector_store.py` | ✅ 完成 |
| WebSocket通信 | FastAPI WebSocket | `integrations/websocket_integration.py` | ✅ 完成 |
| Reflexion失败学习 | Reflexion (NeurIPS 2023) | `patterns/reflexion.py` | ✅ 完成 |
| 统一错误响应 | RFC 7807 Problem Details | `core/error_response.py` | ✅ 完成 |

---

## 1. 并行工具执行器 (ToolExecutor)

**接口参考**: OpenAI Function Calling API  
**将来可替换**: LangChain ToolExecutor, LiteLLM

### 使用示例

```python
from agentflow.providers import (
    ToolExecutor, ToolCall, SimpleFallbackStrategy
)

# 创建执行器
executor = ToolExecutor(
    tool_provider=my_tools,
    fallback_strategy=SimpleFallbackStrategy({
        "search_v1": ["search_v2", "search_fallback"]  # 失败时自动切换
    }),
    max_concurrent=10,  # 最大并发数
)

# 并行执行（OpenAI parallel function calling 兼容）
results = await executor.execute_parallel([
    ToolCall.create("search", {"query": "AI"}),
    ToolCall.create("fetch", {"url": "https://..."}),
    ToolCall.create("analyze", {"data": "..."}),
])

# 结果格式与 OpenAI tool message 兼容
for result in results.results:
    print(f"Tool: {result.name}, Status: {result.status}")
    print(f"Content: {result.content}")
```

### 核心特性
- ✅ OpenAI tool message 格式兼容
- ✅ 并行执行 (asyncio.gather + Semaphore)
- ✅ 自动重试 (指数退避)
- ✅ 失败时自动 Fallback
- ✅ 执行统计

---

## 2. 向量存储 (VectorStore)

**接口参考**: LlamaIndex VectorStore, LangChain VectorStore  
**将来可替换**: Qdrant, Pinecone, Milvus, Chroma

### 使用示例

```python
from agentflow.memory import (
    Document, InMemoryVectorStore, create_vector_store
)

# 方式1: 直接使用
store = InMemoryVectorStore()

# 方式2: 工厂创建（推荐，便于将来切换）
store = create_vector_store("memory")  # 将来: create_vector_store("qdrant", url="...")

# 添加文档（LangChain Document 兼容）
await store.add_documents([
    Document(page_content="AgentFlow是一个Agent框架", metadata={"source": "doc1"}),
    Document(page_content="支持多种LLM提供商", metadata={"source": "doc2"}),
])

# 类似度搜索
results = await store.similarity_search("什么是AgentFlow?", k=3)
for r in results:
    print(f"Score: {r.score:.2f}, Content: {r.document.page_content}")

# MMR搜索（多样性优化）
results = await store.max_marginal_relevance_search(
    "Agent框架", k=5, lambda_mult=0.5  # 平衡关联性和多样性
)
```

### 核心特性
- ✅ LlamaIndex Node / LangChain Document 双兼容
- ✅ 相似度搜索 + MMR搜索
- ✅ 元数据过滤
- ✅ 嵌入模型可插拔
- ✅ 工厂函数便于切换实现

---

## 3. WebSocket 通信

**接口参考**: FastAPI WebSocket, Socket.IO  
**将来可替换**: Socket.IO, Pusher

### 使用示例

```python
from fastapi import FastAPI, WebSocket
from agentflow.integrations import (
    WebSocketManager, WSEvent, WSEventType, create_websocket_router
)

app = FastAPI()
manager = WebSocketManager(
    heartbeat_interval=30.0,
    connection_timeout=300.0,
)

# 方式1: 使用路由工厂
app.include_router(create_websocket_router(manager))

# 方式2: 手动处理
@app.websocket("/ws/{session_id}")
async def websocket_endpoint(websocket: WebSocket, session_id: str):
    await manager.handle_connection(websocket, session_id)

# Agent 中发送事件
await manager.send(session_id, WSEvent(
    type=WSEventType.PROGRESS,
    data={"task": "分析数据", "progress": 50},
))

# 广播到所有连接
await manager.broadcast(WSEvent(
    type=WSEventType.STATE_UPDATE,
    data={"agents_online": 5},
))

# 注册命令处理器（HITL支持）
manager.register_command_handler("approve", async_approve_handler)
```

### 核心特性
- ✅ FastAPI WebSocket 原生支持
- ✅ AG-UI 事件格式兼容
- ✅ 心跳检测 + 超时清理
- ✅ 命令处理器注册
- ✅ 连接统计

---

## 4. Reflexion 失败学习

**接口参考**: Reflexion 论文 (NeurIPS 2023)  
**将来可扩展**: 集成 DSPy 优化

### 使用示例

```python
from agentflow.patterns import ReflectiveEvolver

evolver = ReflectiveEvolver(llm_client=my_llm)

# 失败时学习
try:
    result = await agent.execute(task)
except Exception as e:
    reflection = await evolver.learn_from_failure(
        task="数据库连接",
        error=e,
        context={"retry_count": 3},
    )
    print(f"学到了: {reflection.how_to_avoid}")

# 成功时也学习
await evolver.learn_from_success(task, result)

# 获取相关反省用于增强 prompt
reflections = evolver.get_relevant_reflections("数据库操作")
enhanced_prompt = f"""
{original_prompt}

## 过去的失败教训
{evolver.get_reflection_prompt("数据库操作")}
"""

# 记录反省是否有效
evolver.record_outcome(reflection.id, task, success=True)
```

### 核心特性
- ✅ verbal reflection 生成
- ✅ 失败模式追踪
- ✅ 相关反省检索
- ✅ 学习效果反馈
- ✅ 信任度自动调整

---

## 5. 统一错误响应 (RFC 7807)

**接口参考**: RFC 7807 Problem Details  
**将来可扩展**: 集成 Sentry/Datadog

### 使用示例

```python
from agentflow.core import (
    ErrorCode, create_error_response, AgentFlowAPIError,
    NotFoundError, create_exception_handlers
)

# 创建标准错误响应
error = create_error_response(
    code=ErrorCode.AGENT_NOT_FOUND,
    detail="Agent 'MyAgent' was not found",
)
# 返回 RFC 7807 格式:
# {
#   "type": "https://agentflow.dev/errors/agent_not_found",
#   "title": "Agent Not Found",
#   "status": 404,
#   "detail": "Agent 'MyAgent' was not found",
#   "code": "agent_not_found",
#   "trace_id": "trace_abc123..."
# }

# 抛出类型化错误
raise NotFoundError(resource_type="agent", resource_id="MyAgent")

# FastAPI 自动处理
from fastapi import FastAPI
app = FastAPI()
for exc_type, handler in create_exception_handlers().items():
    app.add_exception_handler(exc_type, handler)
```

### 核心特性
- ✅ RFC 7807 完全兼容
- ✅ 统一错误码体系
- ✅ HTTP 状态码自动映射
- ✅ trace_id 分布式追踪
- ✅ FastAPI 异常处理器

---

## 接口设计原则

### 1. 标准接口，自研实现

```
┌─────────────────────────────────────────────────────────┐
│                    应用层 (Apps)                         │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│              标准接口层 (Interfaces)                     │
│  - VectorStore (LlamaIndex/LangChain 兼容)              │
│  - ToolExecutor (OpenAI 兼容)                           │
│  - ErrorResponse (RFC 7807 兼容)                        │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│              自研实现层 (Implementations)                │
│  - InMemoryVectorStore                                  │
│  - ToolExecutor + SimpleFallbackStrategy                │
│  - ErrorCode + ErrorResponse                            │
└─────────────────────────────────────────────────────────┘
                          │
                    将来可替换
                          ▼
┌─────────────────────────────────────────────────────────┐
│              外部框架层 (External - 将来)                │
│  - QdrantVectorStore / PineconeVectorStore              │
│  - LangChain ToolExecutor                               │
│  - Sentry Error Tracking                                │
└─────────────────────────────────────────────────────────┘
```

### 2. 将来替换零成本

由于接口与成熟框架兼容，替换时只需：

```python
# Before (自研)
store = create_vector_store("memory")

# After (Qdrant)
store = create_vector_store("qdrant", url="localhost:6333")
# 接口完全相同，应用代码无需修改
```

---

## 导入方式

```python
# 推荐：从模块顶层导入
from agentflow.providers import ToolExecutor, ToolCall
from agentflow.memory import VectorStore, Document, create_vector_store
from agentflow.integrations import WebSocketManager, WSEvent
from agentflow.patterns import ReflectiveEvolver, Reflection
from agentflow.core import ErrorCode, ErrorResponse, create_error_response
```

---

## 下一步计划

| 优先级 | 计划 | 描述 |
|--------|------|------|
| P1 | DSPy 集成 | Prompt 自动优化 |
| P1 | Token 计数优化 | tiktoken 集成 |
| P2 | Qdrant 实现 | VectorStore 生产实现 |
| P2 | WebSocket 集群 | Redis PubSub 支持 |
| P3 | Observability | OpenTelemetry 集成 |

---

*最后更新: 2026-01-13*
