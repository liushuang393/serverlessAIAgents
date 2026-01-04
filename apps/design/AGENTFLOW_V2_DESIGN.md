# AgentFlow v2.0 设计方案

> 统一入口、最佳实践默认、渐进式复杂度

---

## 📌 设计目标

```
┌────────────────────────────────────────────────────────────────────────┐
│                         AgentFlow v2.0 设计原则                         │
├────────────────────────────────────────────────────────────────────────┤
│                                                                        │
│  🎯 用户视角                                                           │
│  ├── 5 分钟上手：第一个 Agent 在 5 分钟内运行                          │
│  ├── 10 行代码：简单 Agent 只需 10 行代码                              │
│  └── 2 个入口：@agent + create_flow() 覆盖 90% 场景                    │
│                                                                        │
│  🔧 框架视角                                                           │
│  ├── 默认最佳实践：LLM、retry、timeout、memory 开箱即用                │
│  ├── 零配置启动：环境变量自动检测                                       │
│  └── 渐进式复杂度：简单场景简单，复杂场景可扩展                         │
│                                                                        │
└────────────────────────────────────────────────────────────────────────┘
```

---

## 🏛️ 架构概览

```
                           AgentFlow v2.0 架构
┌─────────────────────────────────────────────────────────────────────────┐
│                              用户层                                      │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐      │
│  │  @agent 装饰器   │  │  create_flow()   │  │  @tool 装饰器    │      │
│  └────────┬─────────┘  └────────┬─────────┘  └────────┬─────────┘      │
└───────────┼─────────────────────┼─────────────────────┼─────────────────┘
            │                     │                     │
┌───────────┼─────────────────────┼─────────────────────┼─────────────────┐
│           ▼                     ▼                     ▼                 │
│  ┌──────────────────────────────────────────────────────────────┐      │
│  │                      统一运行时                                │      │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐     │      │
│  │  │ LLM 管理 │  │ 重试策略 │  │ 超时控制 │  │ 内存系统 │     │      │
│  │  └──────────┘  └──────────┘  └──────────┘  └──────────┘     │      │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐     │      │
│  │  │工具注册表│  │ 事件发射 │  │ 上下文   │  │ 可观测性 │     │      │
│  │  └──────────┘  └──────────┘  └──────────┘  └──────────┘     │      │
│  └──────────────────────────────────────────────────────────────┘      │
│                              核心层                                      │
└─────────────────────────────────────────────────────────────────────────┘
            │                     │                     │
┌───────────┼─────────────────────┼─────────────────────┼─────────────────┐
│           ▼                     ▼                     ▼                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐                  │
│  │ LLM Provider │  │ DB Provider  │  │Vector Provider│                  │
│  │  (OpenAI/    │  │ (Supabase/   │  │  (Pinecone/  │                  │
│  │   Claude/    │  │  PostgreSQL/ │  │   Qdrant/    │                  │
│  │   Gemini)    │  │   SQLite)    │  │   Chroma)    │                  │
│  └──────────────┘  └──────────────┘  └──────────────┘                  │
│                           提供者层                                       │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 📋 API 设计

### 1. @agent 装饰器（增强版）

```python
# agentflow/agent_decorator_v2.py

from typing import Any, Callable, TypeVar
from functools import wraps
import asyncio

T = TypeVar("T")

def agent(
    cls: type[T] | None = None,
    *,
    # 基本配置
    name: str | None = None,
    description: str | None = None,
    
    # LLM 配置（可选，有默认值）
    model: str | None = None,        # 默认从 AGENTFLOW_MODEL 环境变量
    temperature: float = 0.7,
    max_tokens: int = 2000,
    
    # 健壮性配置（可选，有默认值）
    max_retries: int = 3,
    timeout: int = 30,
    retry_backoff: str = "exponential",  # "fixed" | "exponential"
    
    # 功能开关
    enable_memory: bool = True,
    enable_streaming: bool = True,
    enable_tracing: bool = True,
    
    # 工具配置
    tools: list[str] | None = None,      # 指定可用的工具
    skills: list[str] | None = None,     # 指定使用的 Skills
) -> Callable[[type[T]], type[T]] | type[T]:
    """Agent 装饰器 - 统一入口
    
    特性:
    - 自动注入: self.llm, self.memory, self.tools, self.context
    - 默认重试: 3 次，指数退避
    - 默认超时: 30 秒
    - 自动事件发射: agent.start, agent.complete, agent.error
    
    Example:
        @agent
        class MyAgent:
            async def process(self, input_data: dict) -> dict:
                response = await self.llm.chat(...)
                return {"result": response}
        
        # 直接调用
        result = await MyAgent.run({"question": "..."})
        
        # 流式调用
        async for event in MyAgent.stream({"question": "..."}):
            print(event)
    """
    def decorator(cls: type[T]) -> type[T]:
        # 保存原始配置
        cls._agent_config = {
            "name": name or cls.__name__,
            "description": description or cls.__doc__ or "",
            "model": model,
            "temperature": temperature,
            "max_tokens": max_tokens,
            "max_retries": max_retries,
            "timeout": timeout,
            "retry_backoff": retry_backoff,
            "enable_memory": enable_memory,
            "enable_streaming": enable_streaming,
            "enable_tracing": enable_tracing,
            "tools": tools or [],
            "skills": skills or [],
        }
        
        # 包装 process 方法，添加重试和超时
        original_process = getattr(cls, "process", None)
        if original_process:
            @wraps(original_process)
            async def wrapped_process(self, input_data: dict) -> dict:
                last_error = None
                for attempt in range(max_retries):
                    try:
                        async with asyncio.timeout(timeout):
                            return await original_process(self, input_data)
                    except asyncio.TimeoutError as e:
                        last_error = e
                        if attempt < max_retries - 1:
                            delay = 1.0 * (2 ** attempt) if retry_backoff == "exponential" else 1.0
                            await asyncio.sleep(delay)
                    except Exception as e:
                        last_error = e
                        if attempt < max_retries - 1:
                            delay = 1.0 * (2 ** attempt) if retry_backoff == "exponential" else 1.0
                            await asyncio.sleep(delay)
                raise last_error
            
            cls.process = wrapped_process
        
        # 添加类方法
        @classmethod
        async def run(cls, input_data: dict[str, Any]) -> dict[str, Any]:
            """运行 Agent"""
            instance = _get_or_create_instance(cls)
            return await instance.process(input_data)
        
        @classmethod
        async def stream(cls, input_data: dict[str, Any]):
            """流式运行 Agent"""
            instance = _get_or_create_instance(cls)
            if hasattr(instance, "process_stream"):
                async for event in instance.process_stream(input_data):
                    yield event
            else:
                result = await instance.process(input_data)
                yield {"type": "result", "data": result}
        
        cls.run = run
        cls.stream = stream
        
        # 注册到全局
        _register_agent(cls)
        
        return cls
    
    if cls is not None:
        return decorator(cls)
    return decorator


# 实例缓存
_instances: dict[type, Any] = {}

def _get_or_create_instance(cls: type) -> Any:
    """获取或创建 Agent 实例，并注入依赖"""
    if cls not in _instances:
        instance = cls()
        config = cls._agent_config
        
        # 注入 LLM
        from agentflow.providers import get_llm
        instance.llm = get_llm(
            model=config["model"],
            temperature=config["temperature"],
            max_tokens=config["max_tokens"],
        )
        
        # 注入 Memory
        if config["enable_memory"]:
            from agentflow.memory import MemoryManager
            instance.memory = MemoryManager()
        
        # 注入 Tools
        from agentflow.providers.tool_provider import ToolRegistry
        instance.tools = ToolRegistry.get_tools(config["tools"])
        
        # 注入 Context
        instance.context = {}
        
        _instances[cls] = instance
    
    return _instances[cls]


# Agent 注册表
_agent_registry: dict[str, type] = {}

def _register_agent(cls: type) -> None:
    """注册 Agent"""
    name = cls._agent_config["name"]
    _agent_registry[name] = cls
```

### 2. @tool 装饰器（自动注册）

```python
# agentflow/providers/tool_provider_v2.py

from typing import Any, Callable, TypeVar
from functools import wraps

F = TypeVar("F", bound=Callable[..., Any])

# 全局工具注册表
_tool_registry: dict[str, "ToolDefinition"] = {}


class ToolDefinition:
    """工具定义"""
    def __init__(
        self,
        name: str,
        func: Callable,
        description: str,
        parameters: dict[str, Any],
        rate_limit: int | None = None,
        cache_ttl: int | None = None,
    ):
        self.name = name
        self.func = func
        self.description = description
        self.parameters = parameters
        self.rate_limit = rate_limit
        self.cache_ttl = cache_ttl


def tool(
    func: F | None = None,
    *,
    name: str | None = None,
    description: str | None = None,
    rate_limit: int | None = None,    # 每秒最大调用次数
    cache_ttl: int | None = None,      # 缓存时间（秒）
) -> F | Callable[[F], F]:
    """工具装饰器 - 自动注册
    
    Example:
        @tool
        async def web_search(query: str, max_results: int = 5) -> list[dict]:
            '''搜索网络信息'''
            ...
        
        # 任何 Agent 都可以使用
        @agent(tools=["web_search"])
        class MyAgent:
            async def process(self, input_data: dict) -> dict:
                results = await self.tools.call("web_search", query="AI")
                return {"results": results}
    """
    def decorator(func: F) -> F:
        tool_name = name or func.__name__
        tool_desc = description or func.__doc__ or ""
        
        # 从函数签名提取参数信息
        import inspect
        sig = inspect.signature(func)
        parameters = {}
        for param_name, param in sig.parameters.items():
            if param_name == "self":
                continue
            param_type = "string"
            if param.annotation != inspect.Parameter.empty:
                if param.annotation == int:
                    param_type = "integer"
                elif param.annotation == float:
                    param_type = "number"
                elif param.annotation == bool:
                    param_type = "boolean"
                elif param.annotation == list:
                    param_type = "array"
                elif param.annotation == dict:
                    param_type = "object"
            
            parameters[param_name] = {
                "type": param_type,
                "required": param.default == inspect.Parameter.empty,
            }
        
        # 注册到全局
        tool_def = ToolDefinition(
            name=tool_name,
            func=func,
            description=tool_desc,
            parameters=parameters,
            rate_limit=rate_limit,
            cache_ttl=cache_ttl,
        )
        _tool_registry[tool_name] = tool_def
        
        return func
    
    if func is not None:
        return decorator(func)
    return decorator


class ToolRegistry:
    """工具注册表访问器"""
    
    @classmethod
    def get_tools(cls, tool_names: list[str]) -> "ToolCaller":
        """获取指定工具"""
        tools = {}
        for name in tool_names:
            if name in _tool_registry:
                tools[name] = _tool_registry[name]
            elif name == "*":
                tools = _tool_registry.copy()
                break
        return ToolCaller(tools)
    
    @classmethod
    def list_tools(cls) -> list[str]:
        """列出所有工具"""
        return list(_tool_registry.keys())


class ToolCaller:
    """工具调用器"""
    
    def __init__(self, tools: dict[str, ToolDefinition]):
        self._tools = tools
    
    async def call(self, name: str, **kwargs) -> Any:
        """调用工具"""
        if name not in self._tools:
            raise ValueError(f"Tool not found: {name}")
        
        tool_def = self._tools[name]
        return await tool_def.func(**kwargs)
    
    def list(self) -> list[str]:
        """列出可用工具"""
        return list(self._tools.keys())
```

### 3. create_flow（增强版）

```python
# agentflow/quick_v2.py

from typing import Any, Literal
from collections.abc import AsyncIterator


def create_flow(
    agents: list[type] | list[Any],
    *,
    pattern: Literal["sequential", "concurrent", "conditional"] = "sequential",
    name: str | None = None,
    
    # 功能开关
    enable_memory: bool = True,
    enable_streaming: bool = True,
    enable_agui: bool = True,          # AG-UI 协议支持
    
    # 条件路由（pattern="conditional" 时使用）
    conditions: list["Condition"] | None = None,
) -> "Flow":
    """创建 Flow - 编排多 Agent
    
    Example:
        # 顺序执行
        flow = create_flow(
            agents=[CollectorAgent, AnalyzerAgent, ReporterAgent],
            pattern="sequential",
        )
        result = await flow.run({"keywords": ["AI"]})
        
        # 并行执行
        flow = create_flow(
            agents=[Agent1, Agent2, Agent3],
            pattern="concurrent",
        )
        
        # 条件路由
        flow = create_flow(
            agents=[
                GatekeeperAgent,
                when(lambda ctx: ctx.get("is_valid")).then([DaoAgent, FaAgent]),
                ReviewAgent,
            ],
            pattern="conditional",
        )
        
        # 流式执行（SSE）
        async for event in flow.stream({"question": "..."}):
            print(event)
    """
    # 实例化 Agent（如果传入的是类）
    agent_instances = []
    for agent in agents:
        if isinstance(agent, type):
            agent_instances.append(_get_or_create_instance(agent))
        elif isinstance(agent, Condition):
            agent_instances.append(agent)
        else:
            agent_instances.append(agent)
    
    return Flow(
        agents=agent_instances,
        pattern=pattern,
        name=name or f"flow-{len(agents)}agents",
        enable_memory=enable_memory,
        enable_streaming=enable_streaming,
        enable_agui=enable_agui,
    )


class Flow:
    """Flow 执行器"""
    
    def __init__(
        self,
        agents: list[Any],
        pattern: str,
        name: str,
        enable_memory: bool,
        enable_streaming: bool,
        enable_agui: bool,
    ):
        self.agents = agents
        self.pattern = pattern
        self.name = name
        self._enable_memory = enable_memory
        self._enable_streaming = enable_streaming
        self._enable_agui = enable_agui
        
        # 初始化内存
        if enable_memory:
            from agentflow.patterns.multi_agent import SharedContext
            self._context = SharedContext(enable_memory=True)
        else:
            self._context = {}
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """执行 Flow"""
        if self.pattern == "sequential":
            return await self._run_sequential(input_data)
        elif self.pattern == "concurrent":
            return await self._run_concurrent(input_data)
        elif self.pattern == "conditional":
            return await self._run_conditional(input_data)
        else:
            raise ValueError(f"Unknown pattern: {self.pattern}")
    
    async def stream(self, input_data: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """流式执行 Flow（自动发射事件）"""
        yield {"type": "flow.start", "flow": self.name, "data": input_data}
        
        try:
            if self.pattern == "sequential":
                async for event in self._stream_sequential(input_data):
                    yield event
            else:
                result = await self.run(input_data)
                yield {"type": "result", "data": result}
            
            yield {"type": "flow.complete", "flow": self.name}
        except Exception as e:
            yield {"type": "flow.error", "flow": self.name, "error": str(e)}
            raise
    
    async def _run_sequential(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """顺序执行"""
        result = input_data
        for agent in self.agents:
            if isinstance(agent, Condition):
                result = await agent.evaluate_and_run(result, self._context)
            else:
                result = await agent.process(result)
        return result
    
    async def _stream_sequential(self, input_data: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """顺序执行（流式）"""
        result = input_data
        for i, agent in enumerate(self.agents):
            agent_name = getattr(agent, "_agent_config", {}).get("name", agent.__class__.__name__)
            
            # 发射 node.start 事件
            yield {
                "type": "node.start",
                "node": agent_name,
                "index": i,
                "total": len(self.agents),
            }
            
            # 执行 Agent
            if isinstance(agent, Condition):
                result = await agent.evaluate_and_run(result, self._context)
            else:
                result = await agent.process(result)
            
            # 发射 node.complete 事件
            yield {
                "type": "node.complete",
                "node": agent_name,
                "index": i,
                "result": result,
            }
            
            # 发射 progress 事件
            yield {
                "type": "progress",
                "current": i + 1,
                "total": len(self.agents),
                "percentage": (i + 1) / len(self.agents) * 100,
            }
        
        yield {"type": "result", "data": result}
    
    async def _run_concurrent(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """并行执行"""
        import asyncio
        
        tasks = [agent.process(input_data) for agent in self.agents]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        return {
            "results": [r for r in results if not isinstance(r, Exception)],
            "errors": [str(r) for r in results if isinstance(r, Exception)],
        }
    
    async def _run_conditional(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """条件执行"""
        result = input_data
        for agent in self.agents:
            if isinstance(agent, Condition):
                result = await agent.evaluate_and_run(result, self._context)
            else:
                result = await agent.process(result)
        return result


# 条件路由
class Condition:
    """条件分支"""
    
    def __init__(self, predicate: callable):
        self.predicate = predicate
        self._then_agents: list = []
        self._else_agents: list = []
    
    def then(self, agents: list) -> "Condition":
        """条件为真时执行"""
        self._then_agents = agents
        return self
    
    def else_(self, agents: list) -> "Condition":
        """条件为假时执行"""
        self._else_agents = agents
        return self
    
    async def evaluate_and_run(self, input_data: dict, context: Any) -> dict:
        """评估条件并执行"""
        if self.predicate(input_data):
            agents = self._then_agents
        else:
            agents = self._else_agents
        
        result = input_data
        for agent in agents:
            if isinstance(agent, type):
                instance = _get_or_create_instance(agent)
                result = await instance.process(result)
            else:
                result = await agent.process(result)
        return result


def when(predicate: callable) -> Condition:
    """创建条件分支
    
    Example:
        flow = create_flow([
            GatekeeperAgent,
            when(lambda ctx: ctx.get("is_valid")).then([
                DaoAgent,
                FaAgent,
            ]).else_([
                RejectionAgent,
            ]),
            ReviewAgent,
        ])
    """
    return Condition(predicate)
```

---

## 📝 使用示例

### 示例1: 最简单的 Agent

```python
from agentflow import agent

@agent
class HelloAgent:
    """简单的问候 Agent"""
    
    async def process(self, input_data: dict) -> dict:
        name = input_data.get("name", "World")
        response = await self.llm.chat([
            {"role": "user", "content": f"Say hello to {name}"}
        ])
        return {"greeting": response.content}

# 使用
result = await HelloAgent.run({"name": "Alice"})
print(result)  # {"greeting": "Hello, Alice!"}
```

### 示例2: 带工具的 Agent

```python
from agentflow import agent, tool

# 定义工具（自动注册）
@tool
async def calculate(expression: str) -> float:
    """计算数学表达式"""
    return eval(expression)

@tool
async def web_search(query: str) -> list[dict]:
    """搜索网络"""
    # 实现搜索逻辑
    return [{"title": "Result", "url": "..."}]

# 定义 Agent
@agent(tools=["calculate", "web_search"])
class MathAgent:
    """数学助手"""
    
    async def process(self, input_data: dict) -> dict:
        question = input_data["question"]
        
        # 使用工具
        result = await self.tools.call("calculate", expression="2 + 2")
        
        # 使用 LLM
        response = await self.llm.chat([
            {"role": "user", "content": f"Explain: {question}, answer is {result}"}
        ])
        
        return {"answer": result, "explanation": response.content}
```

### 示例3: 多 Agent 协作

```python
from agentflow import agent, create_flow

@agent
class CollectorAgent:
    """数据收集"""
    async def process(self, input_data: dict) -> dict:
        return {"articles": [...]}

@agent
class AnalyzerAgent:
    """数据分析"""
    async def process(self, input_data: dict) -> dict:
        articles = input_data["articles"]
        return {"trends": [...]}

@agent
class ReporterAgent:
    """报告生成"""
    async def process(self, input_data: dict) -> dict:
        trends = input_data["trends"]
        report = await self.llm.chat([
            {"role": "user", "content": f"Generate report for: {trends}"}
        ])
        return {"report": report.content}

# 创建 Flow
flow = create_flow(
    agents=[CollectorAgent, AnalyzerAgent, ReporterAgent],
    pattern="sequential",
    name="market-analysis",
)

# 运行
result = await flow.run({"keywords": ["AI", "ML"]})

# 流式运行（SSE）
async for event in flow.stream({"keywords": ["AI", "ML"]}):
    print(event["type"], event.get("node"))
```

### 示例4: 条件路由

```python
from agentflow import agent, create_flow, when

@agent
class GatekeeperAgent:
    """入口检验"""
    async def process(self, input_data: dict) -> dict:
        question = input_data["question"]
        is_valid = len(question) > 10  # 简单验证
        return {**input_data, "is_valid": is_valid}

@agent
class ProcessAgent:
    """正常处理"""
    async def process(self, input_data: dict) -> dict:
        return {"status": "processed"}

@agent
class RejectAgent:
    """拒绝处理"""
    async def process(self, input_data: dict) -> dict:
        return {"status": "rejected", "reason": "Invalid question"}

# 条件路由
flow = create_flow(
    agents=[
        GatekeeperAgent,
        when(lambda ctx: ctx.get("is_valid")).then([
            ProcessAgent,
        ]).else_([
            RejectAgent,
        ]),
    ],
    pattern="conditional",
)

result = await flow.run({"question": "How to invest?"})
```

---

## 🔄 迁移指南

### 从 AgentBlock 迁移

```python
# Before (AgentBlock)
class MyAgent(AgentBlock):
    def __init__(self):
        super().__init__()
        self._llm = get_llm()
    
    async def run(self, input_data: dict) -> dict:
        response = await self._llm.chat([...])
        return {"result": response}

# After (@agent)
@agent
class MyAgent:
    async def process(self, input_data: dict) -> dict:
        response = await self.llm.chat([...])  # 自动注入
        return {"result": response}
```

### 从 MCPTool 迁移

```python
# Before (MCPTool)
class MyTool(MCPTool):
    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        # ...
        return MCPToolResponse(success=True, output={...})

client = MCPClient()
client.register_tool("my_tool", MyTool())
response = await client.call_tool_by_name("my_tool", {...})

# After (@tool)
@tool
async def my_tool(arg: str) -> dict:
    return {...}

# 自动注册，任何 Agent 都可使用
@agent(tools=["my_tool"])
class MyAgent:
    async def process(self, input_data: dict) -> dict:
        result = await self.tools.call("my_tool", arg="...")
        return result
```

---

## 📊 对比总结

| 方面 | 当前版本 | v2.0 |
|------|----------|------|
| Agent 定义代码行 | 50-100 | 10-20 |
| 工具注册步骤 | 3 步 | 0 步（自动） |
| 重试/超时 | 手动实现 | 框架默认 |
| LLM 初始化 | 手动 | 自动注入 |
| 事件发射 | 手动 | 框架自动 |
| 入口数量 | 6 种 | 2 种 |
| 学习曲线 | 高 | 低 |

---

## 🎯 实施计划

### Phase 1: 核心实现（2 周）
- [ ] 增强 @agent 装饰器（重试、超时、自动注入）
- [ ] 增强 @tool 装饰器（自动注册）
- [ ] 增强 create_flow（条件路由、自动事件）

### Phase 2: 迁移支持（1 周）
- [ ] 向后兼容层
- [ ] 迁移指南文档
- [ ] 自动迁移工具

### Phase 3: 文档和测试（1 周）
- [ ] 更新文档
- [ ] 添加测试用例
- [ ] 示例应用迁移

---

## 📚 参考

- [LangGraph](https://github.com/langchain-ai/langgraph) - 图结构编排
- [CrewAI](https://github.com/joaomdmoura/crewAI) - 简洁的 Agent API
- [AutoGen](https://github.com/microsoft/autogen) - 多 Agent 对话

