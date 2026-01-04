# AgentFlow 框架重构计划

> 将应用层重复的框架职责代码移动到 agentflow 核心

## ✅ 实施状态: 已完成

### 已完成的改动

1. **新建文件**
   - `agentflow/core/resilient_agent.py` - ResilientAgent 基类（重试/超时/LLM）
   - `agentflow/protocols/mcp_tool.py` - MCPTool, MCPToolClient, MCPToolRequest, MCPToolResponse

2. **修改文件**
   - `agentflow/core/exceptions.py` - 添加 AgentExecutionError, AgentTimeoutError, AgentRetryExhaustedError
   - `agentflow/core/__init__.py` - 导出新类
   - `agentflow/protocols/__init__.py` - 导出 MCP Tool 类
   - `agentflow/__init__.py` - 更新版本到 0.3.0，导出新 API

3. **应用层更新（后向兼容）**
   - `apps/decision_governance_engine/agents/base_agent.py` - 改为 re-export 框架类
   - `apps/code_migration_assistant/mcp_tools/base.py` - 改为 re-export 框架类
   - `apps/code_migration_assistant/mcp_client.py` - 改为 re-export 框架类

4. **新测试文件**
   - `tests/unit/test_resilient_agent.py` - 13 个测试
   - `tests/unit/test_mcp_tool.py` - 19 个测试

---

---

## 📋 需要移动的代码清单

### 1. decision_governance_engine/agents/base_agent.py → agentflow/core/

| 组件 | 描述 | 目标位置 |
|------|------|----------|
| `AgentExecutionError` | Agent执行异常 | `agentflow/core/exceptions.py` |
| `AgentTimeoutError` | Agent超时异常 | `agentflow/core/exceptions.py` |
| `AgentRetryExhaustedError` | 重试耗尽异常 | `agentflow/core/exceptions.py` |
| `BaseDecisionAgent` | 带重试/超时/LLM的基类 | **新建** `agentflow/core/resilient_agent.py` |

**功能特性:**
- Pydantic 输入/输出类型安全
- 自动重试（可配置次数和延迟）
- 超时控制（asyncio.timeout）
- LLM 客户端自动注入
- Skills/Prompt 加载

### 2. decision_governance_engine/agents/decorators.py → agentflow/agent_decorator.py

| 组件 | 描述 | 处理方式 |
|------|------|----------|
| `@decision_agent` | 领域特定装饰器 | 合并到 `@agent`，添加 `domain` 参数 |
| `DecisionAgentConfig` | 配置类 | 合并到 `RegisteredAgent` |

### 3. code_migration_assistant/mcp_tools/base.py → agentflow/protocols/mcp_tool.py

| 组件 | 描述 | 目标位置 |
|------|------|----------|
| `MCPToolRequest` | MCP工具请求 | `agentflow/protocols/mcp_tool.py` |
| `MCPToolResponse` | MCP工具响应 | `agentflow/protocols/mcp_tool.py` |
| `MCPTool` | MCP工具基类 | `agentflow/protocols/mcp_tool.py` |

### 4. code_migration_assistant/mcp_client.py → agentflow/protocols/mcp_tool.py

| 组件 | 描述 | 目标位置 |
|------|------|----------|
| `MCPClient` | MCP客户端 | `agentflow/protocols/mcp_tool.py` |

---

## 🏗️ 新架构设计

### agentflow/core/resilient_agent.py（新文件）

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         ResilientAgent                                   │
│  ┌─────────────────────────────────────────────────────────────────────┐│
│  │  继承: AgentBlock                                                   ││
│  │  特性:                                                              ││
│  │  ├── 自动重试（RetryConfig）                                        ││
│  │  ├── 超时控制（timeout_seconds）                                    ││
│  │  ├── LLM 自动注入（get_llm()）                                      ││
│  │  ├── Pydantic 类型安全                                              ││
│  │  └── Skills/Prompt 加载                                             ││
│  └─────────────────────────────────────────────────────────────────────┘│
│                                                                         │
│  用法:                                                                  │
│  class MyAgent(ResilientAgent[InputModel, OutputModel]):                │
│      name = "MyAgent"                                                   │
│      timeout_seconds = 30                                               │
│      max_retries = 3                                                    │
│                                                                         │
│      async def process(self, input_data: InputModel) -> OutputModel:    │
│          response = await self.llm.chat([...])                          │
│          return OutputModel(...)                                        │
└─────────────────────────────────────────────────────────────────────────┘
```

### agentflow/protocols/mcp_tool.py（整合）

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         MCP Tool System                                  │
│  ┌─────────────────────────────────────────────────────────────────────┐│
│  │  MCPToolRequest / MCPToolResponse  - Pydantic 模型                  ││
│  │  MCPTool(ABC)                      - 工具基类                       ││
│  │  MCPToolClient                     - 工具客户端（改名自 MCPClient） ││
│  │  @mcp_tool                         - 装饰器（新增）                  ││
│  └─────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────┘
```

### @agent 装饰器增强

```python
@agent(
    name="MyAgent",
    # 新增: 健壮性配置（默认值）
    timeout=30,              # 超时秒数
    max_retries=3,           # 最大重试次数
    retry_delay=1.0,         # 重试间隔
    retry_backoff="exponential",  # 退避策略
    
    # 新增: LLM 配置
    model=None,              # 默认从环境变量
    temperature=0.7,
    max_tokens=2000,
    
    # 新增: 领域配置
    domain=None,             # "decision", "migration", etc.
    prompts_dir=None,        # 自定义 prompts 目录
    skills_dir=None,         # 自定义 skills 目录
)
class MyAgent:
    ...
```

---

## 📁 文件变更清单

### 新建文件

| 文件 | 描述 |
|------|------|
| `agentflow/core/resilient_agent.py` | 健壮Agent基类（重试/超时/LLM） |
| `agentflow/protocols/mcp_tool.py` | MCP工具基类和客户端 |

### 修改文件

| 文件 | 变更 |
|------|------|
| `agentflow/core/exceptions.py` | 添加 Agent 相关异常 |
| `agentflow/core/__init__.py` | 导出新类 |
| `agentflow/agent_decorator.py` | 增强 @agent 装饰器 |
| `agentflow/__init__.py` | 导出新 API |

### 应用层修改

| 文件 | 变更 |
|------|------|
| `apps/decision_governance_engine/agents/base_agent.py` | 改为导入框架类 |
| `apps/decision_governance_engine/agents/*.py` | 更新导入 |
| `apps/code_migration_assistant/mcp_tools/base.py` | 改为导入框架类 |
| `apps/code_migration_assistant/mcp_client.py` | 改为导入框架类 |
| `apps/market_trend_monitor/backend/agents/*.py` | 可选：使用 ResilientAgent |

---

## 🔄 CLI/Studio/STDIO 影响分析

### CLI (agentflow/cli/main.py)
- ✅ 已支持 `@agent` 装饰器
- ✅ 已支持 `create_flow` 流式执行
- ⚠️ 需要: 添加对 `ResilientAgent` 的识别

### Studio (agentflow/studio/)
- ✅ 已支持 `@agent` 和 `agent.yaml`
- ⚠️ 需要: API 支持 ResilientAgent 的配置展示

### STDIO (如果存在)
- 需要检查并确保兼容

---

## 📐 迁移步骤

### Phase 1: 框架层实现

1. 创建 `agentflow/core/resilient_agent.py`
2. 更新 `agentflow/core/exceptions.py`
3. 创建 `agentflow/protocols/mcp_tool.py`
4. 更新 `agentflow/__init__.py` 导出

### Phase 2: 应用层迁移

1. `decision_governance_engine/agents/base_agent.py` 改为 re-export
2. 更新所有 Agent 导入
3. `code_migration_assistant/mcp_tools/base.py` 改为 re-export
4. 更新 MCPClient 导入

### Phase 3: 测试验证

1. 运行现有测试
2. 验证 CLI 命令
3. 验证 Studio API
4. 验证三个应用正常工作

---

## ⚠️ 向后兼容策略

```python
# apps/decision_governance_engine/agents/base_agent.py
# 向后兼容 - 从框架导入并 re-export
from agentflow.core.resilient_agent import (
    ResilientAgent as BaseDecisionAgent,  # 别名
    AgentExecutionError,
    AgentTimeoutError,
    AgentRetryExhaustedError,
)

# 保持原有导入路径可用
__all__ = [
    "BaseDecisionAgent",
    "AgentExecutionError",
    "AgentTimeoutError",
    "AgentRetryExhaustedError",
]
```

---

## 🧪 测试计划

1. **单元测试**
   - `ResilientAgent` 重试逻辑
   - `ResilientAgent` 超时逻辑
   - `MCPTool` 基类
   - `MCPToolClient` 客户端

2. **集成测试**
   - `decision_governance_engine` 完整流程
   - `code_migration_assistant` 完整流程
   - `market_trend_monitor` 完整流程

3. **CLI 测试**
   - `agentflow run` 命令
   - `agentflow studio` 命令
   - `agentflow chat` 命令

