# AgentFlow 框架设计愿景与改进路线

> **版本**: 2.0.0
> **日期**: 2026-01-15
> **状态**: 活跃演进中

---

## 1. 框架定位与核心理念

### 1.1 定位

AgentFlow 是一个**轻量级、高可靠、垂直深耕**的 AI Agent 框架：

- **不是**通用的 Manus 替代品
- **而是**面向特定垂直领域的高可靠 Agent 系统
- **目标**：在3个垂直产品上做到"95%自动完成 + 几乎不出错"

### 1.2 设计哲学

| 原则 | 说明 | 优先级 |
|------|------|--------|
| **简洁优先** | 核心代码保持~500行，学习成本最小 | ⭐⭐⭐⭐⭐ |
| **高可靠性** | 出错率极低，错了能自愈或快速告警 | ⭐⭐⭐⭐⭐ |
| **垂直深耕** | 不求通用，求特定场景的极致 | ⭐⭐⭐⭐⭐ |
| **接口标准化** | 对齐知名框架接口，便于将来替换 | ⭐⭐⭐⭐ |
| **高内聚低耦合** | 模块清晰可替换，依赖注入为主 | ⭐⭐⭐⭐ |
| **自研为主** | 核心自研但接口对齐业界标准 | ⭐⭐⭐⭐ |

### 1.3 与Manus的关系

```
Manus: 通用跨领域超级Agent平台（追求广度）
       ↓
AgentFlow: 特定垂直领域高可靠Agent（追求深度+可靠性）
       = Manus在某类场景的"进化版"：更快、更稳、更专注
```

---

## 2. 架构设计原则

### 2.1 分层架构（对齐Manus思想）

```
┌─────────────────────────────────────────────────────────────────┐
│  L1 📱 应用层          3个垂直产品（薄App层，只写业务）          │
├─────────────────────────────────────────────────────────────────┤
│  L2 🤖 编排层          Planner + Executor + Monitor              │
│                        (参考Manus多Agent编排思想)                 │
├─────────────────────────────────────────────────────────────────┤
│  L3 🔧 执行层          CodeAct + Sandbox + ToolExecutor          │
│                        (代码即工具，Python生态全利用)             │
├─────────────────────────────────────────────────────────────────┤
│  L4 🔒 可靠性层        强约束 + 多重校验 + 回滚重试              │
│                        (Manus缺失，我们的核心竞争力)              │
├─────────────────────────────────────────────────────────────────┤
│  L5 🧠 记忆层          3层记忆 + 上下文压缩 + 长期学习           │
├─────────────────────────────────────────────────────────────────┤
│  L6 🔌 Provider层      LLM/DB/Vector/Embedding (黑盒设计)        │
├─────────────────────────────────────────────────────────────────┤
│  L7 🌐 协议层          MCP/A2A/AG-UI/A2UI (四协议统一)           │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 核心设计模式

| 模式 | 用途 | 接口参考 |
|------|------|----------|
| Registry Pattern | 统一组件注册/发现 | 自研（标准化） |
| Provider Pattern | 资源抽象（黑盒） | LangChain/LlamaIndex |
| Coordinator Pattern | 多Agent协调 | LangGraph/AutoGen |
| CodeAct Pattern | 代码即工具 | Manus/LangGraph-CodeAct |

---

## 3. 对标Manus的关键改进

### 3.1 从Manus学习的优点

| Manus特性 | AgentFlow现状 | 改进方向 |
|-----------|--------------|----------|
| **多代理分工清晰** | DeepAgentCoordinator有但不够 | 强化Planner/Executor/Monitor分离 |
| **计划驱动Flow** | 有PlanExecutor | 增强动态重计划能力 |
| **CodeAct架构** | 有Sandbox但弱 | 深化代码工具组合能力 |
| **长上下文管理** | 有ContextCompressor | 增强分层记忆和自动要约 |
| **工具标准化** | 有@tool和MCP | 增强ToolResult统一和Fallback |

### 3.2 我们的差异化竞争力（Manus缺失的）

| 特性 | 说明 | 状态 |
|------|------|------|
| **强约束执行** | 限制Agent自由度，只开放安全步骤 | 待实现 |
| **多重校验** | 每步双重验证，审核Agent复核 | 待实现 |
| **回滚重试** | 操作日志+版本号，可回滚到N步前 | 部分实现 |
| **垂直场景优化** | 针对3个产品深度优化 | 进行中 |

---

## 4. 3个垂直产品定位

| 产品 | 领域 | 核心价值 | 可靠性目标 |
|------|------|----------|------------|
| Decision Governance Engine | 企业决策 | 道法术器方法论 | >95% |
| Market Trend Monitor | 市场分析 | 实时监控+洞察 | >90% |
| Code Migration Assistant | 代码迁移 | 自动化迁移 | >95% |

---

## 5. 接口标准化策略

### 5.1 原则

```
自研实现 → 标准接口 → 将来可替换
```

### 5.2 接口对齐表

| 模块 | 接口参考 | 当前实现 | 可替换为 |
|------|----------|----------|----------|
| VectorStore | LlamaIndex/LangChain | InMemoryVectorStore | Qdrant/Pinecone |
| ToolExecutor | OpenAI Function Calling | 自研ToolExecutor | LangChain Tool |
| Checkpointer | LangGraph | MemoryCheckpointer | Redis/Postgres |
| ErrorResponse | RFC 7807 | 自研 | Sentry |
| WebSocket | FastAPI WebSocket | 自研Manager | Socket.IO |

---

## 6. 改进路线图

### Phase 1: 可靠性基础（本周）

#### P1-1: ConstraintValidator（强约束验证器）
```python
# 新增模块: agentflow/reliability/constraint_validator.py
class ConstraintValidator:
    """限制Agent自由度，只开放安全步骤."""

    def validate_input(self, schema: JsonSchema, data: dict) -> ValidationResult
    def validate_output(self, schema: JsonSchema, data: dict) -> ValidationResult
    def validate_tool_call(self, allowed_tools: list[str], call: ToolCall) -> bool
```

#### P1-2: DualVerifier（双重校验Agent）
```python
# 新增模块: agentflow/reliability/dual_verifier.py
class DualVerifier:
    """对关键结果进行双重验证."""

    async def verify(self, result: dict, context: dict) -> VerifyResult
    async def cross_validate(self, agent1_result: dict, agent2_result: dict) -> bool
```

#### P1-3: RollbackableCheckpointer（可回滚检查点）
```python
# 增强: agentflow/hitl/checkpointer.py
class RollbackableCheckpointer(Checkpointer):
    """支持回滚到任意历史检查点."""

    async def create_savepoint(self, label: str) -> str
    async def rollback_to(self, checkpoint_id: str) -> CheckpointData
    async def list_savepoints(self) -> list[SavepointInfo]
```

### Phase 2: 编排层强化（下周）

#### P2-1: PlanningFlow重构
```python
# 增强: agentflow/patterns/deep_agent.py
class PlanningFlow:
    """计划驱动的执行流（Manus思想）."""

    # 现有功能增强
    async def create_plan(self, goal: str) -> Plan
    async def execute_step(self, step: PlanStep) -> StepResult

    # 新增：动态重计划
    async def replan(self, failed_step: PlanStep, error: Exception) -> Plan
    async def insert_recovery_steps(self, plan: Plan, recovery: list[PlanStep]) -> Plan
```

#### P2-2: Monitor层增强
```python
# 新增模块: agentflow/patterns/monitor.py
class ExecutionMonitor:
    """监控执行进度，检测异常，触发恢复."""

    async def watch(self, execution_id: str) -> AsyncIterator[MonitorEvent]
    async def detect_stuck(self, timeout: float) -> bool
    async def trigger_recovery(self, failure_info: FailureInfo) -> RecoveryAction
```

### Phase 3: CodeAct增强（第3周）

#### P3-1: CodeActAgent增强
```python
# 增强: agentflow/sandbox/code_act_agent.py
class CodeActAgent:
    """代码即工具，支持组合调用."""

    # 现有功能
    async def execute_code(self, code: str) -> ExecutionResult

    # 新增：工具链式调用
    async def execute_chain(self, tool_chain: list[ToolCall]) -> ChainResult
    async def execute_with_retry(self, code: str, max_retries: int) -> ExecutionResult
    async def execute_conditional(self, code: str, condition: str) -> ExecutionResult
```

#### P3-2: ToolChain支持
```python
# 新增模块: agentflow/providers/tool_chain.py
class ToolChain:
    """工具链式调用，支持管道和条件."""

    def pipe(self, tool1: str, tool2: str) -> ToolChain
    def when(self, condition: Callable) -> ToolChain
    def retry(self, max_attempts: int) -> ToolChain
    async def execute(self) -> ChainResult
```

---

## 7. 已实现 vs 待实现对照表

| 模块 | Manus对应 | 当前状态 | 改进优先级 |
|------|-----------|----------|------------|
| **Planner Agent** | ✅ | ⚠️ DeepAgentCoordinator有 | P2 |
| **Executor Agent** | ✅ | ⚠️ DynamicAgent有 | P2 |
| **Monitor** | ✅ | ❌ 缺失 | P2 |
| **CodeAct** | ✅ | ⚠️ Sandbox有但弱 | P3 |
| **强约束验证** | ❌ | ❌ 缺失（我们特色） | P1 |
| **双重校验** | ❌ | ❌ 缺失（我们特色） | P1 |
| **可回滚检查点** | ⚠️ | ⚠️ 基础有 | P1 |
| **工具链调用** | ✅ | ⚠️ ToolExecutor有 | P3 |
| **长上下文压缩** | ✅ | ✅ ContextCompressor | - |
| **失败学习** | ⚠️ | ✅ ReflectiveEvolver | - |
| **3层记忆** | ⚠️ | ✅ MemoryManager | - |

---

## 8. 代码规范要求

### 8.1 新增模块要求
- UTF-8编码，BOM无
- 日语或中文详细注释
- Pydantic模型定义输入输出
- 完整类型注解
- 单元测试覆盖

### 8.2 接口设计原则
- 对齐业界标准接口（便于替换）
- 支持依赖注入
- 支持异步操作
- 返回结果包含成功/失败状态

---

*最后更新: 2026-01-15*

