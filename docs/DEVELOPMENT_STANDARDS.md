# AgentFlow 开发规范

> **版本**: 1.0.0
> **最后更新**: 2025-01-15
> **适用范围**: AgentFlow Framework 所有代码

**其他语言**: [English](DEVELOPMENT_STANDARDS_EN.md) | [日本語](DEVELOPMENT_STANDARDS_JA.md)

---

## 📋 目录

1. [核心原则](#核心原则)
2. [代码质量标准](#代码质量标准)
3. [架构设计原则](#架构设计原则)
4. [类型系统规范](#类型系统规范)
5. [测试标准](#测试标准)
6. [文档规范](#文档规范)
7. [错误处理](#错误处理)
8. [AI 常犯错误清单](#ai-常犯错误清单)
9. [工具配置](#工具配置)
10. [检查清单](#检查清单)

---

## 核心原则

### 1. 零容忍政策

- ❌ **禁止**: 静态分析错误（mypy, ruff）
- ❌ **禁止**: 未处理的警告
- ❌ **禁止**: `Any` 类型（除非有充分理由并注释）
- ❌ **禁止**: `type: ignore`（除非有充分理由并注释）
- ❌ **禁止**: 硬编码配置、密钥、路径
- ❌ **禁止**: `print()` 调试语句（使用 logging）
- ❌ **禁止**: 可变默认参数 `def foo(x=[])`

### 2. 质量优先

- ✅ **类型覆盖率**: 100%
- ✅ **测试覆盖率**: ≥ 80%
- ✅ **文档覆盖率**: 所有公共 API
- ✅ **代码审查**: 所有 PR 必须经过审查

---

## 代码质量标准

### 1. Python 版本

```python
# 最低版本: Python 3.13+
# 原因: AG-UI 协议要求 + 性能优势
requires-python = ">=3.13"
```

### 2. 代码风格

使用 **Ruff** 作为统一的 linter 和 formatter：

```toml
[tool.ruff]
line-length = 100
target-version = "py313"

[tool.ruff.lint]
select = ["E", "W", "F", "I", "N", "UP", "ANN", "B", "A", "C4", "DTZ",
          "T10", "EM", "ISC", "ICN", "PIE", "PT", "Q", "RSE", "RET",
          "SIM", "TID", "TCH", "ARG", "PTH", "ERA", "PL", "TRY", "RUF"]
ignore = ["ANN101", "ANN102"]
```

### 3. 命名规范

```python
# 模块名: 小写+下划线
# agent_flow.py ✅  |  AgentFlow.py ❌

# 类名: PascalCase
class AgentFlowEngine: ...  # ✅
class agent_flow_engine: ...  # ❌

# 函数/变量: snake_case
def create_agent() -> Agent: ...  # ✅
def createAgent() -> Agent: ...  # ❌

# 常量: UPPER_SNAKE_CASE
MAX_RETRIES = 3  # ✅
maxRetries = 3  # ❌

# 私有成员: 单下划线前缀
class Agent:
    def __init__(self) -> None:
        self._internal_state: dict[str, Any] = {}  # ✅
```

---

## 架构设计原则

### 1. SOLID 原则

#### S - 单一职责原则（Single Responsibility）

```python
# ❌ 错误: 一个类做太多事
class Agent:
    def execute(self) -> None: ...
    def save_to_db(self) -> None: ...
    def send_email(self) -> None: ...

# ✅ 正确: 职责分离
class Agent:
    def execute(self) -> None: ...

class AgentRepository:
    def save(self, agent: Agent) -> None: ...

class NotificationService:
    def send_email(self, to: str, content: str) -> None: ...
```

#### O - 开闭原则（Open/Closed）

```python
# ✅ 使用协议和抽象基类
from typing import Protocol

class ProtocolAdapter(Protocol):
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]: ...
```

#### L - 里氏替换原则（Liskov Substitution）

```python
# ✅ 子类可以替换父类
class BaseAdapter:
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]:
        raise NotImplementedError

class MCPAdapter(BaseAdapter):
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]:
        return {"status": "ok"}
```

#### I - 接口隔离原则（Interface Segregation)

```python
# ✅ 小而专注的接口
class Executable(Protocol):
    def execute(self) -> None: ...

class Validatable(Protocol):
    def validate(self) -> bool: ...
```

#### D - 依赖倒置原则（Dependency Inversion）

```python
# ✅ 依赖抽象而非具体实现
from typing import Protocol

class Storage(Protocol):
    async def save(self, key: str, value: str) -> None: ...
    async def load(self, key: str) -> str: ...

class AgentManager:
    def __init__(self, storage: Storage) -> None:
        self._storage = storage
```

### 2. 依赖注入

```python
# ✅ 构造函数注入
class AgentFlowEngine:
    def __init__(
        self,
        workflow_engine: WorkflowEngine,
        protocol_adapters: dict[str, ProtocolAdapter],
        logger: logging.Logger | None = None,
    ) -> None:
        self._workflow = workflow_engine
        self._adapters = protocol_adapters
        self._logger = logger or logging.getLogger(__name__)
```

### 3. 异步优先

```python
# ✅ 所有 I/O 操作使用 async/await
async def load_agent(agent_id: str) -> Agent:
    async with aiofiles.open(f"agents/{agent_id}.yaml") as f:
        content = await f.read()
    return Agent.from_yaml(content)

# ❌ 避免阻塞调用
def load_agent_sync(agent_id: str) -> Agent:
    with open(f"agents/{agent_id}.yaml") as f:  # 阻塞!
        content = f.read()
    return Agent.from_yaml(content)
```

---

## 类型系统规范

### 1. 类型注解 100% 覆盖

```python
# ✅ 完整的类型注解
from typing import Any
from collections.abc import Callable, Awaitable

async def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
    hooks: list[Callable[[str], Awaitable[None]]] | None = None,
) -> dict[str, Any]:
    ...

# ❌ 缺少类型注解
async def execute_workflow(workflow_id, inputs, hooks=None):
    ...
```

### 2. 使用 Pydantic 进行数据验证

```python
from pydantic import BaseModel, Field, field_validator

class AgentMetadata(BaseModel):
    """Agent 元数据模型"""

    name: str = Field(..., min_length=1, max_length=100)
    version: str = Field(..., pattern=r"^\d+\.\d+\.\d+$")
    protocols: list[str] = Field(default_factory=list)

    @field_validator("protocols")
    @classmethod
    def validate_protocols(cls, v: list[str]) -> list[str]:
        valid = {"mcp", "a2a", "ag-ui"}
        if invalid := set(v) - valid:
            raise ValueError(f"Invalid protocols: {invalid}")
        return v
```

### 3. 泛型使用

```python
from typing import TypeVar, Generic

T = TypeVar("T")
R = TypeVar("R")

class AsyncProcessor(Generic[T, R]):
    async def process(self, input_data: T) -> R:
        raise NotImplementedError
```

---

## 测试标准

### 1. 测试覆盖率要求

- **单元测试**: ≥ 80% 代码覆盖率
- **集成测试**: 覆盖所有核心流程
- **端到端测试**: 覆盖关键用户场景

### 2. 测试结构

```python
import pytest
from agentflow.core.engine import AgentFlowEngine

class TestAgentFlowEngine:
    """AgentFlowEngine 单元测试"""

    @pytest.fixture
    def engine(self) -> AgentFlowEngine:
        return AgentFlowEngine()

    async def test_execute_workflow_success(
        self,
        engine: AgentFlowEngine,
    ) -> None:
        """测试工作流执行成功场景"""
        result = await engine.execute(workflow_id="test", inputs={})
        assert result["status"] == "success"
```

### 3. Mock 外部依赖

```python
from unittest.mock import AsyncMock

async def test_agent_with_mocked_storage() -> None:
    mock_storage = AsyncMock()
    mock_storage.load.return_value = '{"name": "test"}'

    agent = Agent(storage=mock_storage)
    await agent.load("test_id")

    mock_storage.load.assert_called_once_with("test_id")
```

---

## 文档规范

### Docstring 格式（Google Style）

```python
async def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
    *,
    timeout: float = 30.0,
) -> dict[str, Any]:
    """执行指定的工作流。

    Args:
        workflow_id: 工作流唯一标识符
        inputs: 工作流输入参数字典
        timeout: 执行超时时间（秒），默认 30 秒

    Returns:
        包含执行结果的字典

    Raises:
        ValueError: 当 workflow_id 为空时
        TimeoutError: 当执行超时时

    Example:
        >>> result = await execute_workflow("my-workflow", {"input": "test"})
        >>> print(result["status"])
        success
    """
    ...
```

---

## 错误处理

### 1. 自定义异常层次

```python
class AgentFlowError(Exception):
    """AgentFlow 基础异常"""

class WorkflowError(AgentFlowError):
    """工作流相关异常"""

class WorkflowNotFoundError(WorkflowError):
    """工作流未找到"""
```

### 2. 最佳实践

```python
# ✅ 正确: 捕获具体异常
try:
    result = await execute_workflow(workflow_id)
except WorkflowNotFoundError:
    logger.error(f"Workflow not found: {workflow_id}")
    raise
except TimeoutError:
    logger.warning(f"Workflow timeout: {workflow_id}")
    return {"status": "timeout"}

# ❌ 错误: 捕获所有异常
try:
    result = await execute_workflow(workflow_id)
except Exception:  # 太宽泛!
    pass
```

---

## AI 常犯错误清单

### ❌ 错误 1: 忘记 async/await

```python
# ❌ 错误
def load_data():
    return aiofiles.open("data.txt")  # 返回协程对象!

# ✅ 正确
async def load_data() -> str:
    async with aiofiles.open("data.txt") as f:
        return await f.read()
```

### ❌ 错误 2: 可变默认参数

```python
# ❌ 错误
def add_item(item: str, items: list[str] = []) -> list[str]:
    items.append(item)  # 所有调用共享同一个列表!
    return items

# ✅ 正确
def add_item(item: str, items: list[str] | None = None) -> list[str]:
    if items is None:
        items = []
    items.append(item)
    return items
```

### ❌ 错误 3: 忘记关闭资源

```python
# ❌ 错误
async def process_file(path: str) -> str:
    f = await aiofiles.open(path)
    content = await f.read()
    return content  # 文件未关闭!

# ✅ 正确
async def process_file(path: str) -> str:
    async with aiofiles.open(path) as f:
        return await f.read()
```

### ❌ 错误 4: 循环导入

```python
# ✅ 正确: 使用 TYPE_CHECKING
from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from b import B

class A:
    def use_b(self, b: "B") -> None: ...
```

### ❌ 错误 5: 类型注解不完整

```python
# ❌ 错误
def process(data):  # 缺少类型
    return data

# ✅ 正确
def process(data: dict[str, Any]) -> dict[str, Any]:
    return data
```

### ❌ 错误 6: 日文全角括号和符号

```python
# ❌ 错误 - 使用全角括号
"""エージェントを実行（サブクラスで実装必須）."""
# コメント（テスト用）

# ✅ 正确 - 使用半角括号
"""エージェントを実行 (サブクラスで実装必須)."""
# コメント (テスト用)
```

### ❌ 错误 7: 异常消息使用字符串字面量

```python
# ❌ 错误
raise ValueError(f"Invalid value: {value}")
raise ValueError("Missing required field")

# ✅ 正确
msg = f"Invalid value: {value}"
raise ValueError(msg)

msg = "Missing required field"
raise ValueError(msg)
```

### ❌ 错误 8: 魔术数字未定义为常量

```python
# ❌ 错误
if len(text) > 50:
    text = text[:50] + "..."

# ✅ 正确
MAX_DESCRIPTION_LENGTH = 50
if len(text) > MAX_DESCRIPTION_LENGTH:
    text = text[:MAX_DESCRIPTION_LENGTH] + "..."
```

### ❌ 错误 9: 可变类属性未使用 ClassVar

```python
# ❌ 错误
class MyClass:
    OPERATORS = {"+": add, "-": sub}  # 可变类属性

# ✅ 正确
from typing import ClassVar

class MyClass:
    OPERATORS: ClassVar[dict[str, Callable]] = {"+": add, "-": sub}
```

### ❌ 错误 10: 函数缺少返回类型注解

```python
# ❌ 错误
def main():
    """メイン関数."""
    pass

# ✅ 正确
def main() -> None:
    """メイン関数."""
    pass
```

### ❌ 错误 11: 未使用的函数参数

```python
# ❌ 错误
def process(data: str, unused_param: int) -> str:
    return data.upper()

# ✅ 正确 - 使用下划线前缀
def process(data: str, _unused_param: int) -> str:
    return data.upper()

# 或者删除未使用的参数
def process(data: str) -> str:
    return data.upper()
```

### ❌ 错误 12: datetime.now() 未指定时区

```python
# ❌ 错误
from datetime import datetime
now = datetime.now()

# ✅ 正确
from datetime import datetime, timezone
now = datetime.now(timezone.utc)
```

---

## 工具配置

所有工具配置集中在 `pyproject.toml` 中。

---

## 检查清单

在提交代码前，确保：

- [ ] `ruff check .` 无错误
- [ ] `ruff format .` 已格式化
- [ ] `mypy .` 无类型错误
- [ ] `pytest` 所有测试通过
- [ ] 测试覆盖率 ≥ 80%
- [ ] 所有公共 API 有 docstring
- [ ] 无 `print()` 调试语句
- [ ] 无硬编码配置
- [ ] 异常处理完整
- [ ] 资源正确关闭

---

**严格遵守本规范，确保代码质量！**
