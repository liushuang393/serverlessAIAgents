# AgentFlow Development Standards

> **Version**: 1.0.0  
> **Last Updated**: 2025-01-15  
> **Scope**: All AgentFlow Framework code

**Other Languages**: [日本語](DEVELOPMENT_STANDARDS_JA.md) | [中文](DEVELOPMENT_STANDARDS.md)

---

## 📋 Table of Contents

1. [Core Principles](#core-principles)
2. [Code Quality Standards](#code-quality-standards)
3. [Architecture Design Principles](#architecture-design-principles)
4. [Type System Specifications](#type-system-specifications)
5. [Testing Standards](#testing-standards)
6. [Documentation Standards](#documentation-standards)
7. [Error Handling](#error-handling)
8. [Common AI Mistakes Checklist](#common-ai-mistakes-checklist)
9. [Tool Configuration](#tool-configuration)
10. [Checklist](#checklist)

---

## Core Principles

### 1. Zero Tolerance Policy

- ❌ **FORBIDDEN**: Static analysis errors (mypy, ruff)
- ❌ **FORBIDDEN**: Unhandled warnings
- ❌ **FORBIDDEN**: `Any` type (unless justified with comments)
- ❌ **FORBIDDEN**: `type: ignore` (unless justified with comments)
- ❌ **FORBIDDEN**: Hardcoded configuration, secrets, paths
- ❌ **FORBIDDEN**: `print()` debug statements (use logging)
- ❌ **FORBIDDEN**: Mutable default arguments `def foo(x=[])`

### 2. Quality First

- ✅ **Type Coverage**: 100%
- ✅ **Test Coverage**: ≥ 80%
- ✅ **Documentation Coverage**: All public APIs
- ✅ **Code Review**: All PRs must be reviewed

---

## Code Quality Standards

### 1. Python Version

```python
# Minimum version: Python 3.13+
# Reason: AG-UI protocol requirement + performance advantages
requires-python = ">=3.13"
```

### 2. Code Style

Use **Ruff** as unified linter and formatter:

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

### 3. Naming Conventions

```python
# Module names: lowercase + underscore
# agent_flow.py ✅  |  AgentFlow.py ❌

# Class names: PascalCase
class AgentFlowEngine: ...  # ✅
class agent_flow_engine: ...  # ❌

# Functions/Variables: snake_case
def create_agent() -> Agent: ...  # ✅
def createAgent() -> Agent: ...  # ❌

# Constants: UPPER_SNAKE_CASE
MAX_RETRIES = 3  # ✅
maxRetries = 3  # ❌

# Private members: single underscore prefix
class Agent:
    def __init__(self) -> None:
        self._internal_state: dict[str, Any] = {}  # ✅
```

---

## Architecture Design Principles

### 1. SOLID Principles

#### S - Single Responsibility Principle

```python
# ❌ Wrong: One class does too much
class Agent:
    def execute(self) -> None: ...
    def save_to_db(self) -> None: ...
    def send_email(self) -> None: ...

# ✅ Correct: Separation of concerns
class Agent:
    def execute(self) -> None: ...

class AgentRepository:
    def save(self, agent: Agent) -> None: ...

class NotificationService:
    def send_email(self, to: str, content: str) -> None: ...
```

#### O - Open/Closed Principle

```python
# ✅ Use protocols and abstract base classes
from typing import Protocol

class ProtocolAdapter(Protocol):
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]: ...
```

#### L - Liskov Substitution Principle

```python
# ✅ Subclasses can replace parent classes
class BaseAdapter:
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]:
        raise NotImplementedError

class MCPAdapter(BaseAdapter):
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]:
        return {"status": "ok"}
```

#### I - Interface Segregation Principle

```python
# ✅ Small and focused interfaces
class Executable(Protocol):
    def execute(self) -> None: ...

class Validatable(Protocol):
    def validate(self) -> bool: ...
```

#### D - Dependency Inversion Principle

```python
# ✅ Depend on abstractions, not concrete implementations
from typing import Protocol

class Storage(Protocol):
    async def save(self, key: str, value: str) -> None: ...
    async def load(self, key: str) -> str: ...

class AgentManager:
    def __init__(self, storage: Storage) -> None:
        self._storage = storage
```

### 2. Dependency Injection

```python
# ✅ Constructor injection
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

### 3. Async First

```python
# ✅ All I/O operations use async/await
async def load_agent(agent_id: str) -> Agent:
    async with aiofiles.open(f"agents/{agent_id}.yaml") as f:
        content = await f.read()
    return Agent.from_yaml(content)

# ❌ Avoid blocking calls
def load_agent_sync(agent_id: str) -> Agent:
    with open(f"agents/{agent_id}.yaml") as f:  # Blocking!
        content = f.read()
    return Agent.from_yaml(content)
```

---

## Type System Specifications

### 1. 100% Type Annotation Coverage

```python
# ✅ Complete type annotations
from typing import Any
from collections.abc import Callable, Awaitable

async def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
    hooks: list[Callable[[str], Awaitable[None]]] | None = None,
) -> dict[str, Any]:
    ...

# ❌ Missing type annotations
async def execute_workflow(workflow_id, inputs, hooks=None):
    ...
```

### 2. Use Pydantic for Data Validation

```python
from pydantic import BaseModel, Field, field_validator

class AgentMetadata(BaseModel):
    """Agent metadata model"""

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

### 3. Generic Usage

```python
from typing import TypeVar, Generic

T = TypeVar("T")
R = TypeVar("R")

class AsyncProcessor(Generic[T, R]):
    async def process(self, input_data: T) -> R:
        raise NotImplementedError
```

---

## Testing Standards

### 1. Test Coverage Requirements

- **Unit Tests**: ≥ 80% code coverage
- **Integration Tests**: Cover all core workflows
- **End-to-End Tests**: Cover key user scenarios

### 2. Test Structure

```python
import pytest
from agentflow.core.engine import AgentFlowEngine

class TestAgentFlowEngine:
    """AgentFlowEngine unit tests"""

    @pytest.fixture
    def engine(self) -> AgentFlowEngine:
        return AgentFlowEngine()

    async def test_execute_workflow_success(
        self,
        engine: AgentFlowEngine,
    ) -> None:
        """Test successful workflow execution"""
        result = await engine.execute(workflow_id="test", inputs={})
        assert result["status"] == "success"
```

### 3. Mock External Dependencies

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

## Documentation Standards

### Docstring Format (Google Style)

```python
async def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
    *,
    timeout: float = 30.0,
) -> dict[str, Any]:
    """Execute the specified workflow.

    Args:
        workflow_id: Unique workflow identifier
        inputs: Workflow input parameters dictionary
        timeout: Execution timeout in seconds, default 30 seconds

    Returns:
        Dictionary containing execution results

    Raises:
        ValueError: When workflow_id is empty
        TimeoutError: When execution times out

    Example:
        >>> result = await execute_workflow("my-workflow", {"input": "test"})
        >>> print(result["status"])
        success
    """
    ...
```

---

## Error Handling

### 1. Custom Exception Hierarchy

```python
class AgentFlowError(Exception):
    """AgentFlow base exception"""

class WorkflowError(AgentFlowError):
    """Workflow related exception"""

class WorkflowNotFoundError(WorkflowError):
    """Workflow not found"""
```

### 2. Best Practices

```python
# ✅ Correct: Catch specific exceptions
try:
    result = await execute_workflow(workflow_id)
except WorkflowNotFoundError:
    logger.error(f"Workflow not found: {workflow_id}")
    raise
except TimeoutError:
    logger.warning(f"Workflow timeout: {workflow_id}")
    return {"status": "timeout"}

# ❌ Wrong: Catch all exceptions
try:
    result = await execute_workflow(workflow_id)
except Exception:  # Too broad!
    pass
```

---

## Common AI Mistakes Checklist

### ❌ Mistake 1: Forgetting async/await

```python
# ❌ Wrong
def load_data():
    return aiofiles.open("data.txt")  # Returns coroutine!

# ✅ Correct
async def load_data() -> str:
    async with aiofiles.open("data.txt") as f:
        return await f.read()
```

### ❌ Mistake 2: Mutable Default Arguments

```python
# ❌ Wrong
def add_item(item: str, items: list[str] = []) -> list[str]:
    items.append(item)  # Shared list!
    return items

# ✅ Correct
def add_item(item: str, items: list[str] | None = None) -> list[str]:
    if items is None:
        items = []
    items.append(item)
    return items
```

### ❌ Mistake 3: Forgetting to Close Resources

```python
# ❌ Wrong
async def process_file(path: str) -> str:
    f = await aiofiles.open(path)
    content = await f.read()
    return content  # File not closed!

# ✅ Correct
async def process_file(path: str) -> str:
    async with aiofiles.open(path) as f:
        return await f.read()
```

### ❌ Mistake 4: Circular Imports

```python
# ✅ Correct: Use TYPE_CHECKING
from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from b import B

class A:
    def use_b(self, b: "B") -> None: ...
```

### ❌ Mistake 5: Incomplete Type Annotations

```python
# ❌ Wrong
def process(data):  # Missing types
    return data

# ✅ Correct
def process(data: dict[str, Any]) -> dict[str, Any]:
    return data
```

---

## Tool Configuration

All tool configurations are centralized in `pyproject.toml`.

---

## Checklist

Before submitting code, ensure:

- [ ] `ruff check .` passes with no errors
- [ ] `ruff format .` has formatted code
- [ ] `mypy .` passes with no type errors
- [ ] `pytest` all tests pass
- [ ] Test coverage ≥ 80%
- [ ] All public APIs have docstrings
- [ ] No `print()` debug statements
- [ ] No hardcoded configuration
- [ ] Complete exception handling
- [ ] Resources properly closed

---

**Strictly follow these standards to ensure code quality!**
