# AgentFlow 開発規範

> **バージョン**: 1.0.0  
> **最終更新**: 2025-01-15  
> **適用範囲**: AgentFlow Framework の全コード

**他の言語**: [English](DEVELOPMENT_STANDARDS_EN.md) | [中文](DEVELOPMENT_STANDARDS.md)

---

## 📋 目次

1. [核心原則](#核心原則)
2. [コード品質基準](#コード品質基準)
3. [アーキテクチャ設計原則](#アーキテクチャ設計原則)
4. [型システム仕様](#型システム仕様)
5. [テスト基準](#テスト基準)
6. [ドキュメント規範](#ドキュメント規範)
7. [エラー処理](#エラー処理)
8. [AI がよく犯す間違いチェックリスト](#ai-がよく犯す間違いチェックリスト)
9. [ツール設定](#ツール設定)
10. [チェックリスト](#チェックリスト)

---

## 核心原則

### 1. ゼロトレランスポリシー

- ❌ **禁止**: 静的解析エラー（mypy、ruff）
- ❌ **禁止**: 未処理の警告
- ❌ **禁止**: `Any` 型（十分な理由とコメントがない限り）
- ❌ **禁止**: `type: ignore`（十分な理由とコメントがない限り）
- ❌ **禁止**: ハードコードされた設定、シークレット、パス
- ❌ **禁止**: `print()` デバッグ文（logging を使用）
- ❌ **禁止**: 可変デフォルト引数 `def foo(x=[])`

### 2. 品質優先

- ✅ **型カバレッジ**: 100%
- ✅ **テストカバレッジ**: ≥ 80%
- ✅ **ドキュメントカバレッジ**: すべての公開 API
- ✅ **コードレビュー**: すべての PR は必ずレビュー

---

## コード品質基準

### 1. Python バージョン

```python
# 最小バージョン: Python 3.13+
# 理由: AG-UI プロトコル要件 + パフォーマンス優位性
requires-python = ">=3.13"
```

### 2. コードスタイル

**Ruff** を統一的な linter および formatter として使用：

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

### 3. 命名規則

```python
# モジュール名: 小文字 + アンダースコア
# agent_flow.py ✅  |  AgentFlow.py ❌

# クラス名: PascalCase
class AgentFlowEngine: ...  # ✅
class agent_flow_engine: ...  # ❌

# 関数/変数: snake_case
def create_agent() -> Agent: ...  # ✅
def createAgent() -> Agent: ...  # ❌

# 定数: UPPER_SNAKE_CASE
MAX_RETRIES = 3  # ✅
maxRetries = 3  # ❌

# プライベートメンバー: 単一アンダースコアプレフィックス
class Agent:
    def __init__(self) -> None:
        self._internal_state: dict[str, Any] = {}  # ✅
```

---

## アーキテクチャ設計原則

### 1. SOLID 原則

#### S - 単一責任原則（Single Responsibility）

```python
# ❌ 間違い: 1つのクラスが多くのことをする
class Agent:
    def execute(self) -> None: ...
    def save_to_db(self) -> None: ...
    def send_email(self) -> None: ...

# ✅ 正しい: 責任の分離
class Agent:
    def execute(self) -> None: ...

class AgentRepository:
    def save(self, agent: Agent) -> None: ...

class NotificationService:
    def send_email(self, to: str, content: str) -> None: ...
```

#### O - 開閉原則（Open/Closed）

```python
# ✅ プロトコルと抽象基底クラスを使用
from typing import Protocol

class ProtocolAdapter(Protocol):
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]: ...
```

#### L - リスコフの置換原則（Liskov Substitution）

```python
# ✅ サブクラスは親クラスを置き換えられる
class BaseAdapter:
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]:
        raise NotImplementedError

class MCPAdapter(BaseAdapter):
    async def execute(self, request: dict[str, Any]) -> dict[str, Any]:
        return {"status": "ok"}
```

#### I - インターフェース分離原則（Interface Segregation）

```python
# ✅ 小さく焦点を絞ったインターフェース
class Executable(Protocol):
    def execute(self) -> None: ...

class Validatable(Protocol):
    def validate(self) -> bool: ...
```

#### D - 依存性逆転原則（Dependency Inversion）

```python
# ✅ 具体的な実装ではなく抽象に依存
from typing import Protocol

class Storage(Protocol):
    async def save(self, key: str, value: str) -> None: ...
    async def load(self, key: str) -> str: ...

class AgentManager:
    def __init__(self, storage: Storage) -> None:
        self._storage = storage
```

### 2. 依存性注入

```python
# ✅ コンストラクタ注入
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

### 3. 非同期優先

```python
# ✅ すべての I/O 操作で async/await を使用
async def load_agent(agent_id: str) -> Agent:
    async with aiofiles.open(f"agents/{agent_id}.yaml") as f:
        content = await f.read()
    return Agent.from_yaml(content)

# ❌ ブロッキング呼び出しを避ける
def load_agent_sync(agent_id: str) -> Agent:
    with open(f"agents/{agent_id}.yaml") as f:  # ブロッキング！
        content = f.read()
    return Agent.from_yaml(content)
```

---

## 型システム仕様

### 1. 100% 型アノテーションカバレッジ

```python
# ✅ 完全な型アノテーション
from typing import Any
from collections.abc import Callable, Awaitable

async def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
    hooks: list[Callable[[str], Awaitable[None]]] | None = None,
) -> dict[str, Any]:
    ...

# ❌ 型アノテーションが不足
async def execute_workflow(workflow_id, inputs, hooks=None):
    ...
```

### 2. データ検証に Pydantic を使用

```python
from pydantic import BaseModel, Field, field_validator

class AgentMetadata(BaseModel):
    """Agent メタデータモデル"""

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

### 3. ジェネリックの使用

```python
from typing import TypeVar, Generic

T = TypeVar("T")
R = TypeVar("R")

class AsyncProcessor(Generic[T, R]):
    async def process(self, input_data: T) -> R:
        raise NotImplementedError
```

---

## テスト基準

### 1. テストカバレッジ要件

- **単体テスト**: ≥ 80% コードカバレッジ
- **統合テスト**: すべてのコアワークフローをカバー
- **エンドツーエンドテスト**: 主要なユーザーシナリオをカバー

### 2. テスト構造

```python
import pytest
from agentflow.core.engine import AgentFlowEngine

class TestAgentFlowEngine:
    """AgentFlowEngine 単体テスト"""

    @pytest.fixture
    def engine(self) -> AgentFlowEngine:
        return AgentFlowEngine()

    async def test_execute_workflow_success(
        self,
        engine: AgentFlowEngine,
    ) -> None:
        """ワークフロー実行成功のテスト"""
        result = await engine.execute(workflow_id="test", inputs={})
        assert result["status"] == "success"
```

### 3. 外部依存関係のモック

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

## ドキュメント規範

### Docstring フォーマット（Google スタイル）

```python
async def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
    *,
    timeout: float = 30.0,
) -> dict[str, Any]:
    """指定されたワークフローを実行する。

    Args:
        workflow_id: ワークフローの一意識別子
        inputs: ワークフロー入力パラメータ辞書
        timeout: 実行タイムアウト（秒）、デフォルト 30 秒

    Returns:
        実行結果を含む辞書

    Raises:
        ValueError: workflow_id が空の場合
        TimeoutError: 実行がタイムアウトした場合

    Example:
        >>> result = await execute_workflow("my-workflow", {"input": "test"})
        >>> print(result["status"])
        success
    """
    ...
```

---

## エラー処理

### 1. カスタム例外階層

```python
class AgentFlowError(Exception):
    """AgentFlow 基底例外"""

class WorkflowError(AgentFlowError):
    """ワークフロー関連例外"""

class WorkflowNotFoundError(WorkflowError):
    """ワークフローが見つからない"""
```

### 2. ベストプラクティス

```python
# ✅ 正しい: 特定の例外をキャッチ
try:
    result = await execute_workflow(workflow_id)
except WorkflowNotFoundError:
    logger.error(f"Workflow not found: {workflow_id}")
    raise
except TimeoutError:
    logger.warning(f"Workflow timeout: {workflow_id}")
    return {"status": "timeout"}

# ❌ 間違い: すべての例外をキャッチ
try:
    result = await execute_workflow(workflow_id)
except Exception:  # 広すぎる！
    pass
```

---

## AI がよく犯す間違いチェックリスト

### ❌ 間違い 1: async/await を忘れる

```python
# ❌ 間違い
def load_data():
    return aiofiles.open("data.txt")  # コルーチンを返す！

# ✅ 正しい
async def load_data() -> str:
    async with aiofiles.open("data.txt") as f:
        return await f.read()
```

### ❌ 間違い 2: 可変デフォルト引数

```python
# ❌ 間違い
def add_item(item: str, items: list[str] = []) -> list[str]:
    items.append(item)  # すべての呼び出しで同じリストを共有！
    return items

# ✅ 正しい
def add_item(item: str, items: list[str] | None = None) -> list[str]:
    if items is None:
        items = []
    items.append(item)
    return items
```

### ❌ 間違い 3: リソースを閉じ忘れる

```python
# ❌ 間違い
async def process_file(path: str) -> str:
    f = await aiofiles.open(path)
    content = await f.read()
    return content  # ファイルが閉じられていない！

# ✅ 正しい
async def process_file(path: str) -> str:
    async with aiofiles.open(path) as f:
        return await f.read()
```

### ❌ 間違い 4: 循環インポート

```python
# ✅ 正しい: TYPE_CHECKING を使用
from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from b import B

class A:
    def use_b(self, b: "B") -> None: ...
```

### ❌ 間違い 5: 不完全な型アノテーション

```python
# ❌ 間違い
def process(data):  # 型が不足
    return data

# ✅ 正しい
def process(data: dict[str, Any]) -> dict[str, Any]:
    return data
```

---

## ツール設定

すべてのツール設定は `pyproject.toml` に集中管理されています。

---

## チェックリスト

コードを提出する前に、以下を確認：

- [ ] `ruff check .` がエラーなしで通過
- [ ] `ruff format .` でコードがフォーマット済み
- [ ] `mypy .` が型エラーなしで通過
- [ ] `pytest` ですべてのテストが通過
- [ ] テストカバレッジ ≥ 80%
- [ ] すべての公開 API に docstring がある
- [ ] `print()` デバッグ文がない
- [ ] ハードコードされた設定がない
- [ ] 例外処理が完全
- [ ] リソースが適切に閉じられている

---

**この規範を厳格に遵守し、コード品質を確保してください！**
