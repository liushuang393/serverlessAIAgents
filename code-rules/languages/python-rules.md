# コーディング規約

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Framework 全 Python コード
> **最終更新**: 2025-01-19

## 📋 目次

1. [Python バージョン要件](#python-バージョン要件)
2. [コードフォーマッティング](#コードフォーマッティング)
3. [インポート管理](#インポート管理)
4. [型アノテーション](#型アノテーション)
5. [コード品質ツール](#コード品質ツール)
6. [自動化チェック](#自動化チェック)

---

## 🐍 Python バージョン要件

### サポートバージョン
```python
# 最小バージョン: Python 3.13+
# 理由: AG-UI プロトコル要件 + パフォーマンス優位性
requires-python = ">=3.13"
```

### バージョン互換性
- **3.13+**: 完全サポート（推奨）
- **3.12**: 限定的サポート（レガシー環境のみ）
- **3.11 以下**: 非サポート

---

## 🎨 コードフォーマッティング

### Ruff 設定
Ruff を統一的な linter および formatter として使用：

```toml
[tool.ruff]
line-length = 100
target-version = "py313"

[tool.ruff.lint]
select = [
    "E", "W", "F", "I", "N", "UP", "ANN", "B", "A", "C4", "DTZ",
    "T10", "EM", "ISC", "ICN", "PIE", "PT", "Q", "RSE", "RET",
    "SIM", "TID", "TCH", "ARG", "PTH", "ERA", "PL", "TRY", "RUF"
]
ignore = ["ANN101", "ANN102"]
```

### フォーマットルール

#### 基本フォーマット
```python
# ✅ 正しい: 100文字以内で適切な改行
def process_user_data(
    user_id: str,
    data: dict[str, Any],
    *,
    validate: bool = True,
) -> dict[str, Any]:
    """ユーザー処理を実行する。"""

# ❌ 間違い: 行が長すぎる
def process_user_data(user_id: str, data: dict[str, Any], validate: bool = True) -> dict[str, Any]:
```

#### インデントと空白
```python
# ✅ 正しい: 4スペースインデント、一貫した空白
class UserProcessor:
    def __init__(self, config: dict[str, Any]) -> None:
        self.config = config

    def process(self, user_id: str) -> dict[str, Any]:
        if user_id:
            return {"status": "ok"}
        else:
            return {"status": "error"}

# ❌ 間違い: タブとスペースの混在、不適切な空白
class UserProcessor:
	def __init__(self,config:dict[str,Any])->None:
		self.config=config
```

#### 文字列引用符
```python
# ✅ 推奨: ダブルクォートを優先
user_name = "john_doe"
query = f"SELECT * FROM users WHERE id = {user_id}"

# ✅ 許可: シングルクォート（SQL等で必要時）
sql = 'SELECT * FROM users WHERE status = "active"'
```

---

## 📦 インポート管理

### インポート順序
```python
# 1. 標準ライブラリ
import os
import sys
from typing import Any, Dict, List

# 2. サードパーティライブラリ
import aiofiles
import pydantic
from fastapi import FastAPI

# 3. ローカルインポート
from agentflow.core.engine import AgentFlowEngine
from agentflow.services import UserService
```

### インポートスタイル
```python
# ✅ 推奨: 明示的なインポート
from typing import Any, Dict, List, Optional
from pathlib import Path

# ✅ 許可: モジュールインポート（頻繁使用時）
import json
import logging

# ❌ 禁止: ワイルドカードインポート
from typing import *
```

### 相対インポート
```python
# ✅ 正しい: 明示的な相対インポート
from .core import BaseAgent
from ..services import UserService

# ❌ 禁止: 暗黙的な相対インポート
from core import BaseAgent
```

---

## 🔷 型アノテーション

### 必須原則
```python
# ✅ 必須: すべての関数に完全な型アノテーション
async def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
    *,
    timeout: float = 30.0,
) -> dict[str, Any]:
    """ワークフロー実行。"""
    ...

# ❌ 禁止: 型アノテーションなし
def execute_workflow(workflow_id, inputs, timeout=30.0):
    ...
```

### ジェネリック型
```python
from typing import TypeVar, Generic, Protocol

T = TypeVar("T")
R = TypeVar("R")

class Processor(Generic[T, R]):
    async def process(self, input_data: T) -> R:
        ...

class Storage(Protocol):
    async def save(self, key: str, value: str) -> None: ...
    async def load(self, key: str) -> str: ...
```

### 特殊型パターン
```python
# ✅ 推奨: Union より | 演算子を使用
def process_data(data: str | int | None = None) -> str:
    ...

# ✅ 推奨: 明示的な Optional
from typing import Optional
def get_user(user_id: str) -> Optional[dict[str, Any]]:
    ...

# ✅ 推奨: 適切なコレクション型
from collections.abc import Callable, Awaitable

async def execute_with_callback(
    task: Callable[[], Awaitable[None]],
    callback: Callable[[dict[str, Any]], Awaitable[None]],
) -> None:
    ...
```

---

## 🛠️ コード品質ツール

### 必須ツール

#### Ruff (Lint + Format)
```bash
# インストール
pip install ruff

# 実行
ruff check .          # チェックのみ
ruff format .         # フォーマット適用
ruff check --fix .    # 自動修正
```

#### MyPy (型チェック)
```bash
# インストール
pip install mypy

# 実行
mypy agentflow        # 型チェック
mypy --show-error-codes agentflow  # エラーコード表示
```

### 設定ファイル
```toml
# pyproject.toml
[tool.mypy]
python_version = "3.13"
warn_return_any = true
warn_unused_configs = true
disallow_untyped_defs = true
disallow_incomplete_defs = true
check_untyped_defs = true
disallow_untyped_decorators = true
no_implicit_optional = true
warn_redundant_casts = true
warn_unused_ignores = true
warn_no_return = true
warn_unreachable = true
strict_equality = true

[[tool.mypy.overrides]]
module = "agentflow.*"
disallow_untyped_defs = true
disallow_incomplete_defs = true
```

---

## 🤖 自動化チェック

### Pre-commit フック
```yaml
# .pre-commit-config.yaml
repos:
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.1.0
    hooks:
      - id: ruff
        args: [--fix]
      - id: ruff-format

  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.0.0
    hooks:
      - id: mypy
        additional_dependencies: [types-all]
```

### CI/CD 統合
```yaml
# .github/workflows/ci.yml
- name: Run Ruff
  run: ruff check .

- name: Run MyPy
  run: mypy agentflow

- name: Run Tests
  run: pytest --cov=agentflow --cov-fail-under=80
```

### ローカル開発チェック
```bash
# 推奨スクリプト (check.sh)
#!/bin/bash

echo "🔍 Running code quality checks..."

# Format code
echo "📝 Formatting code..."
ruff format .

# Lint code
echo "🔎 Linting code..."
ruff check . --fix

# Type check
echo "🔷 Type checking..."
mypy agentflow

# Run tests
echo "🧪 Running tests..."
pytest --cov=agentflow --cov-fail-under=80

echo "✅ All checks passed!"
```

---

## 🚫 禁止パターン

### ゼロトレランスルール
```python
# ❌ 禁止: Any 型の乱用（正当な理由とコメントがない限り）
def process(data: Any) -> Any:  # コメント必須
    ...

# ❌ 禁止: type: ignore（正当な理由とコメントがない限り）
result = complex_function()  # type: ignore  # コメント必須

# ❌ 禁止: print() デバッグ文（logging を使用）
print("Debug info")  # ❌

# 代わりに:
import logging
logger = logging.getLogger(__name__)
logger.debug("Debug info")  # ✅
```

### コード品質ルール
```python
# ❌ 禁止: 可変デフォルト引数
def add_item(item: str, items: list[str] = []) -> list[str]:  # ❌
    items.append(item)
    return items

# ✅ 正しい:
def add_item(item: str, items: list[str] | None = None) -> list[str]:
    if items is None:
        items = []
    items.append(item)
    return items

# ❌ 禁止: ハードコードされた設定・シークレット
DATABASE_URL = "postgresql://localhost/db"  # ❌

# ✅ 正しい: 環境変数を使用
import os
DATABASE_URL = os.getenv("DATABASE_URL")  # ✅
```

---

## 📚 関連ドキュメント

- [**命名規約**](naming-conventions.md) - 変数・関数・クラスの命名規則
- [**アーキテクチャ設計**](architecture-guidelines.md) - システム設計原則
- [**テスト規約**](testing-standards.md) - テスト作成と実行の標準
- [**コントリビューションガイド**](../../../CONTRIBUTING.md) - 開発環境セットアップ

---

**これらのコーディング規約を守ることで、高品質で保守性の高いコードを維持できます。** 🎯

*最終更新: 2025-01-19 | バージョン: 1.0.0*