# スタイル & フォーマット

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow 全 Python コード
> **最終更新**: 2026-01-19

## 📋 目次

1. [基本フォーマット原則](#基本フォーマット原則)
2. [インポート管理](#インポート管理)
3. [関数・メソッド定義](#関数・メソッド定義)
4. [クラス定義](#クラス定義)
5. [制御構造](#制御構造)
6. [型アノテーション](#型アノテーション)
7. [コメント・ドキュメント](#コメント・ドキュメント)
8. [Ruff設定](#ruff設定)
9. [自動化チェック](#自動化チェック)

---

## 🎯 基本フォーマット原則

### 行長制限
- **最大100文字**: 可読性を確保
- **柔軟な改行**: 論理的な単位で分割

```python
# ✅ 100文字以内
async def process_workflow_data(
    workflow_id: str,
    input_data: dict[str, Any],
    *,
    validate: bool = True,
) -> dict[str, Any]:
    pass

# ❌ 行が長すぎる
async def process_workflow_data(workflow_id: str, input_data: dict[str, Any], validate: bool = True) -> dict[str, Any]:
    pass
```

### インデント
- **4スペース**: Python 標準
- **継続行**: 8スペース（括弧内）
- **揃え**: 演算子揃えを推奨

```python
# ✅ 正しいインデント
def calculate_metrics(
    revenue: float,
    costs: float,
    tax_rate: float = 0.1,
) -> dict[str, float]:
    gross_profit = revenue - costs
    net_profit = gross_profit * (1 - tax_rate)

    return {
        "gross_profit": gross_profit,
        "net_profit": net_profit,
        "margin": net_profit / revenue if revenue > 0 else 0,
    }
```

---

## 📦 インポート管理

### インポート順序
1. **標準ライブラリ**
2. **サードパーティ**
3. **ローカルモジュール**

```python
# ✅ 正しいインポート順序
import asyncio
import json
from typing import Any, Dict, List, Protocol

import aiofiles
import yaml
from pydantic import BaseModel

from agentflow.core.interfaces import ICodeGenerator
from agentflow.services import PreviewService
from .utils import validate_workflow
```

### インポートグループ化
- **空行で分離**: 各グループ間に1行空行
- **アルファベット順**: 各グループ内でソート

```python
# ✅ グループ化されたインポート
# Standard library
import asyncio
import json
from typing import Any, Protocol

# Third-party
import aiofiles
import yaml
from pydantic import BaseModel

# Local
from agentflow.core.interfaces import ICodeGenerator
from agentflow.services import PreviewService
```

### インポート文スタイル
- **絶対インポート**を優先
- **相対インポート**: 同じパッケージ内のみ
- **ワイルドカード禁止**: `from module import *`

```python
# ✅ 推奨: 絶対インポート
from agentflow.core.interfaces import ICodeGenerator
from agentflow.services import PreviewService, PublishService

# ✅ 許容: 相対インポート（同じパッケージ内）
from .interfaces import ICodeGenerator
from .services import PreviewService

# ❌ 禁止: ワイルドカード
from agentflow.services import *
```

---

## 🔧 関数・メソッド定義

### 関数シグネチャ
- **型アノテーション必須**
- **キーワード専用引数**: `*` で分離
- **デフォルト引数**: 変更不可オブジェクト

```python
# ✅ 正しい関数定義
async def generate_code(
    workflow: WorkflowDefinition,
    output_type: CodeOutputType,
    *,
    template_dir: str | None = None,
    validate: bool = True,
) -> GeneratedCode:
    """ワークフローからコードを生成する.

    Args:
        workflow: 生成対象のワークフロー定義
        output_type: 出力タイプ（FRONTEND/BACKEND/FULLSTACK）
        template_dir: テンプレートディレクトリ（オプション）
        validate: 入力検証を行うか

    Returns:
        生成されたコード
    """
```

### パラメータ順序
1. **必須パラメータ**
2. **デフォルトパラメータ**
3. **`*` 区切り**
4. **キーワード専用パラメータ**

```python
# ✅ 正しいパラメータ順序
def create_flow(
    name: str,                    # 必須
    agents: list[AgentBlock],     # 必須
    *,
    memory_enabled: bool = True,  # キーワード専用
    max_retries: int = 3,         # キーワード専用
) -> WorkflowFlow:
    pass
```

---

## 🏗️ クラス定義

### クラス構造
- **基底クラス**: 明確に指定
- **メソッド順序**: `__init__` → プロパティ → 公開メソッド → プライベートメソッド

```python
# ✅ 正しいクラス定義
class CodeGenerator:
    """コード生成器基底クラス."""

    def __init__(self, template_dir: str = "templates"):
        """初期化.

        Args:
            template_dir: テンプレートディレクトリ
        """
        self.template_dir = template_dir
        self._cache: dict[str, str] = {}

    @property
    def supported_types(self) -> list[CodeOutputType]:
        """サポートされる出力タイプ."""
        return [CodeOutputType.BACKEND, CodeOutputType.FRONTEND]

    async def generate(self, workflow: WorkflowDefinition) -> GeneratedCode:
        """コードを生成."""
        pass

    def _load_template(self, template_name: str) -> str:
        """テンプレートを読み込み（プライベート）."""
        pass
```

### Protocol 定義
- **Protocol 継承**: `Protocol` を明示
- **メソッドシグネチャ**: 実装を強制

```python
# ✅ Protocol 定義
from typing import Protocol

class ICodeGenerator(Protocol):
    """コード生成インターフェース."""

    async def generate(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
    ) -> GeneratedCode:
        """コードを生成する."""
        ...

    async def preview(
        self,
        workflow: WorkflowDefinition,
    ) -> CodeOutput:
        """コードをプレビューする."""
        ...
```

---

## 🎛️ 制御構造

### 条件分岐
- **早期リターン**: ネストを避ける
- **条件の明確化**: 複雑な条件は変数化

```python
# ✅ 早期リターン
async def validate_workflow(workflow: WorkflowDefinition) -> bool:
    if not workflow.nodes:
        logger.warning("workflow_has_no_nodes", workflow_id=workflow.workflow_id)
        return False

    if workflow.workflow_id in self._invalid_cache:
        return False

    return await self._validate_nodes(workflow.nodes)

# ✅ 条件の明確化
is_valid_config = (
    config.api_key is not None and
    config.region in SUPPORTED_REGIONS and
    config.timeout > 0
)

if is_valid_config:
    return await self._deploy_to_target(config)
```

### ループ
- **async for**: 非同期イテレーション
- **リスト内包表記**: 単純な変換のみ

```python
# ✅ async for
async def process_batch(items: list[dict[str, Any]]) -> list[Result]:
    results = []
    async for item in items:
        result = await self._process_item(item)
        results.append(result)
    return results

# ✅ リスト内包表記（単純な場合）
agent_names = [agent.name for agent in agents if agent.is_active]
```

---

## 🧬 型アノテーション

### 基本型ヒント
- **全てのパラメータ**: 型を明示
- **戻り値**: 型を明示
- **複雑な型**: `typing` モジュール使用

```python
# ✅ 完全な型アノテーション
from typing import Any, Dict, List, Optional, Union

async def process_data(
    input_data: Dict[str, Any],
    filters: Optional[List[str]] = None,
) -> Union[Dict[str, Any], None]:
    """データを処理する.

    Args:
        input_data: 入力データ
        filters: 適用するフィルター（オプション）

    Returns:
        処理結果、失敗時はNone
    """
    pass
```

### Generic 型
- **TypeVar**: ジェネリック型変数
- **具体的な型**: 可能な限り具体的に

```python
# ✅ Generic 型使用
from typing import TypeVar, Generic

T = TypeVar('T')
ResultT = TypeVar('ResultT', bound='BaseResult')

class Result(Generic[T]):
    """ジェネリック結果クラス."""

    def __init__(self, data: T, success: bool = True):
        self.data = data
        self.success = success
```

### Union 型
- **`|` 記法**: Python 3.10+
- **Optional**: `T | None` 形式

```python
# ✅ Union 型
def find_agent(
    agent_id: str,
    include_inactive: bool = False,
) -> AgentBlock | None:
    """Agentを検索.

    Args:
        agent_id: Agent ID
        include_inactive: 非アクティブも含めるか

    Returns:
        見つかったAgent、存在しない場合はNone
    """
```

---

## 📝 コメント・ドキュメント

### Docstring
- **必須**: 全ての公開API
- **Google スタイル**: 標準フォーマット
- **Args/Returns**: 詳細に記述

```python
# ✅ Google スタイル Docstring
def deploy_to_vercel(
    code: GeneratedCode,
    config: VercelDeployConfig,
) -> DeployResult:
    """Vercel にコードをデプロイする.

    指定されたコードを Vercel にデプロイし、デプロイ結果を返す。
    デプロイプロセスにはビルド、アップロード、ドメイン設定が含まれる。

    Args:
        code: デプロイする生成コード
        config: Vercel デプロイ設定（APIキー、プロジェクト名など）

    Returns:
        デプロイ結果（URL、ステータス、ログを含む）

    Raises:
        DeployError: デプロイ失敗時
        ConfigError: 設定不正時

    Example:
        ```python
        config = VercelDeployConfig(api_key="...", project_name="my-app")
        result = await deploy_to_vercel(code, config)
        print(f"Deployed to: {result.url}")
        ```
    """
```

### インラインコメント
- **必要な場合のみ**: コードが自明でない場合
- **なぜ**: 何をするのかではなく、なぜそうするのか

```python
# ✅ 有意義なコメント
# 早期リターンを優先: ネストを浅く保ち可読性を確保
if not workflow.nodes:
    return ValidationError("empty_workflow")

# キャッシュを優先: DB負荷を軽減するため
if cached := self._cache.get(workflow_id):
    return cached

# ❌ 自明なコメント（不要）
# workflow_id を取得
workflow_id = workflow.workflow_id

# 結果を返す
return result
```

---

## ⚙️ Ruff設定

### pyproject.toml 設定
```toml
[tool.ruff]
line-length = 100
target-version = "py313"

[tool.ruff.lint]
select = [
    "E",  # pycodestyle errors
    "W",  # pycodestyle warnings
    "F",  # pyflakes
    "I",  # isort
    "N",  # pep8-naming
    "UP", # pyupgrade
]

ignore = [
    "E501",  # line too long (handled by formatter)
]

[tool.ruff.lint.isort]
known-first-party = ["agentflow"]
force-sort-within-sections = true

[tool.ruff.format]
quote-style = "double"
indent-style = "space"
skip-magic-trailing-comma = false
line-ending = "auto"
```

---

## ✅ 自動化チェック

### フォーマットチェック
```bash
# フォーマット適用
ruff format .

# フォーマットチェック（CI用）
ruff format . --check
```

### リントチェック
```bash
# リント実行
ruff check .

# 自動修正可能な問題を修正
ruff check . --fix
```

### CI/CD 統合
```yaml
- name: Check formatting and linting
  run: |
    ruff format . --check
    ruff check .
    mypy agentflow
```

### プリコミット設定
```yaml
# .pre-commit-config.yaml
repos:
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.1.0
    hooks:
      - id: ruff
        args: [--fix]
      - id: ruff-format
```

---

## 📋 スタイルチートシート

| 要素 | スタイル | 例 |
|------|----------|-----|
| **行長** | 最大100文字 | 適宜改行 |
| **インデント** | 4スペース | Python標準 |
| **インポート** | グループ化+ソート | std → 3rd → local |
| **関数** | 型アノテーション必須 | `def func(arg: Type) -> Return:` |
| **クラス** | PascalCase | `class MyClass:` |
| **変数** | snake_case | `my_variable` |
| **定数** | UPPER_SNAKE | `MY_CONSTANT` |
| **プライベート** | `_` 接頭語 | `_private_method` |

*最終更新: 2026-01-19 | Ruff + mypy 対応*