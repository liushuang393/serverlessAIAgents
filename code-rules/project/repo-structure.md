# リポジトリ構造

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow リポジトリ全体
> **最終更新**: 2026-01-19

## 📋 目次

1. [ディレクトリ構造原則](#ディレクトリ構造原則)
2. [モジュール組織化](#モジュール組織化)
3. [ファイル命名規則](#ファイル命名規則)
4. [設定ファイル管理](#設定ファイル管理)
5. [ドキュメント構造](#ドキュメント構造)
6. [テスト構造](#テスト構造)
7. [CI/CD構造](#cicd構造)
8. [自動化チェック](#自動化チェック)

---

## 🗂️ ディレクトリ構造原則

### ルートレベル構造
```
agentflow/
├── 📁 agentflow/           # コアパッケージ
├── 📁 docs/               # ドキュメント
├── 📁 tests/              # テスト
├── 📁 examples/           # 使用例
├── 📁 scripts/            # ユーティリティスクリプト
├── 📁 apps/               # アプリケーション例
├── 📁 .github/            # GitHub設定
├── 📄 pyproject.toml      # Pythonプロジェクト設定
├── 📄 README.md           # プロジェクト概要
├── 📄 CONTRIBUTING.md     # 貢献ガイド
└── 📄 LICENSE             # ライセンス
```

### パッケージ構造
```
agentflow/
├── 📁 core/                # 核心インターフェース
│   ├── 📁 interfaces/      # 安定した契約
│   └── 📁 implementations/ # 実装クラス
├── 📁 applications/        # アプリケーション層
├── 📁 uis/                 # UI層
├── 📁 flows/               # フロー層
├── 📁 agents/              # Agent層
├── 📁 tools/               # ツール層
├── 📁 providers/           # Provider層
├── 📁 protocols/           # プロトコル層
├── 📁 infra/               # インフラ層
└── 📁 services/            # サービス層
```

---

## 📦 モジュール組織化

### 8層アーキテクチャの反映
各層を独立したパッケージとして組織化します。

#### Core層（安定インターフェース）
```
agentflow/core/
├── interfaces/
│   ├── __init__.py
│   ├── code_generator.py     # ICodeGenerator
│   ├── deploy_executor.py    # IDeployExecutor
│   ├── workflow_runner.py    # IWorkflowRunner
│   ├── config_manager.py     # IConfigManager
│   └── types.py              # 共有型定義
├── implementations/
│   └── __init__.py           # 実装クラスのエクスポート
└── __init__.py
```

#### 各機能層
```
agentflow/agents/
├── __init__.py
├── block.py                  # AgentBlock 基底クラス
├── decorator.py              # @agent デコレータ
├── registry.py               # Agent レジストリ
└── builtin/                  # 組み込みAgent
    ├── __init__.py
    ├── gatekeeper.py
    ├── dao_agent.py
    └── review_agent.py

agentflow/tools/
├── __init__.py
├── decorator.py              # @tool デコレータ
├── mcp/                      # MCP ツール統合
├── skills/                   # Skills エンジン
└── builtin/                  # 組み込みツール
    ├── database.py
    ├── payment.py
    └── deployment.py
```

### プロトコル層の組織化
```
agentflow/protocols/
├── __init__.py
├── mcp/
│   ├── __init__.py
│   ├── client.py
│   ├── server.py
│   └── types.py
├── a2a/
│   ├── __init__.py
│   ├── messaging.py
│   └── routing.py
├── ag_ui/
│   ├── __init__.py
│   ├── events.py
│   └── streaming.py
└── a2ui/
    ├── __init__.py
    ├── components.py
    └── rendering.py
```

### Provider層の統一アクセス
```
agentflow/providers/
├── __init__.py               # 統一APIエクスポート
├── llm/
│   ├── __init__.py
│   ├── base.py
│   ├── openai.py
│   ├── anthropic.py
│   └── google.py
├── data/
│   ├── __init__.py
│   ├── database.py
│   ├── vector.py
│   └── cache.py
└── events/
    ├── __init__.py
    ├── sse.py
    └── websocket.py
```

---

## 📄 ファイル命名規則

### Python モジュール
- **snake_case**: 全て小文字、アンダースコア区切り
- **役割反映**: ファイル名で何をするかがわかる

```python
# ✅ 正しいファイル命名
agentflow/
├── agents/
│   ├── block.py              # Agent 基底ブロック
│   ├── decorator.py          # @agent デコレータ
│   └── registry.py           # Agent レジストリ
├── tools/
│   ├── decorator.py          # @tool デコレータ
│   └── skills_engine.py      # Skills エンジン
└── protocols/
    ├── mcp_client.py         # MCP クライアント
    └── a2a_messaging.py      # A2A メッセージング
```

### 設定ファイル
- **kebab-case**: 設定ファイルのみ
- **拡張子**: `.toml`, `.yaml`, `.json`

```python
# 設定ファイル
├── pyproject.toml            # Python プロジェクト設定
├── .pre-commit-config.yaml   # Pre-commit 設定
├── .github/
│   ├── workflows/
│   │   └── ci.yml            # CI ワークフロー
│   └── dependabot.yml        # Dependabot 設定
└── docs/
    └── mkdocs.yml            # MkDocs 設定
```

### ドキュメントファイル
- **snake_case**: 全て小文字、アンダースコア区切り
- **接頭語**: 種類を表す接頭語

```python
docs/
├── guide-studio-ui.md        # Studio UI 操作ガイド
├── guide-cli.md              # CLI 操作ガイド
├── guide-coding.md           # コーディングガイド
├── architecture.md           # アーキテクチャ
├── protocols.md              # プロトコル仕様
└── api.md                    # API リファレンス
```

---

## ⚙️ 設定ファイル管理

### pyproject.toml 構造
```toml
[build-system]
requires = ["hatchling"]
build-backend = "hatchling.build"

[project]
name = "agentflow"
version = "0.4.0"
description = "軽量 AI エージェント開発フレームワーク"
readme = "README.md"
license = {text = "MIT"}
requires-python = ">=3.10"
authors = [
    {name = "AgentFlow Team", email = "team@agentflow.dev"},
]
dependencies = [
    "aiofiles>=0.23.0",
    "pydantic>=2.0.0",
    "structlog>=23.0.0",
    # ... 依存関係
]
optional-dependencies = {
    dev = [
        "ruff>=0.1.0",
        "mypy>=1.0.0",
        "pytest>=7.0.0",
        "pytest-cov>=4.0.0",
    ],
    docs = [
        "mkdocs>=1.4.0",
        "mkdocs-material>=9.0.0",
    ],
}

[project.urls]
Homepage = "https://github.com/agentflow/agentflow"
Documentation = "https://agentflow.dev"
Repository = "https://github.com/agentflow/agentflow"
Issues = "https://github.com/agentflow/agentflow/issues"

[tool.ruff]
line-length = 100
target-version = "py313"

[tool.ruff.lint]
select = [
    "E", "W", "F", "I", "N", "UP", "BLE", "TRY"
]

[tool.mypy]
python_version = "3.10"
warn_return_any = true
warn_unused_configs = true
disallow_untyped_defs = true
disallow_incomplete_defs = true

[tool.pytest.ini_options]
minversion = "7.0"
addopts = "-ra -q --strict-markers --strict-config"
testpaths = ["tests"]
python_files = ["test_*.py", "*_test.py"]
python_classes = ["Test*"]
python_functions = ["test_*"]

[tool.coverage.run]
source = ["agentflow"]
omit = [
    "*/tests/*",
    "*/test_*.py",
]

[tool.coverage.report]
exclude_lines = [
    "pragma: no cover",
    "def __repr__",
    "raise AssertionError",
    "raise NotImplementedError",
]
```

### 環境設定ファイル
```bash
# .env.example - 環境変数テンプレート
# LLM Providers
OPENAI_API_KEY=
ANTHROPIC_API_KEY=
GOOGLE_API_KEY=

# Databases
DATABASE_URL=
SUPABASE_URL=your_supabase_url
SUPABASE_KEY=

# Vector Databases
PINECONE_API_KEY=
PINECONE_INDEX=agentflow-index

# Cache
REDIS_URL=redis://localhost:6379

# Application
APP_ENV=development
LOG_LEVEL=INFO
```

---

## 📚 ドキュメント構造

### docs/ ディレクトリ構造
```
docs/
├── 📁 rules/                 # ルール体系
│   ├── 📄 index_ja.md        # ルールインデックス（日本語）
│   ├── 📄 coding-standards_ja.md
│   ├── 📄 testing-standards_ja.md
│   └── 📄 ...
├── 📁 guide/                 # 操作ガイド
│   ├── 📄 studio-ui.md       # Studio UI 操作
│   ├── 📄 cli.md             # CLI 操作
│   ├── 📄 coding.md          # コーディング
│   └── 📄 ...
├── 📁 api/                   # API リファレンス
│   ├── 📄 services.md        # サービスAPI
│   ├── 📄 protocols.md       # プロトコルAPI
│   └── 📄 ...
├── 📄 index.md               # ドキュメントインデックス
├── 📄 architecture.md        # アーキテクチャ
├── 📄 quickstart.md          # クイックスタート
└── 📄 mkdocs.yml             # MkDocs設定
```

### ドキュメント命名規則
- **guide-*.md**: 操作ガイド
- **api-*.md**: API リファレンス
- ***-standards*.md**: 開発標準
- ***-guide.md**: ガイドドキュメント

---

## 🧪 テスト構造

### tests/ ディレクトリ構造
```
tests/
├── 📁 unit/                  # ユニットテスト
│   ├── 📁 agents/
│   ├── 📁 tools/
│   ├── 📁 providers/
│   └── 📁 protocols/
├── 📁 integration/           # 統合テスト
│   ├── 📁 services/
│   └── 📁 workflows/
├── 📁 e2e/                   # エンドツーエンドテスト
│   └── 📁 apps/
├── 📁 fixtures/              # テストデータ
├── 📁 conftest.py            # pytest 設定
└── 📁 __init__.py
```

### テストファイル命名
- **test_*.py**: テストモジュール
- ***_test.py**: テストファイル
- **Test***: テストクラス
- **test_***: テスト関数

```python
# ✅ 正しいテスト構造
tests/
├── unit/
│   ├── agents/
│   │   ├── test_block.py
│   │   ├── test_decorator.py
│   │   └── test_registry.py
│   └── tools/
│       ├── test_decorator.py
│       └── test_skills_engine.py
├── integration/
│   └── services/
│       └── test_publish_service.py
└── e2e/
    └── apps/
        └── test_fullstack_app.py
```

---

## 🔄 CI/CD構造

### .github/workflows/ 構造
```
.github/
├── workflows/
│   ├── ci.yml                # 継続的インテグレーション
│   ├── cd.yml                # 継続的デプロイ
│   ├── security.yml          # セキュリティスキャン
│   └── release.yml           # リリース自動化
├── ISSUE_TEMPLATE/
│   ├── bug_report.md
│   ├── feature_request.md
│   └── security_report.md
├── PULL_REQUEST_TEMPLATE.md
└── dependabot.yml
```

### CI パイプライン設計
```yaml
# .github/workflows/ci.yml
name: CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        python-version: ["3.10", "3.11", "3.12", "3.13"]

    steps:
    - uses: actions/checkout@v4

    - name: Set up Python ${{ matrix.python-version }}
      uses: actions/setup-python@v4
      with:
        python-version: ${{ matrix.python-version }}

    - name: Install dependencies
      run: |
        python -m pip install --upgrade pip
        pip install -e ".[dev]"

    - name: Lint with Ruff
      run: ruff check .

    - name: Type check with mypy
      run: mypy agentflow

    - name: Test with pytest
      run: pytest --cov=agentflow --cov-report=xml

    - name: Upload coverage to Codecov
      uses: codecov/codecov-action@v3
```

---

## ✅ 自動化チェック

### リポジトリ構造検証スクリプト
```python
#!/usr/bin/env python3
# scripts/validate_repo_structure.py

import os
import sys
from pathlib import Path
from typing import Dict, List, Set

class RepoStructureValidator:
    """リポジトリ構造を検証."""

    REQUIRED_STRUCTURE = {
        "agentflow": ["core", "agents", "tools", "providers", "protocols", "services"],
        "docs": ["rules", "guide", "api"],
        "tests": ["unit", "integration", "e2e", "fixtures"],
        ".github": ["workflows", "ISSUE_TEMPLATE"],
    }

    REQUIRED_FILES = [
        "README.md",
        "pyproject.toml",
        "LICENSE",
        ".gitignore",
        ".pre-commit-config.yaml",
    ]

    def __init__(self, repo_root: Path):
        self.repo_root = repo_root
        self.errors: List[str] = []

    def validate(self) -> bool:
        """構造検証を実行."""
        self._check_required_files()
        self._check_directory_structure()
        self._check_python_packages()
        self._check_file_naming()
        return len(self.errors) == 0

    def _check_required_files(self):
        """必須ファイルの存在チェック."""
        for file_path in self.REQUIRED_FILES:
            if not (self.repo_root / file_path).exists():
                self.errors.append(f"必須ファイルが見つかりません: {file_path}")

    def _check_directory_structure(self):
        """ディレクトリ構造のチェック."""
        for parent, children in self.REQUIRED_STRUCTURE.items():
            parent_path = self.repo_root / parent
            if not parent_path.exists():
                self.errors.append(f"必須ディレクトリが見つかりません: {parent}")
                continue

            for child in children:
                child_path = parent_path / child
                if not child_path.exists():
                    self.errors.append(f"必須サブディレクトリが見つかりません: {parent}/{child}")

    def _check_python_packages(self):
        """Python パッケージ構造のチェック."""
        for py_file in self.repo_root.rglob("*.py"):
            if py_file.name == "__init__.py":
                continue

            # パッケージ内に __init__.py が存在するかチェック
            package_root = py_file.parent
            while package_root != self.repo_root:
                if (package_root / "__init__.py").exists():
                    break
                package_root = package_root.parent
            else:
                # __init__.py が見つからない場合
                rel_path = py_file.relative_to(self.repo_root)
                self.errors.append(f"Python ファイルがパッケージに属していません: {rel_path}")

    def _check_file_naming(self):
        """ファイル命名規則のチェック."""
        for file_path in self.repo_root.rglob("*"):
            if file_path.is_file() and not file_path.name.startswith('.'):
                self._validate_file_name(file_path)

    def _validate_file_name(self, file_path: Path):
        """個別のファイル命名を検証."""
        name = file_path.name
        suffix = file_path.suffix

        # Python ファイル
        if suffix == ".py":
            if not self._is_valid_python_filename(name):
                rel_path = file_path.relative_to(self.repo_root)
                self.errors.append(f"Python ファイル命名規則違反: {rel_path}")

        # マークダウンファイル
        elif suffix == ".md":
            if not self._is_valid_markdown_filename(name):
                rel_path = file_path.relative_to(self.repo_root)
                self.errors.append(f"Markdown ファイル命名規則違反: {rel_path}")

    def _is_valid_python_filename(self, filename: str) -> bool:
        """Python ファイル名の妥当性チェック."""
        import re
        # snake_case のみ許可（test_*.py, *_test.py は例外）
        if filename.startswith("test_") or filename.endswith("_test.py"):
            return True
        return bool(re.match(r'^[a-z][a-z0-9_]*\.py$', filename))

    def _is_valid_markdown_filename(self, filename: str) -> bool:
        """Markdown ファイル名の妥当性チェック."""
        import re
        # snake_case または kebab-case を許可
        return bool(re.match(r'^[a-z][a-z0-9_-]*\.md$', filename))

    def report_errors(self):
        """エラーをレポート."""
        if self.errors:
            print("❌ リポジトリ構造違反:")
            for error in self.errors:
                print(f"  - {error}")
            return False
        else:
            print("✅ リポジトリ構造検証通過")
            return True

def main():
    repo_root = Path(__file__).parent.parent
    validator = RepoStructureValidator(repo_root)
    validator.validate()
    success = validator.report_errors()
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
```

### インポート順序検証スクリプト
```python
#!/usr/bin/env python3
# scripts/check_import_order.py

import ast
import sys
from pathlib import Path
from typing import List

class ImportOrderChecker:
    """インポート順序を検証."""

    IMPORT_GROUPS = [
        "standard_library",
        "third_party",
        "local_modules"
    ]

    STANDARD_LIBRARY = {
        'asyncio', 'collections', 'contextlib', 'dataclasses', 'datetime',
        'functools', 'importlib', 'inspect', 'json', 'logging', 'os',
        'pathlib', 're', 'sys', 'time', 'typing', 'uuid'
    }

    def __init__(self):
        self.violations: List[str] = []

    def check_file(self, file_path: Path):
        """ファイルをチェック."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                tree = ast.parse(f.read())

            imports = []
            for node in ast.walk(tree):
                if isinstance(node, (ast.Import, ast.ImportFrom)):
                    imports.append(node)

            if imports:
                self._check_import_order(file_path, imports)

        except Exception as e:
            self.violations.append(f"{file_path}: パースエラー - {e}")

    def _check_import_order(self, file_path: Path, imports: List[ast.stmt]):
        """インポート順序をチェック."""
        current_group = -1

        for import_node in imports:
            import_group = self._classify_import(import_node)

            if import_group < current_group:
                rel_path = file_path.relative_to(Path.cwd())
                self.violations.append(
                    f"{rel_path}:{import_node.lineno}: "
                    f"インポート順序違反 - {self._import_to_string(import_node)}"
                )
            else:
                current_group = import_group

    def _classify_import(self, node: ast.stmt) -> int:
        """インポートを分類."""
        if isinstance(node, ast.Import):
            module = node.names[0].name.split('.')[0]
        elif isinstance(node, ast.ImportFrom):
            module = node.module.split('.')[0] if node.module else ""

        if module in self.STANDARD_LIBRARY:
            return 0  # standard_library
        elif module and not module.startswith('agentflow'):
            return 1  # third_party
        else:
            return 2  # local_modules

    def _import_to_string(self, node: ast.stmt) -> str:
        """インポート文を文字列化."""
        if isinstance(node, ast.Import):
            return f"import {node.names[0].name}"
        elif isinstance(node, ast.ImportFrom):
            module = node.module or ""
            names = ", ".join(name.name for name in node.names)
            return f"from {module} import {names}"

    def check_all_python_files(self):
        """全Pythonファイルをチェック."""
        for py_file in Path("agentflow").rglob("*.py"):
            self.check_file(py_file)

    def report(self) -> bool:
        """結果をレポート."""
        if self.violations:
            print("❌ インポート順序違反:")
            for violation in self.violations:
                print(f"  - {violation}")
            return False
        else:
            print("✅ インポート順序検証通過")
            return True

def main():
    checker = ImportOrderChecker()
    checker.check_all_python_files()
    success = checker.report()
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
```

---

## 📋 リポジトリ構造 チートシート

| 要素 | 規則 | 例 | 説明 |
|------|------|-----|------|
| **ディレクトリ** | snake_case | `agentflow/core/` | 機能グループ化 |
| **Pythonファイル** | snake_case | `code_generator.py` | 役割を反映 |
| **設定ファイル** | kebab-case | `pyproject.toml` | 設定ファイルのみ |
| **ドキュメント** | snake_case | `architecture.md` | 内容を反映 |
| **テスト** | test_*.py | `test_agent_block.py` | pytest 慣習 |
| **パッケージ** | __init__.py | `agents/__init__.py` | Python パッケージ |

*最終更新: 2026-01-19 | AgentFlow リポジトリ構造標準*
