# リンター & フォーマッター

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow 全 Python コード
> **最終更新**: 2026-01-19

## 📋 目次

1. [Ruff 設定](#ruff-設定)
2. [Mypy 設定](#mypy-設定)
3. [Pre-commit 設定](#pre-commit-設定)
4. [CI/CD 統合](#cicd-統合)
5. [自動修正スクリプト](#自動修正スクリプト)
6. [品質チェック](#品質チェック)

---

## ⚙️ Ruff 設定

Ruff を統一のリンターおよびフォーマッターとして使用します。

### pyproject.toml 設定
```toml
[tool.ruff]
# 基本設定
line-length = 100
target-version = "py313"
src = ["agentflow"]

[tool.ruff.lint]
# 有効にするルール
select = [
    "E",      # pycodestyle errors
    "W",      # pycodestyle warnings
    "F",      # pyflakes
    "I",      # isort
    "N",      # pep8-naming
    "UP",     # pyupgrade
    "BLE",    # blind-except
    "TRY",    # try-except patterns
    "FBT",    # boolean-trap
    "B",      # bugbear
    "A",      # builtin-attribute-shadowing
    "COM",    # comman
    "C4",     # complexity
    "DTZ",    # datetimez
    "T10",    # debugger
    "DJ",     # django
    "EM",     # error-messages
    "EXE",    # executable
    "FA",     # future-annotations
    "ISC",    # implicit-str-concat
    "ICN",    # import-conventions
    "G",      # logging
    "INP",    # input
    "PIE",    # pie
    "T20",    # print
    "PYI",    # pyi
    "PT",     # pytest
    "Q",      # quotes
    "RSE",    # raise
    "RET",    # return
    "SLF",    # self
    "SLOT",   # slots
    "SIM",    # simplify
    "TID",    # tidy
    "TCH",    # type-checking
    "INT",    # typing
    "ARG",    # unused-argument
    "PTH",    # use-pathlib
    "ERA",    # eradicate
    "PD",     # pandas
    "PGH",    # pygrep-hooks
    "PL",     # pylint
    "TRY",    # tryceratops
    "FLY",    # flynt
    "NPY",    # numpy
    "AIR",    # airflow
    "PERF",   # perflint
    "FURB",   # refurb
    "LOG",    # logging
    "RUF",    # ruff
]

# 除外するルール
ignore = [
    "E501",     # line too long (formatter が処理)
    "COM812",   # trailing comma missing (formatter が処理)
    "ISC001",   # implicit-str-concat (formatter が処理)
]

[tool.ruff.lint.per-file-ignores]
# ファイルごとの除外設定
"__init__.py" = ["F401"]  # unused imports in __init__.py
"tests/**/*" = [
    "S101",     # assert used
    "ARG001",   # unused function argument
    "PLR2004",  # magic value comparison
    "FBT001",   # boolean positional arg in function definition
    "FBT003",   # boolean positional value in function call
]

[tool.ruff.lint.isort]
known-first-party = ["agentflow"]
force-sort-within-sections = true
split-on-trailing-comma = true

[tool.ruff.lint.flake8-tidy-imports]
ban-relative-imports = "all"

[tool.ruff.lint.flake8-bugbear]
extend-immutable-calls = ["fastapi.Depends", "fastapi.Query"]

[tool.ruff.format]
quote-style = "double"
indent-style = "space"
skip-magic-trailing-comma = false
line-ending = "auto"
docstring-code-format = true
docstring-code-line-length = 80
```

### Ruff 使用方法
```bash
# チェックのみ
ruff check .

# 自動修正可能な問題を修正
ruff check . --fix

# フォーマット適用
ruff format .

# フォーマットチェック（CI用）
ruff format . --check

# 特定のルールのみチェック
ruff check . --select F401  # unused imports

# 特定のファイルを除外
ruff check . --exclude "tests/fixtures/*"
```

---

## 🧬 Mypy 設定

Mypy を型チェッカーとして使用します。

### pyproject.toml 設定
```toml
[tool.mypy]
python_version = "3.10"
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
show_error_codes = true

# パッケージごとの設定
[[tool.mypy.overrides]]
module = "agentflow.*"
disallow_untyped_defs = true
disallow_incomplete_defs = true

[[tool.mypy.overrides]]
module = "tests.*"
disallow_untyped_defs = false
disallow_incomplete_defs = false
check_untyped_defs = false

# サードパーティライブラリのスタブ
[[tool.mypy.overrides]]
module = [
    "aiofiles",
    "pydantic",
    "structlog",
    "fastapi",
    "uvicorn",
]
ignore_missing_imports = true
```

### Mypy 使用方法
```bash
# 型チェック実行
mypy agentflow

# 特定のファイルのみチェック
mypy agentflow/core/interfaces/code_generator.py

# エラーレポート生成
mypy agentflow --html-report mypy-report

# 設定ファイルの検証
mypy --config-file pyproject.toml --show-config
```

---

## 🔗 Pre-commit 設定

Pre-commit をコミット前の品質チェックに使用します。

### .pre-commit-config.yaml
```yaml
repos:
  # Ruff (高速Pythonリンター)
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.1.0
    hooks:
      - id: ruff
        args: [--fix]
      - id: ruff-format

  # Mypy (型チェッカー)
  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.7.0
    hooks:
      - id: mypy
        additional_dependencies: [types-all]
        args: [--config-file=pyproject.toml]

  # シークレットスキャン
  - repo: https://github.com/Yelp/detect-secrets
    rev: v1.4.0
    hooks:
      - id: detect-secrets
        args: ['--baseline', '.secrets.baseline']

  # YAML フォーマット
  - repo: https://github.com/pre-commit/mirrors-prettier
    rev: v4.0.0-alpha.8
    hooks:
      - id: prettier
        types: [yaml]
        files: \.(yaml|yml)$

  # コミットメッセージチェック
  - repo: https://github.com/commitizen-tools/commitizen
    rev: v3.12.0
    hooks:
      - id: cz-check
        stages: [commit-msg]

  # 一般的なチェック
  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.5.0
    hooks:
      - id: trailing-whitespace
      - id: end-of-file-fixer
      - id: check-yaml
      - id: check-added-large-files
      - id: check-merge-conflict
      - id: debug-statements
      - id: check-ast
```

### Pre-commit 使用方法
```bash
# インストール
pre-commit install

# 全フック実行
pre-commit run --all-files

# 特定のフック実行
pre-commit run ruff --all-files

# コミット時に自動実行される
git commit -m "feat: add new feature"
```

---

## 🔄 CI/CD 統合

### GitHub Actions ワークフロー
```yaml
# .github/workflows/ci.yml
name: CI

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

jobs:
  quality:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        python-version: ['3.10', '3.11', '3.12', '3.13']

    steps:
    - uses: actions/checkout@v4

    - name: Setup Python ${{ matrix.python-version }}
      uses: actions/setup-python@v4
      with:
        python-version: ${{ matrix.python-version }}

    - name: Cache dependencies
      uses: actions/cache@v3
      with:
        path: ~/.cache/pre-commit
        key: pre-commit-${{ matrix.python-version }}-${{ hashFiles('.pre-commit-config.yaml') }}

    - name: Run pre-commit hooks
      run: |
        pre-commit run --all-files --show-diff-on-failure

    - name: Run tests with coverage
      run: |
        pip install -e ".[dev]"
        pytest --cov=agentflow --cov-report=xml --cov-report=term-missing

    - name: Upload coverage
      uses: codecov/codecov-action@v3
      with:
        file: ./coverage.xml
```

### 品質ゲート設定
```yaml
# 品質チェックジョブ
quality-gate:
  runs-on: ubuntu-latest
  needs: [test, quality]

  steps:
  - name: Quality gate check
    run: |
      # カバレッジチェック
      COVERAGE=$(python -c "import xml.etree.ElementTree as ET; print(float(ET.parse('coverage.xml').getroot().attrib['line-rate']) * 100)")
      if (( $(echo "$COVERAGE < 80" | bc -l) )); then
        echo "❌ カバレッジが80%未満: ${COVERAGE}%"
        exit 1
      fi

      # リントチェック
      if ! ruff check .; then
        echo "❌ リントチェック失敗"
        exit 1
      fi

      # 型チェック
      if ! mypy agentflow; then
        echo "❌ 型チェック失敗"
        exit 1
      fi

      echo "✅ 品質ゲート通過"
```

---

## 🤖 自動修正スクリプト

### 一括品質改善スクリプト
```bash
#!/bin/bash
# scripts/fix_code_quality.sh

set -e

echo "🔧 コード品質自動修正開始..."

# 1. Ruff 修正
echo "  📏 Ruff 修正実行..."
ruff check . --fix
echo "  ✅ Ruff 修正完了"

# 2. フォーマット適用
echo "  🎨 フォーマット適用..."
ruff format .
echo "  ✅ フォーマット完了"

# 3. インポートソート
echo "  📦 インポートソート..."
isort . --profile black --line-length 100
echo "  ✅ インポートソート完了"

# 4. 型チェック（修正可能なもの）
echo "  🧬 型チェック..."
if mypy agentflow 2>&1 | grep -q "error:"; then
    echo "⚠️  型エラーが残っています。手動修正が必要です。"
else
    echo "  ✅ 型チェック通過"
fi

# 5. テスト実行
echo "  🧪 テスト実行..."
pytest --tb=short -q
echo "  ✅ テスト通過"

echo ""
echo "🎉 自動修正完了！"
echo "   必要に応じて手動修正を行ってください。"
```

### インクリメンタル修正スクリプト
```python
#!/usr/bin/env python3
# scripts/incremental_fix.py

import subprocess
import sys
from pathlib import Path
from typing import List

class IncrementalFixer:
    """段階的なコード修正."""

    def __init__(self):
        self.fixed_files: List[Path] = []

    def fix_step_by_step(self):
        """段階的に修正."""
        print("🔧 インクリメンタル修正開始...")

        # ステップ1: 安全な修正
        print("\n📏 ステップ1: フォーマットと安全な修正...")
        self._run_command(["ruff", "format", "."])
        self._run_command(["ruff", "check", ".", "--fix"])

        # ステップ2: インポート整理
        print("\n📦 ステップ2: インポート整理...")
        self._run_command(["isort", ".", "--profile", "black", "--line-length", "100"])

        # ステップ3: 型チェック
        print("\n🧬 ステップ3: 型チェック...")
        result = subprocess.run(["mypy", "agentflow"], capture_output=True, text=True)
        if result.returncode != 0:
            print("⚠️  型エラーが検出されました:")
            print(result.stdout)
            print("手動修正をお願いします。")
        else:
            print("✅ 型チェック通過")

        # ステップ4: テスト
        print("\n🧪 ステップ4: テスト実行...")
        result = subprocess.run(["pytest", "--tb=short"], capture_output=True, text=True)
        if result.returncode != 0:
            print("⚠️  テスト失敗:")
            print(result.stdout)
            print("テスト修正をお願いします。")
        else:
            print("✅ テスト通過")

        print("
🎉 インクリメンタル修正完了!"        print(f"修正されたファイル数: {len(self.fixed_files)}")

    def _run_command(self, cmd: List[str]):
        """コマンド実行."""
        try:
            result = subprocess.run(cmd, check=True, capture_output=True, text=True)
            print(f"✅ {' '.join(cmd)} 完了")
        except subprocess.CalledProcessError as e:
            print(f"❌ {' '.join(cmd)} 失敗:")
            print(e.stdout)
            print(e.stderr)
            sys.exit(1)

def main():
    fixer = IncrementalFixer()
    fixer.fix_step_by_step()

if __name__ == "__main__":
    main()
```

### 品質ダッシュボード生成
```python
#!/usr/bin/env python3
# scripts/generate_quality_dashboard.py

import json
import subprocess
from pathlib import Path
from datetime import datetime

def generate_quality_dashboard():
    """品質ダッシュボード生成."""

    dashboard = {
        "timestamp": datetime.now().isoformat(),
        "metrics": {}
    }

    # Ruff チェック
    print("📏 Ruff チェック実行...")
    result = subprocess.run(["ruff", "check", ".", "--output-format", "json"],
                          capture_output=True, text=True)
    dashboard["metrics"]["ruff"] = {
        "passed": result.returncode == 0,
        "output": result.stdout if result.returncode != 0 else "OK"
    }

    # Mypy チェック
    print("🧬 Mypy チェック実行...")
    result = subprocess.run(["mypy", "agentflow", "--show-error-codes"],
                          capture_output=True, text=True)
    dashboard["metrics"]["mypy"] = {
        "passed": result.returncode == 0,
        "error_count": result.stdout.count("error:") if result.returncode != 0 else 0,
        "output": result.stdout[:1000] if result.returncode != 0 else "OK"
    }

    # テスト実行
    print("🧪 テスト実行...")
    result = subprocess.run(["pytest", "--cov=agentflow", "--cov-report=json"],
                          capture_output=True, text=True)
    dashboard["metrics"]["tests"] = {
        "passed": result.returncode == 0,
        "output": result.stdout if result.returncode != 0 else "OK"
    }

    # カバレッジ取得
    coverage_file = Path("coverage.json")
    if coverage_file.exists():
        with open(coverage_file) as f:
            coverage_data = json.load(f)
        total_coverage = coverage_data["totals"]["percent_covered"]
        dashboard["metrics"]["coverage"] = {
            "percentage": total_coverage,
            "passed": total_coverage >= 80.0
        }

    # ダッシュボード保存
    dashboard_file = Path("quality-dashboard.json")
    with open(dashboard_file, 'w', encoding='utf-8') as f:
        json.dump(dashboard, f, indent=2, ensure_ascii=False)

    print("✅ 品質ダッシュボード生成完了")

    # サマリー表示
    print("\n📊 品質サマリー:")
    for tool, metrics in dashboard["metrics"].items():
        status = "✅" if metrics.get("passed", False) else "❌"
        print(f"  {status} {tool}: {metrics}")

if __name__ == "__main__":
    generate_quality_dashboard()
```

---

## 🔍 品質チェック

### 品質チェックスクリプト
```bash
#!/bin/bash
# scripts/check_quality.sh

set -e

echo "🔍 包括的品質チェック開始..."

# 設定ファイル存在チェック
echo "  📄 設定ファイルチェック..."
if [ ! -f "pyproject.toml" ]; then
    echo "❌ pyproject.toml が見つかりません"
    exit 1
fi

if [ ! -f ".pre-commit-config.yaml" ]; then
    echo "❌ .pre-commit-config.yaml が見つかりません"
    exit 1
fi

echo "  ✅ 設定ファイルOK"

# 依存関係チェック
echo "  📦 依存関係チェック..."
if ! python -c "import ruff, mypy, pytest"; then
    echo "❌ 必要なツールがインストールされていません"
    echo "   pip install ruff mypy pytest"
    exit 1
fi

echo "  ✅ 依存関係OK"

# Ruff 設定チェック
echo "  ⚙️ Ruff 設定チェック..."
if ! ruff check --config pyproject.toml --help > /dev/null 2>&1; then
    echo "❌ Ruff 設定エラー"
    exit 1
fi

echo "  ✅ Ruff 設定OK"

# Mypy 設定チェック
echo "  🧬 Mypy 設定チェック..."
if ! mypy --config-file pyproject.toml --help > /dev/null 2>&1; then
    echo "❌ Mypy 設定エラー"
    exit 1
fi

echo "  ✅ Mypy 設定OK"

# 品質チェック実行
echo "  🔍 品質チェック実行..."
if ! ruff check .; then
    echo "❌ Ruff チェック失敗"
    exit 1
fi

if ! ruff format . --check; then
    echo "❌ フォーマットチェック失敗"
    exit 1
fi

if ! mypy agentflow; then
    echo "❌ 型チェック失敗"
    exit 1
fi

echo "  ✅ 品質チェックOK"

# テスト実行
echo "  🧪 テスト実行..."
if ! pytest --cov=agentflow --cov-fail-under=80 -q; then
    echo "❌ テスト失敗"
    exit 1
fi

echo "  ✅ テストOK"

echo ""
echo "🎉 全ての品質チェック通過！"
```

### 品質レポート生成
```python
#!/usr/bin/env python3
# scripts/generate_quality_report.py

import json
import subprocess
from pathlib import Path
from datetime import datetime
from typing import Dict, Any

def generate_quality_report():
    """包括的な品質レポート生成."""

    report = {
        "timestamp": datetime.now().isoformat(),
        "project": "AgentFlow",
        "version": "0.4.0",
        "checks": {}
    }

    # 1. コード品質チェック
    report["checks"]["code_quality"] = check_code_quality()

    # 2. テスト品質チェック
    report["checks"]["test_quality"] = check_test_quality()

    # 3. アーキテクチャチェック
    report["checks"]["architecture"] = check_architecture()

    # 4. セキュリティチェック
    report["checks"]["security"] = check_security()

    # レポート保存
    report_file = Path("quality-report.json")
    with open(report_file, 'w', encoding='utf-8') as f:
        json.dump(report, f, indent=2, ensure_ascii=False)

    # HTMLレポート生成
    generate_html_report(report)

    print("✅ 品質レポート生成完了")

def check_code_quality() -> Dict[str, Any]:
    """コード品質チェック."""
    result = {"status": "unknown", "details": {}}

    # Ruff チェック
    try:
        proc = subprocess.run(["ruff", "check", ".", "--output-format", "json"],
                            capture_output=True, text=True, timeout=60)
        result["details"]["ruff"] = {
            "passed": proc.returncode == 0,
            "violations": len(json.loads(proc.stdout)) if proc.stdout else 0
        }
    except Exception as e:
        result["details"]["ruff"] = {"error": str(e)}

    # Mypy チェック
    try:
        proc = subprocess.run(["mypy", "agentflow"], capture_output=True, text=True, timeout=60)
        result["details"]["mypy"] = {
            "passed": proc.returncode == 0,
            "errors": proc.stdout.count("error:") if proc.returncode != 0 else 0
        }
    except Exception as e:
        result["details"]["mypy"] = {"error": str(e)}

    # 全体ステータス
    all_passed = all(details.get("passed", False)
                    for details in result["details"].values()
                    if isinstance(details, dict))
    result["status"] = "passed" if all_passed else "failed"

    return result

def check_test_quality() -> Dict[str, Any]:
    """テスト品質チェック."""
    result = {"status": "unknown", "details": {}}

    try:
        proc = subprocess.run([
            "pytest", "--cov=agentflow", "--cov-report=json",
            "--cov-fail-under=80"
        ], capture_output=True, text=True, timeout=300)

        result["details"]["tests"] = {
            "passed": proc.returncode == 0,
            "output": proc.stdout
        }

        # カバレッジ取得
        coverage_file = Path("coverage.json")
        if coverage_file.exists():
            with open(coverage_file) as f:
                coverage_data = json.load(f)
            result["details"]["coverage"] = coverage_data["totals"]["percent_covered"]

    except Exception as e:
        result["details"]["tests"] = {"error": str(e)}

    result["status"] = "passed" if result["details"].get("tests", {}).get("passed") else "failed"
    return result

def check_architecture() -> Dict[str, Any]:
    """アーキテクチャチェック."""
    result = {"status": "unknown", "details": {}}

    # レイヤー依存チェック
    try:
        proc = subprocess.run(["python", "scripts/validate_layer_dependencies.py"],
                            capture_output=True, text=True, timeout=30)
        result["details"]["layer_dependencies"] = {
            "passed": proc.returncode == 0,
            "output": proc.stdout
        }
    except Exception as e:
        result["details"]["layer_dependencies"] = {"error": str(e)}

    result["status"] = "passed" if result["details"].get("layer_dependencies", {}).get("passed") else "failed"
    return result

def check_security() -> Dict[str, Any]:
    """セキュリティチェック."""
    result = {"status": "unknown", "details": {}}

    # Bandit 実行
    try:
        proc = subprocess.run(["bandit", "-r", "agentflow", "-f", "json"],
                            capture_output=True, text=True, timeout=60)
        bandit_result = json.loads(proc.stdout) if proc.stdout else {}
        result["details"]["bandit"] = {
            "passed": proc.returncode == 0,
            "issues": len(bandit_result.get("results", []))
        }
    except Exception as e:
        result["details"]["bandit"] = {"error": str(e)}

    result["status"] = "passed" if result["details"].get("bandit", {}).get("passed") else "failed"
    return result

def generate_html_report(report: Dict[str, Any]):
    """HTMLレポート生成."""
    html_content = f"""
<!DOCTYPE html>
<html>
<head>
    <title>AgentFlow 品質レポート</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 40px; }}
        .status-passed {{ color: green; }}
        .status-failed {{ color: red; }}
        .status-unknown {{ color: orange; }}
        .section {{ margin: 20px 0; padding: 20px; border: 1px solid #ddd; }}
        .metric {{ margin: 10px 0; }}
    </style>
</head>
<body>
    <h1>AgentFlow 品質レポート</h1>
    <p><strong>生成日時:</strong> {report['timestamp']}</p>
    <p><strong>バージョン:</strong> {report['version']}</p>

    {"".join(generate_section_html(name, data) for name, data in report['checks'].items())}
</body>
</html>
"""

    html_file = Path("quality-report.html")
    with open(html_file, 'w', encoding='utf-8') as f:
        f.write(html_content)

def generate_section_html(name: str, data: Dict[str, Any]) -> str:
    """セクションHTML生成."""
    status_class = f"status-{data.get('status', 'unknown')}"
    status_icon = {"passed": "✅", "failed": "❌", "unknown": "⚠️"}.get(data.get("status"), "❓")

    details_html = ""
    if "details" in data:
        for key, value in data["details"].items():
            if isinstance(value, dict):
                details_html += f"<div class='metric'><strong>{key}:</strong> {value}</div>"
            else:
                details_html += f"<div class='metric'><strong>{key}:</strong> {value}</div>"

    return f"""
    <div class="section">
        <h2 class="{status_class}">{status_icon} {name.replace('_', ' ').title()}</h2>
        <div class="metric"><strong>Status:</strong> <span class="{status_class}">{data.get('status', 'unknown').upper()}</span></div>
        {details_html}
    </div>
    """

if __name__ == "__main__":
    generate_quality_report()
```

---

## 📋 リンター & フォーマッター チートシート

| ツール | 目的 | コマンド | CI |
|--------|------|----------|-----|
| **Ruff** | リント+フォーマット | `ruff check . --fix` | ✅ |
| **Mypy** | 型チェック | `mypy agentflow` | ✅ |
| **Pre-commit** | コミット前チェック | `pre-commit run --all-files` | ✅ |
| **Bandit** | セキュリティ | `bandit -r agentflow` | ✅ |

*最終更新: 2026-01-19 | Ruff + Mypy + Pre-commit 統合*