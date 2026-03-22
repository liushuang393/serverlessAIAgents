# AI Harness 検証体系 & CI/CD 強化 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** AI コーディング助手と開発者がアーキテクチャルールを確実に遵守していることを自動検証し、CI/CD で強制するシステムを構築する

**Architecture:** 既存の `scripts/check_layer_boundaries.py` と `scripts/check_no_direct_provider_calls.py` を基盤に、(1) App 品質検証ツール、(2) ルールコンプライアンス総合チェッカー、(3) pre-commit フック統合、(4) CI/CD 明示的統合を追加する。すべて Python スクリプト + pytest テストで実装し、`check.sh` と GitHub Actions に統合する。

**Tech Stack:** Python 3.13+, ast module, pytest, GitHub Actions, pre-commit, ruff, mypy

---

## 現状分析サマリ

### 既に存在するもの (✅)
| ツール | ファイル | 統合先 |
|--------|----------|--------|
| レイヤー境界チェック | `scripts/check_layer_boundaries.py` | check.sh lint, pytest |
| プロバイダ直接 import 禁止 | `scripts/check_no_direct_provider_calls.py` | check.sh lint, pytest |
| 境界テスト (pytest) | `tests/contracts/test_layer_boundaries.py` | pytest |
| 境界スクリプト統合テスト | `tests/integration/boundary/test_boundary_scripts.py` | pytest |

### 不足しているもの (❌)
| ギャップ | 影響 |
|----------|------|
| pre-commit に境界チェック hook なし | ローカルコミット時に違反が素通り |
| GitHub Actions で境界スクリプト未直接呼出 | CI が pytest 経由の間接検出のみ |
| App 品質検証ツールなし | 各 app のフレームワーク活用度が不明 |
| ルールコンプライアンス総合レポートなし | AI/人間が全ルール遵守しているか一覧不能 |
| code-rules にアーキ検証ツールの記載なし | 開発者がツールの存在を知らない |

### Apps Harness 活用状況
| App | フレームワーク活用 | レイヤー準拠 | テスト | 品質 |
|-----|-------------------|-------------|--------|------|
| code_migration_assistant | ✅ PipelineEngine + @agent | ✅ | ✅ | Good |
| decision_governance_engine | ✅ PipelineEngine + ResilientAgent | ✅ | ✅ | Good |
| faq_system | ✅ SimpleEngine + AgentBlock + RAG | ✅ | ✅ | Good |
| market_trend_monitor | ✅ PipelineEngine + create_flow | ✅ | ✅ | Good |
| messaging_hub | ✅ Coordinator + ResilientAgent | ✅ | ✅ | Good |
| design_skills_engine | ✅ Re-export (kernel) | ✅ | ❌ テストなし | Partial |
| legacy_modernization_geo_platform | ✅ Supervisor 11 agents | ✅ | ✅ | Moderate |
| orchestration_guardian | ⚠️ SimpleEngine 最小 | ✅ | ❌ テストなし | Minimal |
| auth_service | ❌ モデルのみ | ❌ 構造不足 | ❌ | Critical |
| dev_studio | ❌ プレースホルダー | ❌ 不完全 | ❌ | Critical |

---

## ファイル構造

### 新規作成
```
scripts/
  check_app_compliance.py       # App 品質 & フレームワーク活用度チェッカー
  check_rules_compliance.py     # ルールコンプライアンス総合レポーター
tests/
  integration/boundary/
    test_app_compliance.py      # App コンプライアンスの pytest ラッパー
    test_rules_compliance.py    # ルールコンプライアンスの pytest ラッパー
```

### 修正
```
.pre-commit-config.yaml         # 境界チェック + App コンプライアンス hook 追加
.github/workflows/quality-gate.yml  # 明示的な境界 & コンプライアンスステップ追加
check.sh                        # App コンプライアンスチェック統合
code-rules/CLAUDE.md            # アーキ検証ツール一覧セクション追加
code-rules/project/ci-cd-guidelines.md  # 検証パイプライン記載更新
```

---

## Task 1: App コンプライアンスチェッカー作成

**目的:** 各 app がフレームワーク規約を守っているかを自動検証するスクリプト

**Files:**
- Create: `scripts/check_app_compliance.py`
- Test: `tests/integration/boundary/test_app_compliance.py`

### 検証項目
1. `app_config.json` 存在 & 必須フィールド (`name`, `version`, `agents`)
2. エントリーポイント (`main.py` or `__init__.py`) 存在
3. フレームワーク import パターン (kernel/harness/infrastructure からの import)
4. LLM 直接 import 禁止 (既存スクリプトと同じルール)
5. 型アノテーション率 (関数定義の引数・戻り値)
6. テストディレクトリ or テストファイル存在
7. README.md 存在

- [ ] **Step 1: テストを書く**

```python
# tests/integration/boundary/test_app_compliance.py
"""App コンプライアンスチェッカーのテスト."""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts" / "check_app_compliance.py"


def test_app_compliance_script_runs_without_crash() -> None:
    """スクリプトがクラッシュせずに実行できること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    # 終了コードは 0(全合格) or 1(違反あり) のみ
    assert result.returncode in (0, 1), f"Unexpected exit: {result.stderr}"


def test_app_compliance_known_good_apps() -> None:
    """成熟した app は全項目パスすること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json", "--app", "faq_system"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    import json
    report = json.loads(result.stdout)
    faq = report["apps"]["faq_system"]
    assert faq["app_config_exists"] is True
    assert faq["has_entry_point"] is True
    assert faq["has_tests"] is True


def test_app_compliance_known_bad_apps() -> None:
    """構造不足の app が正しく非準拠と判定されること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json", "--app", "auth_service"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    import json
    report = json.loads(result.stdout)
    auth = report["apps"]["auth_service"]
    assert auth["compliant"] is False
    assert len(auth["issues"]) > 0
```

- [ ] **Step 2: テストが失敗することを確認**

Run: `conda run -n agentflow pytest tests/integration/boundary/test_app_compliance.py -v`
Expected: FAIL (スクリプト未存在)

- [ ] **Step 3: check_app_compliance.py を実装**

```python
#!/usr/bin/env python3
"""各 app のフレームワーク活用度 & 品質コンプライアンスを検証する.

検証項目:
  1. app_config.json 存在 & 必須フィールド
  2. エントリーポイント存在
  3. フレームワーク import (kernel/harness/infrastructure)
  4. 型アノテーション率
  5. テスト存在
  6. README.md 存在

Usage:
  python scripts/check_app_compliance.py              # 全 app チェック (テーブル出力)
  python scripts/check_app_compliance.py --json        # JSON 出力
  python scripts/check_app_compliance.py --app faq_system  # 特定 app のみ
"""
from __future__ import annotations

import argparse
import ast
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
APPS_DIR = ROOT / "apps"

REQUIRED_CONFIG_FIELDS = {"name", "version", "agents"}
FRAMEWORK_MODULES = {"kernel", "harness", "infrastructure", "shared", "contracts", "domain"}


def check_app(app_dir: Path) -> dict:
    """単一 app のコンプライアンスを検証."""
    name = app_dir.name
    result: dict = {"name": name, "issues": []}

    # 1. app_config.json
    config_path = app_dir / "app_config.json"
    result["app_config_exists"] = config_path.exists()
    if config_path.exists():
        try:
            config = json.loads(config_path.read_text())
            missing = REQUIRED_CONFIG_FIELDS - set(config.keys())
            result["config_missing_fields"] = sorted(missing)
            if missing:
                result["issues"].append(f"app_config.json に必須フィールド不足: {missing}")
        except json.JSONDecodeError:
            result["issues"].append("app_config.json が不正な JSON")
    else:
        result["issues"].append("app_config.json が存在しない")

    # 2. エントリーポイント
    has_main = (app_dir / "main.py").exists()
    has_init = (app_dir / "__init__.py").exists()
    result["has_entry_point"] = has_main or has_init
    if not result["has_entry_point"]:
        result["issues"].append("エントリーポイント (main.py / __init__.py) なし")

    # 3. フレームワーク import
    framework_imports = set()
    py_files = list(app_dir.rglob("*.py"))
    for py_file in py_files:
        try:
            tree = ast.parse(py_file.read_text(encoding="utf-8"))
        except SyntaxError:
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module:
                top = node.module.split(".")[0]
                if top in FRAMEWORK_MODULES:
                    framework_imports.add(top)
    result["framework_imports"] = sorted(framework_imports)
    result["uses_framework"] = len(framework_imports) > 0
    if not result["uses_framework"]:
        result["issues"].append("フレームワーク (kernel/harness等) を利用していない")

    # 4. 型アノテーション率
    total_funcs = 0
    typed_funcs = 0
    for py_file in py_files:
        try:
            tree = ast.parse(py_file.read_text(encoding="utf-8"))
        except SyntaxError:
            continue
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                total_funcs += 1
                has_return = node.returns is not None
                args = node.args
                all_args = args.args + args.posonlyargs + args.kwonlyargs
                # self/cls は除外
                non_self = [a for a in all_args if a.arg not in ("self", "cls")]
                all_typed = all(a.annotation is not None for a in non_self)
                if has_return and all_typed:
                    typed_funcs += 1
    result["total_functions"] = total_funcs
    result["typed_functions"] = typed_funcs
    result["type_coverage"] = (
        round(typed_funcs / total_funcs * 100, 1) if total_funcs > 0 else 0.0
    )

    # 5. テスト存在
    has_test_dir = any(
        (app_dir / d).is_dir() for d in ("tests", "test")
    )
    has_test_files = any(app_dir.rglob("test_*.py"))
    # tests/ ルートにも app 名のテストがあるか
    root_tests = list((ROOT / "tests").rglob(f"*{name}*"))
    result["has_tests"] = has_test_dir or has_test_files or len(root_tests) > 0
    if not result["has_tests"]:
        result["issues"].append("テストが存在しない")

    # 6. README
    result["has_readme"] = (app_dir / "README.md").exists() or (
        app_dir / "README_JA.md"
    ).exists()
    if not result["has_readme"]:
        result["issues"].append("README.md が存在しない")

    result["compliant"] = len(result["issues"]) == 0
    return result


def main() -> int:
    """メインエントリポイント."""
    parser = argparse.ArgumentParser(description="App コンプライアンスチェッカー")
    parser.add_argument("--json", action="store_true", help="JSON 出力")
    parser.add_argument("--app", type=str, help="特定 app のみチェック")
    parser.add_argument(
        "--strict", action="store_true",
        help="1件でも issue があれば exit 1",
    )
    args = parser.parse_args()

    app_dirs = sorted(
        d for d in APPS_DIR.iterdir()
        if d.is_dir() and not d.name.startswith(("_", "."))
    )
    if args.app:
        app_dirs = [d for d in app_dirs if d.name == args.app]

    results = {}
    for app_dir in app_dirs:
        report = check_app(app_dir)
        results[report["name"]] = report

    if args.json:
        print(json.dumps({"apps": results}, indent=2, ensure_ascii=False))
    else:
        # テーブル出力
        print(f"{'App':<35} {'Config':>6} {'Entry':>5} {'FW':>4} "
              f"{'Type%':>5} {'Test':>4} {'README':>6} {'Status':>8}")
        print("-" * 85)
        for name, r in results.items():
            status = "✅ OK" if r["compliant"] else "❌ NG"
            print(
                f"{name:<35} "
                f"{'✅' if r['app_config_exists'] else '❌':>6} "
                f"{'✅' if r['has_entry_point'] else '❌':>5} "
                f"{'✅' if r['uses_framework'] else '❌':>4} "
                f"{r['type_coverage']:>5.1f} "
                f"{'✅' if r['has_tests'] else '❌':>4} "
                f"{'✅' if r['has_readme'] else '❌':>6} "
                f"{status:>8}"
            )
        if not all(r["compliant"] for r in results.values()):
            print("\n--- Issues ---")
            for name, r in results.items():
                if r["issues"]:
                    for issue in r["issues"]:
                        print(f"  [{name}] {issue}")

    has_issues = any(not r["compliant"] for r in results.values())
    if args.strict and has_issues:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 4: テストが通ることを確認**

Run: `conda run -n agentflow pytest tests/integration/boundary/test_app_compliance.py -v`
Expected: PASS

- [ ] **Step 5: 手動実行で結果を確認**

Run: `conda run -n agentflow python scripts/check_app_compliance.py`
Expected: 全 app のテーブル表示、auth_service と dev_studio に issue

- [ ] **Step 6: コミット**

```bash
git add scripts/check_app_compliance.py tests/integration/boundary/test_app_compliance.py
git commit -m "feat: Add app compliance checker for framework usage validation"
```

---

## Task 2: ルールコンプライアンス総合チェッカー作成

**目的:** CLAUDE.md の全ルールカテゴリについて遵守状況を集計する統合レポーター

**Files:**
- Create: `scripts/check_rules_compliance.py`
- Test: `tests/integration/boundary/test_rules_compliance.py`

### 検証項目
1. レイヤー境界違反数 (既存スクリプト呼出)
2. プロバイダ直接 import 違反数 (既存スクリプト呼出)
3. ファイルサイズ違反 (1000行超)
4. モジュールインポート数違反 (20 import 超)
5. `# type: ignore` 使用数 (理由なし)
6. `Any` 型使用数 (理由コメントなし)
7. `cast()` 使用数

NOTE: App コンプライアンス (Task 1) は独立ツールとして CI で別途実行する。
本スクリプトは「コードレベル」のルール遵守に集中し、「App レベル」は分離する。

- [ ] **Step 1: テストを書く**

```python
# tests/integration/boundary/test_rules_compliance.py
"""ルールコンプライアンス総合チェッカーのテスト."""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts" / "check_rules_compliance.py"


def test_rules_compliance_script_runs() -> None:
    """スクリプトがクラッシュせず実行できること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    assert result.returncode in (0, 1), f"Unexpected: {result.stderr}"


def test_rules_compliance_has_all_categories() -> None:
    """レポートに全カテゴリが含まれること."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--json"],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    import json
    report = json.loads(result.stdout)
    expected_keys = {
        "layer_boundary_violations",
        "provider_direct_imports",
        "file_size_violations",
        "type_ignore_without_reason",
    }
    assert expected_keys.issubset(set(report.keys()))
```

- [ ] **Step 2: テスト失敗確認**

Run: `conda run -n agentflow pytest tests/integration/boundary/test_rules_compliance.py -v`
Expected: FAIL

- [ ] **Step 3: check_rules_compliance.py を実装**

```python
#!/usr/bin/env python3
"""ルールコンプライアンス総合レポーター.

CLAUDE.md の主要ルールカテゴリごとに違反数を集計し、
CI/CD 品質ゲートとして機能する。

Usage:
  python scripts/check_rules_compliance.py              # テーブル出力
  python scripts/check_rules_compliance.py --json        # JSON 出力
  python scripts/check_rules_compliance.py --strict      # 閾値超過で exit 1
"""
from __future__ import annotations

import argparse
import ast
import json
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

# スキャン対象ディレクトリ
SCAN_DIRS = [
    "contracts", "infrastructure", "shared", "kernel",
    "harness", "control_plane", "domain", "apps",
]

# 閾値設定
THRESHOLDS = {
    "layer_boundary_violations": 25,      # 現行許容値
    "provider_direct_imports": 0,         # ゼロ許容
    "file_size_violations": 5,            # 1000行超ファイル数
    "excessive_imports": 10,              # 20 import 超ファイル数
    "type_ignore_without_reason": 20,     # 理由なし type: ignore
    "bare_any_usage": 50,                 # 理由なし Any 使用
    "cast_usage": 10,                     # cast() 使用数
}


def count_lines(filepath: Path) -> int:
    """ファイルの行数を数える."""
    return len(filepath.read_text(encoding="utf-8").splitlines())


def scan_python_files() -> list[Path]:
    """スキャン対象の Python ファイルを収集."""
    files = []
    for d in SCAN_DIRS:
        dir_path = ROOT / d
        if dir_path.is_dir():
            files.extend(dir_path.rglob("*.py"))
    return sorted(files)


def check_file_sizes(files: list[Path]) -> list[dict]:
    """1000行超のファイルを検出."""
    violations = []
    for f in files:
        lines = count_lines(f)
        if lines > 1000:
            violations.append({
                "file": str(f.relative_to(ROOT)),
                "lines": lines,
            })
    return violations


def check_excessive_imports(files: list[Path]) -> list[dict]:
    """20 import 超のファイルを検出."""
    violations = []
    for f in files:
        try:
            tree = ast.parse(f.read_text(encoding="utf-8"))
        except SyntaxError:
            continue
        import_count = sum(
            1 for node in ast.iter_child_nodes(tree)
            if isinstance(node, (ast.Import, ast.ImportFrom))
        )
        if import_count > 20:
            violations.append({
                "file": str(f.relative_to(ROOT)),
                "import_count": import_count,
            })
    return violations


def check_type_ignore(files: list[Path]) -> list[dict]:
    """理由なし # type: ignore を検出.

    NOTE: `# type: ignore[code]` はエラーコードのみで理由説明ではないため、
    こちらも違反とする。許容するのは `# type: ignore[code]  # 理由説明` の形式のみ。
    """
    # 裸の type: ignore (コード指定なし) を検出
    bare_pattern = re.compile(r"#\s*type:\s*ignore\s*$")
    # コード指定ありだが理由コメントなし: # type: ignore[code] のみで終わる
    code_only_pattern = re.compile(r"#\s*type:\s*ignore\[[^\]]+\]\s*$")
    violations = []
    for f in files:
        for i, line in enumerate(f.read_text(encoding="utf-8").splitlines(), 1):
            if bare_pattern.search(line) or code_only_pattern.search(line):
                violations.append({
                    "file": str(f.relative_to(ROOT)),
                    "line": i,
                    "content": line.strip(),
                })
    return violations


def check_bare_any(files: list[Path]) -> list[dict]:
    """理由コメントなしの Any 型使用を検出.

    Any を使う場合は同一行に理由コメント (5文字以上) が必要:
      field: Any  # 外部 API の戻り値が不定のため
    """
    violations = []
    for f in files:
        try:
            source = f.read_text(encoding="utf-8")
            tree = ast.parse(source)
        except SyntaxError:
            continue
        lines = source.splitlines()
        for node in ast.walk(tree):
            if isinstance(node, ast.Name) and node.id == "Any":
                lineno = node.lineno
                line_text = lines[lineno - 1] if lineno <= len(lines) else ""
                # コメント部分を抽出
                if "#" in line_text:
                    comment = line_text.split("#", 1)[1].strip()
                    # 5文字以上の理由コメントがあれば OK
                    if len(comment) >= 5:
                        continue
                # コメントなし or 理由不足 → 違反
                violations.append({
                    "file": str(f.relative_to(ROOT)),
                    "line": lineno,
                })
    return violations


def check_cast_usage(files: list[Path]) -> list[dict]:
    """cast() の使用を検出."""
    violations = []
    for f in files:
        try:
            tree = ast.parse(f.read_text(encoding="utf-8"))
        except SyntaxError:
            continue
        for node in ast.walk(tree):
            if (
                isinstance(node, ast.Call)
                and isinstance(node.func, ast.Name)
                and node.func.id == "cast"
            ):
                violations.append({
                    "file": str(f.relative_to(ROOT)),
                    "line": node.lineno,
                })
    return violations


def run_existing_script(script_name: str) -> int:
    """既存の境界チェックスクリプトを実行して違反数を取得.

    既存スクリプトの出力形式: "Total: N violation(s)"
    """
    script = ROOT / "scripts" / script_name
    if not script.exists():
        return -1
    result = subprocess.run(
        [sys.executable, str(script)],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    if result.returncode == 0:
        return 0
    # 出力から "Total: N violation(s)" パターンを探す
    total_pattern = re.compile(r"Total:\s*(\d+)\s+violation")
    for line in (result.stdout + result.stderr).splitlines():
        m = total_pattern.search(line)
        if m:
            return int(m.group(1))
    # パターンが見つからない場合、exit code が非ゼロなら 1 として扱う
    return 1 if result.returncode != 0 else 0


def main() -> int:
    """メインエントリポイント."""
    parser = argparse.ArgumentParser(description="ルールコンプライアンス総合レポーター")
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--strict", action="store_true")
    args = parser.parse_args()

    files = scan_python_files()

    # 各チェックを1回だけ実行して結果をキャッシュ
    file_size_results = check_file_sizes(files)
    excessive_import_results = check_excessive_imports(files)

    report = {
        "layer_boundary_violations": run_existing_script("check_layer_boundaries.py"),
        "provider_direct_imports": run_existing_script("check_no_direct_provider_calls.py"),
        "file_size_violations": len(file_size_results),
        "file_size_details": file_size_results,
        "excessive_imports": len(excessive_import_results),
        "excessive_imports_details": excessive_import_results,
        "type_ignore_without_reason": len(check_type_ignore(files)),
        "bare_any_usage": len(check_bare_any(files)),
        "cast_usage": len(check_cast_usage(files)),
        "total_files_scanned": len(files),
    }

    # 閾値チェック
    over_threshold = {}
    for key, threshold in THRESHOLDS.items():
        value = report.get(key, 0)
        if isinstance(value, int) and value > threshold:
            over_threshold[key] = {"value": value, "threshold": threshold}
    report["over_threshold"] = over_threshold

    if args.json:
        # 詳細を除外したサマリのみ
        summary = {k: v for k, v in report.items() if not k.endswith("_details")}
        print(json.dumps(summary, indent=2, ensure_ascii=False))
    else:
        print("=" * 60)
        print("  ルールコンプライアンス総合レポート")
        print("=" * 60)
        print(f"  スキャンファイル数: {report['total_files_scanned']}")
        print()
        for key, threshold in THRESHOLDS.items():
            value = report.get(key, 0)
            status = "✅" if value <= threshold else "❌"
            print(f"  {status} {key}: {value} (閾値: {threshold})")
        print()
        if over_threshold:
            print("  --- 閾値超過 ---")
            for key, info in over_threshold.items():
                print(f"  ❌ {key}: {info['value']} > {info['threshold']}")

    if args.strict and over_threshold:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 4: テスト通過確認**

Run: `conda run -n agentflow pytest tests/integration/boundary/test_rules_compliance.py -v`
Expected: PASS

- [ ] **Step 5: 手動実行で結果確認**

Run: `conda run -n agentflow python scripts/check_rules_compliance.py`
Expected: 全カテゴリのサマリテーブル表示

- [ ] **Step 6: コミット**

```bash
git add scripts/check_rules_compliance.py tests/integration/boundary/test_rules_compliance.py
git commit -m "feat: Add rules compliance reporter for comprehensive quality gate"
```

---

## Task 3: pre-commit に境界チェック hook を追加

**目的:** ローカルコミット時に境界違反を防止

**Files:**
- Modify: `.pre-commit-config.yaml`

- [ ] **Step 1: 追加する hook を確認**

以下の local hooks を `.pre-commit-config.yaml` の Python セクション末尾に追加:

```yaml
  # ========================================
  # Python: アーキテクチャ境界チェック
  # ========================================
  - repo: local
    hooks:
      # NOTE: language: system は $PATH 上の python を使う。
      # conda 環境の python を確実に使うため entry に conda run を指定。
      # pre-commit 自体が language: python の場合は default_language_version が効くが、
      # language: system では効かないため明示的に conda run を使う。
      - id: check-layer-boundaries
        name: "Arch: Layer boundary check"
        language: system
        entry: conda run --no-banner -n agentflow python scripts/check_layer_boundaries.py
        pass_filenames: false
        types: [python]
        always_run: true

      - id: check-no-direct-provider
        name: "Arch: Provider SDK direct import check"
        language: system
        entry: conda run --no-banner -n agentflow python scripts/check_no_direct_provider_calls.py
        pass_filenames: false
        types: [python]
        always_run: true
```

- [ ] **Step 2: .pre-commit-config.yaml を編集**

Ruff セクションの後、Type Checking (disabled) セクションの前に上記を挿入する。

- [ ] **Step 3: pre-commit が動くことを確認**

Run: `conda run -n agentflow pre-commit run check-layer-boundaries --all-files`
Expected: PASS (or known violations within threshold)

Run: `conda run -n agentflow pre-commit run check-no-direct-provider --all-files`
Expected: PASS

- [ ] **Step 4: コミット**

```bash
git add .pre-commit-config.yaml
git commit -m "feat: Add architecture boundary checks to pre-commit hooks"
```

---

## Task 4: GitHub Actions quality-gate.yml に明示的境界チェックステップを追加

**目的:** CI/CD で境界スクリプトを直接呼び出し、pytest 間接検出への依存を排除

**Files:**
- Modify: `.github/workflows/quality-gate.yml`

- [ ] **Step 1: 追加するステップを確認**

Lint (Ruff) と Type check の間に以下を挿入:

```yaml
      - name: Architecture boundary check
        run: |
          python scripts/check_layer_boundaries.py
          python scripts/check_no_direct_provider_calls.py

      - name: App compliance check
        run: python scripts/check_app_compliance.py

      - name: Rules compliance check
        run: python scripts/check_rules_compliance.py --json
```

- [ ] **Step 2: quality-gate.yml を編集**

```yaml
name: Quality Gate

on:
  pull_request:
    branches: [main, develop]

jobs:
  quality-gate:
    name: Quality Gate (lint + arch + type + security + test)
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v6
        with:
          python-version: "3.13"

      - name: Install dependencies
        run: pip install -e ".[apps,dev]"

      - name: Lint (Ruff)
        run: ruff check .

      - name: Architecture boundary check
        run: |
          python scripts/check_layer_boundaries.py
          python scripts/check_no_direct_provider_calls.py

      - name: App compliance check
        run: python scripts/check_app_compliance.py

      - name: Rules compliance report
        run: python scripts/check_rules_compliance.py

      - name: Type check (MyPy)
        run: mypy contracts infrastructure shared kernel harness control_plane domain apps tests --strict --ignore-missing-imports

      - name: Security scan (bandit HIGH)
        run: bandit -r contracts infrastructure shared kernel harness control_plane domain apps -lll -iii

      - name: Tests (pytest)
        run: pytest --tb=short -q

      - name: Generate check report on failure
        if: failure()
        run: |
          echo "# Quality Gate Failure Report" > check-report.md
          echo "Generated: $(date)" >> check-report.md
          echo "" >> check-report.md
          python scripts/check_rules_compliance.py >> check-report.md 2>&1 || true
          python scripts/check_app_compliance.py >> check-report.md 2>&1 || true
          ruff check . >> check-report.md 2>&1 || true

      - name: Upload failure report
        if: failure()
        uses: actions/upload-artifact@v4
        with:
          name: check-report
          path: check-report.md
          if-no-files-found: ignore
```

- [ ] **Step 3: lint.yml にも境界チェックを追加**

NOTE: 既存 lint.yml は `actions/setup-python@v6` を使用 (quality-gate は v5 だったが v6 に統一)。

`.github/workflows/lint.yml` の `Run Ruff (lint)` ステップの後に以下を挿入:

```yaml
      - name: Architecture boundary check
        run: |
          python scripts/check_layer_boundaries.py
          python scripts/check_no_direct_provider_calls.py
```

- [ ] **Step 4: コミット**

```bash
git add .github/workflows/quality-gate.yml .github/workflows/lint.yml
git commit -m "feat: Add explicit architecture & compliance checks to CI/CD"
```

---

## Task 5: check.sh に App コンプライアンスチェック統合

**目的:** ローカル `./check.sh all` で App コンプライアンスもチェックされるようにする

**Files:**
- Modify: `check.sh`

- [ ] **Step 1: check.sh の do_lint() 関数末尾に追加**

`do_lint()` 内の既存境界チェックの後に:

```bash
    # App コンプライアンスチェック
    printf "${BLUE}[App Compliance]${NC} Checking app framework usage...\\n"
    run_py_tool python scripts/check_app_compliance.py
    local app_rc=$?
    if [ $app_rc -ne 0 ]; then
        printf "${YELLOW}[App Compliance] Some apps have compliance issues (non-blocking)${NC}\\n"
    fi
```

- [ ] **Step 2: do_report() にルールコンプライアンスを追加**

```bash
    # ルールコンプライアンスレポート
    printf "${BLUE}[Rules Compliance]${NC} Generating compliance report...\\n"
    run_py_tool python scripts/check_rules_compliance.py
```

- [ ] **Step 3: 実行確認**

Run: `./check.sh lint`
Expected: 既存チェック + App Compliance チェックが表示

- [ ] **Step 4: コミット**

```bash
git add check.sh
git commit -m "feat: Integrate app compliance check into check.sh"
```

---

## Task 6: code-rules/CLAUDE.md にアーキ検証ツール一覧を追加

**目的:** AI/開発者が検証ツールの存在と使い方を把握できるようにする

**Files:**
- Modify: `code-rules/CLAUDE.md`

- [ ] **Step 1: 「使用ツール」セクションに追加**

既存の「リンター & フォーマッター」の後に:

```markdown
- [アーキテクチャ検証ツール](tools/architecture-validation.md)
```

- [ ] **Step 2: tools/architecture-validation.md を新規作成**

```markdown
# アーキテクチャ検証ツール

## 概要

本プロジェクトでは以下の自動検証ツールで 7コア層 + Apps のアーキテクチャルールを強制する。

## ツール一覧

| ツール | スクリプト | 検証内容 | 統合先 |
|--------|-----------|---------|--------|
| レイヤー境界チェック | `scripts/check_layer_boundaries.py` | 7層の import 方向ルール | check.sh, pre-commit, CI |
| プロバイダ直接 import 禁止 | `scripts/check_no_direct_provider_calls.py` | LLM SDK 直接 import 禁止 | check.sh, pre-commit, CI |
| App コンプライアンス | `scripts/check_app_compliance.py` | 各 app のフレームワーク活用度 | check.sh, CI |
| ルールコンプライアンス | `scripts/check_rules_compliance.py` | CLAUDE.md ルール総合遵守 | check.sh, CI |

## 実行方法

### 一括実行
./check.sh all

### 個別実行
conda run -n agentflow python scripts/check_layer_boundaries.py
conda run -n agentflow python scripts/check_no_direct_provider_calls.py
conda run -n agentflow python scripts/check_app_compliance.py
conda run -n agentflow python scripts/check_app_compliance.py --json --app faq_system
conda run -n agentflow python scripts/check_rules_compliance.py
conda run -n agentflow python scripts/check_rules_compliance.py --json --strict

## CI/CD 統合

### pre-commit (ローカル)
- `check-layer-boundaries`: コミット前にレイヤー境界違反を検出
- `check-no-direct-provider`: コミット前にプロバイダ直接 import を検出

### GitHub Actions (CI)
- `quality-gate.yml`: Lint → **Architecture boundary** → **App compliance** → **Rules compliance** → Type check → Security → Tests
- 境界違反は pytest 経由の間接検出ではなく、スクリプト直接呼出で明示的にチェック

## 閾値設定

`scripts/check_rules_compliance.py` の `THRESHOLDS` で管理:

| カテゴリ | 閾値 | 説明 |
|---------|------|------|
| layer_boundary_violations | 25 | レイヤー境界違反数 (段階的に 0 へ) |
| provider_direct_imports | 0 | プロバイダ直接 import (即時ゼロ) |
| file_size_violations | 5 | 1000行超ファイル数 |
| type_ignore_without_reason | 20 | 理由なし type: ignore |
| cast_usage | 10 | cast() 使用数 |

## AI コーディング助手向け

**コード生成・編集後に以下を確認すること:**

1. `conda run -n agentflow python scripts/check_layer_boundaries.py` → 新規違反なし
2. 新規ファイルの import が上位層→下位層の方向のみ
3. LLM SDK は `infrastructure.llm` 経由のみ
4. 型アノテーション率 100% を目標
```

- [ ] **Step 3: コミット**

```bash
git add code-rules/CLAUDE.md code-rules/tools/architecture-validation.md
git commit -m "docs: Add architecture validation tools documentation to code-rules"
```

---

## Task 7: code-rules/project/ci-cd-guidelines.md にアーキ検証パイプラインを追記

**目的:** CI/CD ガイドラインにアーキ検証ステージを正式に記載

**Files:**
- Modify: `code-rules/project/ci-cd-guidelines.md`

- [ ] **Step 1: ci-cd-guidelines.md を読む**

Run: 対象ファイルの「品質ゲート」セクションを確認

- [ ] **Step 2: アーキテクチャ検証ステージを追記**

品質ゲートの既存ステージ一覧に以下を追加:

```markdown
### アーキテクチャ検証ステージ (Architecture Validation)

**実行タイミング:** Lint 後、Type check 前
**ツール:**
- `python scripts/check_layer_boundaries.py` - 7層境界ルール
- `python scripts/check_no_direct_provider_calls.py` - プロバイダ隔離
- `python scripts/check_app_compliance.py` - App フレームワーク準拠
- `python scripts/check_rules_compliance.py` - ルール総合遵守

**ブロッキング条件:**
- レイヤー境界違反 > 25 (段階的に 0 へ削減)
- プロバイダ直接 import > 0 (即時ブロック)

**非ブロッキング (レポートのみ):**
- App コンプライアンス (改善追跡用)
- ルールコンプライアンス (トレンド監視)
```

- [ ] **Step 3: コミット**

```bash
git add code-rules/project/ci-cd-guidelines.md
git commit -m "docs: Add architecture validation stage to CI/CD guidelines"
```

---

## 実行順序サマリ

```
Task 1 → Task 2 → Task 3 → Task 4 → Task 5 → Task 6 → Task 7
 (app)   (rules)  (pre-    (CI/CD)  (check   (CLAUDE  (CI/CD
 checker  checker  commit)           .sh)     .md)     docs)
```

Task 1-2 は独立して並行実装可能。Task 3-7 は Task 1-2 の成果物に依存。

---

## 検証パイプライン完成形

```
Developer/AI writes code
         │
         ▼
   pre-commit hooks
   ├── ruff format/lint
   ├── check-layer-boundaries    ← NEW
   └── check-no-direct-provider  ← NEW
         │
         ▼
   git commit
         │
         ▼
   git push → PR
         │
         ▼
   GitHub Actions quality-gate.yml
   ├── Lint (Ruff)
   ├── Architecture boundary     ← NEW (explicit)
   ├── App compliance            ← NEW
   ├── Rules compliance report   ← NEW
   ├── Type check (MyPy)
   ├── Security scan (bandit)
   └── Tests (pytest)
         │
         ▼
   ✅ Merge allowed / ❌ Blocked
```
