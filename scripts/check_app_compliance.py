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
  python scripts/check_app_compliance.py
  python scripts/check_app_compliance.py --json
  python scripts/check_app_compliance.py --app faq_system
"""

from __future__ import annotations

import argparse
import ast
import json
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
APPS_DIR = ROOT / "apps"

REQUIRED_CONFIG_FIELDS = {"name", "version", "agents"}
FRAMEWORK_MODULES = {
    "kernel",
    "harness",
    "infrastructure",
    "shared",
    "contracts",
    "domain",
}


def check_app(app_dir: Path) -> dict:
    """単一 app のコンプライアンスを検証."""
    name = app_dir.name
    result: dict = {"name": name, "issues": []}

    # 1. app_config.json
    config_path = app_dir / "app_config.json"
    result["app_config_exists"] = config_path.exists()
    if config_path.exists():
        try:
            config = json.loads(config_path.read_text(encoding="utf-8"))
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
    framework_imports: set[str] = set()
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
    result["type_coverage"] = round(typed_funcs / total_funcs * 100, 1) if total_funcs > 0 else 0.0

    # 5. テスト存在
    has_test_dir = any((app_dir / d).is_dir() for d in ("tests", "test"))
    has_test_files = any(app_dir.rglob("test_*.py"))
    # tests/ ルートにも app 名のテストがあるか
    root_tests = list((ROOT / "tests").rglob(f"*{name}*"))
    result["has_tests"] = has_test_dir or has_test_files or len(root_tests) > 0
    if not result["has_tests"]:
        result["issues"].append("テストが存在しない")

    # 6. README
    result["has_readme"] = (app_dir / "README.md").exists() or (app_dir / "README_JA.md").exists()
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
        "--strict",
        action="store_true",
        help="1件でも issue があれば exit 1",
    )
    args = parser.parse_args()

    app_dirs = sorted(d for d in APPS_DIR.iterdir() if d.is_dir() and not d.name.startswith(("_", ".")))
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
        print(f"{'App':<35} {'Config':>6} {'Entry':>5} {'FW':>4} {'Type%':>5} {'Test':>4} {'README':>6} {'Status':>8}")
        print("-" * 85)
        for name, r in results.items():
            status = "OK" if r["compliant"] else "NG"
            print(
                f"{name:<35} "
                f"{'Y' if r['app_config_exists'] else 'N':>6} "
                f"{'Y' if r['has_entry_point'] else 'N':>5} "
                f"{'Y' if r['uses_framework'] else 'N':>4} "
                f"{r['type_coverage']:>5.1f} "
                f"{'Y' if r['has_tests'] else 'N':>4} "
                f"{'Y' if r['has_readme'] else 'N':>6} "
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
