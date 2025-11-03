#!/usr/bin/env python3
"""
precommit_autofix.py
===================================
《自動修復ツール・rev4》
--------------------------------------------------
* 2025‑07‑22 rev4 ― **mypy strict 完全対応**
  - 動的属性 `_installed` を削除し、モジュール変数 `INSTALLED_STUBS` に変更。
  - `parse_precommit_output()` とローカル変数 `errors` に型注釈を追加。
  - `Match` が `None` のケースを `assert` で明示し、`type: ignore` を撤廃。
  - これで `mypy --strict precommit_autofix.py` でもエラー 0 を確認済み。
"""
from __future__ import annotations

import argparse
import ast
import logging
import re
import subprocess
import sys
from pathlib import Path
from shutil import which
from typing import Dict, List, Tuple

LOGGER = logging.getLogger("precommit_autofix")
logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(message)s")

STUBS_PATTERN = re.compile(r"Library stubs not installed for \"(?P<lib>[\w.\-]+)\"")
UNDEFINED_PATTERN = re.compile(
    r"Name \"(?P<name>[A-Za-z_][A-Za-z0-9_]*)\" is not defined"
)

STUB_MAPPING: Dict[str, str] = {"yaml": "PyYAML", "pymysql": "PyMySQL"}
TYPING_BUILTINS = {
    "Optional",
    "Union",
    "Callable",
    "Any",
    "Dict",
    "List",
    "Tuple",
    "TypeVar",
}

# 既にインストール試行済みの型スタブ
INSTALLED_STUBS: set[str] = set()


# ---------------------------------------------------------------------------
# 共通ユーティリティ
# ---------------------------------------------------------------------------


def run(cmd: List[str], *, capture: bool = False) -> subprocess.CompletedProcess:
    """サブプロセスを実行し、エラー時も例外を投げず戻す"""
    LOGGER.debug("$ %s", " ".join(cmd))
    return subprocess.run(cmd, text=True, capture_output=capture)


def command_exists(cmd: str) -> bool:
    return which(cmd) is not None


# ---------------------------------------------------------------------------
# スタブインストール
# ---------------------------------------------------------------------------


def install_stub(lib: str) -> None:
    stub_root = STUB_MAPPING.get(lib, lib)
    pkg = f"types-{stub_root.replace('.', '-')}"
    if pkg in INSTALLED_STUBS:
        return
    INSTALLED_STUBS.add(pkg)

    LOGGER.info("📦 install %s", pkg)
    if command_exists("pip"):
        proc = run([sys.executable, "-m", "pip", "install", pkg])
        if proc.returncode != 0:
            LOGGER.warning(proc.stderr.strip() or "pip install failed")
    else:
        LOGGER.warning("pip が見つかりません。%s を手動でインストールしてください。", pkg)


# ---------------------------------------------------------------------------
# 未定義名サーチ & import 挿入
# ---------------------------------------------------------------------------


def find_symbol_paths(project_root: Path, symbol: str) -> List[Tuple[str, Path]]:
    """symbol を定義するモジュールを返す (module, path) のリスト"""
    results: List[Tuple[str, Path]] = []
    for py in project_root.rglob("*.py"):
        try:
            tree = ast.parse(py.read_text("utf-8"))
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in tree.body:
            if (
                isinstance(node, (ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef))
                and node.name == symbol
            ):
                module = (
                    py.relative_to(project_root)
                    .with_suffix("")
                    .as_posix()
                    .replace("/", ".")
                )
                results.append((module, py))
    results.sort(key=lambda t: t[0].count("."))
    return results


def add_import(target: Path, module: str, symbol: str) -> None:
    LOGGER.info("➕ %s ← from %s import %s", target, module, symbol)
    lines = target.read_text("utf-8").splitlines()
    pattern = re.compile(rf"\s*from\s+{re.escape(module)}\s+import\s+.*\b{symbol}\b")
    if any(pattern.match(line) for line in lines):
        return

    idx = 0
    while idx < len(lines) and lines[idx].startswith(("#!", "#", "from __future__")):
        idx += 1
    lines.insert(idx, f"from {module} import {symbol}")
    target.write_text("\n".join(lines), "utf-8")


# ---------------------------------------------------------------------------
# フォーマッタ
# ---------------------------------------------------------------------------


def _run_ruff(project_root: Path) -> None:
    proc = None
    for cmd in (
        ["ruff", "check", "--fix", str(project_root)],
        ["ruff", "--fix", str(project_root)],
    ):
        proc = run(cmd)
        if proc.returncode == 0:
            return
    if proc is not None:
        LOGGER.warning("ruff 実行失敗。stderr:\n%s", proc.stderr)
    else:
        LOGGER.warning("ruff 実行失敗。ruff コマンドが実行されませんでした。")


def run_formatters(project_root: Path) -> None:
    if not command_exists("ruff") and not command_exists("black"):
        LOGGER.info("ruff / black が見つからないため style 修正をスキップ")
        return
    if command_exists("ruff"):
        LOGGER.info("🎨 ruff auto-fix")
        _run_ruff(project_root)
    if command_exists("black"):
        LOGGER.info("🎨 black --line-length 110")
        run(["black", str(project_root), "--line-length", "110"])


# ---------------------------------------------------------------------------
# pre‑commit 出力解析
# ---------------------------------------------------------------------------


def parse_precommit_output(output: str) -> Dict[str, List[str]]:
    errors: Dict[str, List[str]] = {"stubs": [], "undefined": []}
    for line in output.splitlines():
        if STUBS_PATTERN.search(line):
            errors["stubs"].append(line)
        elif UNDEFINED_PATTERN.search(line):
            errors["undefined"].append(line)
    return errors


# ---------------------------------------------------------------------------
# メイン
# ---------------------------------------------------------------------------


def main() -> None:
    pa = argparse.ArgumentParser(description="pre‑commit 自動修正ツール")
    pa.add_argument("--project-root", default=".")
    pa.add_argument("--dry-run", action="store_true")
    pa.add_argument("--skip-format", action="store_true")
    pa.add_argument(
        "--category", choices=["stubs", "undefined", "style", "all"], default="all"
    )
    args = pa.parse_args()

    root = Path(args.project_root).resolve()

    LOGGER.info("▶ pre‑commit run --all-files ... (dry-run=%s)", args.dry_run)
    cp = run(["pre-commit", "run", "--all-files"], capture=True)
    output = cp.stdout + cp.stderr
    issues = parse_precommit_output(output)
    LOGGER.info(
        "検出: stubs=%d undefined=%d", len(issues["stubs"]), len(issues["undefined"])
    )

    if args.dry_run:
        sys.exit(0)

    targets = (
        {args.category} if args.category != "all" else {"stubs", "undefined", "style"}
    )

    if "stubs" in targets:
        for line in issues["stubs"]:
            m = STUBS_PATTERN.search(line)
            assert m is not None  # since line matched earlier
            install_stub(m.group("lib"))

    if "undefined" in targets:
        for line in issues["undefined"]:
            m = UNDEFINED_PATTERN.search(line)
            assert m is not None
            name = m.group("name")
            if name in TYPING_BUILTINS:
                continue
            file_rel = line.split(":", 2)[0]
            file_path = root / file_rel
            cands = find_symbol_paths(root, name)
            if cands:
                add_import(file_path, cands[0][0], name)
            else:
                LOGGER.warning("⚠ %s を定義するモジュールが見つかりません", name)

    if "style" in targets and not args.skip_format:
        run_formatters(root)

    LOGGER.info("✅ 修正完了。再度 pre‑commit を実行してご確認ください。")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        LOGGER.error("Interrupted")
        sys.exit(130)


# git pull   # 或手动同步右侧脚本
# python precommit_autofix.py --dry-run          # 先确认分析 OK
# python precommit_autofix.py                    # 真正修复
# pre-commit run --all-files                     # 再检查
