"""7コア層 + apps 外層の import 境界テスト."""

from __future__ import annotations

import ast
import subprocess
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]

# --- 移行中 allowlist（移行完了時に空にする） ---
# フォーマット: "relative/path:lineno:imported_root"
ALLOWLIST: set[str] = set()


def test_no_layer_boundary_violations() -> None:
    """scripts/check_layer_boundaries.py が違反ゼロを返すことを検証."""
    result = subprocess.run(
        [sys.executable, str(ROOT / "scripts" / "check_layer_boundaries.py")],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    if result.returncode == 0:
        return

    # allowlist でフィルタ
    remaining: list[str] = []
    for line in result.stdout.splitlines():
        line = line.strip()
        if not line or line.startswith(("Layer boundary", "Total:", "No layer")):
            continue
        # "  path:lineno: forbidden import root 'xxx'" をパース
        key = line.lstrip()
        if key and key not in ALLOWLIST:
            remaining.append(key)

    if not remaining:
        return

    msg = "Layer boundary violations (not in allowlist):\n"
    for v in remaining:
        msg += f"  {v}\n"
    pytest.fail(msg)


def test_no_agentflow_imports_in_layers() -> None:
    """7コア層ディレクトリが agentflow.* を eager import していないことを検証."""

    layer_dirs = ["contracts", "infrastructure", "shared", "kernel", "harness", "domain", "control_plane"]
    violations: list[str] = []

    for layer in layer_dirs:
        layer_path = ROOT / layer
        if not layer_path.exists():
            continue
        for py_file in layer_path.rglob("*.py"):
            try:
                source = py_file.read_text(encoding="utf-8")
                tree = ast.parse(source)
            except (SyntaxError, UnicodeDecodeError, OSError):
                continue

            for node in ast.walk(tree):
                if isinstance(node, ast.ImportFrom) and node.module:
                    if node.module.split(".")[0] == "agentflow":
                        # TYPE_CHECKING / 関数内は許容
                        if _is_top_level_eager(node, tree):
                            rel = py_file.relative_to(ROOT)
                            violations.append(f"{rel}:{node.lineno}: {node.module}")
                elif isinstance(node, ast.Import):
                    for alias in node.names:
                        if alias.name.split(".")[0] == "agentflow":
                            if _is_top_level_eager(node, tree):
                                rel = py_file.relative_to(ROOT)
                                violations.append(f"{rel}:{node.lineno}: {alias.name}")

    if violations:
        msg = "Layers still import from kernel.*:\n"
        for v in violations:
            msg += f"  {v}\n"
        pytest.fail(msg)


def _is_top_level_eager(node: ast.AST, tree: ast.Module) -> bool:
    """ノードがトップレベル eager import かどうかを判定."""
    # TYPE_CHECKING ブロック内チェック
    for top in ast.iter_child_nodes(tree):
        if isinstance(top, ast.If):
            test = top.test
            is_tc = (isinstance(test, ast.Name) and test.id == "TYPE_CHECKING") or (
                isinstance(test, ast.Attribute) and test.attr == "TYPE_CHECKING"
            )
            if is_tc:
                for child in ast.walk(top):
                    if child is node:
                        return False

    # 関数内チェック
    for top in ast.walk(tree):
        if isinstance(top, (ast.FunctionDef, ast.AsyncFunctionDef)):
            for child in ast.walk(top):
                if child is node:
                    return False

    return True
