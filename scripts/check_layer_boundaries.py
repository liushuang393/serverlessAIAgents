#!/usr/bin/env python3
"""新しい六層パッケージの import 境界を検証する.

六層構成:
  L0: contracts (全層が参照可)
  L1: infrastructure
  L2: shared
  L3: kernel
  L4: harness
  L5: platform
  L6: apps

ルール:
  - 各層は自層以下＋ contracts のみ eager import 可
  - TYPE_CHECKING / 関数内ローカルインポートは境界違反に含めない
  - kernel は agentflow への eager import 禁止（TYPE_CHECKING / ローカルは許可）
"""

from __future__ import annotations

import ast
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]

# stdlib モジュール名で六層パッケージ名と衝突するもの（誤検知回避）
_STDLIB_NAMES: set[str] = {"platform"}

# 後方互換shimファイル: 下位層から上位層への re-export のみを行うファイル。
# これらは移行期間中の互換性維持のため意図的に境界を越えるので検査対象外とする。
_SHIM_EXCLUSIONS: set[str] = {
    "infrastructure/providers/unified_tool.py",
    "infrastructure/sandbox/unified_tool.py",
    "infrastructure/security/policy_engine.py",
    "infrastructure/secrets/security/policy_engine.py",
    "infrastructure/secrets/security/__init__.py",
    "shared/rag/isolated_kb.py",
    "shared/rag/rag_access_control.py",
    "shared/knowledge/isolated_kb.py",
}


def _is_stdlib_import(
    imported_root: str, source_path: Path, *, is_bare_import: bool = False
) -> bool:
    """imported_root がプロジェクトパッケージではなく stdlib を指すか判定.

    Args:
        imported_root: インポート先のルートパッケージ名
        source_path: インポート元ファイルのパス
        is_bare_import: ``import platform`` のような bare import かどうか。
            bare import の場合、ソースが同名パッケージ外なら stdlib と判定する。
            ``from platform.xxx import ...`` の場合はプロジェクトパッケージ。
    """
    if imported_root not in _STDLIB_NAMES:
        return False
    # ソースファイルが同名パッケージ内なら自パッケージ import
    project_pkg = ROOT / imported_root
    try:
        source_path.relative_to(project_pkg)
        return False  # 自パッケージ内 → プロジェクト import
    except ValueError:
        pass
    # bare import (例: ``import platform``) はパッケージ外からなら stdlib
    if is_bare_import:
        return True
    # ``from platform.xxx import ...`` はプロジェクトパッケージ
    return not (project_pkg.is_dir() and (project_pkg / "__init__.py").exists())


# 検査対象レイヤー
LAYER_ROOTS: dict[str, Path] = {
    "contracts": ROOT / "contracts",
    "infrastructure": ROOT / "infrastructure",
    "shared": ROOT / "shared",
    "kernel": ROOT / "kernel",
    "harness": ROOT / "harness",
    "platform": ROOT / "platform",
}

# 層ごとの禁止 eager import 先（トップレベルパッケージ名）
FORBIDDEN_IMPORTS: dict[str, set[str]] = {
    "infrastructure": {"shared", "kernel", "harness", "platform", "apps"},
    "shared": {"kernel", "harness", "platform", "apps"},
    "kernel": {"harness", "platform", "apps", "agentflow"},
    "harness": {"platform", "apps"},
    "platform": {"apps"},
}


def _build_parent_map(tree: ast.Module) -> dict[int, ast.AST]:
    """各ノードの id() → 親ノードのマップを1パスで構築する."""
    parent: dict[int, ast.AST] = {}
    for node in ast.walk(tree):
        for child in ast.iter_child_nodes(node):
            parent[id(child)] = node
    return parent


def _is_inside_type_checking(node: ast.AST, parent_map: dict[int, ast.AST]) -> bool:
    """node が TYPE_CHECKING ブロック内かどうかを判定する."""
    cur: ast.AST | None = node
    while cur is not None:
        p = parent_map.get(id(cur))
        if p is None:
            break
        if isinstance(p, ast.If):
            test = p.test
            if (isinstance(test, ast.Name) and test.id == "TYPE_CHECKING") or (
                isinstance(test, ast.Attribute) and test.attr == "TYPE_CHECKING"
            ):
                return True
        cur = p
    return False


def _is_inside_function(node: ast.AST, parent_map: dict[int, ast.AST]) -> bool:
    """node が関数/メソッド内かどうかを判定する."""
    cur: ast.AST | None = node
    while cur is not None:
        p = parent_map.get(id(cur))
        if p is None:
            break
        if isinstance(p, (ast.FunctionDef, ast.AsyncFunctionDef)):
            return True
        cur = p
    return False


def _eager_imported_roots(tree: ast.Module) -> list[tuple[int, str, bool]]:
    """トップレベルの eager import のルートパッケージ名を返す.

    Returns:
        (行番号, ルートパッケージ名, bare import かどうか) のリスト
    """
    parent_map = _build_parent_map(tree)
    roots: list[tuple[int, str, bool]] = []
    for node in ast.walk(tree):
        if not isinstance(node, (ast.Import, ast.ImportFrom)):
            continue
        # TYPE_CHECKING ブロック内 → スキップ
        if _is_inside_type_checking(node, parent_map):
            continue
        # 関数内ローカルインポート → スキップ
        if _is_inside_function(node, parent_map):
            continue
        if isinstance(node, ast.Import):
            for alias in node.names:
                name = alias.name
                is_bare = "." not in name
                roots.append((node.lineno, name.split(".")[0], is_bare))
        elif isinstance(node, ast.ImportFrom) and node.module:
            roots.append((node.lineno, node.module.split(".")[0], False))
    return roots


def find_violations() -> list[tuple[str, int, str]]:
    """禁止依存違反を集計する."""
    violations: list[tuple[str, int, str]] = []
    for layer_name, layer_root in LAYER_ROOTS.items():
        if not layer_root.exists():
            continue
        forbidden = FORBIDDEN_IMPORTS.get(layer_name)
        if not forbidden:
            continue
        for path in layer_root.rglob("*.py"):
            # shimファイルは境界違反検査をスキップ
            rel = str(path.relative_to(ROOT))
            if rel in _SHIM_EXCLUSIONS:
                continue
            try:
                source = path.read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            try:
                tree = ast.parse(source, filename=str(path))
            except SyntaxError:
                continue
            for line_number, imported_root, is_bare in _eager_imported_roots(tree):
                if imported_root not in forbidden:
                    continue
                if _is_stdlib_import(
                    imported_root, path, is_bare_import=is_bare
                ):
                    continue
                violations.append(
                    (str(path.relative_to(ROOT)), line_number, imported_root)
                )
    return violations


def main() -> int:
    """メインエントリポイント."""
    violations = find_violations()
    if not violations:
        print("No layer boundary violations detected.")
        return 0

    print("Layer boundary violations detected:")
    for path, line_number, imported_root in violations:
        print(f"  {path}:{line_number}: forbidden import root '{imported_root}'")
    print(f"\nTotal: {len(violations)} violation(s)")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
