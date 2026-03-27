"""kernel/skills/ 配下のトップレベルに infrastructure 直接 import がないことを確認."""

import ast
from pathlib import Path


def _is_inside_function(tree: ast.Module, target_lineno: int) -> bool:
    """target_lineno が関数/メソッド本体内にあるかを判定."""
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            # 関数本体の行範囲内かチェック
            if node.body:
                start = node.body[0].lineno
                end = node.body[-1].end_lineno or node.body[-1].lineno
                if start <= target_lineno <= end:
                    return True
    return False


def test_skills_no_top_level_infrastructure_import() -> None:
    """kernel/skills/ のトップレベルに infrastructure 直接 import がないこと.

    トップレベル import と TYPE_CHECKING ブロック内 import の両方を検出する。
    関数/メソッド内の遅延 import は許容する。
    """
    violations: list[str] = []
    for py_file in Path("kernel/skills").rglob("*.py"):
        try:
            source = py_file.read_text()
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module and "infrastructure" in node.module:
                # 関数/メソッド内の遅延 import は許容
                if _is_inside_function(tree, node.lineno):
                    continue
                violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], f"infrastructure import が {len(violations)} 箇所残存:\n" + "\n".join(violations)
