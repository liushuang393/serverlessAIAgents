"""shared/gateway/ のトップレベルに infrastructure import がないことを確認."""

import ast
from pathlib import Path


def test_shared_gateway_no_top_level_infrastructure() -> None:
    """トップレベル(col_offset==0) の infrastructure import を検出する."""
    violations: list[str] = []
    for py_file in Path("shared/gateway").rglob("*.py"):
        try:
            source = py_file.read_text()
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in ast.walk(tree):
            if (
                isinstance(node, ast.ImportFrom)
                and node.module
                and "infrastructure" in node.module
                and node.col_offset == 0
            ):
                violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], "violations:\n" + "\n".join(violations)
