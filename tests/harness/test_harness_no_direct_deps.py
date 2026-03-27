"""harness/ が kernel/infrastructure をトップレベル import していないことを確認."""

import ast
from pathlib import Path


def test_harness_no_kernel_infrastructure_top_level() -> None:
    violations: list[str] = []
    for py_file in Path("harness").rglob("*.py"):
        try:
            source = py_file.read_text()
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module and node.col_offset == 0:
                top = node.module.split(".")[0]
                if top in ("kernel", "infrastructure"):
                    violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], "直接依存:\n" + "\n".join(violations)
