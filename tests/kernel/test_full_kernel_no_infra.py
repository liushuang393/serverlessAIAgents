"""kernel/ 全体のトップレベルに infrastructure 直接 import がないことを確認."""

import ast
from pathlib import Path


# plugins/packs/ は外部プラグインのため除外
EXCLUDE_PREFIXES = {"kernel/plugins/packs"}


def test_full_kernel_no_top_level_infrastructure_import() -> None:
    """kernel/ 全体のトップレベルに infrastructure 直接 import がゼロ."""
    violations: list[str] = []
    for py_file in Path("kernel").rglob("*.py"):
        rel = str(py_file)
        if any(rel.startswith(e) for e in EXCLUDE_PREFIXES):
            continue
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
    assert violations == [], f"infrastructure トップレベル import が {len(violations)} 箇所残存:\n" + "\n".join(
        violations
    )
