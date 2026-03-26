"""shared/integrations/ が kernel を直接トップレベル import していないことを確認."""
import ast
from pathlib import Path


def test_shared_integrations_no_kernel_top_level_import() -> None:
    """shared/integrations/ のトップレベルに kernel import がないこと."""
    violations: list[str] = []
    for py_file in Path("shared/integrations").rglob("*.py"):
        try:
            source = py_file.read_text()
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in ast.walk(tree):
            if (
                isinstance(node, ast.ImportFrom)
                and node.module
                and node.module.startswith("kernel")
                and node.col_offset == 0
            ):
                violations.append(f"{py_file}:{node.lineno}: from {node.module}")
    assert violations == [], f"kernel 逆依存:\n" + "\n".join(violations)
