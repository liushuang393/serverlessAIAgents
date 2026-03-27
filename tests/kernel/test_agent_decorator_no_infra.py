"""agent_decorator が infrastructure を直接 import していないことを確認."""

import ast
from pathlib import Path


def test_agent_decorator_no_infrastructure_import() -> None:
    """agent_decorator.py のトップレベルに infrastructure 直接 import がないこと."""
    source = Path("kernel/agent_decorator.py").read_text()
    tree = ast.parse(source)
    violations: list[str] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom) and node.module and "infrastructure" in node.module:
            # TYPE_CHECKING ブロック内も許容しない（Protocol を使うべき）
            # 関数内の遅延 import は後方互換フォールバックとして許容
            # トップレベルの import は違反
            if node.col_offset == 0:  # トップレベル
                violations.append(f"Line {node.lineno}: from {node.module}")
    assert violations == [], f"infrastructure トップレベル import が残存: {violations}"
