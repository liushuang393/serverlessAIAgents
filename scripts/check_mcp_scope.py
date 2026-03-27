"""scripts/check_mcp_scope.py — MCP が integration 層を超えていないかチェック."""

from __future__ import annotations

import argparse
import ast
import json
import sys
from pathlib import Path


ALLOWED_DIRS = {
    "kernel/protocols",
    "contracts/protocol",
    "apps",
    "control_plane",
    "tests",
    "plugins",
}


def check_mcp_scope(root: Path) -> list[str]:
    """MCP スコープ違反を検出."""
    violations: list[str] = []
    for py_file in root.rglob("*.py"):
        rel = str(py_file.relative_to(root))
        if any(rel.startswith(d) for d in ALLOWED_DIRS):
            continue
        if "__pycache__" in rel or ".venv" in rel:
            continue
        try:
            source = py_file.read_text()
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom) and node.module and "mcp" in node.module.lower():
                violations.append(f"{rel}:{node.lineno}: from {node.module}")
    return violations


def main() -> int:
    """メインエントリポイント."""
    parser = argparse.ArgumentParser(description="MCP スコープチェッカー")
    parser.add_argument("--root", default=".")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    violations = check_mcp_scope(Path(args.root))
    if args.json:
        print(json.dumps({"violations": violations, "count": len(violations)}))
    else:
        for v in violations:
            print(f"❌ {v}")
        if not violations:
            print("✅ MCP スコープチェック: 全ファイル OK")
    return 1 if violations else 0


if __name__ == "__main__":
    sys.exit(main())
