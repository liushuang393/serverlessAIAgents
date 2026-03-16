#!/usr/bin/env python3
"""新しい六層パッケージの import 境界を検証する."""

from __future__ import annotations

import ast
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]

LAYER_ROOTS = {
    "contracts": ROOT / "contracts",
    "infrastructure": ROOT / "infrastructure",
    "shared": ROOT / "shared",
    "kernel": ROOT / "kernel",
    "harness": ROOT / "harness",
    "control_plane": ROOT / "control_plane",
}

FORBIDDEN_IMPORTS: dict[str, set[str]] = {
    "infrastructure": {"shared", "kernel", "harness", "control_plane", "apps"},
    "shared": {"kernel", "harness", "control_plane", "apps"},
    "kernel": {"harness", "control_plane", "apps"},
    "harness": {"control_plane", "apps"},
    "control_plane": set(),
}


def _module_name(path: Path) -> str | None:
    for layer_name, layer_root in LAYER_ROOTS.items():
        if not path.is_relative_to(layer_root):
            continue
        relative = path.relative_to(ROOT).with_suffix("")
        parts = relative.parts
        if parts[-1] == "__init__":
            parts = parts[:-1]
        return ".".join(parts)
    return None


def _imported_roots(tree: ast.AST) -> list[tuple[int, str]]:
    roots: list[tuple[int, str]] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                roots.append((node.lineno, alias.name.split(".")[0]))
        elif isinstance(node, ast.ImportFrom):
            if node.module:
                roots.append((node.lineno, node.module.split(".")[0]))
    return roots


def find_violations() -> list[tuple[str, int, str]]:
    """禁止依存違反を集計する."""
    violations: list[tuple[str, int, str]] = []
    for layer_name, layer_root in LAYER_ROOTS.items():
        forbidden = FORBIDDEN_IMPORTS.get(layer_name)
        if not forbidden:
            continue
        for path in layer_root.rglob("*.py"):
            module_name = _module_name(path)
            if module_name is None:
                continue
            tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
            for line_number, imported_root in _imported_roots(tree):
                if imported_root not in forbidden:
                    continue
                violations.append((str(path.relative_to(ROOT)), line_number, imported_root))
    return violations


def main() -> int:
    violations = find_violations()
    if not violations:
        return 0

    print("Layer boundary violations detected:")
    for path, line_number, imported_root in violations:
        print(f"- {path}:{line_number}: forbidden import root '{imported_root}'")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
