"""削除済み legacy import/path の再導入を検知する静的ガード。"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Final


ROOT_DIR: Final[Path] = Path(__file__).resolve().parents[1]
SCAN_ROOTS: Final[tuple[str, ...]] = ("apps", "control_plane", "shared", "tests", "scripts")
EXCLUDED_DIR_NAMES: Final[frozenset[str]] = frozenset(
    {
        ".git",
        ".mypy_cache",
        ".pytest_cache",
        ".ruff_cache",
        ".venv",
        "__pycache__",
        "dist",
        "docs",
        "htmlcov",
        "migration_output",
        "node_modules",
        "playwright-report",
        "test-results",
    }
)
SCANNED_SUFFIXES: Final[frozenset[str]] = frozenset(
    {".py", ".pyi", ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".sh", ".template"}
)
EXCLUDED_RELATIVE_PATHS: Final[frozenset[str]] = frozenset(
    {
        "scripts/check_no_apps_platform_dependencies.py",
        "tests/unit/test_no_apps_platform_dependencies.py",
    }
)
FORBIDDEN_PATTERNS: Final[tuple[tuple[str, re.Pattern[str]], ...]] = (
    (
        "apps.platform import",
        re.compile(r"\b(?:from|import)\s+apps\.platform(?:\.[A-Za-z_][A-Za-z0-9_]*)*"),
    ),
    (
        "platform import",
        re.compile(r"\b(?:from|import)\s+platform\.[A-Za-z_][A-Za-z0-9_\.]*"),
    ),
    (
        "legacy app path",
        re.compile(r"apps" + r"/" + r"platform"),
    ),
    (
        "agentflow import",
        re.compile(r"\b(?:from|import)\s+agentflow(?:\.[A-Za-z_][A-Za-z0-9_\.]*)?"),
    ),
    (
        "invalid kernel.core.agent_block import",
        re.compile(r"\b(?:from|import)\s+kernel\.core\.agent_block(?:\.[A-Za-z_][A-Za-z0-9_]*)*"),
    ),
)
FORBIDDEN_PATHS: Final[tuple[Path, ...]] = (
    ROOT_DIR / "apps" / "platform",
    ROOT_DIR / "tests" / "apps" / "platform",
)


def _should_scan(path: Path) -> bool:
    """検査対象ファイルか判定する。"""
    if path.suffix not in SCANNED_SUFFIXES:
        return False

    relative_path = path.relative_to(ROOT_DIR).as_posix()
    if relative_path in EXCLUDED_RELATIVE_PATHS:
        return False

    return not any(part in EXCLUDED_DIR_NAMES for part in path.relative_to(ROOT_DIR).parts)


def _scan_file(path: Path) -> list[str]:
    """1ファイルを走査して違反を返す。"""
    content = path.read_text(encoding="utf-8", errors="ignore")
    relative_path = path.relative_to(ROOT_DIR).as_posix()
    violations: list[str] = []

    for label, pattern in FORBIDDEN_PATTERNS:
        for match in pattern.finditer(content):
            line_number = content.count("\n", 0, match.start()) + 1
            violations.append(f"{relative_path}:{line_number}: forbidden {label}")

    return violations


def find_violations() -> list[str]:
    """削除済み legacy import/path の残存箇所を返す。"""
    violations: list[str] = []

    for forbidden_path in FORBIDDEN_PATHS:
        if forbidden_path.exists():
            relative_path = forbidden_path.relative_to(ROOT_DIR).as_posix()
            violations.append(f"{relative_path}: forbidden legacy path exists")

    for root_name in SCAN_ROOTS:
        root_path = ROOT_DIR / root_name
        if not root_path.exists():
            continue

        for path in root_path.rglob("*"):
            if path.is_file() and _should_scan(path):
                violations.extend(_scan_file(path))

    return sorted(violations)
