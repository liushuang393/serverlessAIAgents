#!/usr/bin/env python3
"""Fail when non-gateway code imports provider SDKs directly.

Allowed:
- agentflow/llm/gateway/**
- tests/**
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]

TARGET_DIRS = [ROOT / "agentflow", ROOT / "apps"]

ALLOWED_PATH_PREFIXES = [
    ROOT / "agentflow" / "llm" / "gateway",
]

IMPORT_PATTERN = re.compile(
    r"^\s*(from\s+(openai|anthropic|google\.genai|google\.generativeai)\b|"
    r"import\s+(openai|anthropic|google\.genai|google\.generativeai)\b|"
    r"from\s+google\s+import\s+genai\b)",
    re.MULTILINE,
)


def _is_allowed(path: Path) -> bool:
    if "tests" in path.parts:
        return True
    if path.name == "__init__.py" and "templates" in path.parts:
        return True
    return any(path.is_relative_to(prefix) for prefix in ALLOWED_PATH_PREFIXES)


def find_violations() -> list[tuple[Path, int, str]]:
    """Collect all forbidden imports."""
    violations: list[tuple[Path, int, str]] = []

    for target_dir in TARGET_DIRS:
        if not target_dir.exists():
            continue
        for file_path in target_dir.rglob("*.py"):
            if _is_allowed(file_path):
                continue
            text = file_path.read_text(encoding="utf-8")
            for match in IMPORT_PATTERN.finditer(text):
                line_number = text.count("\n", 0, match.start()) + 1
                line = text.splitlines()[line_number - 1].strip()
                violations.append((file_path, line_number, line))

    return violations


def main() -> int:
    violations = find_violations()
    if not violations:
        return 0

    print("Direct provider SDK imports are forbidden outside gateway layer:")
    for file_path, line_number, line in violations:
        rel = file_path.relative_to(ROOT)
        print(f"- {rel}:{line_number}: {line}")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
