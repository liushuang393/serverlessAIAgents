"""scripts/check_encoding.py — ファイルエンコーディングチェッカー."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


UTF8_BOM = b"\xef\xbb\xbf"
BOM_ALLOWED_EXTENSIONS = {".ps1"}
TARGET_EXTENSIONS = {
    ".py",
    ".md",
    ".yaml",
    ".yml",
    ".json",
    ".toml",
    ".ts",
    ".tsx",
    ".js",
    ".jsx",
    ".css",
    ".html",
    ".sh",
    ".ps1",
    ".txt",
    ".cfg",
    ".ini",
}
EXCLUDE_DIRS = {"node_modules", ".venv", "__pycache__", ".git", "dist", "htmlcov"}


def check_file(path: Path) -> str | None:
    """ファイルのエンコーディングを検証."""
    try:
        raw = path.read_bytes()
    except (OSError, PermissionError):
        return None
    if raw.startswith(UTF8_BOM) and path.suffix not in BOM_ALLOWED_EXTENSIONS:
        return f"{path}: UTF-8 BOM 検出（BOM なしに変更してください）"
    try:
        raw.decode("utf-8")
    except UnicodeDecodeError:
        return f"{path}: UTF-8 デコード不可"
    return None


def main() -> int:
    """メインエントリポイント."""
    parser = argparse.ArgumentParser(description="ファイルエンコーディングチェッカー")
    parser.add_argument("paths", nargs="*", default=["."])
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    violations: list[str] = []
    for target in args.paths:
        p = Path(target)
        files = (
            [p]
            if p.is_file()
            else (
                f
                for f in p.rglob("*")
                if f.is_file() and f.suffix in TARGET_EXTENSIONS and not any(ex in f.parts for ex in EXCLUDE_DIRS)
            )
        )
        for f in files:
            issue = check_file(f)
            if issue:
                violations.append(issue)

    if args.json:
        print(json.dumps({"violations": violations, "count": len(violations)}))
    else:
        for v in violations:
            print(f"❌ {v}")
        if not violations:
            print("✅ エンコーディングチェック: 全ファイル OK")
    return 1 if violations else 0


if __name__ == "__main__":
    sys.exit(main())
