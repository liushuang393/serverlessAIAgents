#!/usr/bin/env python3
"""MyPy のエラーを集計し、コード別・ファイル別のサマリーを出力する。

使用例:
  conda activate agentflow
  python scripts/mypy_error_summary.py

型エラー解消の進捗確認や、check-errors-analysis の分類確認に利用する。
"""

from __future__ import annotations

import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent

# 既知の mypy エラーコード（集計対象）
KNOWN_CODES: set[str] = {
    "call-arg",
    "no-any-return",
    "attr-defined",
    "assignment",
    "union-attr",
    "type-arg",
    "unreachable",
    "arg-type",
    "no-untyped-call",
    "no-untyped-def",
    "valid-type",
    "override",
    "unused-ignore",
    "redundant-cast",
    "var-annotated",
    "return-value",
    "misc",
    "operator",
    "index",
    "list-item",
    "dict-item",
    "has-type",
    "type-var",
    "untyped-decorator",
    "str-bytes-safe",
}


def run_mypy() -> str:
    """mypy agentflow を --no-pretty で実行し、1行1エラーの形式で返す。"""
    result = subprocess.run(
        [
            "mypy",
            "agentflow",
            "--strict",
            "--ignore-missing-imports",
            "--no-error-summary",
            "--no-pretty",
        ],
        capture_output=True,
        text=True,
        timeout=300,
        cwd=ROOT,
    )
    return result.stdout + result.stderr


def parse_errors(output: str) -> tuple[list[tuple[str, str, str]], str]:
    """mypy 出力をパースし (file, detail, code) のリストと集計行を返す。

    --no-pretty 出力は 1 行に完結しているため、行末の [code] を確実に取得できる。
    例: agentflow/foo.py:10:5: error: Message text [no-any-return]
    """
    # ファイル:行[:列]: error: メッセージ [コード]
    error_re = re.compile(r"^(agentflow/[^\s:]+):(\d+)(?::\d+)?:\s*error:\s*(.*?)\s*\[([a-z][a-z0-9-]*)\]\s*$")
    # コードが不明な行（行末に [code] が無い場合）
    error_nocode_re = re.compile(r"^(agentflow/[^\s:]+):(\d+)(?::\d+)?:\s*error:\s*(.+)$")

    errors: list[tuple[str, str, str]] = []
    summary_line = ""

    for line in output.splitlines():
        m = error_re.match(line)
        if m:
            path, num, msg, code = m.group(1), m.group(2), m.group(3), m.group(4)
            errors.append((path, f"{num}: {msg[:60]}", code))
            continue

        m2 = error_nocode_re.match(line)
        if m2:
            path, num, msg = m2.group(1), m2.group(2), m2.group(3)
            errors.append((path, f"{num}: {msg[:60]}", "?"))
            continue

        if "Found " in line and " error" in line:
            summary_line = line.strip()

    return errors, summary_line


def main() -> int:
    print("Running: mypy agentflow --strict --ignore-missing-imports ...")
    raw = run_mypy()
    errors, summary_line = parse_errors(raw)
    if not errors:
        print("✅ No mypy errors found!")
        return 0

    # コード別集計
    by_code: dict[str, int] = defaultdict(int)
    by_file: dict[str, int] = defaultdict(int)
    for path, _msg, code in errors:
        by_code[code] += 1
        by_file[path] += 1

    print("\n========== MyPy エラーサマリー ==========\n")
    total = len(errors)
    known = sum(v for k, v in by_code.items() if k != "?")
    print(summary_line or f"Total: {total} errors (うち未分類: {by_code.get('?', 0)}件)")
    print(f"(集計対象: {known} / {total} 件)\n")

    print("【コード別】")
    for code in sorted(by_code.keys(), key=lambda c: -by_code[c]):
        print(f"  {code}: {by_code[code]}")

    print("\n【ファイル別（上位 20）】")
    for path in sorted(by_file.keys(), key=lambda p: -by_file[p])[:20]:
        print(f"  {by_file[path]:4d}  {path}")

    print("\n詳細は code-rules/tools/check-errors-analysis.md を参照してください。")
    return 1


if __name__ == "__main__":
    sys.exit(main())
