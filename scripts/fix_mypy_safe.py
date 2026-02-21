#!/usr/bin/env python3
"""MyPy のうち「安全に自動修正できる」項目のみを修正するスクリプト。

対応するエラーコード:
- unused-ignore: 不要になった # type: ignore コメントを削除（mypy 出力を解析）

使用例:
  conda activate agentflow
  python scripts/fix_mypy_safe.py              # 修正実行
  python scripts/fix_mypy_safe.py --dry-run    # 変更せずに削除対象のみ表示

注意: 修正後は必ず mypy とテストを再実行して確認すること。
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def run_mypy() -> str:
    """mypy を --no-pretty で実行し、1行1エラーの標準出力を返す。"""
    r = subprocess.run(
        [
            "mypy", "agentflow",
            "--strict", "--ignore-missing-imports",
            "--no-error-summary", "--no-pretty",
        ],
        capture_output=True,
        text=True,
        timeout=300,
        cwd=ROOT,
    )
    return r.stdout + r.stderr


def find_unused_ignores(output: str) -> list[tuple[str, int]]:
    """mypy 出力から unused-ignore の (相対パス, 行番号) を抽出する。

    --no-pretty 形式の例:
      agentflow/foo.py:10:5: error: Unused "type: ignore" comment  [unused-ignore]
    """
    pattern = re.compile(
        r"^(agentflow/[^\s:]+):(\d+)(?::\d+)?:\s*error:.*?Unused \"type: ignore\".*\[unused-ignore\]",
        re.MULTILINE,
    )
    results = [(m.group(1), int(m.group(2))) for m in pattern.finditer(output)]
    # 同一ファイルの場合、行番号が大きい順に処理（前から消すとズレが起きる）
    return sorted(results, key=lambda x: (x[0], -x[1]))


def remove_unused_ignore(file_path: Path, line_no: int) -> bool:
    """指定行の # type: ignore[...] コメントを削除する。変更があれば True を返す。"""
    text = file_path.read_text(encoding="utf-8")
    lines = text.splitlines(keepends=True)
    if line_no < 1 or line_no > len(lines):
        print(f"  [SKIP] {file_path}:{line_no} - 行番号が範囲外", file=sys.stderr)
        return False

    original = lines[line_no - 1]
    # 末尾の # type: ignore[...] または # type: ignore を除去（前後の空白も含む）
    cleaned = re.sub(r"\s*#\s*type:\s*ignore(?:\[[^\]]*\])?\s*$", "", original.rstrip())
    if cleaned == original.rstrip():
        # コメントが見つからなかった（既に消えているか形式が違う）
        return False

    # 元の改行文字を維持
    newline = "\n" if original.endswith("\n") else ""
    lines[line_no - 1] = cleaned + newline
    file_path.write_text("".join(lines), encoding="utf-8")
    return True


def main() -> int:
    parser = argparse.ArgumentParser(
        description="unused type: ignore コメントを安全に自動削除する。"
    )
    parser.add_argument(
        "--dry-run", action="store_true",
        help="変更を加えず、削除対象だけ表示する。"
    )
    args = parser.parse_args()

    print("mypy を実行して unused type: ignore を検出中...")
    raw = run_mypy()
    unused = find_unused_ignores(raw)

    if not unused:
        print("✅ unused-ignore は見つかりませんでした（0 件）。")
        return 0

    print(f"対象: {len(unused)} 件")
    changed = 0
    for rel_path, line_no in unused:
        path = ROOT / rel_path
        if not path.exists():
            print(f"  [SKIP] {rel_path} - ファイルが見つかりません", file=sys.stderr)
            continue
        if args.dry_run:
            print(f"  [dry-run] {rel_path}:{line_no}")
            changed += 1
        else:
            if remove_unused_ignore(path, line_no):
                print(f"  ✓ 削除: {rel_path}:{line_no}")
                changed += 1
            else:
                print(f"  [SKIP] {rel_path}:{line_no} - コメントが見つかりません")

    if changed:
        if args.dry_run:
            print(f"\n[dry-run] {changed} 件を削除予定。--dry-run を外すと実行されます。")
        else:
            print(f"\n✅ 完了: {changed} 件を削除しました。mypy とテストで確認してください。")
    return 0


if __name__ == "__main__":
    sys.exit(main())
