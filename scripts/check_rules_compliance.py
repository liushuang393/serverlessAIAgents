#!/usr/bin/env python3
"""ルールコンプライアンス総合レポーター.

CLAUDE.md の主要ルールカテゴリごとに違反数を集計し、
CI/CD 品質ゲートとして機能する。

Usage:
  python scripts/check_rules_compliance.py
  python scripts/check_rules_compliance.py --json
  python scripts/check_rules_compliance.py --strict
"""

from __future__ import annotations

import argparse
import ast
import json
import re
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]

# スキャン対象ディレクトリ
SCAN_DIRS = [
    "contracts",
    "infrastructure",
    "shared",
    "kernel",
    "harness",
    "control_plane",
    "domain",
    "apps",
]

# 閾値設定
# NOTE: ベースライン値は 2026-03-22 時点の実測値を基に、
# 段階的削減を目標として設定。新規違反の追加を防止しつつ、
# 既存違反は計画的に削減する。
THRESHOLDS = {
    "layer_boundary_violations": 25,  # 現行 0 → 維持
    "provider_direct_imports": 0,  # 即時ゼロ
    "file_size_violations": 30,  # 現行 25 → 段階的に 10 へ
    "excessive_imports": 10,  # 現行 8 → 維持
    "type_ignore_without_reason": 150,  # 現行 128 → 段階的に 50 へ
    "bare_any_usage": 7000,  # 現行 6691 → 段階的に 3000 へ
    "cast_usage": 80,  # 現行 68 → 段階的に 30 へ
}


def count_lines(filepath: Path) -> int:
    """ファイルの行数を数える."""
    return len(filepath.read_text(encoding="utf-8").splitlines())


def scan_python_files() -> list[Path]:
    """スキャン対象の Python ファイルを収集."""
    files = []
    for d in SCAN_DIRS:
        dir_path = ROOT / d
        if dir_path.is_dir():
            files.extend(dir_path.rglob("*.py"))
    return sorted(files)


def check_file_sizes(files: list[Path]) -> list[dict]:
    """1000行超のファイルを検出."""
    violations = []
    for f in files:
        lines = count_lines(f)
        if lines > 1000:
            violations.append(
                {
                    "file": str(f.relative_to(ROOT)),
                    "lines": lines,
                }
            )
    return violations


def check_excessive_imports(files: list[Path]) -> list[dict]:
    """20 import 超のファイルを検出."""
    violations = []
    for f in files:
        try:
            tree = ast.parse(f.read_text(encoding="utf-8"))
        except SyntaxError:
            continue
        import_count = sum(1 for node in ast.iter_child_nodes(tree) if isinstance(node, (ast.Import, ast.ImportFrom)))
        if import_count > 20:
            violations.append(
                {
                    "file": str(f.relative_to(ROOT)),
                    "import_count": import_count,
                }
            )
    return violations


def check_type_ignore(files: list[Path]) -> list[dict]:
    """理由なし # type: ignore を検出.

    NOTE: `# type: ignore[code]` はエラーコードのみで理由説明ではないため、
    こちらも違反とする。許容するのは `# type: ignore[code]  # 理由説明` の形式のみ。
    """
    bare_pattern = re.compile(r"#\s*type:\s*ignore\s*$")
    code_only_pattern = re.compile(r"#\s*type:\s*ignore\[[^\]]+\]\s*$")
    violations = []
    for f in files:
        for i, line in enumerate(
            f.read_text(encoding="utf-8").splitlines(),
            1,
        ):
            if bare_pattern.search(line) or code_only_pattern.search(
                line,
            ):
                violations.append(
                    {
                        "file": str(f.relative_to(ROOT)),
                        "line": i,
                        "content": line.strip(),
                    }
                )
    return violations


def check_bare_any(files: list[Path]) -> list[dict]:
    """理由コメントなしの Any 型使用を検出.

    Any を使う場合は同一行に理由コメント (5文字以上) が必要:
      field: Any  # 外部 API の戻り値が不定のため
    """
    violations = []
    for f in files:
        try:
            source = f.read_text(encoding="utf-8")
            tree = ast.parse(source)
        except SyntaxError:
            continue
        lines = source.splitlines()
        for node in ast.walk(tree):
            if isinstance(node, ast.Name) and node.id == "Any":
                lineno = node.lineno
                line_text = lines[lineno - 1] if lineno <= len(lines) else ""
                # コメント部分を抽出
                if "#" in line_text:
                    comment = line_text.split("#", 1)[1].strip()
                    # 5文字以上の理由コメントがあれば OK
                    if len(comment) >= 5:
                        continue
                # コメントなし or 理由不足 → 違反
                violations.append(
                    {
                        "file": str(f.relative_to(ROOT)),
                        "line": lineno,
                    }
                )
    return violations


def check_cast_usage(files: list[Path]) -> list[dict]:
    """cast() の使用を検出."""
    violations = []
    for f in files:
        try:
            tree = ast.parse(f.read_text(encoding="utf-8"))
        except SyntaxError:
            continue
        for node in ast.walk(tree):
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id == "cast":
                violations.append(
                    {
                        "file": str(f.relative_to(ROOT)),
                        "line": node.lineno,
                    }
                )
    return violations


def run_existing_script(script_name: str) -> int:
    """既存の境界チェックスクリプトを実行して違反数を取得.

    既存スクリプトの出力形式: "Total: N violation(s)"
    """
    script = ROOT / "scripts" / script_name
    if not script.exists():
        return -1
    result = subprocess.run(
        [sys.executable, str(script)],
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    if result.returncode == 0:
        return 0
    total_pattern = re.compile(r"Total:\s*(\d+)\s+violation")
    for line in (result.stdout + result.stderr).splitlines():
        m = total_pattern.search(line)
        if m:
            return int(m.group(1))
    return 1 if result.returncode != 0 else 0


def main() -> int:
    """メインエントリポイント."""
    parser = argparse.ArgumentParser(
        description="ルールコンプライアンス総合レポーター",
    )
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--strict", action="store_true")
    args = parser.parse_args()

    files = scan_python_files()

    # 各チェックを1回だけ実行して結果をキャッシュ
    file_size_results = check_file_sizes(files)
    excessive_import_results = check_excessive_imports(files)

    report: dict = {
        "layer_boundary_violations": run_existing_script(
            "check_layer_boundaries.py",
        ),
        "provider_direct_imports": run_existing_script(
            "check_no_direct_provider_calls.py",
        ),
        "file_size_violations": len(file_size_results),
        "file_size_details": file_size_results,
        "excessive_imports": len(excessive_import_results),
        "excessive_imports_details": excessive_import_results,
        "type_ignore_without_reason": len(check_type_ignore(files)),
        "bare_any_usage": len(check_bare_any(files)),
        "cast_usage": len(check_cast_usage(files)),
        "total_files_scanned": len(files),
    }

    # 閾値チェック
    over_threshold: dict[str, dict[str, int]] = {}
    for key, threshold in THRESHOLDS.items():
        value = report.get(key, 0)
        if isinstance(value, int) and value > threshold:
            over_threshold[key] = {
                "value": value,
                "threshold": threshold,
            }
    report["over_threshold"] = over_threshold

    if args.json:
        summary = {k: v for k, v in report.items() if not k.endswith("_details")}
        print(json.dumps(summary, indent=2, ensure_ascii=False))
    else:
        print("=" * 60)
        print("  ルールコンプライアンス総合レポート")
        print("=" * 60)
        print(f"  スキャンファイル数: {report['total_files_scanned']}")
        print()
        for key, threshold in THRESHOLDS.items():
            value = report.get(key, 0)
            status = "OK" if value <= threshold else "NG"
            print(f"  {status} {key}: {value} (閾値: {threshold})")
        print()
        if over_threshold:
            print("  --- 閾値超過 ---")
            for key, info in over_threshold.items():
                print(
                    f"  NG {key}: {info['value']} > {info['threshold']}",
                )

    if args.strict and over_threshold:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
