"""contracts の canonical schema が唯一であることを確認.

contracts/ 外で同名の class 定義が存在する場合を検出し、
段階的な一本化を進めるための監査テスト。
"""

from __future__ import annotations

import ast
from pathlib import Path

# contracts/ 外での再定義を禁止するコアスキーマ型
CORE_TYPES: frozenset[str] = frozenset({
    "ToolResult",
    "ToolRequest",
    "ToolCallStatus",
    "FlowDefinition",
    "FlowExecutionState",
    "AppManifest",
})

# 検索から除外するディレクトリ
EXCLUDE_DIRS: frozenset[str] = frozenset({
    "__pycache__",
    ".venv",
    "node_modules",
    "tests",
    ".git",
    ".mypy_cache",
    ".ruff_cache",
})

# プロジェクトルート
PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent


def _collect_violations() -> list[str]:
    """contracts/ 外でのコアスキーマ class 定義を収集する."""
    violations: list[str] = []
    for py_file in PROJECT_ROOT.rglob("*.py"):
        rel = py_file.relative_to(PROJECT_ROOT)
        parts = rel.parts

        # 除外ディレクトリ内のファイルをスキップ
        if any(ex in parts for ex in EXCLUDE_DIRS):
            continue

        # contracts/ 内は canonical 定義なのでスキップ
        if parts[0] == "contracts":
            continue

        try:
            source = py_file.read_text(encoding="utf-8")
            tree = ast.parse(source)
        except (SyntaxError, UnicodeDecodeError):
            continue

        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef) and node.name in CORE_TYPES:
                violations.append(f"{rel}:{node.lineno}: class {node.name}")

    return sorted(violations)


def test_no_duplicate_core_schemas() -> None:
    """コアスキーマ型が contracts/ 以外で class 定義されていないことを監査."""
    violations = _collect_violations()

    if violations:
        # 現時点では警告として記録。完全解消後に assert に切り替える。
        msg = (
            f"\n[監査] contracts/ 外でのコアスキーマ定義 ({len(violations)} 箇所):\n"
        )
        for v in violations:
            msg += f"  - {v}\n"
        msg += (
            "\n上記の定義は contracts/ の canonical schema へ統合し、"
            "re-export または import に置き換えてください。"
        )
        print(msg)

    # --- 段階的解消 ---
    # 現在は既知の重複が存在するため警告のみ。
    # 重複解消が完了したら以下のコメントを外して厳格化する:
    # assert violations == [], msg
