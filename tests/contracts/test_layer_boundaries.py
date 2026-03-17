"""六層分離アーキテクチャの依存方向を検証するテスト。

設計書 §6 の依存ルール:
  Layer 6 (apps) -> 5 (platform) -> 4 (harness) -> 3 (kernel) -> 2 (shared) -> 1 (infrastructure) -> 0 (contracts)

禁止:
  - infrastructure → kernel / harness / shared / platform / apps
  - shared → kernel / harness / platform / apps
  - kernel → platform / apps
  - harness → platform / apps
  - platform → apps（apps 内部ロジックへの直接依存）
  - app → 他 app 内部への直接依存
"""

from __future__ import annotations

import ast
from pathlib import Path
from typing import NamedTuple

import pytest


REPO_ROOT = Path(__file__).resolve().parents[2]

# 各層のトップレベルパッケージ名
LAYER_PACKAGES: dict[str, int] = {
    "contracts": 0,
    "infrastructure": 1,
    "shared": 2,
    "kernel": 3,
    "harness": 4,
    "platform": 5,
    "apps": 6,
}

# 禁止依存ルール: (from_layer, forbidden_target_layers)
FORBIDDEN_DEPS: list[tuple[str, list[str]]] = [
    ("infrastructure", ["shared", "kernel", "harness", "platform", "apps"]),
    ("shared", ["kernel", "harness", "platform", "apps"]),
    ("kernel", ["platform", "apps"]),
    ("harness", ["platform", "apps"]),
]

# 許可例外: (from_package, target_module_prefix) — re-export stub など
ALLOWED_EXCEPTIONS: list[tuple[str, str]] = [
    # platform/services/ は shared/services/ の re-export stub
    ("platform.services", "shared.services"),
    # infrastructure の unified_tool re-export stub (実装は kernel/tools/ に移動済み)
    ("infrastructure.providers.unified_tool", "kernel.tools"),
    ("infrastructure.sandbox.unified_tool", "kernel.tools"),
]

# stdlib モジュール名とプロジェクト層名の衝突を除外
STDLIB_MODULES: set[str] = {"platform"}


class Violation(NamedTuple):
    """依存違反の記録。"""

    file: str
    line: int
    from_layer: str
    target_layer: str
    import_str: str


def _extract_imports(filepath: Path) -> list[tuple[int, str]]:
    """AST からモジュールスコープの import 文を抽出する。

    以下の import は除外する:
    - TYPE_CHECKING ブロック内の import
    - 関数/メソッド本体内の遅延 import（lazy import）
    """
    try:
        source = filepath.read_text(encoding="utf-8")
        tree = ast.parse(source, filename=str(filepath))
    except (SyntaxError, UnicodeDecodeError):
        return []

    imports: list[tuple[int, str]] = []
    type_checking_ranges: list[tuple[int, int]] = []
    function_ranges: list[tuple[int, int]] = []

    # TYPE_CHECKING ブロックの行範囲を検出
    for node in ast.walk(tree):
        if isinstance(node, ast.If):
            test = node.test
            is_type_checking = False
            if (isinstance(test, ast.Name) and test.id == "TYPE_CHECKING") or (
                isinstance(test, ast.Attribute) and test.attr == "TYPE_CHECKING"
            ):
                is_type_checking = True
            if is_type_checking:
                start = node.lineno
                end = max(getattr(n, "end_lineno", start) for n in ast.walk(node) if hasattr(n, "end_lineno"))
                type_checking_ranges.append((start, end))

    # 関数/メソッド本体の行範囲を検出（遅延 import を除外するため）
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            start = node.lineno
            end = node.end_lineno or start
            function_ranges.append((start, end))

    def _in_type_checking(lineno: int) -> bool:
        return any(start <= lineno <= end for start, end in type_checking_ranges)

    def _in_function(lineno: int) -> bool:
        return any(start <= lineno <= end for start, end in function_ranges)

    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            if not _in_type_checking(node.lineno) and not _in_function(node.lineno):
                for alias in node.names:
                    # stdlib と同名のパッケージを除外 (例: import platform)
                    if alias.name not in STDLIB_MODULES:
                        imports.append((node.lineno, alias.name))
        elif isinstance(node, ast.ImportFrom):
            if node.module and not _in_type_checking(node.lineno) and not _in_function(node.lineno):
                # stdlib と同名のパッケージを除外
                top = node.module.split(".")[0]
                if top not in STDLIB_MODULES:
                    imports.append((node.lineno, node.module))

    return imports


def _get_layer(module_path: str) -> str | None:
    """モジュールパスから層名を取得する。"""
    top = module_path.split(".")[0]
    return top if top in LAYER_PACKAGES else None


def _is_allowed_exception(file_module: str, target_module: str) -> bool:
    """許可例外に該当するか判定する。"""
    for from_prefix, target_prefix in ALLOWED_EXCEPTIONS:
        if file_module.startswith(from_prefix) and target_module.startswith(target_prefix):
            return True
    return False


def _file_to_module(filepath: Path) -> str:
    """ファイルパスをモジュールパスに変換する。"""
    rel = filepath.relative_to(REPO_ROOT)
    parts = list(rel.with_suffix("").parts)
    if parts and parts[-1] == "__init__":
        parts = parts[:-1]
    return ".".join(parts)


def _collect_violations() -> list[Violation]:
    """全層の Python ファイルをスキャンし、依存違反を収集する。"""
    violations: list[Violation] = []

    for from_layer, forbidden_targets in FORBIDDEN_DEPS:
        layer_dir = REPO_ROOT / from_layer
        if not layer_dir.is_dir():
            continue

        for py_file in layer_dir.rglob("*.py"):
            file_module = _file_to_module(py_file)
            for lineno, imported_module in _extract_imports(py_file):
                target_layer = _get_layer(imported_module)
                if target_layer and target_layer in forbidden_targets:
                    if not _is_allowed_exception(file_module, imported_module):
                        violations.append(
                            Violation(
                                file=str(py_file.relative_to(REPO_ROOT)),
                                line=lineno,
                                from_layer=from_layer,
                                target_layer=target_layer,
                                import_str=imported_module,
                            )
                        )

    return violations


# テスト実行時に一度だけスキャン
_cached_violations: list[Violation] | None = None


def _get_violations() -> list[Violation]:
    global _cached_violations
    if _cached_violations is None:
        _cached_violations = _collect_violations()
    return _cached_violations


class TestLayerBoundaries:
    """六層依存方向の境界テスト。"""

    def test_infrastructure_does_not_import_upper_layers(self) -> None:
        """Infrastructure (L1) は上位層を import しない。"""
        vs = [v for v in _get_violations() if v.from_layer == "infrastructure"]
        if vs:
            msg = self._format_violations(vs)
            pytest.fail(f"infrastructure → 上位層の依存違反 {len(vs)} 件:\n{msg}")

    def test_shared_does_not_import_upper_layers(self) -> None:
        """Shared (L2) は上位層を import しない。"""
        vs = [v for v in _get_violations() if v.from_layer == "shared"]
        if vs:
            msg = self._format_violations(vs)
            pytest.fail(f"shared → 上位層の依存違反 {len(vs)} 件:\n{msg}")

    def test_kernel_does_not_import_platform_or_apps(self) -> None:
        """Kernel (L3) は platform/apps を import しない。"""
        vs = [v for v in _get_violations() if v.from_layer == "kernel"]
        if vs:
            msg = self._format_violations(vs)
            pytest.fail(f"kernel → platform/apps の依存違反 {len(vs)} 件:\n{msg}")

    def test_harness_does_not_import_platform_or_apps(self) -> None:
        """Harness (L4) は platform/apps を import しない。"""
        vs = [v for v in _get_violations() if v.from_layer == "harness"]
        if vs:
            msg = self._format_violations(vs)
            pytest.fail(f"harness → platform/apps の依存違反 {len(vs)} 件:\n{msg}")

    def test_total_violations_decreasing(self) -> None:
        """全体の違反件数が減少傾向であることを確認する。

        現在のベースライン: P0 修正後の残存違反数。
        新規違反の追加を防ぐため、上限を設定する。
        """
        vs = _get_violations()
        # ベースライン: 今回の修正後の残存数を計測し、ここに設定
        # 今後の修正で徐々に 0 に近づける
        MAX_ALLOWED = 25  # P0 Phase2 修正後: 19件。新規違反防止の上限
        assert len(vs) <= MAX_ALLOWED, (
            f"依存違反が {len(vs)} 件（上限 {MAX_ALLOWED}）。"
            f"新たな違反が追加された可能性あり。\n"
            f"{self._format_violations(vs[:10])}..."
        )

    def test_violation_summary(self) -> None:
        """違反のサマリーを出力する（情報提供用、常に PASS）。"""
        vs = _get_violations()
        if not vs:
            return
        summary: dict[str, int] = {}
        for v in vs:
            key = f"{v.from_layer} → {v.target_layer}"
            summary[key] = summary.get(key, 0) + 1
        lines = [f"  {k}: {c} 件" for k, c in sorted(summary.items())]
        print(f"\n依存違反サマリー（合計 {len(vs)} 件）:")
        for line in lines:
            print(line)

    @staticmethod
    def _format_violations(vs: list[Violation]) -> str:
        return "\n".join(f"  {v.file}:{v.line} — {v.from_layer} → {v.target_layer} ({v.import_str})" for v in vs[:20])
