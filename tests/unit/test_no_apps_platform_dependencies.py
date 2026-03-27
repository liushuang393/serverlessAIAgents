"""Static guard: 削除済み legacy import/path は再導入しない。"""

from __future__ import annotations

from scripts.check_no_apps_platform_dependencies import find_violations


def test_no_apps_platform_dependencies() -> None:
    """実コードとテストに旧 import/path が残っていないことを確認する。"""
    violations = find_violations()
    assert not violations, f"Legacy import/path references found: {violations}"
