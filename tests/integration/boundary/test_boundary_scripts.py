"""静的境界チェックのテスト."""

from __future__ import annotations

from scripts.check_layer_boundaries import find_violations as find_layer_violations
from scripts.check_no_direct_provider_calls import find_violations as find_provider_violations


def test_new_layer_boundaries_have_no_violations() -> None:
    """新しい層パッケージに禁止依存が無いこと."""
    assert not find_layer_violations()


def test_provider_sdk_direct_imports_are_still_forbidden() -> None:
    """Gateway/Infrastructure 以外で provider SDK を直呼びしないこと."""
    assert not find_provider_violations()
