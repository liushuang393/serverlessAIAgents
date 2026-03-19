"""遅延 import スモークテスト.

__getattr__ で定義された __all__ シンボルが全て eager resolve できることを検証。
循環 import のリグレッション検出に使用する。
"""

from __future__ import annotations

import importlib

import pytest


# テスト対象: __getattr__ を使用しているパッケージと __all__ の対応
_LAZY_PACKAGES = [
    "kernel",
    "control_plane.services",
]


@pytest.mark.parametrize("package_name", _LAZY_PACKAGES)
def test_all_symbols_resolvable(package_name: str) -> None:
    """__all__ に定義された全シンボルが getattr で解決できる."""
    mod = importlib.import_module(package_name)
    all_symbols = getattr(mod, "__all__", [])
    assert all_symbols, f"{package_name} has empty __all__"

    failures: list[str] = []
    for symbol in all_symbols:
        try:
            obj = getattr(mod, symbol)
            assert obj is not None, f"{package_name}.{symbol} resolved to None"
        except (AttributeError, ImportError, ModuleNotFoundError) as exc:
            failures.append(f"{package_name}.{symbol}: {exc}")

    if failures:
        msg = f"Failed to resolve {len(failures)} symbol(s):\n" + "\n".join(failures)
        pytest.fail(msg)


@pytest.mark.parametrize("package_name", _LAZY_PACKAGES)
def test_no_duplicate_symbols(package_name: str) -> None:
    """__all__ に重複シンボルがない."""
    mod = importlib.import_module(package_name)
    all_symbols = getattr(mod, "__all__", [])
    seen: set[str] = set()
    duplicates: list[str] = []
    for s in all_symbols:
        if s in seen:
            duplicates.append(s)
        seen.add(s)
    assert not duplicates, f"Duplicate symbols in {package_name}.__all__: {duplicates}"
