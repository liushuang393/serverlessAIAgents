"""Static guard: provider SDK direct imports are forbidden outside gateway layer."""

from __future__ import annotations

from scripts.check_no_direct_provider_calls import find_violations


def test_no_direct_provider_imports_outside_gateway() -> None:
    violations = find_violations()
    assert not violations, f"Forbidden direct provider imports found: {violations}"
