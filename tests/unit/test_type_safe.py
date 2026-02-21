"""type_safe ユーティリティの単体テスト."""

from enum import Enum

from agentflow.core.type_safe import safe_enum, safe_float, safe_int


class _Mode(str, Enum):
    A = "A"
    B = "B"


def test_safe_enum_with_valid_value() -> None:
    """有効値はそのまま変換."""
    assert safe_enum(_Mode, "A", _Mode.B) == _Mode.A


def test_safe_enum_with_invalid_value_returns_default() -> None:
    """不正値はデフォルトにフォールバック."""
    assert safe_enum(_Mode, "X", _Mode.B) == _Mode.B


def test_safe_int_with_bounds() -> None:
    """整数変換と範囲クリップ."""
    assert safe_int("9", 0, min_value=1, max_value=5) == 5


def test_safe_float_with_default() -> None:
    """不正浮動小数はデフォルト."""
    assert safe_float("invalid", 1.5) == 1.5
