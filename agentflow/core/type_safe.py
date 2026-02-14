"""型変換ユーティリティ.

LLM出力の揺れを吸収するための共通変換関数を提供する。
"""

from enum import Enum
from typing import Any, TypeVar


EnumT = TypeVar("EnumT", bound=Enum)


def safe_enum(enum_cls: type[EnumT], value: Any, default: EnumT) -> EnumT:
    """列挙型への安全変換."""
    if isinstance(value, enum_cls):
        return value
    try:
        return enum_cls(str(value))
    except (TypeError, ValueError):
        return default


def safe_int(
    value: Any,
    default: int,
    *,
    min_value: int | None = None,
    max_value: int | None = None,
) -> int:
    """整数への安全変換（任意で範囲クリップ）."""
    try:
        result = int(value)
    except (TypeError, ValueError):
        result = default

    if min_value is not None:
        result = max(min_value, result)
    if max_value is not None:
        result = min(max_value, result)
    return result


def safe_float(
    value: Any,
    default: float,
    *,
    min_value: float | None = None,
    max_value: float | None = None,
) -> float:
    """浮動小数への安全変換（任意で範囲クリップ）."""
    try:
        result = float(value)
    except (TypeError, ValueError):
        result = default

    if min_value is not None:
        result = max(min_value, result)
    if max_value is not None:
        result = min(max_value, result)
    return result

