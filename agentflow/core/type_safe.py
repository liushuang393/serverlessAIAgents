"""型変換ユーティリティ.

LLM出力の揺れを吸収するための共通変換関数を提供する。

設計原則:
- 裸の Enum(value) 変換は禁止。必ず safe_enum を経由する
- 未知の値はクラッシュせずデフォルトにフォールバック
- 後方互換マッピング（aliases）で旧値を自動変換
"""

import logging
from enum import Enum
from typing import Any, TypeVar

_logger = logging.getLogger(__name__)

EnumT = TypeVar("EnumT", bound=Enum)


def safe_enum(
    enum_cls: type[EnumT],
    value: Any,
    default: EnumT,
    *,
    aliases: dict[str, str] | None = None,
) -> EnumT:
    """列挙型への安全変換（エイリアス・フォールバック付き）.

    Args:
        enum_cls: 変換先の Enum クラス
        value: 変換元の値（str, Enum, Any）
        default: 変換失敗時のフォールバック値
        aliases: 後方互換マッピング（例: {"REJECT": "COACH"}）

    Returns:
        変換された Enum 値。失敗時は default
    """
    if isinstance(value, enum_cls):
        return value

    raw = str(value).strip().upper()

    # エイリアスマッピング（後方互換: 旧値 → 新値）
    if aliases and raw in aliases:
        mapped = aliases[raw]
        _logger.info(
            "%s: エイリアス適用 '%s' → '%s'",
            enum_cls.__name__, raw, mapped,
        )
        raw = mapped

    try:
        return enum_cls(raw)
    except (TypeError, ValueError, KeyError):
        _logger.warning(
            "%s: 未知の値 '%s' → デフォルト '%s' にフォールバック",
            enum_cls.__name__, value, default.value,
        )
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

