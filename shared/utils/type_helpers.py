"""型変換ヘルパーユーティリティ.

このモジュールはAgentFlowで使用される型変換ヘルパー関数を提供します。
"""

from enum import Enum
from typing import Any, TypeVar


T = TypeVar("T", bound=Enum)


def convert_enum[T: Enum](value: str | Enum, enum_class: type[T]) -> T:
    """Enum型への自動変換ヘルパー.

    Args:
        value: 変換する値（文字列またはEnum）
        enum_class: 変換先のEnum型

    Returns:
        Enumインスタンス

    Raises:
        ValueError: 無効な値の場合
        TypeError: 変換できない型の場合

    Example:
        >>> from enum import Enum
        >>> class Status(Enum):
        ...     ACTIVE = "active"
        ...     INACTIVE = "inactive"
        >>> status = convert_enum("active", Status)
        >>> assert status == Status.ACTIVE
        >>> # 大文字小文字を区別しない
        >>> status = convert_enum("ACTIVE", Status)
        >>> assert status == Status.ACTIVE
    """
    if isinstance(value, enum_class):
        return value
    if isinstance(value, str):
        try:
            # 直接変換を試みる
            return enum_class(value)
        except ValueError:
            # 大文字小文字を区別しない変換を試みる
            for member in enum_class:
                if member.value.lower() == value.lower():
                    return member
            # 見つからない場合はエラー
            valid_values = [m.value for m in enum_class]
            msg = f"Invalid value '{value}' for {enum_class.__name__}. Valid values: {valid_values}"
            raise ValueError(msg) from None
    msg = f"Cannot convert {type(value).__name__} to {enum_class.__name__}"
    raise TypeError(msg)


def safe_dict_get[T: Enum](
    data: dict[str, Any], key: str, default: Any = None, enum_class: type[T] | None = None
) -> Any:
    """辞書から値を安全に取得し、必要に応じてEnum変換を行う.

    Args:
        data: 辞書
        key: キー
        default: デフォルト値
        enum_class: Enum型（指定された場合は自動変換）

    Returns:
        取得した値（Enum変換済みの場合もある）

    Example:
        >>> from enum import Enum
        >>> class Status(Enum):
        ...     ACTIVE = "active"
        >>> data = {"status": "active", "name": "test"}
        >>> status = safe_dict_get(data, "status", enum_class=Status)
        >>> assert status == Status.ACTIVE
        >>> name = safe_dict_get(data, "name")
        >>> assert name == "test"
        >>> missing = safe_dict_get(data, "missing", default="default")
        >>> assert missing == "default"
    """
    value = data.get(key, default)
    if value is not None and enum_class is not None:
        return convert_enum(value, enum_class)
    return value


def flatten_dict(data: dict[str, Any], parent_key: str = "", sep: str = "_") -> dict[str, Any]:
    """ネストされた辞書をフラット化.

    Args:
        data: フラット化する辞書
        parent_key: 親キー（再帰用）
        sep: キーの区切り文字

    Returns:
        フラット化された辞書

    Example:
        >>> data = {"a": {"b": {"c": 1}}, "d": 2}
        >>> result = flatten_dict(data)
        >>> assert result == {"a_b_c": 1, "d": 2}
    """
    items: list[tuple[str, Any]] = []
    for k, v in data.items():
        new_key = f"{parent_key}{sep}{k}" if parent_key else k
        if isinstance(v, dict):
            items.extend(flatten_dict(v, new_key, sep=sep).items())
        else:
            items.append((new_key, v))
    return dict(items)


__all__ = [
    "convert_enum",
    "flatten_dict",
    "safe_dict_get",
]
