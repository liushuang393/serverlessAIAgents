"""Layer 5 設定集約サービス."""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class ConfigEntry:
    """設定キーの最小表現."""

    key: str
    value: str
    scope: str = "global"


class ConfigCenterService:
    """設定エントリを保持する."""

    def __init__(self) -> None:
        self._entries: dict[tuple[str, str], ConfigEntry] = {}

    def set(self, entry: ConfigEntry) -> None:
        """設定値を保存する。"""
        self._entries[(entry.scope, entry.key)] = entry

    def get(self, key: str, *, scope: str = "global") -> ConfigEntry | None:
        """設定値を取得する。"""
        return self._entries.get((scope, key))
