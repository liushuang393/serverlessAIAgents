"""Control-plane governance console サービス."""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class GovernanceNotice:
    """Platform で表示するガバナンス通知."""

    notice_id: str
    title: str
    severity: str


class GovernanceConsoleService:
    """通知を一覧化する軽量サービス."""

    def __init__(self) -> None:
        self._notices: list[GovernanceNotice] = []

    def publish(self, notice: GovernanceNotice) -> None:
        """通知を追加する。"""
        self._notices.append(notice)

    def list_notices(self) -> list[GovernanceNotice]:
        """通知一覧を返す。"""
        return list(self._notices)
