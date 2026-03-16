"""Harness 承認サービス."""

from __future__ import annotations

from typing import TYPE_CHECKING

from shared.registry import ComponentToggle


if TYPE_CHECKING:
    from contracts.policy import ApprovalRequest


class ApprovalService:
    """承認要求の有効/無効を切り替える薄いサービス."""

    def __init__(self, toggle: ComponentToggle | None = None) -> None:
        self._toggle = toggle or ComponentToggle()
        self._pending: dict[str, ApprovalRequest] = {}

    def submit(self, request: ApprovalRequest) -> ApprovalRequest:
        """承認要求を登録する."""
        if not self._toggle.enabled:
            return request
        self._pending[request.id] = request
        return request

    def pending(self) -> list[ApprovalRequest]:
        """保留中の要求一覧を返す."""
        return list(self._pending.values())
