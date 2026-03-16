"""Layer 2 の Audit サービス."""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from contracts.policy import PolicyDecision


class AuditService:
    """PolicyDecision を監査ログへ流す共有サービス."""

    def __init__(self, logger: logging.Logger | None = None) -> None:
        self._logger = logger or logging.getLogger("shared.audit")

    def record(self, decision: PolicyDecision, *, extra: dict[str, Any] | None = None) -> None:
        """判断記録を構造化ログへ残す."""
        payload = decision.to_payload()
        if extra:
            payload["extra"] = extra
        self._logger.warning("AUDIT %s", payload)
