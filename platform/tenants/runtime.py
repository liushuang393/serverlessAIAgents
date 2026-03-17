"""Layer 5 tenant 実装の互換窓口."""

from __future__ import annotations

from typing import Any

from platform._legacy import resolve_export


_EXPORT_MAP = {
    "ChallengeMethod": ("platform.tenants.service", "ChallengeMethod"),
    "EmailTransport": ("platform.tenants.service", "EmailTransport"),
    "InMemoryEmailTransport": ("platform.tenants.service", "InMemoryEmailTransport"),
    "InvitationRecord": ("platform.tenants.service", "InvitationRecord"),
    "InviteServiceError": ("platform.tenants.service", "InviteServiceError"),
    "OutboundEmail": ("platform.tenants.service", "OutboundEmail"),
    "SMTPEmailTransport": ("platform.tenants.service", "SMTPEmailTransport"),
    "TenantInvitationService": ("platform.tenants.service", "TenantInvitationService"),
}


def __getattr__(name: str) -> Any:
    """tenant 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
