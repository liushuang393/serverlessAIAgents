"""Control-plane tenant 実装の互換窓口."""

from __future__ import annotations

from typing import Any

from control_plane._legacy import resolve_export


_EXPORT_MAP = {
    "ChallengeMethod": ("control_plane.tenants.service", "ChallengeMethod"),
    "EmailTransport": ("control_plane.tenants.service", "EmailTransport"),
    "InMemoryEmailTransport": ("control_plane.tenants.service", "InMemoryEmailTransport"),
    "InvitationRecord": ("control_plane.tenants.service", "InvitationRecord"),
    "InviteServiceError": ("control_plane.tenants.service", "InviteServiceError"),
    "OutboundEmail": ("control_plane.tenants.service", "OutboundEmail"),
    "SMTPEmailTransport": ("control_plane.tenants.service", "SMTPEmailTransport"),
    "TenantInvitationService": ("control_plane.tenants.service", "TenantInvitationService"),
}


def __getattr__(name: str) -> Any:
    """tenant 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
