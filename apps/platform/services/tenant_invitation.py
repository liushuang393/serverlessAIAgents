"""互換 facade.

tenant invitation の正規実装は top-level `platform.tenants.service` に移設済み。
"""

from platform.tenants.service import (
    ChallengeMethod,
    EmailTransport,
    InMemoryEmailTransport,
    InvitationRecord,
    InviteServiceError,
    OutboundEmail,
    SMTPEmailTransport,
    TenantInvitationService,
)


__all__ = [
    "ChallengeMethod",
    "EmailTransport",
    "InMemoryEmailTransport",
    "InvitationRecord",
    "InviteServiceError",
    "OutboundEmail",
    "SMTPEmailTransport",
    "TenantInvitationService",
]
