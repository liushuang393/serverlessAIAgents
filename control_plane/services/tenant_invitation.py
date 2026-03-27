"""互換 facade.

tenant invitation の正規実装は `control_plane.tenants.service`。
旧 platform app 再編時の互換 import 窓口として残している。
"""

from control_plane.tenants.service import (
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
