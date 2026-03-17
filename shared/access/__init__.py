"""共有 access 公開 API."""

from shared.access.auth_service import AuthService, UserInfo, get_auth_service, reset_auth_service
from shared.access.contracts import AccessContext
from shared.access.service import build_access_context


__all__ = [
    "AccessContext",
    "AuthService",
    "UserInfo",
    "build_access_context",
    "get_auth_service",
    "reset_auth_service",
]
