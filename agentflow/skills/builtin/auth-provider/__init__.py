"""Auth Provider Skill - 认证集成.

支持 Supabase Auth、Clerk、Firebase Auth 等主流认证服务。
"""

from agentflow.skills.builtin.auth_provider.exceptions import (
    AuthError,
    EmailNotConfirmedError,
    InvalidCredentialsError,
    MFARequiredError,
    TokenExpiredError,
    UserNotFoundError,
)
from agentflow.skills.builtin.auth_provider.provider import (
    AuthConfig,
    AuthProvider,
    ClerkConfig,
    Session,
    SupabaseAuthConfig,
    User,
)


__all__ = [
    "AuthConfig",
    "AuthError",
    "AuthProvider",
    "ClerkConfig",
    "EmailNotConfirmedError",
    "InvalidCredentialsError",
    "MFARequiredError",
    "Session",
    "SupabaseAuthConfig",
    "TokenExpiredError",
    "User",
    "UserNotFoundError",
]
