"""FAQ システム認証モジュール.

認証・認可機能を提供する。

機能:
- JWT トークン認証
- ログイン/ログアウト
- ユーザー情報取得
- ロールベースアクセス制御

使用例:
    >>> from apps.faq_system.backend.auth import auth_router
    >>> app.include_router(auth_router)
"""

from apps.faq_system.backend.auth.dependencies import (
    get_current_user,
    get_optional_user,
    require_auth,
    require_role,
    resolve_user,
)
from apps.faq_system.backend.auth.models import (
    AuthResponse,
    ChangePasswordRequest,
    ForgotPasswordRequest,
    LoginRequest,
    ProfileUpdateRequest,
    ResetPasswordRequest,
    TokenResponse,
    UserInfo,
)
from apps.faq_system.backend.auth.router import router as auth_router
from apps.faq_system.backend.auth.service import AuthService, get_auth_service


__all__ = [
    "AuthResponse",
    "AuthService",
    "ChangePasswordRequest",
    "ForgotPasswordRequest",
    "LoginRequest",
    "ProfileUpdateRequest",
    "ResetPasswordRequest",
    "TokenResponse",
    "UserInfo",
    "auth_router",
    "get_auth_service",
    "get_current_user",
    "get_optional_user",
    "require_auth",
    "require_role",
    "resolve_user",
]
