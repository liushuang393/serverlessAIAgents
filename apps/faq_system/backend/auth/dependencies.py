"""FAQ システム認証依存関係.

FastAPI の Depends() で使用する認証関数を提供する。

使用例:
    >>> @app.get("/api/protected")
    >>> async def protected(user: UserInfo = Depends(require_auth)):
    ...     return {"user": user.username}
"""

from __future__ import annotations

import logging
import os
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.backend.auth.service import AuthService, get_auth_service
from apps.faq_system.backend.security.proxy_auth import proxy_auth_verifier
from fastapi import Cookie, Depends, Header, HTTPException, Request


if TYPE_CHECKING:
    from collections.abc import Callable
    from collections.abc import Mapping


logger = logging.getLogger(__name__)

def _trust_proxy_auth_enabled() -> bool:
    return os.getenv("FAQ_TRUST_PROXY_AUTH", "false").lower() in {"1", "true", "yes", "on"}


def _get_auth_service() -> AuthService:
    return get_auth_service()


def _get_header_candidates(env_name: str, default: str) -> tuple[str, ...]:
    raw = os.getenv(env_name, default).strip().lower()
    return tuple(item.strip() for item in raw.split(",") if item.strip())


def _pick_header(headers: Mapping[str, str], candidates: tuple[str, ...]) -> str | None:
    for key in candidates:
        value = headers.get(key)
        if value is not None and value.strip():
            return value.strip()
    return None


def _normalize_proxy_role(raw_role: str | None) -> str | None:
    if raw_role is None:
        return None
    value = raw_role.strip()
    if not value:
        return None

    if "," in value:
        groups = [item.strip() for item in value.split(",") if item.strip()]
        normalized = [item.lower() for item in groups]
        if "admin" in normalized:
            return "admin"
        if "manager" in normalized:
            return "manager"
        return groups[0]
    return value


def _proxy_user_id(username: str) -> str:
    base = f"user-{username}"
    if len(base) <= 64:
        return base
    import hashlib

    digest = hashlib.sha256(username.encode("utf-8")).hexdigest()[:59]
    return f"user-{digest}"


async def get_current_user(
    request: Request,
    authorization: str | None = Header(None),
    session_token: str | None = Cookie(None, alias="session_token"),
) -> UserInfo | None:
    """現在のユーザーを取得 (任意).

    JWT トークン / セッション Cookie / (任意)認証プロキシヘッダーから認証する。

    Args:
        authorization: Authorization ヘッダー
        session_token: セッション Cookie
    Returns:
        認証ユーザー、または None
    """
    headers = request.headers
    proxy_values = {
        "x_auth_user": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_USER_HEADERS",
                "x-forwarded-user,x-auth-user",
            ),
        ),
        "x_auth_name": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_NAME_HEADERS",
                "x-forwarded-preferred-username,x-auth-name,x-forwarded-name",
            ),
        ),
        "x_auth_role": _normalize_proxy_role(
            _pick_header(
                headers,
                _get_header_candidates(
                    "FAQ_PROXY_AUTH_ROLE_HEADERS",
                    "x-forwarded-groups,x-auth-role",
                ),
            )
        ),
        "x_auth_department": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_DEPARTMENT_HEADERS",
                "x-auth-department,x-forwarded-department",
            ),
        ),
        "x_auth_position": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_POSITION_HEADERS",
                "x-auth-position,x-forwarded-title",
            ),
        ),
        "x_auth_timestamp": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_TIMESTAMP_HEADERS",
                "x-auth-timestamp",
            ),
        ),
        "x_auth_nonce": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_NONCE_HEADERS",
                "x-auth-nonce",
            ),
        ),
        "x_auth_signature": _pick_header(
            headers,
            _get_header_candidates(
                "FAQ_PROXY_AUTH_SIGNATURE_HEADERS",
                "x-auth-signature",
            ),
        ),
    }

    return await resolve_user(
        authorization=authorization,
        session_token=session_token,
        request_method=request.method,
        request_path=request.url.path,
        **proxy_values,
    )


async def resolve_user(
    *,
    authorization: str | None = None,
    session_token: str | None = None,
    x_auth_user: str | None = None,
    x_auth_name: str | None = None,
    x_auth_role: str | None = None,
    x_auth_department: str | None = None,
    x_auth_position: str | None = None,
    x_auth_timestamp: str | None = None,
    x_auth_nonce: str | None = None,
    x_auth_signature: str | None = None,
    request_method: str = "GET",
    request_path: str = "/",
) -> UserInfo | None:
    """認証情報からユーザーを解決.

    Args:
        authorization: Authorization ヘッダー
        session_token: セッション Cookie
        x_auth_user: 認証プロキシユーザー名
        x_auth_name: 認証プロキシ表示名
        x_auth_role: 認証プロキシロール
        x_auth_department: 認証プロキシ部署
        x_auth_position: 認証プロキシ役職
        x_auth_timestamp: 認証プロキシ時刻
        x_auth_nonce: 認証プロキシ nonce
        x_auth_signature: 認証プロキシ署名
        request_method: HTTP メソッド
        request_path: リクエストパス

    Returns:
        認証ユーザー、または None
    """
    auth_service = _get_auth_service()

    # JWT トークン認証を試行
    if authorization:
        auth_user = await auth_service.verify_token(authorization)
        if auth_user:
            metadata = auth_user.metadata or {}
            username = metadata.get("username", auth_user.id)

            # 可能なら最新プロフィールを優先（トークン内メタデータの陳腐化対策）
            current_user = await auth_service.get_user(username)
            if current_user:
                return current_user

            role = auth_user.roles[0] if auth_user.roles else "employee"
            return UserInfo(
                user_id=auth_user.id,
                username=username,
                display_name=metadata.get("display_name", auth_user.id),
                department=metadata.get("department", ""),
                position=metadata.get("position", ""),
                role=role,
            )

    # セッション Cookie 認証を試行
    if session_token:
        session_user = await auth_service.get_user_by_session_token(session_token)
        if session_user:
            return session_user

    # 認証プロキシ（Identity-Aware Proxy 等）を信頼する設定
    if _trust_proxy_auth_enabled() and x_auth_user:
        verified = await proxy_auth_verifier.verify(
            user=x_auth_user,
            display_name=x_auth_name,
            role=x_auth_role,
            department=x_auth_department,
            position=x_auth_position,
            timestamp=x_auth_timestamp,
            nonce=x_auth_nonce,
            signature=x_auth_signature,
            request_method=request_method,
            request_path=request_path,
        )
        if verified is None:
            logger.warning("Proxy auth verification failed for user=%s", x_auth_user)
            return None
        return UserInfo(
            user_id=_proxy_user_id(verified.username),
            username=verified.username,
            display_name=verified.display_name,
            department=verified.department,
            position=verified.position,
            role=verified.role,
        )

    return None


async def get_optional_user(
    user: UserInfo | None = Depends(get_current_user),
) -> UserInfo | None:
    """オプショナルユーザー取得 (未認証でもエラーにならない).

    Args:
        user: 現在のユーザー

    Returns:
        ユーザー情報、または None
    """
    return user


async def require_auth(
    user: UserInfo | None = Depends(get_current_user),
) -> UserInfo:
    """認証必須.

    未認証の場合は 401 エラーを返す。

    Args:
        user: 現在のユーザー

    Returns:
        認証済みユーザー

    Raises:
        HTTPException: 未認証の場合
    """
    if not user:
        raise HTTPException(
            status_code=401,
            detail="認証が必要です。ログインしてください。",
        )
    return user


def require_role(*roles: str) -> Callable[..., Any]:
    """ロール必須依存関係ファクトリ.

    Args:
        roles: 許可ロール

    Returns:
        FastAPI 依存関係

    使用例:
        >>> @app.get("/admin")
        >>> async def admin(user = Depends(require_role("admin"))):
        ...     return {"ok": True}
    """

    async def _check_role(
        user: UserInfo = Depends(require_auth),
    ) -> UserInfo:
        if user.role not in roles:
            raise HTTPException(
                status_code=403,
                detail=f"権限不足です。必要なロール: {', '.join(roles)}",
            )
        return user

    return _check_role
