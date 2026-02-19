"""FAQ システム認証ルーター.

エンドポイント:
    - POST /api/auth/login: ログイン
    - POST /api/auth/logout: ログアウト
    - GET /api/auth/me: 現在のユーザー情報
    - POST /api/auth/token: トークン更新
    - POST /api/auth/password/change: パスワード変更
    - POST /api/auth/password/forgot: パスワード再設定要求
    - POST /api/auth/password/reset: パスワード再設定
    - PATCH /api/auth/profile: プロフィール更新
"""

from __future__ import annotations

import logging
import os
from typing import Any

from apps.faq_system.backend.auth.dependencies import (
    get_current_user,
    require_auth,
)
from apps.faq_system.backend.auth.models import (
    AuthResponse,
    ChangePasswordRequest,
    ForgotPasswordRequest,
    LoginRequest,
    MfaSetupResponse,
    MfaVerifyRequest,
    ProfileUpdateRequest,
    RegisterRequest,
    ResetPasswordRequest,
    UserInfo,
)
from apps.faq_system.backend.auth.service import get_auth_service
from fastapi import APIRouter, Cookie, Depends, Response


logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/auth", tags=["認証"])

_SESSION_COOKIE_MAX_AGE = int(os.getenv("FAQ_SESSION_TTL_SECONDS", str(86400 * 7)))


def _auth_service() -> Any:
    """認証サービス取得."""
    return get_auth_service()


@router.post("/login", response_model=AuthResponse)
async def login(req: LoginRequest, response: Response) -> AuthResponse:
    """ログイン処理."""
    service = _auth_service()
    # Updated authenticate returns tuple(bool, str, UserInfo | None)
    success, message, user = await service.authenticate(req.username, req.password, req.totp_code)

    if not success or not user:
        if message == "MFA_REQUIRED":
            # Return specific message for frontend to detect
            logger.info("ログイン: MFAコードが必要: username=%s", req.username)
            return AuthResponse(
                success=False,
                message="MFA_REQUIRED",
            )

        logger.warning("ログイン失敗: username=%s, reason=%s", req.username, message)
        return AuthResponse(
            success=False,
            message=message,
        )

    # JWT トークン生成
    access_token = service.create_access_token(user)

    # セッション Cookie 生成 (WebSocket 用)
    session_token = await service.create_session(user)

    # Cookie 設定
    response.set_cookie(
        key="session_token",
        value=session_token,
        httponly=True,
        samesite="lax",
        max_age=_SESSION_COOKIE_MAX_AGE,
    )

    logger.info("ログイン成功: username=%s, role=%s", req.username, user.role)
    return AuthResponse(
        success=True,
        message=message,
        user=user,
        access_token=access_token,
        token_type="bearer",
    )


@router.post("/register", response_model=AuthResponse, status_code=201)
async def register(req: RegisterRequest, response: Response) -> AuthResponse:
    """ユーザー登録."""
    service = _auth_service()
    success, message, user = await service.register_user(
        username=req.username,
        password=req.password,
        display_name=req.display_name,
        department=req.department,
        position=req.position,
        email=req.email,
    )

    if not success or not user:
        logger.warning("登録失敗: username=%s, reason=%s", req.username, message)
        return AuthResponse(success=False, message=message)

    # 自動ログイン
    access_token = service.create_access_token(user)
    session_token = await service.create_session(user)

    response.set_cookie(
        key="session_token",
        value=session_token,
        httponly=True,
        samesite="lax",
        max_age=_SESSION_COOKIE_MAX_AGE,
    )

    logger.info("登録成功: username=%s", req.username)
    return AuthResponse(
        success=True,
        message="ユーザー登録が完了しました",
        user=user,
        access_token=access_token,
        token_type="bearer",
    )


@router.post("/logout")
async def logout(
    response: Response,
    user: UserInfo | None = Depends(get_current_user),
    session_token: str | None = Cookie(None, alias="session_token"),
) -> dict[str, Any]:
    """ログアウト処理.

    セッション Cookie を削除し、セッションストアからも削除する。

    Args:
        response: HTTP レスポンス
        user: 現在のユーザー

    Returns:
        処理結果
    """
    response.delete_cookie(key="session_token")
    service = _auth_service()
    await service.revoke_session_token(session_token)

    if user:
        await service.revoke_sessions_for_user(user.user_id)
        logger.info("ログアウト: username=%s", user.username)

    return {"success": True, "message": "ログアウトしました"}


@router.get("/me", response_model=AuthResponse)
async def get_me(
    user: UserInfo | None = Depends(get_current_user),
) -> AuthResponse:
    """現在のユーザー情報を取得.

    Args:
        user: 現在のユーザー

    Returns:
        ユーザー情報
    """
    if not user:
        return AuthResponse(success=False, message="未認証", user=None)
    return AuthResponse(success=True, message="認証済み", user=user)


@router.post("/token")
async def refresh_token(
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """トークン更新.

    現在認証済みのユーザーに新しいアクセストークンを発行する。

    Args:
        user: 認証済みユーザー

    Returns:
        新しいトークン
    """
    service = _auth_service()
    access_token = service.create_access_token(user)
    logger.info("トークン更新: username=%s", user.username)
    return {
        "access_token": access_token,
        "token_type": "bearer",
        "expires_in": service.get_expire_seconds(),
    }


@router.post("/password/change")
async def change_password(
    req: ChangePasswordRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """パスワード変更."""
    success, message = await _auth_service().change_password(
        username=user.username,
        current_password=req.current_password,
        new_password=req.new_password,
    )
    if success:
        logger.info("パスワード変更成功: username=%s", user.username)
    else:
        logger.warning("パスワード変更失敗: username=%s, reason=%s", user.username, message)
    return {"success": success, "message": message}


@router.post("/password/forgot")
async def forgot_password(req: ForgotPasswordRequest) -> dict[str, Any]:
    """パスワード再設定トークン発行."""
    service = _auth_service()
    reset_token = await service.create_password_reset_token(req.username)
    response: dict[str, Any] = {
        "success": True,
        "message": "アカウントが存在する場合、再設定手順を送信しました。",
    }
    if reset_token and service.expose_reset_token_in_response():
        response["reset_token"] = reset_token
    return response


@router.post("/password/reset")
async def reset_password(req: ResetPasswordRequest) -> dict[str, Any]:
    """パスワード再設定."""
    success, message = await _auth_service().reset_password(
        reset_token=req.reset_token,
        new_password=req.new_password,
    )
    if success:
        logger.info("パスワード再設定成功")
    else:
        logger.warning("パスワード再設定失敗: reason=%s", message)
    return {"success": success, "message": message}


@router.patch("/profile", response_model=AuthResponse)
async def update_profile(
    req: ProfileUpdateRequest,
    user: UserInfo = Depends(require_auth),
) -> AuthResponse:
    """プロフィール更新."""
    updated = await _auth_service().update_profile(
        username=user.username,
        display_name=req.display_name,
        department=req.department,
        position=req.position,
    )
    if not updated:
        return AuthResponse(success=False, message="プロフィール更新に失敗しました")
    logger.info("プロフィール更新成功: username=%s", user.username)
    return AuthResponse(
        success=True,
        message="プロフィールを更新しました",
        user=updated,
    )


@router.post("/mfa/setup", response_model=MfaSetupResponse)
async def setup_mfa(
    user: UserInfo = Depends(require_auth),
) -> MfaSetupResponse:
    """MFA設定開始."""
    secret, uri = await _auth_service().enable_mfa(user.username)
    return MfaSetupResponse(secret=secret, uri=uri)


@router.post("/mfa/verify")
async def verify_mfa(
    req: MfaVerifyRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """MFA設定検証 (有効化)."""
    success = await _auth_service().verify_mfa_setup(user.username, req.totp_code)
    if not success:
        return {"success": False, "message": "コードが無効です"}
    return {"success": True, "message": "MFAを有効化しました"}


@router.post("/mfa/disable")
async def disable_mfa(
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """MFA無効化."""
    await _auth_service().disable_mfa(user.username)
    return {"success": True, "message": "MFAを無効化しました"}
