"""auth_service API ルーター.

エンドポイント一覧:
    POST /auth/register          - ユーザー登録
    POST /auth/login             - ログイン
    POST /auth/refresh           - トークンリフレッシュ
    POST /auth/logout            - ログアウト
    GET  /auth/me                - 現在のユーザー情報
    POST /auth/password/change   - パスワード変更
    POST /auth/password/forgot   - パスワード再設定要求
    POST /auth/password/reset    - パスワード再設定
    PUT  /auth/profile           - プロフィール更新
    POST /auth/mfa/setup         - MFA 設定開始
    POST /auth/mfa/verify        - MFA 設定完了
    POST /auth/mfa/disable       - MFA 無効化
    GET  /auth/oauth2/{provider} - OAuth2 認可開始
    GET  /auth/oauth2/{provider}/callback - OAuth2 コールバック
    GET  /auth/.well-known/jwks.json - JWKS エンドポイント
"""

from __future__ import annotations

import logging
import secrets
from typing import Any

from apps.auth_service.api.schemas import (
    AuthResponse,
    ChangePasswordRequest,
    ForgotPasswordRequest,
    LoginRequest,
    MessageResponse,
    MfaSetupResponse,
    MfaVerifyRequest,
    ProfileUpdateRequest,
    RegisterRequest,
    ResetPasswordRequest,
    TokenRefreshRequest,
    TokenResponse,
    UserInfo,
)
from apps.auth_service.config import get_settings
from apps.auth_service.service import AuthService, get_auth_service
from fastapi import APIRouter, Cookie, Depends, Header, HTTPException, Response, status


logger = logging.getLogger(__name__)

router = APIRouter(prefix="/auth", tags=["認証"])

_SESSION_COOKIE_KEY = "auth_session"


def _service() -> AuthService:
    return get_auth_service()


def _session_ttl() -> int:
    return get_settings().SESSION_TTL_SECONDS


# ---------------------------------------------------------------------------
# 認証依存関係
# ---------------------------------------------------------------------------


async def get_current_user(
    authorization: str | None = Header(None),
    session_token: str | None = Cookie(None, alias=_SESSION_COOKIE_KEY),
) -> UserInfo | None:
    """現在のユーザーを取得（任意）.

    JWT トークンまたはセッション Cookie から認証する。
    """
    svc = _service()

    if authorization:
        user = await svc.verify_access_token(authorization)
        if user:
            return user

    if session_token:
        user = await svc.get_user_by_session(session_token)
        if user:
            return user

    return None


async def require_auth(
    user: UserInfo | None = Depends(get_current_user),
) -> UserInfo:
    """認証必須依存関係."""
    if user is None:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="認証が必要です",
            headers={"WWW-Authenticate": "Bearer"},
        )
    return user


# ---------------------------------------------------------------------------
# ユーザー登録・ログイン
# ---------------------------------------------------------------------------


@router.post("/register", response_model=AuthResponse, status_code=status.HTTP_201_CREATED)
async def register(req: RegisterRequest, response: Response) -> AuthResponse:
    """ユーザー登録（local_db モードのみ）."""
    svc = _service()
    success, message, user, token_pair = await svc.register(
        username=req.username,
        password=req.password,
        display_name=req.display_name,
        department=req.department or "",
        position=req.position or "",
        email=req.email,
    )

    if not success or user is None or token_pair is None:
        logger.warning("登録失敗: username=%s, reason=%s", req.username, message)
        return AuthResponse(success=False, message=message)

    session_token = await svc.create_session(user)
    _set_session_cookie(response, session_token)

    logger.info("登録成功: username=%s", req.username)
    return AuthResponse(
        success=True,
        message=message,
        user=user,
        access_token=token_pair.access_token,
        refresh_token=token_pair.refresh_token,
        token_type=token_pair.token_type,
        expires_in=token_pair.expires_in,
    )


@router.post("/login", response_model=AuthResponse)
async def login(req: LoginRequest, response: Response) -> AuthResponse:
    """ログイン処理."""
    svc = _service()
    success, message, user, token_pair = await svc.login(
        username=req.username,
        password=req.password,
        totp_code=req.totp_code,
    )

    if not success or user is None or token_pair is None:
        if message == "MFA_REQUIRED":
            logger.info("MFA 必須: username=%s", req.username)
            return AuthResponse(success=False, message="MFA_REQUIRED")

        logger.warning("ログイン失敗: username=%s, reason=%s", req.username, message)
        return AuthResponse(success=False, message=message)

    session_token = await svc.create_session(user)
    _set_session_cookie(response, session_token)

    logger.info("ログイン成功: username=%s, role=%s", req.username, user.role)
    return AuthResponse(
        success=True,
        message=message,
        user=user,
        access_token=token_pair.access_token,
        refresh_token=token_pair.refresh_token,
        token_type=token_pair.token_type,
        expires_in=token_pair.expires_in,
    )


@router.post("/refresh", response_model=TokenResponse)
async def refresh_token(req: TokenRefreshRequest) -> TokenResponse:
    """リフレッシュトークンで新しいアクセストークンを発行."""
    svc = _service()
    success, message, token_pair = await svc.refresh_access_token(req.refresh_token)

    if not success or token_pair is None:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail=message,
        )

    return TokenResponse(
        access_token=token_pair.access_token,
        refresh_token=token_pair.refresh_token,
        token_type=token_pair.token_type,
        expires_in=token_pair.expires_in,
    )


@router.post("/logout", response_model=MessageResponse)
async def logout(
    response: Response,
    user: UserInfo | None = Depends(get_current_user),
    authorization: str | None = Header(None),
    session_token: str | None = Cookie(None, alias=_SESSION_COOKIE_KEY),
) -> MessageResponse:
    """ログアウト処理."""
    response.delete_cookie(key=_SESSION_COOKIE_KEY)

    if user:
        access_token: str | None = None
        if authorization and authorization.startswith("Bearer "):
            access_token = authorization[7:]

        await _service().logout(
            user_id=user.user_id,
            access_token=access_token,
            session_token=session_token,
        )
        logger.info("ログアウト: username=%s", user.username)

    return MessageResponse(success=True, message="ログアウトしました")


# ---------------------------------------------------------------------------
# ユーザー情報
# ---------------------------------------------------------------------------


@router.get("/me", response_model=AuthResponse)
async def get_me(user: UserInfo | None = Depends(get_current_user)) -> AuthResponse:
    """現在のユーザー情報を取得."""
    if user is None:
        return AuthResponse(success=False, message="未認証")
    return AuthResponse(success=True, message="認証済み", user=user)


# ---------------------------------------------------------------------------
# パスワード操作
# ---------------------------------------------------------------------------


@router.post("/password/change", response_model=MessageResponse)
async def change_password(
    req: ChangePasswordRequest,
    user: UserInfo = Depends(require_auth),
) -> MessageResponse:
    """パスワード変更."""
    success, message = await _service().change_password(
        username=user.username,
        current_password=req.current_password,
        new_password=req.new_password,
    )
    if not success:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=message)
    return MessageResponse(success=True, message=message)


@router.post("/password/forgot", response_model=MessageResponse)
async def forgot_password(req: ForgotPasswordRequest) -> MessageResponse:
    """パスワード再設定トークンを発行（メール送信想定）."""
    svc = _service()
    reset_token = await svc.create_password_reset_token(req.username)
    response_data: dict[str, Any] = {
        "success": True,
        "message": "アカウントが存在する場合、再設定手順をメールで送信します。",
    }
    settings = get_settings()
    if reset_token and settings.DEV_MODE:
        # 開発モードのみトークンをレスポンスに含める
        response_data["reset_token"] = reset_token
        logger.debug("DEV_MODE: reset_token=%s", reset_token)
    return MessageResponse(**response_data)


@router.post("/password/reset", response_model=MessageResponse)
async def reset_password(req: ResetPasswordRequest) -> MessageResponse:
    """パスワード再設定."""
    success, message = await _service().reset_password(
        reset_token=req.reset_token,
        new_password=req.new_password,
    )
    if not success:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=message)
    return MessageResponse(success=True, message=message)


# ---------------------------------------------------------------------------
# プロフィール
# ---------------------------------------------------------------------------


@router.put("/profile", response_model=AuthResponse)
async def update_profile(
    req: ProfileUpdateRequest,
    user: UserInfo = Depends(require_auth),
) -> AuthResponse:
    """プロフィール更新."""
    updated = await _service().update_profile(
        username=user.username,
        display_name=req.display_name,
        department=req.department,
        position=req.position,
    )
    if updated is None:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="ユーザーが見つかりません",
        )
    return AuthResponse(success=True, message="プロフィールを更新しました", user=updated)


# ---------------------------------------------------------------------------
# MFA
# ---------------------------------------------------------------------------


@router.post("/mfa/setup", response_model=MfaSetupResponse)
async def setup_mfa(user: UserInfo = Depends(require_auth)) -> MfaSetupResponse:
    """MFA 設定開始（シークレットと QR コード URI を返す）."""
    secret, uri = await _service().setup_mfa(user.username)
    return MfaSetupResponse(secret=secret, uri=uri)


@router.post("/mfa/verify", response_model=MessageResponse)
async def verify_mfa(
    req: MfaVerifyRequest,
    user: UserInfo = Depends(require_auth),
) -> MessageResponse:
    """MFA 設定完了（コード検証して有効化）."""
    success = await _service().verify_mfa_setup(user.username, req.totp_code)
    if not success:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="MFA コードが無効です",
        )
    return MessageResponse(success=True, message="MFA を有効化しました")


@router.post("/mfa/disable", response_model=MessageResponse)
async def disable_mfa(user: UserInfo = Depends(require_auth)) -> MessageResponse:
    """MFA 無効化."""
    await _service().disable_mfa(user.username)
    return MessageResponse(success=True, message="MFA を無効化しました")


# ---------------------------------------------------------------------------
# OAuth2 フロー
# ---------------------------------------------------------------------------


@router.get("/oauth2/{provider}")
async def oauth2_authorize(provider: str) -> dict[str, str]:
    """OAuth2 認可 URL へのリダイレクト情報を返す."""
    settings = get_settings()
    state = secrets.token_urlsafe(16)

    if provider == "google":
        from apps.auth_service.providers.google import GoogleOAuth2Provider

        p = GoogleOAuth2Provider(settings=settings)
        url = p.get_authorization_url(state)
    elif provider in {"azure_ad", "microsoft"}:
        from apps.auth_service.providers.azure_ad import AzureADProvider

        p = AzureADProvider(settings=settings)
        url = p.get_authorization_url(state)
    else:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"未対応のプロバイダー: {provider}",
        )

    return {"authorization_url": url, "state": state}


@router.get("/oauth2/{provider}/callback")
async def oauth2_callback(
    provider: str,
    code: str,
    state: str | None = None,
    response: Response = Response(),  # type: ignore[assignment]
) -> AuthResponse:
    """OAuth2 コールバック処理."""
    settings = get_settings()
    identity = None

    if provider == "google":
        from apps.auth_service.providers.google import GoogleOAuth2Provider

        p = GoogleOAuth2Provider(settings=settings)
        identity = await p.exchange_code(code)
    elif provider in {"azure_ad", "microsoft"}:
        from apps.auth_service.providers.azure_ad import AzureADProvider

        p = AzureADProvider(settings=settings)
        redirect_uri = f"http://localhost:{settings.AUTH_SERVICE_PORT}/auth/oauth2/{provider}/callback"
        identity = await p.exchange_code(code, redirect_uri)
    else:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"未対応のプロバイダー: {provider}",
        )

    if identity is None:
        return AuthResponse(success=False, message="OAuth2 認証に失敗しました")

    svc = _service()
    user, token_pair = await svc.login_with_external_identity(identity, provider)

    session_token = await svc.create_session(user)
    _set_session_cookie(response, session_token)

    return AuthResponse(
        success=True,
        message="OAuth2 ログイン成功",
        user=user,
        access_token=token_pair.access_token,
        refresh_token=token_pair.refresh_token,
        expires_in=token_pair.expires_in,
    )


# ---------------------------------------------------------------------------
# JWKS エンドポイント
# ---------------------------------------------------------------------------


@router.get("/.well-known/jwks.json")
async def get_jwks() -> dict[str, Any]:
    """JWKS (JSON Web Key Set) エンドポイント.

    クライアントがオフラインでトークンを検証するための公開鍵情報を返す。
    HS256 の場合は共有シークレット方式のため鍵メタデータのみを返す。
    """
    return _service().get_jwks()


# ---------------------------------------------------------------------------
# ヘルスチェック（ルーターレベル）
# ---------------------------------------------------------------------------


@router.get("/health")
async def auth_health() -> dict[str, str]:
    """auth サービスヘルスチェック."""
    return {"status": "ok", "service": "auth"}


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


def _set_session_cookie(response: Response, session_token: str) -> None:
    """セッション Cookie を設定."""
    ttl = get_settings().SESSION_TTL_SECONDS
    response.set_cookie(
        key=_SESSION_COOKIE_KEY,
        value=session_token,
        httponly=True,
        samesite="lax",
        max_age=ttl,
    )
