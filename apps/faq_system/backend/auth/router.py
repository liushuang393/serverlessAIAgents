"""FAQ システム認証ルーター.

基本方針:
1. auth_service を優先（tenant SSO）
2. auth_service 到達不可時のみローカル互換 fallback（既定は pytest 時のみ有効）
"""

from __future__ import annotations

import json
import logging
import os
from pathlib import Path
from typing import Any

import httpx
from fastapi import APIRouter, Cookie, Depends, HTTPException, Request, Response
from fastapi.responses import RedirectResponse

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
    TokenRefreshRequest,
    UserInfo,
)
from apps.faq_system.backend.auth.service import get_auth_service


logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/auth", tags=["認証"])

_SESSION_COOKIE_MAX_AGE = int(os.getenv("FAQ_SESSION_TTL_SECONDS", str(86400 * 7)))
_APP_CONFIG_PATH = Path(__file__).resolve().parents[2] / "app_config.json"
_AUTH_MODE_TENANT_SSO = "tenant_sso"
_AUTH_MODE_ENTERPRISE_ISOLATED = "enterprise_isolated"


class AuthProxyUnavailable(Exception):
    """auth_service への到達不可を示す内部例外."""


def _auth_service_base_url() -> str:
    return os.getenv("AUTH_SERVICE_URL", "http://localhost:8010").rstrip("/")


def _env_bool(name: str) -> bool | None:
    raw = os.getenv(name)
    if raw is None:
        return None
    return raw.lower() in {"1", "true", "yes", "on"}


def _local_fallback_enabled() -> bool:
    """ローカル認証フォールバックの有効判定.

    FAQ_AUTH_PROVIDER=local_db の場合、FAQ 側にローカル認証 DB があるため
    auth_service 到達不可時にフォールバック可能。
    テスト時は常に有効。
    """
    if os.getenv("PYTEST_CURRENT_TEST"):
        return True
    return os.getenv("FAQ_AUTH_PROVIDER", "local_db") == "local_db"


def _normalize_auth_mode(value: str | None) -> str:
    cleaned = _clean_text(value)
    if not cleaned:
        return _AUTH_MODE_TENANT_SSO
    normalized = cleaned.lower()
    if normalized in {"tenant_sso", "tenant", "sso"}:
        return _AUTH_MODE_TENANT_SSO
    if normalized in {
        "enterprise_isolated",
        "enterprise",
        "legacy",
        "single_app",
        "app_isolated",
    }:
        return _AUTH_MODE_ENTERPRISE_ISOLATED
    return _AUTH_MODE_TENANT_SSO


def _load_contract_auth_mode() -> str | None:
    if not _APP_CONFIG_PATH.is_file():
        return None
    try:
        payload = json.loads(_APP_CONFIG_PATH.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    contracts_raw = payload.get("contracts")
    contracts = contracts_raw if isinstance(contracts_raw, dict) else {}
    auth_raw = contracts.get("auth")
    auth = auth_raw if isinstance(auth_raw, dict) else {}
    return _clean_text(auth.get("mode"))


def _resolve_auth_mode() -> str:
    env_mode = _clean_text(os.getenv("FAQ_AUTH_MODE"))
    if env_mode:
        return _normalize_auth_mode(env_mode)
    return _normalize_auth_mode(_load_contract_auth_mode())


def _clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _derive_tenant_id(request: Request) -> str | None:
    header_candidates = os.getenv("FAQ_TENANT_HEADER_NAMES", "x-tenant-id").split(",")
    for key in (item.strip().lower() for item in header_candidates if item.strip()):
        value = request.headers.get(key)
        cleaned = _clean_text(value)
        if cleaned:
            return cleaned
    return _clean_text(os.getenv("FAQ_DEFAULT_TENANT_ID"))


def _default_requested_scopes() -> list[str]:
    raw = os.getenv("FAQ_REQUESTED_SCOPES", "api,faq.access")
    scopes = [item.strip() for item in raw.split(",") if item.strip()]
    deduped: list[str] = []
    for scope in scopes:
        if scope not in deduped:
            deduped.append(scope)
    return deduped


def _build_login_payload(req: LoginRequest, request: Request) -> dict[str, Any]:
    payload = req.model_dump()
    if not _clean_text(payload.get("client_app")):
        payload["client_app"] = _clean_text(os.getenv("FAQ_APP_NAME", "faq_system"))
    if _resolve_auth_mode() == _AUTH_MODE_TENANT_SSO:
        if not _clean_text(payload.get("tenant_id")):
            payload["tenant_id"] = _derive_tenant_id(request)
        requested_scopes = payload.get("requested_scopes")
        if not isinstance(requested_scopes, list) or not requested_scopes:
            payload["requested_scopes"] = _default_requested_scopes()
    else:
        payload["tenant_id"] = None
        payload["requested_scopes"] = []
    return payload


async def _forward_json(
    *,
    method: str,
    path: str,
    request: Request,
    payload: dict[str, Any] | None = None,
    params: dict[str, Any] | None = None,
) -> dict[str, Any]:
    headers: dict[str, str] = {"Content-Type": "application/json"}
    authorization = request.headers.get("authorization")
    if authorization:
        headers["Authorization"] = authorization

    target_url = f"{_auth_service_base_url()}{path}"
    try:
        async with httpx.AsyncClient(timeout=15.0, follow_redirects=False) as client:
            response = await client.request(
                method=method,
                url=target_url,
                json=payload,
                params=params,
                headers=headers,
            )
    except httpx.HTTPError as exc:
        raise AuthProxyUnavailable(str(exc)) from exc

    if response.status_code >= 400:
        detail: Any
        try:
            detail = response.json()
        except ValueError:
            detail = {"message": response.text or "auth_service returned error"}
        raise HTTPException(status_code=response.status_code, detail=detail)

    try:
        data = response.json()
    except ValueError as exc:
        raise HTTPException(
            status_code=502,
            detail={"message": "auth_service returned non-json response", "error_code": "auth_proxy_invalid_response"},
        ) from exc

    return data if isinstance(data, dict) else {}


def _local_auth_service() -> Any:
    return get_auth_service()


def _set_local_session_cookie(response: Response, session_token: str) -> None:
    response.set_cookie(
        key="session_token",
        value=session_token,
        httponly=True,
        samesite="lax",
        max_age=_SESSION_COOKIE_MAX_AGE,
    )


def _raise_proxy_unavailable(detail: str) -> None:
    raise HTTPException(
        status_code=502,
        detail={"message": f"auth_service request failed: {detail}", "error_code": "auth_proxy_failed"},
    )


@router.post("/login", response_model=AuthResponse)
async def login(req: LoginRequest, request: Request, response: Response) -> AuthResponse:
    payload = _build_login_payload(req, request)
    try:
        data = await _forward_json(
            method="POST",
            path="/auth/login",
            request=request,
            payload=payload,
        )
        return AuthResponse.model_validate(data)
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    service = _local_auth_service()
    success, message, user = await service.authenticate(req.username, req.password, req.totp_code)
    if not success or user is None:
        if message == "MFA_REQUIRED":
            return AuthResponse(success=False, message="MFA_REQUIRED")
        return AuthResponse(success=False, message=message)

    access_token = service.create_access_token(user)
    session_token = await service.create_session(user)
    _set_local_session_cookie(response, session_token)
    return AuthResponse(
        success=True,
        message=message,
        user=user,
        access_token=access_token,
        token_type="bearer",
        expires_in=service.get_expire_seconds(),
    )


@router.post("/sso/login", response_model=None)
async def sso_login(request: Request) -> Response:
    """SSO ログインを auth_service に委譲し、リダイレクトを透過する."""
    body = await request.json()
    payload = body if isinstance(body, dict) else {}
    if not _clean_text(payload.get("client_app")):
        payload["client_app"] = _clean_text(os.getenv("FAQ_APP_NAME", "faq_system"))
    if _resolve_auth_mode() == _AUTH_MODE_TENANT_SSO:
        if not _clean_text(payload.get("tenant_id")):
            payload["tenant_id"] = _derive_tenant_id(request)
        if not isinstance(payload.get("requested_scopes"), list) or not payload.get("requested_scopes"):
            payload["requested_scopes"] = _default_requested_scopes()
    else:
        payload["tenant_id"] = None
        payload["requested_scopes"] = []

    headers: dict[str, str] = {"Content-Type": "application/json"}
    authorization = request.headers.get("authorization")
    if authorization:
        headers["Authorization"] = authorization

    target_url = f"{_auth_service_base_url()}/auth/sso/login"
    try:
        async with httpx.AsyncClient(timeout=15.0, follow_redirects=False) as client:
            upstream = await client.post(target_url, json=payload, headers=headers)
    except httpx.HTTPError as exc:
        _raise_proxy_unavailable(str(exc))

    if upstream.status_code >= 400:
        try:
            detail = upstream.json()
        except ValueError:
            detail = {"message": upstream.text or "auth_service returned error"}
        raise HTTPException(status_code=upstream.status_code, detail=detail)

    location = upstream.headers.get("location")
    if location:
        return RedirectResponse(url=location, status_code=upstream.status_code)
    try:
        data = upstream.json()
    except ValueError as exc:
        raise HTTPException(
            status_code=502,
            detail={"message": "auth_service returned non-json response", "error_code": "auth_proxy_invalid_response"},
        ) from exc
    return Response(content=json.dumps(data), media_type="application/json", status_code=200)


@router.get("/sso/login")
async def sso_login_page(
    request: Request, redirect_uri: str | None = None, client_app: str | None = None
) -> dict[str, Any]:
    params: dict[str, Any] = {}
    if redirect_uri:
        params["redirect_uri"] = redirect_uri
    if client_app:
        params["client_app"] = client_app
    try:
        return await _forward_json(
            method="GET",
            path="/auth/sso/login",
            request=request,
            params=params or None,
        )
    except AuthProxyUnavailable as exc:
        _raise_proxy_unavailable(str(exc))


@router.post("/register", response_model=AuthResponse, status_code=201)
async def register(req: RegisterRequest, request: Request, response: Response) -> AuthResponse:
    try:
        data = await _forward_json(
            method="POST",
            path="/auth/register",
            request=request,
            payload=req.model_dump(),
        )
        return AuthResponse.model_validate(data)
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    service = _local_auth_service()
    success, message, user = await service.register_user(
        username=req.username,
        password=req.password,
        display_name=req.display_name,
        department=req.department,
        position=req.position,
        email=req.email,
    )
    if not success or user is None:
        return AuthResponse(success=False, message=message)

    access_token = service.create_access_token(user)
    session_token = await service.create_session(user)
    _set_local_session_cookie(response, session_token)
    return AuthResponse(
        success=True,
        message=message,
        user=user,
        access_token=access_token,
        token_type="bearer",
        expires_in=service.get_expire_seconds(),
    )


@router.post("/logout")
async def logout(
    request: Request,
    response: Response,
    user: UserInfo = Depends(require_auth),
    session_token: str | None = Cookie(None, alias="session_token"),
) -> dict[str, Any]:
    try:
        data = await _forward_json(
            method="POST",
            path="/auth/logout",
            request=request,
            payload=None,
        )
        response.delete_cookie(key="session_token")
        return data
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    service = _local_auth_service()
    response.delete_cookie(key="session_token")
    await service.revoke_session_token(session_token)
    await service.revoke_sessions_for_user(user.user_id)
    await service.blacklist_token(request.headers.get("authorization"), user.user_id)
    return {"success": True, "message": "ログアウトしました"}


@router.get("/me", response_model=AuthResponse)
async def get_me(
    request: Request,
    user: UserInfo | None = Depends(get_current_user),
) -> AuthResponse:
    try:
        data = await _forward_json(
            method="GET",
            path="/auth/me",
            request=request,
        )
        return AuthResponse.model_validate(data)
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    if user is None:
        return AuthResponse(success=False, message="未認証", user=None)
    return AuthResponse(success=True, message="認証済み", user=user)


@router.post("/token")
async def refresh_token(
    request: Request,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    # 既存クライアント互換: Bearer トークンを再返却する
    # （auth_service の refresh token 導線へ段階移行するまでの互換エンドポイント）
    authorization = request.headers.get("authorization", "")
    if not authorization.lower().startswith("bearer "):
        raise HTTPException(
            status_code=401,
            detail={"message": "Bearer token is required", "error_code": "token_invalid"},
        )
    access_token = authorization.split(" ", 1)[1].strip()
    if not access_token:
        raise HTTPException(
            status_code=401,
            detail={"message": "Bearer token is required", "error_code": "token_invalid"},
        )
    return {
        "access_token": access_token,
        "token_type": "bearer",
        "expires_in": int(os.getenv("AUTH_ACCESS_EXPIRE_SECONDS", "1800")),
    }


@router.post("/refresh")
async def refresh_token_pair(req: TokenRefreshRequest, request: Request) -> dict[str, Any]:
    try:
        return await _forward_json(
            method="POST",
            path="/auth/refresh",
            request=request,
            payload=req.model_dump(),
        )
    except AuthProxyUnavailable as exc:
        _raise_proxy_unavailable(str(exc))


@router.post("/password/change")
async def change_password(
    req: ChangePasswordRequest,
    request: Request,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    try:
        return await _forward_json(
            method="POST",
            path="/auth/password/change",
            request=request,
            payload=req.model_dump(),
        )
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    success, message = await _local_auth_service().change_password(
        username=user.username,
        current_password=req.current_password,
        new_password=req.new_password,
    )
    return {"success": success, "message": message}


@router.post("/password/forgot")
async def forgot_password(req: ForgotPasswordRequest, request: Request) -> dict[str, Any]:
    try:
        return await _forward_json(
            method="POST",
            path="/auth/password/forgot",
            request=request,
            payload=req.model_dump(),
        )
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    service = _local_auth_service()
    reset_token = await service.create_password_reset_token(req.username)
    payload: dict[str, Any] = {
        "success": True,
        "message": "アカウントが存在する場合、再設定手順を送信しました。",
    }
    if reset_token and service.expose_reset_token_in_response():
        payload["reset_token"] = reset_token
    return payload


@router.post("/password/reset")
async def reset_password(req: ResetPasswordRequest, request: Request) -> dict[str, Any]:
    try:
        return await _forward_json(
            method="POST",
            path="/auth/password/reset",
            request=request,
            payload=req.model_dump(),
        )
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    success, message = await _local_auth_service().reset_password(
        reset_token=req.reset_token,
        new_password=req.new_password,
    )
    return {"success": success, "message": message}


@router.patch("/profile", response_model=AuthResponse)
async def update_profile(
    req: ProfileUpdateRequest,
    request: Request,
    user: UserInfo = Depends(require_auth),
) -> AuthResponse:
    try:
        data = await _forward_json(
            method="PUT",
            path="/auth/profile",
            request=request,
            payload=req.model_dump(exclude_none=True),
        )
        return AuthResponse.model_validate(data)
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    updated = await _local_auth_service().update_profile(
        username=user.username,
        display_name=req.display_name,
        department=req.department,
        position=req.position,
    )
    if updated is None:
        return AuthResponse(success=False, message="プロフィール更新に失敗しました")
    return AuthResponse(success=True, message="プロフィールを更新しました", user=updated)


@router.post("/mfa/setup", response_model=MfaSetupResponse)
async def setup_mfa(
    request: Request,
    user: UserInfo = Depends(require_auth),
) -> MfaSetupResponse:
    try:
        data = await _forward_json(
            method="POST",
            path="/auth/mfa/setup",
            request=request,
            payload=None,
        )
        return MfaSetupResponse.model_validate(data)
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    secret, uri = await _local_auth_service().enable_mfa(user.username)
    return MfaSetupResponse(secret=secret, uri=uri)


@router.post("/mfa/verify")
async def verify_mfa(
    req: MfaVerifyRequest,
    request: Request,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    try:
        return await _forward_json(
            method="POST",
            path="/auth/mfa/verify",
            request=request,
            payload=req.model_dump(),
        )
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    success = await _local_auth_service().verify_mfa_setup(user.username, req.totp_code)
    if not success:
        return {"success": False, "message": "コードが無効です"}
    return {"success": True, "message": "MFAを有効化しました"}


@router.post("/mfa/disable")
async def disable_mfa(
    request: Request,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    try:
        return await _forward_json(
            method="POST",
            path="/auth/mfa/disable",
            request=request,
            payload=None,
        )
    except AuthProxyUnavailable as exc:
        if not _local_fallback_enabled():
            _raise_proxy_unavailable(str(exc))

    await _local_auth_service().disable_mfa(user.username)
    return {"success": True, "message": "MFAを無効化しました"}


@router.get("/oauth2/{provider}")
async def oauth2_authorize(provider: str, request: Request) -> dict[str, str]:
    try:
        data = await _forward_json(
            method="GET",
            path=f"/auth/oauth2/{provider}",
            request=request,
        )
    except AuthProxyUnavailable as exc:
        _raise_proxy_unavailable(str(exc))
    return {k: str(v) for k, v in data.items()}


@router.get("/oauth2/{provider}/authorize", response_model=None)
async def oauth2_authorize_compat(provider: str, request: Request) -> RedirectResponse:
    try:
        data = await _forward_json(
            method="GET",
            path=f"/auth/oauth2/{provider}",
            request=request,
        )
    except AuthProxyUnavailable as exc:
        _raise_proxy_unavailable(str(exc))

    authorization_url = _clean_text(data.get("authorization_url"))
    if not authorization_url:
        raise HTTPException(
            status_code=502,
            detail={"message": "authorization_url is missing", "error_code": "auth_proxy_invalid_response"},
        )
    return RedirectResponse(url=authorization_url, status_code=302)


@router.get("/oauth2/{provider}/callback", response_model=None)
async def oauth2_callback(
    provider: str,
    code: str,
    state: str | None = None,
    redirect_uri: str | None = None,
) -> RedirectResponse:
    target_url = f"{_auth_service_base_url()}/auth/oauth2/{provider}/callback"
    params: dict[str, Any] = {"code": code}
    if state:
        params["state"] = state
    if redirect_uri:
        params["redirect_uri"] = redirect_uri
    query = httpx.QueryParams(params)
    return RedirectResponse(url=f"{target_url}?{query}", status_code=307)
