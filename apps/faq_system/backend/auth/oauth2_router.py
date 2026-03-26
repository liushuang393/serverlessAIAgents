import secrets

from fastapi import APIRouter, HTTPException, Request
from fastapi.responses import RedirectResponse

from apps.faq_system.backend.auth.router import _SESSION_COOKIE_MAX_AGE
from apps.faq_system.backend.auth.service import get_auth_service
from infrastructure.security.oauth2_provider import ExternalIdentity as OAuth2Identity
from shared.auth_service.config import get_settings
from shared.auth_service.providers.azure_ad import AzureADProvider
from shared.auth_service.providers.base import ExternalIdentity as SharedExternalIdentity
from shared.auth_service.providers.google import GoogleOAuth2Provider


router = APIRouter(prefix="/api/auth/oauth2", tags=["OAuth2"])

# State cache (simple in-memory for MVP, use Redis/DB for production)
_oauth_states: dict[str, str] = {}  # state -> provider


def _to_oauth_identity(provider_name: str, identity: SharedExternalIdentity) -> OAuth2Identity:
    """shared.auth_service の外部 ID を FAQ auth 契約へ変換."""
    return OAuth2Identity(
        provider=provider_name,
        username=identity.username,
        email=identity.email or "",
        display_name=identity.display_name,
        department=identity.department,
        position=identity.position,
        role=identity.role,
        raw=identity.raw_info,
    )


@router.get("/{provider_name}/authorize")
async def authorize(provider_name: str, request: Request) -> RedirectResponse:
    """認可リクエストを開始."""
    # Build dynamic redirect URI based on request
    # This assumes the app is served at the same host/port/scheme as request
    # Or use configured base URL. For dev, request.base_url is fine.
    # The callback path is /api/auth/oauth2/{provider_name}/callback
    # Note: request.base_url includes /api/ if mounted there? No, usually root.
    # FastApi mounts, checking request.url for base.

    # We need the frontend URL for redirect_uri usually? No, the callback is backend endpoint.
    # But backend confirms callback.

    # Construct callback URL. request.url.scheme + "://" + request.url.netloc + ...
    # Be careful with proxies.
    # Assuming configured PUBLIC_URL or similar if behind proxy.
    # For MVP local, using request.base_url.

    # Wait, the route is /api/auth/oauth2...
    # The redirect_uri passed to provider MUST match exactly what's registered.
    # Usually http://localhost:8000/api/auth/oauth2/google/callback

    state = secrets.token_urlsafe(32)
    _oauth_states[state] = provider_name

    settings = get_settings()
    if provider_name == "google":
        url = GoogleOAuth2Provider(settings=settings).get_authorization_url(state)
    elif provider_name == "azure_ad":
        url = AzureADProvider(settings=settings).get_authorization_url(state)
    else:
        raise HTTPException(status_code=400, detail=f"Unknown provider: {provider_name}")
    return RedirectResponse(url)


@router.get("/{provider_name}/callback")
async def callback(
    provider_name: str,
    request: Request,
    code: str,
    state: str,
    error: str | None = None,
) -> RedirectResponse:
    """認可コールバック処理."""
    if error:
        # Error from provider
        return RedirectResponse(url=f"/?error={error}")

    if state not in _oauth_states:
        raise HTTPException(status_code=400, detail="Invalid state")

    if _oauth_states[state] != provider_name:
        raise HTTPException(status_code=400, detail="Provider mismatch")

    del _oauth_states[state]

    base_url = str(request.base_url).rstrip("/")
    redirect_uri = f"{base_url}/api/auth/oauth2/{provider_name}/callback"

    settings = get_settings()

    try:
        external_identity: SharedExternalIdentity | None
        if provider_name == "google":
            external_identity = await GoogleOAuth2Provider(settings=settings).exchange_code(code)
        elif provider_name == "azure_ad":
            external_identity = await AzureADProvider(settings=settings).exchange_code(code, redirect_uri)
        else:
            raise HTTPException(status_code=400, detail=f"Unknown provider: {provider_name}")
    except Exception as e:
        # Log error
        import logging

        logging.exception(f"OAuth2 exchange failed: {e}")
        return RedirectResponse(url="/login?error=oauth_failed")
    if external_identity is None:
        return RedirectResponse(url="/login?error=oauth_failed")
    external_user = _to_oauth_identity(provider_name, external_identity)

    # Authenticate/Register logic
    service = get_auth_service()

    # Map external identity to internal user
    # We use _upsert_external_user logic but exposed via service?
    # Service has `_upsert_external_user` as private async method.
    # But `login` calls it.
    # Here we have ExternalIdentity.
    # We should expose a method in Service to handle external identity directly.
    # Or reuse existing methods?
    # `login` takes username/password.
    # We need `login_external(external_identity: ExternalIdentity)`.

    # Let's add `login_external` to AuthService in service.py (will modify service.py next step).
    # Assuming it exists for now, or I'll implement it inline here using private method access (bad practice but effective for MVP).
    # Better to add public method.

    # For now, let's assume `login_external` exists in service.
    # I'll update service.py in next step.

    user_info = await service.login_external(external_user)
    if not user_info:
        return RedirectResponse(url="/login?error=account_creation_failed")

    # Create session
    access_token = service.create_access_token(user_info)
    session_token = await service.create_session(user_info)

    # Redirect to frontend with token
    # How to pass token to frontend?
    # Usually:
    # 1. Set cookie (session_token, access_token?)
    # 2. Redirect to /?token=... (insecure for access token in URL history)
    # 3. Redirect to /auth/callback?token=... handled by frontend.

    response = RedirectResponse(url="/")  # Or /oauth/callback frontend route

    # Secure way: Set session cookie (HttpOnly) and redirect to app.
    # App checks session cookie endpoint (/me) and gets user info.
    # What about access_token? Frontend stores it in localStorage usually in this app.
    # If we only set cookie, frontend needs to fetch access_token via cookie?
    # Current flow: /login returns access_token json.
    # If SSO, we are redirected. We can't return JSON.
    # We can set a temporary cookie with access_token (not HttpOnly) so frontend can read it once and store it?
    # Or just use session cookie for everything?
    # The app seems to use `useAuthStore` with `access_token` in `localStorage`.
    # So we need to give `access_token` to frontend.

    # Pattern: Redirect to frontend `/auth/callback?access_token=...`
    # Frontend reads param, saves to local storage, clears param from URL.

    redirect_url = f"/auth/callback?access_token={access_token}"  # Simple approach

    response = RedirectResponse(url=redirect_url)
    response.set_cookie(
        key="session_token",
        value=session_token,
        httponly=True,
        samesite="lax",
        max_age=_SESSION_COOKIE_MAX_AGE,
    )

    return response
