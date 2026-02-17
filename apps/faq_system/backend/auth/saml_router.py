from fastapi import APIRouter, Depends, HTTPException, Request, Response
from fastapi.responses import RedirectResponse, HTMLResponse
import os
from typing import Any

from agentflow.security.saml import SAMLProvider, SAMLIdentity
from apps.faq_system.backend.auth.service import get_auth_service, AuthService
from apps.faq_system.backend.auth.router import _SESSION_COOKIE_MAX_AGE

router = APIRouter(prefix="/auth/saml", tags=["SAML"])

def _get_saml_settings() -> dict[str, Any]:
    """SAML 設定を環境変数等から取得."""
    # 本来は詳細な設定が必要だが、MVPとして主要な項目のみ指定
    base_url = os.getenv("FAQ_PUBLIC_URL", "http://localhost:8001").rstrip("/")
    
    return {
        "strict": True,
        "debug": True,
        "sp": {
            "entityId": f"{base_url}/api/auth/saml/metadata",
            "assertionConsumerService": {
                "url": f"{base_url}/api/auth/saml/acs",
                "binding": "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST"
            },
            "singleLogoutService": {
                "url": f"{base_url}/api/auth/saml/slo",
                "binding": "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect"
            },
            "NameIDFormat": "urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified",
            "x509cert": os.getenv("FAQ_SAML_SP_CERT", ""),
            "privateKey": os.getenv("FAQ_SAML_SP_KEY", ""),
        },
        "idp": {
            "entityId": os.getenv("FAQ_SAML_IDP_ENTITY_ID", ""),
            "singleSignOnService": {
                "url": os.getenv("FAQ_SAML_IDP_SSO_URL", ""),
                "binding": "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect"
            },
            "singleLogoutService": {
                "url": os.getenv("FAQ_SAML_IDP_SLO_URL", ""),
                "binding": "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect"
            },
            "x509cert": os.getenv("FAQ_SAML_IDP_CERT", "")
        }
    }

async def _prepare_saml_request(request: Request) -> dict[str, Any]:
    """FastAPI リクエストから SAML 用リクエストデータを準備."""
    return {
        'https': request.url.scheme == 'https',
        'http_host': request.url.netloc,
        'server_port': request.url.port or (443 if request.url.scheme == 'https' else 80),
        'script_name': request.url.path,
        'get_data': dict(request.query_params),
        'post_data': await request.form(),
        'query_string': request.url.query
    }

@router.get("/login")
async def saml_login(request: Request):
    """SAML SSO を開始."""
    settings = _get_saml_settings()
    if not settings["idp"]["entityId"]:
        raise HTTPException(status_code=501, detail="SAML is not configured")
        
    provider = SAMLProvider(settings)
    saml_req = await _prepare_saml_request(request)
    sso_url = await provider.get_sso_url(saml_req)
    return RedirectResponse(sso_url)

@router.post("/acs")
async def saml_acs(request: Request):
    """Assertion Consumer Service (ログイン後の受け口)."""
    settings = _get_saml_settings()
    provider = SAMLProvider(settings)
    saml_req = await _prepare_saml_request(request)
    
    identity = await provider.process_response(saml_req)
    if not identity:
        return RedirectResponse(url="/login?error=saml_failed")

    # AuthService を使ってユーザー処理
    # identity.attributes に 'username', 'email' 等が含まれていることを期待
    service = get_auth_service()
    
    # マッピングロジック (IdP ごとに属性名が異なるため適宜調整が必要)
    from agentflow.security.oauth2_provider import ExternalIdentity
    
    attrs = identity.attributes
    # 例: OKTA や Azure AD の一般的な属性名
    email = attrs.get('email', attrs.get('mail', [None]))[0]
    display_name = attrs.get('displayName', attrs.get('name', [identity.name_id]))[0]
    
    ext_user = ExternalIdentity(
        sub=identity.name_id,
        username=email or identity.name_id, # メールを優先
        email=email,
        display_name=display_name,
        provider="saml"
    )
    
    user_info = await service.login_external(ext_user)
    if not user_info:
        return RedirectResponse(url="/login?error=account_creation_failed")

    access_token = service.create_access_token(user_info)
    session_token = await service.create_session(user_info)
    
    redirect_url = f"/auth/callback?access_token={access_token}"
    response = RedirectResponse(url=redirect_url)
    response.set_cookie(
        key="session_token",
        value=session_token,
        httponly=True,
        samesite="lax",
        max_age=_SESSION_COOKIE_MAX_AGE,
    )
    return response

@router.get("/metadata")
async def saml_metadata():
    """SP メタデータを公開."""
    settings = _get_saml_settings()
    from onelogin.saml2.settings import OneLogin_Saml2_Settings
    saml_settings = OneLogin_Saml2_Settings(settings)
    metadata = saml_settings.get_sp_metadata()
    errors = saml_settings.validate_metadata(metadata)
    
    if len(errors) == 0:
        return Response(content=metadata, media_type="text/xml")
    else:
        return HTMLResponse(content=", ".join(errors), status_code=500)
