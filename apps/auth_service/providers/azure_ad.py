"""Azure AD (Microsoft Entra ID) OAuth2 認証プロバイダー.

Microsoft の OAuth2 / OIDC エンドポイントで認証する。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from apps.auth_service.providers.base import AuthProvider, AuthResult, ExternalIdentity


if TYPE_CHECKING:
    from apps.auth_service.config import Settings


logger = logging.getLogger(__name__)


def _azure_base_url(tenant_id: str) -> str:
    """Azure AD エンドポイント基底 URL."""
    return f"https://login.microsoftonline.com/{tenant_id or 'common'}/oauth2/v2.0"


class AzureADProvider(AuthProvider):
    """Azure AD (Entra ID) OAuth2 プロバイダー."""

    def __init__(self, settings: Settings) -> None:
        super().__init__(settings)

    @property
    def provider_name(self) -> str:
        return "azure_ad"

    async def authenticate(self, username: str, password: str) -> AuthResult:
        """パスワード認証は非対応."""
        return AuthResult(
            success=False,
            message="Azure AD 認証はパスワード方式に対応していません。OAuth2 フローを使用してください。",
        )

    def get_authorization_url(self, state: str) -> str:
        """OAuth2 認可 URL を生成.

        Args:
            state: CSRF 防止用のランダム文字列

        Returns:
            Azure AD 認可 URL
        """
        tenant_id = self._settings.AZURE_TENANT_ID or "common"
        base_url = _azure_base_url(tenant_id)
        client_id = self._settings.AZURE_CLIENT_ID
        redirect_uri = f"http://localhost:{self._settings.AUTH_SERVICE_PORT}/auth/oauth2/azure_ad/callback"
        scope = "openid email profile User.Read"

        params = "&".join(
            [
                f"client_id={client_id}",
                f"redirect_uri={redirect_uri}",
                "response_type=code",
                f"scope={scope}",
                f"state={state}",
                "response_mode=query",
            ]
        )
        return f"{base_url}/authorize?{params}"

    async def exchange_code(self, code: str, redirect_uri: str) -> ExternalIdentity | None:
        """認可コードをトークンに交換してユーザー情報を取得.

        Args:
            code: Azure AD から受け取った認可コード
            redirect_uri: リダイレクト URI

        Returns:
            ExternalIdentity または None
        """
        try:
            import httpx
        except ImportError:
            logger.exception("httpx がインストールされていません: pip install httpx")
            return None

        tenant_id = self._settings.AZURE_TENANT_ID or "common"
        base_url = _azure_base_url(tenant_id)
        client_id = self._settings.AZURE_CLIENT_ID
        client_secret = self._settings.AZURE_CLIENT_SECRET

        if not client_id or not client_secret:
            logger.error("AZURE_CLIENT_ID / AZURE_CLIENT_SECRET が未設定です")
            return None

        async with httpx.AsyncClient(timeout=10.0) as client:
            token_resp = await client.post(
                f"{base_url}/token",
                data={
                    "code": code,
                    "client_id": client_id,
                    "client_secret": client_secret,
                    "redirect_uri": redirect_uri,
                    "grant_type": "authorization_code",
                    "scope": "openid email profile User.Read",
                },
            )
            if token_resp.status_code >= 400:
                logger.warning("Azure AD トークン交換失敗: %s", token_resp.text)
                return None

            token_data: dict[str, Any] = token_resp.json()
            access_token = token_data.get("access_token")
            if not access_token:
                return None

            graph_resp = await client.get(
                "https://graph.microsoft.com/v1.0/me",
                headers={"Authorization": f"Bearer {access_token}"},
            )
            if graph_resp.status_code >= 400:
                return None

            userinfo: dict[str, Any] = graph_resp.json()

        email = str(userinfo.get("mail") or userinfo.get("userPrincipalName") or "")
        display_name = str(userinfo.get("displayName") or email)
        username = email.split("@")[0] if email else str(userinfo.get("id", ""))
        department = str(userinfo.get("officeLocation") or "")
        position = str(userinfo.get("jobTitle") or "")

        return ExternalIdentity(
            username=username,
            display_name=display_name,
            email=email,
            role="employee",
            department=department,
            position=position,
            raw_info=userinfo,
        )
