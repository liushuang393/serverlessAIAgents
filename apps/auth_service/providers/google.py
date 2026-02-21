"""Google OAuth2 認証プロバイダー.

Google OAuth2 Authorization Code Flow を実装する。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from apps.auth_service.providers.base import AuthProvider, AuthResult, ExternalIdentity


if TYPE_CHECKING:
    from apps.auth_service.config import Settings


logger = logging.getLogger(__name__)

_GOOGLE_AUTH_URL = "https://accounts.google.com/o/oauth2/v2/auth"
_GOOGLE_TOKEN_URL = "https://oauth2.googleapis.com/token"
_GOOGLE_USERINFO_URL = "https://www.googleapis.com/oauth2/v3/userinfo"


class GoogleOAuth2Provider(AuthProvider):
    """Google OAuth2 プロバイダー.

    OAuth2 Authorization Code Flow で Google アカウントを認証する。
    パスワード認証は非対応（OAuth2 フロー専用）。
    """

    def __init__(self, settings: Settings) -> None:
        super().__init__(settings)

    @property
    def provider_name(self) -> str:
        return "google"

    async def authenticate(self, username: str, password: str) -> AuthResult:
        """パスワード認証は非対応."""
        return AuthResult(
            success=False,
            message="Google 認証はパスワード方式に対応していません。OAuth2 フローを使用してください。",
        )

    def get_authorization_url(self, state: str) -> str:
        """OAuth2 認可 URL を生成.

        Args:
            state: CSRF 防止用のランダム文字列

        Returns:
            Google 認可 URL
        """
        client_id = self._settings.GOOGLE_CLIENT_ID
        redirect_uri = self._settings.GOOGLE_REDIRECT_URI
        scope = "openid email profile"

        params = "&".join(
            [
                f"client_id={client_id}",
                f"redirect_uri={redirect_uri}",
                "response_type=code",
                f"scope={scope}",
                f"state={state}",
                "access_type=offline",
                "prompt=select_account",
            ]
        )
        return f"{_GOOGLE_AUTH_URL}?{params}"

    async def exchange_code(self, code: str) -> ExternalIdentity | None:
        """認可コードをトークンに交換し、ユーザー情報を取得.

        Args:
            code: Google から受け取った認可コード

        Returns:
            ExternalIdentity または None
        """
        try:
            import httpx
        except ImportError:
            logger.exception("httpx がインストールされていません: pip install httpx")
            return None

        client_id = self._settings.GOOGLE_CLIENT_ID
        client_secret = self._settings.GOOGLE_CLIENT_SECRET
        redirect_uri = self._settings.GOOGLE_REDIRECT_URI

        if not client_id or not client_secret:
            logger.error("GOOGLE_CLIENT_ID / GOOGLE_CLIENT_SECRET が未設定です")
            return None

        async with httpx.AsyncClient(timeout=10.0) as client:
            token_resp = await client.post(
                _GOOGLE_TOKEN_URL,
                data={
                    "code": code,
                    "client_id": client_id,
                    "client_secret": client_secret,
                    "redirect_uri": redirect_uri,
                    "grant_type": "authorization_code",
                },
            )
            if token_resp.status_code >= 400:
                logger.warning("Google トークン交換失敗: %s", token_resp.text)
                return None

            token_data: dict[str, Any] = token_resp.json()
            access_token = token_data.get("access_token")
            if not access_token:
                return None

            user_resp = await client.get(
                _GOOGLE_USERINFO_URL,
                headers={"Authorization": f"Bearer {access_token}"},
            )
            if user_resp.status_code >= 400:
                return None

            userinfo: dict[str, Any] = user_resp.json()

        sub = str(userinfo.get("sub", ""))
        email = str(userinfo.get("email", ""))
        name = str(userinfo.get("name", email or sub))
        username = email.split("@")[0] if email else sub

        return ExternalIdentity(
            username=username,
            display_name=name,
            email=email,
            role="employee",
            department="",
            position="",
            raw_info=userinfo,
        )
