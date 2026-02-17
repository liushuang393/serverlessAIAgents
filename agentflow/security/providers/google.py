import httpx
from agentflow.security.oauth2_provider import OAuth2Provider, OAuth2Token, ExternalIdentity

class GoogleOAuth2Provider(OAuth2Provider):
    """Google OAuth2 プロバイダー."""

    DISCOVERY_URL = "https://accounts.google.com/.well-known/openid-configuration"

    async def _get_endpoints(self):
        # Cache this if possible, or fetch every time? Fetching every time is slow.
        # Hardcoding for now, or fetch once per instance?
        # Standard endpoints:
        return {
            "authorization_endpoint": "https://accounts.google.com/o/oauth2/v2/auth",
            "token_endpoint": "https://oauth2.googleapis.com/token",
            "userinfo_endpoint": "https://openidconnect.googleapis.com/v1/userinfo",
        }

    async def get_authorization_url(self, state: str) -> str:
        endpoints = await self._get_endpoints()
        params = {
            "client_id": self.client_id,
            "redirect_uri": self.redirect_uri,
            "response_type": "code",
            "scope": "openid email profile",
            "state": state,
            "access_type": "offline",
            "prompt": "consent",
        }
        query = "&".join([f"{k}={v}" for k, v in params.items()])
        return f"{endpoints['authorization_endpoint']}?{query}"

    async def exchange_code(self, code: str) -> OAuth2Token:
        endpoints = await self._get_endpoints()
        async with httpx.AsyncClient() as client:
            response = await client.post(
                endpoints["token_endpoint"],
                data={
                    "code": code,
                    "client_id": self.client_id,
                    "client_secret": self.client_secret,
                    "redirect_uri": self.redirect_uri,
                    "grant_type": "authorization_code",
                },
            )
            response.raise_for_status()
            data = response.json()
            return OAuth2Token(**data)

    async def get_user_info(self, token: OAuth2Token) -> ExternalIdentity:
        endpoints = await self._get_endpoints()
        async with httpx.AsyncClient() as client:
            response = await client.get(
                endpoints["userinfo_endpoint"],
                headers={"Authorization": f"Bearer {token.access_token}"},
            )
            response.raise_for_status()
            data = response.json()
            # Google returns: sub, name, given_name, family_name, picture, email, email_verified, locale
            return ExternalIdentity(
                sub=data["sub"],
                username=data.get("email"),  # Google doesn't distinct username usually
                email=data.get("email"),
                display_name=data.get("name"),
                picture=data.get("picture"),
                provider="google",
                raw_info=data,
            )
