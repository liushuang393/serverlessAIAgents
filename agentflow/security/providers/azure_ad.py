import httpx
from agentflow.security.oauth2_provider import OAuth2Provider, OAuth2Token, ExternalIdentity

class AzureADOAuth2Provider(OAuth2Provider):
    """Azure AD OAuth2 プロバイダー."""

    def __init__(self, client_id: str, client_secret: str, redirect_uri: str, tenant_id: str = "common"):
        super().__init__(client_id, client_secret, redirect_uri)
        self.tenant_id = tenant_id

    async def _get_endpoints(self):
        return {
            "authorization_endpoint": f"https://login.microsoftonline.com/{self.tenant_id}/oauth2/v2.0/authorize",
            "token_endpoint": f"https://login.microsoftonline.com/{self.tenant_id}/oauth2/v2.0/token",
            "userinfo_endpoint": "https://graph.microsoft.com/v1.0/me",
        }

    async def get_authorization_url(self, state: str) -> str:
        endpoints = await self._get_endpoints()
        params = {
            "client_id": self.client_id,
            "redirect_uri": self.redirect_uri,
            "response_type": "code",
            "scope": "openid email profile User.Read offline_access",
            "state": state,
            "response_mode": "query",
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
                    "scope": "openid email profile User.Read offline_access",
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
            # Azure AD returns: id, displayName, mail, userPrincipalName, jobTitle, mobilePhone, officeLocation, preferredLanguage
            return ExternalIdentity(
                sub=data["id"],
                username=data.get("userPrincipalName") or data.get("mail"),
                email=data.get("mail") or data.get("userPrincipalName"),
                display_name=data.get("displayName"),
                picture=None, # Need separate call to https://graph.microsoft.com/v1.0/me/photo/$value
                provider="azure_ad",
                raw_info=data,
            )
