"""agentflow.security.auth_client - auth_service クライアント SDK.

他のアプリから auth_service を利用するための最小限の統合 SDK。

使用例（アプリの main.py）:
    from agentflow.security.auth_client import AuthClient, require_auth

    auth = AuthClient(
        base_url=settings.AUTH_SERVICE_URL,
        jwt_secret=settings.AUTH_SERVICE_JWT_SECRET,
    )
    app.include_router(auth.router)  # /auth/* プロキシを mount

使用例（ルーター内）:
    from agentflow.security.auth_client import require_auth, get_current_user

    @router.get("/protected")
    async def protected(user=Depends(require_auth)):
        return {"user": user.username}
"""

from agentflow.security.auth_client.client import AuthClient, AuthServiceError
from agentflow.security.auth_client.config import AuthClientConfig
from agentflow.security.auth_client.dependencies import get_current_user, require_auth, require_role


__all__ = [
    "AuthClient",
    "AuthClientConfig",
    "AuthServiceError",
    "get_current_user",
    "require_auth",
    "require_role",
]
