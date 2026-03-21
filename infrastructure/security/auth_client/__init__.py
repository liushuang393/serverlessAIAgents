"""infrastructure.security.auth_client — Auth Service クライアントパッケージ.

AuthClient: JWT ローカル検証 + /auth/me リモートフォールバック。
RemoteUser: auth_service から解決したユーザー情報。

使用例:
    client = AuthClient(
        base_url="http://localhost:8010",
        jwt_secret="your-jwt-secret",
    )
    remote_user = await client.verify_token(token)
"""

from __future__ import annotations

from infrastructure.security.auth_client.client import AuthClient, RemoteUser

__all__ = ["AuthClient", "RemoteUser"]

