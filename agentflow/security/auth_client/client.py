"""auth_service HTTP クライアント.

JWT のローカル検証（オフライン）と auth_service への HTTP 呼び出しの両方をサポートする。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

from fastapi import APIRouter

from agentflow.security.auth_client.config import AuthClientConfig


logger = logging.getLogger(__name__)


class AuthServiceError(Exception):
    """auth_service との通信または認証に失敗した際の例外."""


@dataclass
class RemoteUser:
    """auth_service から返されたユーザー情報."""

    user_id: str
    username: str
    display_name: str = ""
    department: str = ""
    position: str = ""
    role: str = "employee"
    email: str | None = None
    mfa_enabled: bool = False
    extra: dict[str, Any] = field(default_factory=dict)


class AuthClient:
    """auth_service クライアント.

    他の FastAPI アプリから auth_service を利用するためのクライアント。

    使用例:
        auth = AuthClient(base_url="http://localhost:8010", jwt_secret="...")
        app.include_router(auth.router)  # /auth/* プロキシをマウント
    """

    def __init__(
        self,
        base_url: str | None = None,
        jwt_secret: str | None = None,
        jwt_algorithm: str = "HS256",
        jwt_issuer: str = "auth-service",
        jwt_audience: str = "auth-service",
        timeout: float = 10.0,
    ) -> None:
        """初期化.

        Args:
            base_url: auth_service のベース URL
            jwt_secret: JWT 検証用共有シークレット
            jwt_algorithm: JWT アルゴリズム
            jwt_issuer: JWT 発行者
            jwt_audience: JWT 対象者
            timeout: HTTP タイムアウト（秒）
        """
        self._config = AuthClientConfig(
            base_url=base_url,
            jwt_secret=jwt_secret,
            jwt_algorithm=jwt_algorithm,
            jwt_issuer=jwt_issuer,
            jwt_audience=jwt_audience,
            timeout=timeout,
        )
        self._router: APIRouter | None = None

    @property
    def config(self) -> AuthClientConfig:
        """設定を返す."""
        return self._config

    @property
    def router(self) -> APIRouter:
        """auth_service へのプロキシルーターを返す.

        アプリの main.py で app.include_router(auth.router) として使用する。
        /auth/* への全リクエストを auth_service へプロキシする。
        """
        if self._router is None:
            self._router = self._build_proxy_router()
        return self._router

    def verify_token_locally(self, token: str) -> RemoteUser | None:
        """JWT トークンをローカルで検証（auth_service への HTTP 呼び出しなし）.

        共有シークレットを使用して署名を検証する。
        これにより、リクエストごとの HTTP ラウンドトリップが不要になる。

        Args:
            token: JWT アクセストークン文字列

        Returns:
            RemoteUser または None（無効/期限切れの場合）
        """
        if not self._config.jwt_secret:
            logger.warning("AUTH_SERVICE_JWT_SECRET が未設定です。ローカル検証できません。")
            return None

        try:
            import jwt

            algorithm = self._config.jwt_algorithm or "HS256"
            payload = jwt.decode(
                token,
                self._config.jwt_secret,
                algorithms=[algorithm],
                issuer=self._config.jwt_issuer,
                audience=self._config.jwt_audience,
            )
            return RemoteUser(
                user_id=str(payload.get("sub", "")),
                username=str(payload.get("username", payload.get("sub", ""))),
                display_name=str(payload.get("display_name", "")),
                department=str(payload.get("department", "")),
                position=str(payload.get("position", "")),
                role=str(payload.get("role", "employee")),
                email=payload.get("email"),
            )
        except ImportError:
            logger.exception("PyJWT がインストールされていません: pip install pyjwt")
            return None
        except Exception as e:
            logger.debug("JWT ローカル検証失敗: %s", e)
            return None

    async def verify_token_remote(self, token: str) -> RemoteUser | None:
        """auth_service の /auth/me を呼び出してトークンを検証.

        ローカル検証が使えない場合のフォールバック。

        Args:
            token: JWT アクセストークン文字列

        Returns:
            RemoteUser または None
        """
        try:
            import httpx
        except ImportError:
            logger.exception("httpx がインストールされていません: pip install httpx")
            return None

        url = f"{self._config.base_url}/auth/me"
        try:
            async with httpx.AsyncClient(timeout=self._config.timeout) as client:
                resp = await client.get(url, headers={"Authorization": f"Bearer {token}"})
                if resp.status_code != 200:
                    return None
                data = resp.json()
                if not data.get("success") or not data.get("user"):
                    return None
                user_data: dict[str, Any] = data["user"]
                return RemoteUser(
                    user_id=str(user_data.get("user_id", "")),
                    username=str(user_data.get("username", "")),
                    display_name=str(user_data.get("display_name", "")),
                    department=str(user_data.get("department", "")),
                    position=str(user_data.get("position", "")),
                    role=str(user_data.get("role", "employee")),
                    email=user_data.get("email"),
                    mfa_enabled=bool(user_data.get("mfa_enabled", False)),
                )
        except Exception as e:
            logger.warning("auth_service リモート検証失敗: %s", e)
            return None

    async def verify_token(self, token: str) -> RemoteUser | None:
        """トークンを検証（ローカル優先、失敗時はリモート）.

        Args:
            token: JWT アクセストークン文字列

        Returns:
            RemoteUser または None
        """
        # まずローカルで試行（高速、HTTP 不要）
        user = self.verify_token_locally(token)
        if user is not None:
            return user

        # フォールバック: auth_service に問い合わせ
        return await self.verify_token_remote(token)

    def _build_proxy_router(self) -> APIRouter:
        """auth_service へのプロキシルーターを構築."""
        proxy_router = APIRouter(prefix="/auth", tags=["認証プロキシ"])
        base_url = self._config.base_url

        @proxy_router.api_route(
            "/{path:path}",
            methods=["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"],
        )
        async def proxy_auth(path: str) -> Any:
            """auth_service へのプロキシ."""

            msg = (
                "プロキシルーターは httpx の StreamingResponse が必要です。"
                "本番環境ではリバースプロキシ (Nginx/Traefik) を使用することを推奨します。"
                f"auth_service URL: {base_url}"
            )
            raise NotImplementedError(msg)

        return proxy_router
