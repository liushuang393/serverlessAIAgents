"""infrastructure.security.auth_middleware — JWT 認証ミドルウェア.

AuthMiddleware / AuthUser / JWTConfig を提供する。
JWT トークンの生成・検証と API Key 認証をサポート。

注意:
    - secret_key が未設定の場合、JWT 検証はスキップされる
    - api_key_manager が未設定の場合、API Key 認証はスキップされる
    - external_authenticator が指定された場合、その結果を優先する
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime, timedelta
from typing import TYPE_CHECKING, Any, Callable, Coroutine

if TYPE_CHECKING:
    from infrastructure.security.api_key import APIKeyManager

logger = logging.getLogger(__name__)


@dataclass
class JWTConfig:
    """JWT 設定."""

    secret_key: str = ""
    algorithm: str = "HS256"
    expire_minutes: int = 60
    issuer: str = "agentflow"
    audience: str = "agentflow"


@dataclass
class AuthUser:
    """認証済みユーザー情報."""

    id: str = ""
    email: str = ""
    roles: list[str] = field(default_factory=list)
    permissions: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def has_role(self, role: str) -> bool:
        """指定ロールを持つか確認する."""
        return role in self.roles

    def has_permission(self, permission: str) -> bool:
        """指定パーミッションを持つか確認する（ワイルドカード対応）."""
        for p in self.permissions:
            if p == "*":
                return True
            if p.endswith(":*"):
                prefix = p[:-1]  # "users:" のように末尾の * を除く
                if permission.startswith(prefix):
                    return True
            if p == permission:
                return True
        return False


class AuthMiddleware:
    """JWT / API Key / 外部認証を統合した認証ミドルウェア."""

    def __init__(
        self,
        jwt_config: JWTConfig | None = None,
        api_key_manager: "APIKeyManager | None" = None,
        external_authenticator: Callable[..., Coroutine[Any, Any, AuthUser | None]] | None = None,
    ) -> None:
        """初期化.

        Args:
            jwt_config: JWT 設定。未指定時はデフォルト値。
            api_key_manager: API キー管理。未指定時は API Key 認証をスキップ。
            external_authenticator: 外部認証コルーチン。未指定時はスキップ。
        """
        self._jwt_config = jwt_config or JWTConfig()
        self._api_key_manager = api_key_manager
        self._external_authenticator = external_authenticator

    def create_jwt_token(
        self,
        user_id: str,
        email: str = "",
        roles: list[str] | None = None,
        permissions: list[str] | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> str:
        """JWT アクセストークンを生成する.

        Args:
            user_id: ユーザー ID。
            email: メールアドレス。
            roles: ロール一覧。
            permissions: パーミッション一覧。
            metadata: 追加メタデータ（username, display_name 等）。

        Returns:
            署名済み JWT 文字列。
        """
        try:
            import jwt as pyjwt
        except ImportError:
            logger.error("PyJWT がインストールされていません: pip install PyJWT")
            return ""

        now = datetime.now(tz=UTC)
        payload: dict[str, Any] = {
            "sub": user_id,
            "email": email,
            "roles": roles or [],
            "permissions": permissions or [],
            "iat": now,
            "exp": now + timedelta(minutes=self._jwt_config.expire_minutes),
            "iss": self._jwt_config.issuer,
            "aud": self._jwt_config.audience,
        }
        if metadata:
            payload["metadata"] = metadata

        return pyjwt.encode(payload, self._jwt_config.secret_key, algorithm=self._jwt_config.algorithm)

    async def authenticate(
        self,
        authorization: str | None = None,
        api_key: str | None = None,
    ) -> AuthUser | None:
        """リクエストを認証してユーザー情報を返す.

        認証優先順: external_authenticator → API Key → JWT。

        Returns:
            認証済み AuthUser。認証失敗時は None。
        """
        # 1. 外部認証ハンドラ（優先）
        if self._external_authenticator is not None:
            try:
                user = await self._external_authenticator(authorization, api_key)
                if user is not None:
                    return user
            except Exception:
                logger.exception("外部認証ハンドラでエラー発生")

        # 2. API Key 認証
        if api_key and self._api_key_manager is not None:
            try:
                key_info = await self._api_key_manager.verify_key(api_key)
                if key_info is not None:
                    return AuthUser(id=key_info.get("name", api_key[:8]), permissions=key_info.get("scopes", []))
            except Exception:
                logger.exception("API Key 認証でエラー発生")

        # 3. JWT 認証
        if authorization and authorization.startswith("Bearer ") and self._jwt_config.secret_key:
            token = authorization[7:]
            try:
                import jwt as pyjwt

                payload = pyjwt.decode(
                    token,
                    self._jwt_config.secret_key,
                    algorithms=[self._jwt_config.algorithm],
                    audience=self._jwt_config.audience,
                    issuer=self._jwt_config.issuer,
                )
                return AuthUser(
                    id=payload.get("sub", ""),
                    email=payload.get("email", ""),
                    roles=payload.get("roles", []),
                    permissions=payload.get("permissions", []),
                    metadata=payload.get("metadata", {}),
                )
            except Exception:
                logger.debug("JWT 検証失敗", exc_info=True)

        return None


def create_auth_middleware(
    jwt_config: JWTConfig | None = None,
    api_key_manager: "APIKeyManager | None" = None,
    external_authenticator: Callable[..., Coroutine[Any, Any, AuthUser | None]] | None = None,
) -> AuthMiddleware:
    """AuthMiddleware のファクトリ関数.

    Returns:
        設定済み AuthMiddleware インスタンス。
    """
    return AuthMiddleware(
        jwt_config=jwt_config,
        api_key_manager=api_key_manager,
        external_authenticator=external_authenticator,
    )


__all__ = ["AuthMiddleware", "AuthUser", "JWTConfig", "create_auth_middleware"]

