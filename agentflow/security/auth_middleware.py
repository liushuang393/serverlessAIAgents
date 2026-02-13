"""認証ミドルウェアモジュール.

JWT および API Key ベースの認証を提供します。

特徴:
- JWT 認証
- API Key 認証
- 複合認証（JWT + API Key）
- デコレータサポート
"""

from __future__ import annotations

import functools
import logging
import os
from collections.abc import Awaitable, Callable
from contextvars import ContextVar
from dataclasses import dataclass, field
from datetime import UTC, datetime
from inspect import isawaitable
from typing import Any, TypeVar


logger = logging.getLogger(__name__)

F = TypeVar("F", bound=Callable[..., Any])


@dataclass
class JWTConfig:
    """JWT 設定.

    Attributes:
        secret_key: シークレットキー
        algorithm: アルゴリズム
        expire_minutes: トークン有効期限（分）
        issuer: 発行者
        audience: 対象者
    """

    secret_key: str = ""
    algorithm: str = "HS256"
    expire_minutes: int = 60
    issuer: str = "agentflow"
    audience: str = "agentflow"

    def __post_init__(self) -> None:
        """初期化後処理."""
        if not self.secret_key:
            self.secret_key = os.getenv("JWT_SECRET_KEY", "")


@dataclass
class AuthUser:
    """認証ユーザー情報.

    Attributes:
        id: ユーザー ID
        email: メールアドレス
        roles: ロール
        permissions: パーミッション
        metadata: メタデータ
    """

    id: str
    email: str | None = None
    roles: list[str] = field(default_factory=list)
    permissions: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def has_role(self, role: str) -> bool:
        """ロールを持っているか確認.

        Args:
            role: ロール名

        Returns:
            持っている場合 True
        """
        return role in self.roles

    def has_permission(self, permission: str) -> bool:
        """パーミッションを持っているか確認.

        Args:
            permission: パーミッション名

        Returns:
            持っている場合 True
        """
        # ワイルドカードチェック
        if "*" in self.permissions:
            return True

        # プレフィックスマッチ
        for perm in self.permissions:
            if perm.endswith(":*"):
                prefix = perm[:-2]
                if permission.startswith(prefix):
                    return True

        return permission in self.permissions


ExternalAuthHandler = Callable[
    [str | None, str | None],
    AuthUser | Awaitable[AuthUser | None] | None,
]


class AuthMiddleware:
    """認証ミドルウェア.

    JWT および API Key 認証を処理します。

    Example:
        >>> middleware = AuthMiddleware(jwt_config=JWTConfig())
        >>> user = await middleware.authenticate(request)
    """

    def __init__(
        self,
        jwt_config: JWTConfig | None = None,
        api_key_manager: Any = None,
        external_authenticator: ExternalAuthHandler | None = None,
    ) -> None:
        """初期化.

        Args:
            jwt_config: JWT 設定
            api_key_manager: API Key マネージャー
            external_authenticator: 外部認証ハンドラ
        """
        self._jwt_config = jwt_config
        self._api_key_manager = api_key_manager
        self._external_authenticator = external_authenticator
        self._logger = logging.getLogger(__name__)

    async def authenticate(
        self,
        authorization: str | None = None,
        api_key: str | None = None,
    ) -> AuthUser | None:
        """認証を実行.

        Args:
            authorization: Authorization ヘッダー値
            api_key: API Key

        Returns:
            認証ユーザー、または None
        """
        # API Key 認証を試行
        if api_key and self._api_key_manager:
            user = await self._authenticate_api_key(api_key)
            if user:
                return user

        # 外部認証ハンドラを試行（OIDC/SAMLゲートウェイ統合向け）
        if self._external_authenticator:
            user = await self._authenticate_external(authorization, api_key)
            if user:
                return user

        # JWT 認証を試行
        if authorization and self._jwt_config:
            user = await self._authenticate_jwt(authorization)
            if user:
                return user

        return None

    async def _authenticate_external(
        self,
        authorization: str | None,
        api_key: str | None,
    ) -> AuthUser | None:
        """外部認証ハンドラを実行."""
        if not self._external_authenticator:
            return None
        result = self._external_authenticator(authorization, api_key)
        if isawaitable(result):
            return await result
        return result

    async def _authenticate_api_key(self, api_key: str) -> AuthUser | None:
        """API Key 認証.

        Args:
            api_key: API Key

        Returns:
            認証ユーザー、または None
        """
        if not self._api_key_manager:
            return None

        validated_key = self._api_key_manager.validate(api_key)
        if not validated_key:
            return None

        return AuthUser(
            id=validated_key.id,
            permissions=validated_key.scopes,
            metadata=validated_key.metadata,
        )

    async def _authenticate_jwt(self, authorization: str) -> AuthUser | None:
        """JWT 認証.

        Args:
            authorization: Authorization ヘッダー値

        Returns:
            認証ユーザー、または None
        """
        if not self._jwt_config or not self._jwt_config.secret_key:
            return None

        # Bearer トークンを抽出
        if not authorization.startswith("Bearer "):
            return None

        token = authorization[7:]

        try:
            import jwt

            payload = jwt.decode(
                token,
                self._jwt_config.secret_key,
                algorithms=[self._jwt_config.algorithm],
                audience=self._jwt_config.audience,
                issuer=self._jwt_config.issuer,
            )

            return AuthUser(
                id=payload.get("sub", ""),
                email=payload.get("email"),
                roles=payload.get("roles", []),
                permissions=payload.get("permissions", []),
                metadata=payload.get("metadata", {}),
            )

        except ImportError:
            self._logger.warning("PyJWT not installed. Run: pip install pyjwt")
            return None
        except jwt.ExpiredSignatureError:
            self._logger.debug("JWT token expired")
            return None
        except jwt.InvalidTokenError as e:
            self._logger.debug(f"Invalid JWT token: {e}")
            return None

    def create_jwt_token(
        self,
        user_id: str,
        email: str | None = None,
        roles: list[str] | None = None,
        permissions: list[str] | None = None,
        metadata: dict[str, Any] | None = None,
        expire_minutes: int | None = None,
    ) -> str:
        """JWT トークンを作成.

        Args:
            user_id: ユーザー ID
            email: メールアドレス
            roles: ロール
            permissions: パーミッション
            metadata: メタデータ
            expire_minutes: 有効期限（分）

        Returns:
            JWT トークン

        Raises:
            ValueError: JWT 設定がない場合
        """
        if not self._jwt_config or not self._jwt_config.secret_key:
            msg = "JWT config required to create tokens"
            raise ValueError(msg)

        try:
            from datetime import timedelta

            import jwt

            now = datetime.now(UTC)
            expire = now + timedelta(
                minutes=expire_minutes or self._jwt_config.expire_minutes
            )

            payload = {
                "sub": user_id,
                "iat": now,
                "exp": expire,
                "iss": self._jwt_config.issuer,
                "aud": self._jwt_config.audience,
            }

            if email:
                payload["email"] = email
            if roles:
                payload["roles"] = roles
            if permissions:
                payload["permissions"] = permissions
            if metadata:
                payload["metadata"] = metadata

            return jwt.encode(
                payload,
                self._jwt_config.secret_key,
                algorithm=self._jwt_config.algorithm,
            )

        except ImportError:
            msg = "PyJWT not installed. Run: pip install pyjwt"
            raise ImportError(msg) from None


def create_auth_middleware(
    jwt_config: JWTConfig | None = None,
    api_key_manager: Any = None,
    external_authenticator: ExternalAuthHandler | None = None,
) -> AuthMiddleware:
    """認証ミドルウェアを作成.

    Args:
        jwt_config: JWT 設定
        api_key_manager: API Key マネージャー
        external_authenticator: 外部認証ハンドラ

    Returns:
        AuthMiddleware インスタンス
    """
    return AuthMiddleware(jwt_config, api_key_manager, external_authenticator)


# 現在のユーザーを保持（リクエストスコープ）
_current_user: ContextVar[AuthUser | None] = ContextVar("agentflow_current_user", default=None)


def set_current_user(user: AuthUser | None) -> None:
    """現在のユーザーを設定.

    Args:
        user: ユーザー
    """
    _current_user.set(user)


def get_current_user() -> AuthUser | None:
    """現在のユーザーを取得.

    Returns:
        現在のユーザー、または None
    """
    return _current_user.get()


def require_auth[F: Callable[..., Any]](func: F) -> F:
    """認証必須デコレータ.

    Example:
        >>> @require_auth
        ... async def protected_endpoint():
        ...     user = get_current_user()
        ...     return {"user_id": user.id}
    """

    @functools.wraps(func)
    async def async_wrapper(*args: Any, **kwargs: Any) -> Any:
        user = get_current_user()
        if not user:
            from agentflow.core.exceptions import AgentFlowError

            msg = "Authentication required"
            raise AgentFlowError(msg)
        return await func(*args, **kwargs)

    @functools.wraps(func)
    def sync_wrapper(*args: Any, **kwargs: Any) -> Any:
        user = get_current_user()
        if not user:
            from agentflow.core.exceptions import AgentFlowError

            msg = "Authentication required"
            raise AgentFlowError(msg)
        return func(*args, **kwargs)

    import asyncio

    if asyncio.iscoroutinefunction(func):
        return async_wrapper  # type: ignore
    return sync_wrapper  # type: ignore


def require_permission(permission: str) -> Callable[[F], F]:
    """パーミッション必須デコレータ.

    Example:
        >>> @require_permission("admin:write")
        ... async def admin_endpoint():
        ...     return {"status": "ok"}
    """

    def decorator(func: F) -> F:
        @functools.wraps(func)
        async def async_wrapper(*args: Any, **kwargs: Any) -> Any:
            user = get_current_user()
            if not user:
                from agentflow.core.exceptions import AgentFlowError

                msg = "Authentication required"
                raise AgentFlowError(msg)
            if not user.has_permission(permission):
                from agentflow.core.exceptions import AgentFlowError

                msg = f"Permission denied: {permission}"
                raise AgentFlowError(msg)
            return await func(*args, **kwargs)

        @functools.wraps(func)
        def sync_wrapper(*args: Any, **kwargs: Any) -> Any:
            user = get_current_user()
            if not user:
                from agentflow.core.exceptions import AgentFlowError

                msg = "Authentication required"
                raise AgentFlowError(msg)
            if not user.has_permission(permission):
                from agentflow.core.exceptions import AgentFlowError

                msg = f"Permission denied: {permission}"
                raise AgentFlowError(msg)
            return func(*args, **kwargs)

        import asyncio

        if asyncio.iscoroutinefunction(func):
            return async_wrapper  # type: ignore
        return sync_wrapper  # type: ignore

    return decorator
