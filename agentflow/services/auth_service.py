"""Auth Service - フレームワーク級認証サービス.

認証・認可の統一サービス。
Studio/CLI/SDK/API 全てで同一インターフェース。

機能:
- JWT 認証
- API Key 認証
- RBAC（ロールベースアクセス制御）
- レート制限
- セッション管理

使用例:
    >>> from agentflow.services import AuthService
    >>>
    >>> service = AuthService()
    >>>
    >>> # ユーザー認証
    >>> token = await service.execute(
    ...     action="login",
    ...     email="user@example.com",
    ...     password="***",
    ... )
    >>>
    >>> # トークン検証
    >>> user = await service.execute(
    ...     action="verify",
    ...     token="xxx",
    ... )
"""

from __future__ import annotations

import logging
import os
import secrets
import time
from dataclasses import dataclass, field
from datetime import UTC, datetime, timedelta
from typing import TYPE_CHECKING, Any

from agentflow.services.base import (
    ServiceBase,
    ServiceEvent,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


@dataclass
class AuthConfig:
    """認証設定."""

    jwt_secret: str = ""
    jwt_algorithm: str = "HS256"
    jwt_expire_minutes: int = 60
    refresh_expire_days: int = 7
    api_key_prefix: str = "af_"
    api_key_length: int = 32
    rate_limit_requests: int = 100
    rate_limit_window_seconds: int = 60

    def __post_init__(self) -> None:
        if not self.jwt_secret:
            self.jwt_secret = os.getenv("JWT_SECRET_KEY", secrets.token_hex(32))

    @classmethod
    def get_config_fields(cls) -> list[dict[str, Any]]:
        """Studio 設定フィールド定義."""
        return [
            {
                "name": "jwt_expire_minutes",
                "type": "number",
                "label": "トークン有効期限（分）",
                "default": 60,
                "min": 5,
                "max": 1440,
            },
            {
                "name": "rate_limit_requests",
                "type": "number",
                "label": "レート制限（リクエスト数）",
                "default": 100,
                "min": 10,
                "max": 1000,
            },
        ]


@dataclass
class AuthUser:
    """認証ユーザー."""
    id: str
    email: str
    roles: list[str] = field(default_factory=list)
    permissions: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class AuthToken:
    """認証トークン."""
    access_token: str
    refresh_token: str | None = None
    token_type: str = "Bearer"
    expires_in: int = 3600
    expires_at: datetime | None = None


# =============================================================================
# Auth Service 実装
# =============================================================================


class AuthService(ServiceBase):
    """Auth Service - フレームワーク級サービス.

    Actions:
    - login: ユーザー認証
    - verify: トークン検証
    - refresh: トークン更新
    - create_api_key: API Key 生成
    - validate_api_key: API Key 検証
    - check_permission: 権限チェック
    """

    def __init__(self, config: AuthConfig | None = None) -> None:
        """初期化."""
        super().__init__()
        self._config = config or AuthConfig()
        self._users: dict[str, dict[str, Any]] = {}  # 簡易ユーザーストア
        self._api_keys: dict[str, dict[str, Any]] = {}
        self._rate_limits: dict[str, list[float]] = {}

    async def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """内部実行ロジック."""
        action = kwargs.get("action", "verify")

        if action == "login":
            async for event in self._do_login(execution_id, **kwargs):
                yield event
        elif action == "verify":
            async for event in self._do_verify(execution_id, **kwargs):
                yield event
        elif action == "refresh":
            async for event in self._do_refresh(execution_id, **kwargs):
                yield event
        elif action == "create_api_key":
            async for event in self._do_create_api_key(execution_id, **kwargs):
                yield event
        elif action == "validate_api_key":
            async for event in self._do_validate_api_key(execution_id, **kwargs):
                yield event
        elif action == "check_permission":
            async for event in self._do_check_permission(execution_id, **kwargs):
                yield event
        elif action == "check_rate_limit":
            async for event in self._do_check_rate_limit(execution_id, **kwargs):
                yield event
        else:
            yield self._emit_error(execution_id, "invalid_action", f"不明なアクション: {action}")

    async def _do_login(
        self,
        execution_id: str,
        email: str = "",
        password: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """ユーザー認証."""
        start_time = time.time()

        if not email or not password:
            yield self._emit_error(execution_id, "invalid_credentials", "メールアドレスとパスワードが必要です")
            return

        # 簡易認証（実際はDBと連携）
        user = self._users.get(email)
        if not user or user.get("password") != password:
            yield self._emit_error(execution_id, "invalid_credentials", "認証に失敗しました")
            return

        # トークン生成
        token = self._create_token(user)

        yield self._emit_result(execution_id, {
            "access_token": token.access_token,
            "refresh_token": token.refresh_token,
            "token_type": token.token_type,
            "expires_in": token.expires_in,
            "user": {
                "id": user["id"],
                "email": user["email"],
                "roles": user.get("roles", []),
            },
        }, (time.time() - start_time) * 1000)

    async def _do_verify(
        self,
        execution_id: str,
        token: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """トークン検証."""
        start_time = time.time()

        if not token:
            yield self._emit_error(execution_id, "missing_token", "トークンが必要です")
            return

        # Bearer プレフィックスを除去
        if token.startswith("Bearer "):
            token = token[7:]

        try:
            import jwt

            payload = jwt.decode(
                token,
                self._config.jwt_secret,
                algorithms=[self._config.jwt_algorithm],
            )

            user = AuthUser(
                id=payload.get("sub", ""),
                email=payload.get("email", ""),
                roles=payload.get("roles", []),
                permissions=payload.get("permissions", []),
            )

            yield self._emit_result(execution_id, {
                "valid": True,
                "user": {
                    "id": user.id,
                    "email": user.email,
                    "roles": user.roles,
                    "permissions": user.permissions,
                },
            }, (time.time() - start_time) * 1000)

        except jwt.ExpiredSignatureError:
            yield self._emit_error(execution_id, "token_expired", "トークンの有効期限が切れています")
        except jwt.InvalidTokenError as e:
            yield self._emit_error(execution_id, "invalid_token", f"無効なトークン: {e}")

    async def _do_refresh(
        self,
        execution_id: str,
        refresh_token: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """トークン更新."""
        start_time = time.time()

        if not refresh_token:
            yield self._emit_error(execution_id, "missing_token", "リフレッシュトークンが必要です")
            return

        try:
            import jwt

            payload = jwt.decode(
                refresh_token,
                self._config.jwt_secret,
                algorithms=[self._config.jwt_algorithm],
            )

            if payload.get("type") != "refresh":
                yield self._emit_error(execution_id, "invalid_token", "リフレッシュトークンではありません")
                return

            # 新しいトークンを生成
            user_data = {
                "id": payload.get("sub"),
                "email": payload.get("email"),
                "roles": payload.get("roles", []),
            }
            token = self._create_token(user_data)

            yield self._emit_result(execution_id, {
                "access_token": token.access_token,
                "refresh_token": token.refresh_token,
                "token_type": token.token_type,
                "expires_in": token.expires_in,
            }, (time.time() - start_time) * 1000)

        except jwt.InvalidTokenError as e:
            yield self._emit_error(execution_id, "invalid_token", f"無効なトークン: {e}")

    async def _do_create_api_key(
        self,
        execution_id: str,
        user_id: str = "",
        name: str = "",
        permissions: list[str] | None = None,
        expires_days: int = 365,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """API Key 生成."""
        start_time = time.time()

        if not user_id:
            yield self._emit_error(execution_id, "missing_user_id", "ユーザーIDが必要です")
            return

        # API Key 生成
        key = f"{self._config.api_key_prefix}{secrets.token_hex(self._config.api_key_length)}"
        key_id = secrets.token_hex(8)
        expires_at = datetime.now(UTC) + timedelta(days=expires_days)

        self._api_keys[key_id] = {
            "key": key,
            "user_id": user_id,
            "name": name,
            "permissions": permissions or ["*"],
            "created_at": datetime.now(UTC).isoformat(),
            "expires_at": expires_at.isoformat(),
        }

        yield self._emit_result(execution_id, {
            "key_id": key_id,
            "api_key": key,  # 一度だけ表示
            "name": name,
            "expires_at": expires_at.isoformat(),
        }, (time.time() - start_time) * 1000)

    async def _do_validate_api_key(
        self,
        execution_id: str,
        api_key: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """API Key 検証."""
        start_time = time.time()

        if not api_key:
            yield self._emit_error(execution_id, "missing_api_key", "API Keyが必要です")
            return

        # 検索
        key_data = None
        for data in self._api_keys.values():
            if data["key"] == api_key:
                key_data = data
                break

        if not key_data:
            yield self._emit_error(execution_id, "invalid_api_key", "無効なAPI Key")
            return

        # 有効期限チェック
        expires_at = datetime.fromisoformat(key_data["expires_at"])
        if datetime.now(UTC) > expires_at:
            yield self._emit_error(execution_id, "expired_api_key", "API Keyの有効期限が切れています")
            return

        yield self._emit_result(execution_id, {
            "valid": True,
            "user_id": key_data["user_id"],
            "permissions": key_data["permissions"],
            "name": key_data["name"],
        }, (time.time() - start_time) * 1000)

    async def _do_check_permission(
        self,
        execution_id: str,
        user_roles: list[str] | None = None,
        user_permissions: list[str] | None = None,
        required_permission: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """権限チェック."""
        start_time = time.time()

        user_roles = user_roles or []
        user_permissions = user_permissions or []

        # 管理者ロールは全権限
        if "admin" in user_roles:
            yield self._emit_result(execution_id, {"allowed": True, "reason": "admin role"}, (time.time() - start_time) * 1000)
            return

        # ワイルドカードチェック
        if "*" in user_permissions:
            yield self._emit_result(execution_id, {"allowed": True, "reason": "wildcard permission"}, (time.time() - start_time) * 1000)
            return

        # 直接マッチ
        if required_permission in user_permissions:
            yield self._emit_result(execution_id, {"allowed": True, "reason": "direct match"}, (time.time() - start_time) * 1000)
            return

        # プレフィックスマッチ（例: "users:*" は "users:read" にマッチ）
        for perm in user_permissions:
            if perm.endswith(":*"):
                prefix = perm[:-2]
                if required_permission.startswith(prefix + ":"):
                    yield self._emit_result(execution_id, {"allowed": True, "reason": "prefix match"}, (time.time() - start_time) * 1000)
                    return

        yield self._emit_result(execution_id, {"allowed": False, "reason": "permission denied"}, (time.time() - start_time) * 1000)

    async def _do_check_rate_limit(
        self,
        execution_id: str,
        identifier: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """レート制限チェック."""
        start_time = time.time()

        if not identifier:
            yield self._emit_error(execution_id, "missing_identifier", "識別子が必要です")
            return

        now = time.time()
        window_start = now - self._config.rate_limit_window_seconds

        # 古いリクエストを削除
        if identifier in self._rate_limits:
            self._rate_limits[identifier] = [
                ts for ts in self._rate_limits[identifier]
                if ts > window_start
            ]
        else:
            self._rate_limits[identifier] = []

        current_count = len(self._rate_limits[identifier])

        if current_count >= self._config.rate_limit_requests:
            yield self._emit_result(execution_id, {
                "allowed": False,
                "current_count": current_count,
                "limit": self._config.rate_limit_requests,
                "retry_after": int(self._rate_limits[identifier][0] - window_start),
            }, (time.time() - start_time) * 1000)
            return

        # リクエストを記録
        self._rate_limits[identifier].append(now)

        yield self._emit_result(execution_id, {
            "allowed": True,
            "current_count": current_count + 1,
            "limit": self._config.rate_limit_requests,
            "remaining": self._config.rate_limit_requests - current_count - 1,
        }, (time.time() - start_time) * 1000)

    def _create_token(self, user: dict[str, Any]) -> AuthToken:
        """JWT トークン生成."""
        import jwt

        now = datetime.now(UTC)
        expires_at = now + timedelta(minutes=self._config.jwt_expire_minutes)
        refresh_expires_at = now + timedelta(days=self._config.refresh_expire_days)

        # アクセストークン
        access_payload = {
            "sub": user["id"],
            "email": user.get("email", ""),
            "roles": user.get("roles", []),
            "permissions": user.get("permissions", []),
            "type": "access",
            "iat": now,
            "exp": expires_at,
        }
        access_token = jwt.encode(access_payload, self._config.jwt_secret, algorithm=self._config.jwt_algorithm)

        # リフレッシュトークン
        refresh_payload = {
            "sub": user["id"],
            "email": user.get("email", ""),
            "roles": user.get("roles", []),
            "type": "refresh",
            "iat": now,
            "exp": refresh_expires_at,
        }
        refresh_token = jwt.encode(refresh_payload, self._config.jwt_secret, algorithm=self._config.jwt_algorithm)

        return AuthToken(
            access_token=access_token,
            refresh_token=refresh_token,
            expires_in=self._config.jwt_expire_minutes * 60,
            expires_at=expires_at,
        )

    # =========================================================================
    # ユーザー管理（簡易実装）
    # =========================================================================

    def register_user(
        self,
        id: str,
        email: str,
        password: str,
        roles: list[str] | None = None,
    ) -> None:
        """ユーザー登録（簡易実装）."""
        self._users[email] = {
            "id": id,
            "email": email,
            "password": password,
            "roles": roles or ["user"],
        }

    # =========================================================================
    # Studio 統合用メソッド
    # =========================================================================

    @classmethod
    def get_node_definition(cls) -> dict[str, Any]:
        """Studio ノード定義."""
        return {
            "type": "auth",
            "label": "認証",
            "category": "security",
            "icon": "lock",
            "description": "認証・認可を実行",
            "inputs": [
                {"name": "action", "type": "string", "label": "アクション", "required": True},
                {"name": "token", "type": "string", "label": "トークン", "required": False},
            ],
            "outputs": [
                {"name": "valid", "type": "boolean", "label": "有効"},
                {"name": "user", "type": "object", "label": "ユーザー"},
            ],
            "config": AuthConfig.get_config_fields(),
        }


__all__ = [
    "AuthConfig",
    "AuthService",
    "AuthToken",
    "AuthUser",
]
