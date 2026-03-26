"""infrastructure.security.auth_middleware の回帰テスト."""

from __future__ import annotations

from typing import Any

import pytest

from infrastructure.security.auth_middleware import AuthMiddleware, AuthUser, JWTConfig, create_auth_middleware


class StubAPIKeyManager:
    """API Key 認証テスト用の最小スタブ."""

    def __init__(self, result: dict[str, Any] | None) -> None:
        self._result = result
        self.calls: list[str] = []

    async def verify_key(self, api_key: str) -> dict[str, Any] | None:
        """渡された API Key を記録して固定結果を返す."""
        self.calls.append(api_key)
        return self._result


def test_jwt_config_defaults() -> None:
    """JWTConfig のデフォルト値を維持する."""
    config = JWTConfig()

    assert config.algorithm == "HS256"
    assert config.expire_minutes == 60
    assert config.issuer == "agentflow"
    assert config.audience == "agentflow"


def test_auth_user_has_role_and_exact_permission() -> None:
    """AuthUser がロールと厳密一致権限を判定できる."""
    user = AuthUser(id="user-1", roles=["admin"], permissions=["reports:read"])

    assert user.has_role("admin") is True
    assert user.has_role("guest") is False
    assert user.has_permission("reports:read") is True
    assert user.has_permission("reports:write") is False


def test_auth_user_supports_wildcard_permissions() -> None:
    """AuthUser が全権限と prefix wildcard を判定できる."""
    wildcard_user = AuthUser(id="user-1", permissions=["*"])
    prefix_user = AuthUser(id="user-2", permissions=["users:*"])

    assert wildcard_user.has_permission("anything") is True
    assert prefix_user.has_permission("users:read") is True
    assert prefix_user.has_permission("users:write") is True
    assert prefix_user.has_permission("reports:read") is False


@pytest.mark.asyncio
async def test_auth_middleware_prioritizes_external_authenticator() -> None:
    """外部認証ハンドラが最優先で使われる."""

    async def external_auth(
        authorization: str | None,
        _api_key: str | None,
    ) -> AuthUser | None:
        if authorization == "SSO valid-token":
            return AuthUser(id="external-user", roles=["employee"])
        return None

    api_key_manager = StubAPIKeyManager(result={"name": "api-user", "scopes": ["read"]})
    middleware = AuthMiddleware(
        jwt_config=JWTConfig(secret_key="jwt-secret"),
        api_key_manager=api_key_manager,
        external_authenticator=external_auth,
    )

    user = await middleware.authenticate(
        authorization="SSO valid-token",
        api_key="should-not-be-used",
    )

    assert user is not None
    assert user.id == "external-user"
    assert api_key_manager.calls == []


@pytest.mark.asyncio
async def test_auth_middleware_authenticates_api_key_with_stub_manager() -> None:
    """API Key 認証は verify_key の戻り値から AuthUser を構築する."""
    api_key_manager = StubAPIKeyManager(result={"name": "service-user", "scopes": ["read", "write"]})
    middleware = AuthMiddleware(api_key_manager=api_key_manager)

    user = await middleware.authenticate(api_key="sk-test")

    assert user is not None
    assert user.id == "service-user"
    assert user.permissions == ["read", "write"]
    assert api_key_manager.calls == ["sk-test"]


@pytest.mark.asyncio
async def test_auth_middleware_returns_none_for_unknown_api_key() -> None:
    """API Key が無効な場合は None を返す."""
    middleware = AuthMiddleware(api_key_manager=StubAPIKeyManager(result=None))

    user = await middleware.authenticate(api_key="sk-missing")

    assert user is None


@pytest.mark.asyncio
async def test_auth_middleware_jwt_round_trip() -> None:
    """create_jwt_token と authenticate(Bearer) が整合する."""
    config = JWTConfig(
        secret_key="jwt-secret",
        expire_minutes=5,
        issuer="agentflow",
        audience="agentflow",
    )
    middleware = AuthMiddleware(jwt_config=config)

    token = middleware.create_jwt_token(
        user_id="user-123",
        email="user@example.com",
        roles=["admin"],
        permissions=["reports:read"],
        metadata={"team": "platform"},
    )
    user = await middleware.authenticate(authorization=f"Bearer {token}")

    assert user is not None
    assert user.id == "user-123"
    assert user.email == "user@example.com"
    assert user.roles == ["admin"]
    assert user.permissions == ["reports:read"]
    assert user.metadata == {"team": "platform"}


@pytest.mark.asyncio
async def test_auth_middleware_rejects_invalid_jwt() -> None:
    """無効な JWT は認証失敗になる."""
    middleware = AuthMiddleware(jwt_config=JWTConfig(secret_key="jwt-secret"))

    user = await middleware.authenticate(authorization="Bearer invalid-token")

    assert user is None


@pytest.mark.asyncio
async def test_create_auth_middleware_preserves_external_authenticator() -> None:
    """ファクトリ関数から作成した middleware でも外部認証が動作する."""

    async def external_auth(
        authorization: str | None,
        _api_key: str | None,
    ) -> AuthUser | None:
        if authorization:
            return AuthUser(id="factory-user")
        return None

    middleware = create_auth_middleware(external_authenticator=external_auth)
    user = await middleware.authenticate(authorization="External token")

    assert user is not None
    assert user.id == "factory-user"
