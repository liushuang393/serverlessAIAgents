# -*- coding: utf-8 -*-
"""Security 模块的完整测试.

覆盖 api_key.py, rate_limiter.py, auth_middleware.py, rbac.py
"""

import asyncio
import time
import unittest
from datetime import datetime, timedelta, timezone
from unittest.mock import MagicMock


class TestGenerateAPIKey(unittest.TestCase):
    """generate_api_key 函数的测试."""

    def test_default_format(self):
        """默认格式测试."""
        from agentflow.security.api_key import generate_api_key

        key = generate_api_key()
        # 默认前缀是 "sk"，格式是 "{prefix}-{token}"
        self.assertTrue(key.startswith("sk-"))

    def test_custom_prefix(self):
        """自定义前缀测试."""
        from agentflow.security.api_key import generate_api_key

        key = generate_api_key(prefix="custom")
        self.assertTrue(key.startswith("custom-"))

    def test_custom_length(self):
        """自定义长度测试."""
        from agentflow.security.api_key import generate_api_key

        key = generate_api_key(length=16)
        # 长度会因为 url_safe base64 编码而变化
        self.assertIsInstance(key, str)


class TestHashAPIKey(unittest.TestCase):
    """hash_api_key 函数的测试."""

    def test_hashing(self):
        """哈希测试."""
        from agentflow.security.api_key import hash_api_key

        key = "sk-test123"
        hashed = hash_api_key(key)

        self.assertNotEqual(hashed, key)
        self.assertEqual(len(hashed), 64)  # SHA256 hex

    def test_consistent_hashing(self):
        """一致性哈希测试."""
        from agentflow.security.api_key import hash_api_key

        key = "sk-test123"
        hash1 = hash_api_key(key)
        hash2 = hash_api_key(key)

        self.assertEqual(hash1, hash2)

    def test_hashing_with_salt(self):
        """带盐哈希测试."""
        from agentflow.security.api_key import hash_api_key

        key = "sk-test123"
        hash_no_salt = hash_api_key(key)
        hash_with_salt = hash_api_key(key, salt="mysalt")

        self.assertNotEqual(hash_no_salt, hash_with_salt)


class TestAPIKey(unittest.TestCase):
    """APIKey 类的测试."""

    def test_creation_with_defaults(self):
        """默认值创建测试."""
        from agentflow.security.api_key import APIKey

        key = APIKey(
            id="key-123",
            name="test-key",
            key_hash="abc123",
        )

        self.assertEqual(key.id, "key-123")
        self.assertEqual(key.name, "test-key")
        self.assertEqual(key.scopes, [])
        self.assertIsNotNone(key.created_at)
        self.assertIsNone(key.expires_at)
        self.assertTrue(key.enabled)

    def test_creation_with_all_params(self):
        """全参数创建测试."""
        from agentflow.security.api_key import APIKey

        now = datetime.now(timezone.utc)
        key = APIKey(
            id="key-123",
            name="full-key",
            key_hash="abc123",
            scopes=["read", "write"],
            created_at=now,
            expires_at=now + timedelta(days=30),
            enabled=True,
            metadata={"env": "prod"},
        )

        self.assertEqual(key.scopes, ["read", "write"])
        self.assertEqual(key.metadata["env"], "prod")

    def test_is_valid_enabled(self):
        """有效性检查 - 启用状态."""
        from agentflow.security.api_key import APIKey

        key = APIKey(id="1", name="test", key_hash="h")
        self.assertTrue(key.is_valid())

    def test_is_valid_disabled(self):
        """有效性检查 - 禁用状态."""
        from agentflow.security.api_key import APIKey

        key = APIKey(id="1", name="test", key_hash="h", enabled=False)
        self.assertFalse(key.is_valid())

    def test_is_valid_expired(self):
        """有效性检查 - 过期."""
        from agentflow.security.api_key import APIKey

        past = datetime.now(timezone.utc) - timedelta(days=1)
        key = APIKey(id="1", name="test", key_hash="h", expires_at=past)
        self.assertFalse(key.is_valid())

    def test_has_scope(self):
        """作用域检查."""
        from agentflow.security.api_key import APIKey

        key = APIKey(id="1", name="test", key_hash="h", scopes=["read", "write"])
        self.assertTrue(key.has_scope("read"))
        self.assertFalse(key.has_scope("admin"))

    def test_has_scope_wildcard(self):
        """通配符作用域检查."""
        from agentflow.security.api_key import APIKey

        key = APIKey(id="1", name="test", key_hash="h", scopes=["*"])
        self.assertTrue(key.has_scope("anything"))

    def test_to_dict(self):
        """转字典测试."""
        from agentflow.security.api_key import APIKey

        key = APIKey(id="1", name="test", key_hash="h")
        data = key.to_dict()

        self.assertEqual(data["id"], "1")
        self.assertEqual(data["name"], "test")
        self.assertNotIn("key_hash", data)


class TestAPIKeyConfig(unittest.TestCase):
    """APIKeyConfig 的测试."""

    def test_defaults(self):
        """默认值测试."""
        from agentflow.security.api_key import APIKeyConfig

        config = APIKeyConfig()
        self.assertEqual(config.key_prefix, "sk")
        self.assertEqual(config.key_length, 32)
        self.assertEqual(config.default_scopes, ["read"])


class TestAPIKeyManager(unittest.TestCase):
    """APIKeyManager 的完整测试."""

    def test_create_key(self):
        """创建密钥测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        raw_key, api_key = manager.create_key(name="test-key")

        self.assertTrue(raw_key.startswith("sk-"))
        self.assertEqual(api_key.name, "test-key")

    def test_create_key_with_scopes(self):
        """带权限范围创建测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        raw_key, api_key = manager.create_key(name="scoped-key", scopes=["read", "write"])

        self.assertEqual(api_key.scopes, ["read", "write"])

    def test_create_key_with_expiration(self):
        """带过期时间创建测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        expires = datetime.now(timezone.utc) + timedelta(hours=1)
        raw_key, api_key = manager.create_key(name="expiring-key", expires_at=expires)

        self.assertIsNotNone(api_key.expires_at)

    def test_validate(self):
        """验证测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        raw_key, _ = manager.create_key(name="test-key")

        validated = manager.validate(raw_key)
        self.assertIsNotNone(validated)
        self.assertEqual(validated.name, "test-key")

    def test_validate_invalid_key(self):
        """验证无效密钥测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        validated = manager.validate("invalid_key")
        self.assertIsNone(validated)

    def test_revoke(self):
        """撤销密钥测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        raw_key, api_key = manager.create_key(name="to-revoke")

        result = manager.revoke(api_key.id)
        self.assertTrue(result)

        validated = manager.validate(raw_key)
        self.assertIsNone(validated)

    def test_revoke_nonexistent(self):
        """撤销不存在的密钥测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        result = manager.revoke("nonexistent")
        self.assertFalse(result)

    def test_list_keys(self):
        """列出密钥测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        manager.create_key(name="key1")
        manager.create_key(name="key2")

        keys = manager.list_keys()
        self.assertEqual(len(keys), 2)

    def test_list_keys_filter_disabled(self):
        """列出启用的密钥测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        _, api_key1 = manager.create_key(name="active")
        _, api_key2 = manager.create_key(name="to-disable")
        manager.revoke(api_key2.id)

        keys = manager.list_keys()  # exclude disabled by default
        self.assertEqual(len(keys), 1)
        self.assertEqual(keys[0].name, "active")

    def test_get_key(self):
        """获取密钥信息测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        _, api_key = manager.create_key(name="info-key")

        fetched = manager.get_key(api_key.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(fetched.name, "info-key")

    def test_get_key_not_found(self):
        """获取不存在的密钥信息测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        fetched = manager.get_key("nonexistent")
        self.assertIsNone(fetched)

    def test_delete(self):
        """删除密钥测试."""
        from agentflow.security.api_key import APIKeyManager

        manager = APIKeyManager()
        _, api_key = manager.create_key(name="to-delete")

        result = manager.delete(api_key.id)
        self.assertTrue(result)

        keys = manager.list_keys(include_disabled=True)
        self.assertEqual(len(keys), 0)


class TestRateLimitConfig(unittest.TestCase):
    """RateLimitConfig 的测试."""

    def test_defaults(self):
        """默认值测试."""
        from agentflow.security.rate_limiter import RateLimitConfig

        config = RateLimitConfig()
        self.assertEqual(config.requests_per_minute, 60)
        self.assertEqual(config.requests_per_hour, 1000)
        self.assertEqual(config.requests_per_day, 10000)
        self.assertEqual(config.burst_size, 10)
        self.assertTrue(config.enable_sliding_window)

    def test_custom_values(self):
        """自定义值测试."""
        from agentflow.security.rate_limiter import RateLimitConfig

        config = RateLimitConfig(
            requests_per_minute=30,
            requests_per_hour=500,
            burst_size=5,
        )
        self.assertEqual(config.requests_per_minute, 30)


class TestRateLimitInfo(unittest.TestCase):
    """RateLimitInfo 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.security.rate_limiter import RateLimitInfo

        info = RateLimitInfo(
            remaining=50,
            limit=100,
            reset_at=1000.0,
            retry_after=0,
        )

        self.assertEqual(info.remaining, 50)
        self.assertEqual(info.limit, 100)


class TestRateLimiter(unittest.TestCase):
    """RateLimiter 的完整测试."""

    def test_allow_under_limit(self):
        """限制内允许测试."""
        from agentflow.security.rate_limiter import RateLimiter, RateLimitConfig

        config = RateLimitConfig(
            requests_per_minute=10,
            requests_per_hour=100,
            requests_per_day=1000,
        )
        limiter = RateLimiter(config)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            result = loop.run_until_complete(limiter.check("user1"))
            self.assertTrue(result)
        finally:
            loop.close()

    def test_deny_over_limit(self):
        """超限拒绝测试."""
        from agentflow.security.rate_limiter import RateLimiter, RateLimitConfig

        config = RateLimitConfig(
            requests_per_minute=2,
            requests_per_hour=100,
            requests_per_day=1000,
        )
        limiter = RateLimiter(config)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(limiter.check("user1"))
            loop.run_until_complete(limiter.check("user1"))
            result = loop.run_until_complete(limiter.check("user1"))
            self.assertFalse(result)
        finally:
            loop.close()

    def test_separate_identifiers(self):
        """独立标识符测试."""
        from agentflow.security.rate_limiter import RateLimiter, RateLimitConfig

        config = RateLimitConfig(requests_per_minute=2)
        limiter = RateLimiter(config)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(limiter.check("user1"))
            loop.run_until_complete(limiter.check("user1"))

            result = loop.run_until_complete(limiter.check("user2"))
            self.assertTrue(result)
        finally:
            loop.close()

    def test_get_info(self):
        """获取限制信息测试."""
        from agentflow.security.rate_limiter import RateLimiter

        limiter = RateLimiter(requests_per_minute=10)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(limiter.check("user1"))
            loop.run_until_complete(limiter.check("user1"))

            info = loop.run_until_complete(limiter.get_info("user1"))
            self.assertEqual(info.remaining, 8)
            self.assertEqual(info.limit, 10)
        finally:
            loop.close()

    def test_reset(self):
        """重置测试."""
        from agentflow.security.rate_limiter import RateLimiter

        limiter = RateLimiter(requests_per_minute=10)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(limiter.check("user1"))
            limiter.reset("user1")
            info = loop.run_until_complete(limiter.get_info("user1"))
            self.assertEqual(info.remaining, 10)
        finally:
            loop.close()

    def test_reset_all(self):
        """全部重置测试."""
        from agentflow.security.rate_limiter import RateLimiter

        limiter = RateLimiter(requests_per_minute=10)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(limiter.check("user1"))
            loop.run_until_complete(limiter.check("user2"))

            limiter.reset_all()

            self.assertEqual(len(limiter._records), 0)
        finally:
            loop.close()


class TestRateLimitExceeded(unittest.TestCase):
    """RateLimitExceeded 异常的测试."""

    def test_exception_creation(self):
        """异常创建测试."""
        from agentflow.security.rate_limiter import RateLimitExceeded

        exc = RateLimitExceeded(
            message="Rate limit exceeded",
            limit=100,
            reset_at=1000.0,
            retry_after=60.0,
        )

        self.assertEqual(exc.limit, 100)
        self.assertEqual(exc.retry_after, 60.0)


class TestJWTConfig(unittest.TestCase):
    """JWTConfig 的测试."""

    def test_defaults(self):
        """默认值测试."""
        from agentflow.security.auth_middleware import JWTConfig

        config = JWTConfig()
        self.assertEqual(config.algorithm, "HS256")
        self.assertEqual(config.expire_minutes, 60)
        self.assertEqual(config.issuer, "agentflow")


class TestAuthUser(unittest.TestCase):
    """AuthUser 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.security.auth_middleware import AuthUser

        user = AuthUser(
            id="user-123",
            email="test@example.com",
            roles=["admin"],
            permissions=["read", "write"],
        )

        self.assertEqual(user.id, "user-123")
        self.assertEqual(user.email, "test@example.com")

    def test_has_role(self):
        """角色检查测试."""
        from agentflow.security.auth_middleware import AuthUser

        user = AuthUser(id="1", roles=["admin", "user"])

        self.assertTrue(user.has_role("admin"))
        self.assertFalse(user.has_role("guest"))

    def test_has_permission(self):
        """权限检查测试."""
        from agentflow.security.auth_middleware import AuthUser

        user = AuthUser(id="1", permissions=["read", "write"])

        self.assertTrue(user.has_permission("read"))
        self.assertFalse(user.has_permission("delete"))

    def test_has_permission_wildcard(self):
        """通配符权限检查测试."""
        from agentflow.security.auth_middleware import AuthUser

        user = AuthUser(id="1", permissions=["*"])
        self.assertTrue(user.has_permission("anything"))

    def test_has_permission_prefix(self):
        """前缀权限检查测试."""
        from agentflow.security.auth_middleware import AuthUser

        user = AuthUser(id="1", permissions=["users:*"])
        self.assertTrue(user.has_permission("users:read"))
        self.assertTrue(user.has_permission("users:write"))


class TestAuthMiddleware(unittest.TestCase):
    """AuthMiddleware 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.security.auth_middleware import AuthMiddleware

        middleware = AuthMiddleware()
        self.assertIsNotNone(middleware)

    def test_authenticate_api_key(self):
        """API Key 认证测试."""
        from agentflow.security.api_key import APIKeyManager
        from agentflow.security.auth_middleware import AuthMiddleware

        manager = APIKeyManager()
        raw_key, _ = manager.create_key(name="test", scopes=["read"])

        middleware = AuthMiddleware(api_key_manager=manager)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            user = loop.run_until_complete(middleware.authenticate(api_key=raw_key))
            self.assertIsNotNone(user)
        finally:
            loop.close()

    def test_authenticate_external_authenticator(self):
        """外部認証ハンドラ経由で認証できる."""
        from agentflow.security.auth_middleware import AuthMiddleware, AuthUser

        async def external_auth(
            authorization: str | None,
            _api_key: str | None,
        ) -> AuthUser | None:
            if authorization == "SSO demo-token":
                return AuthUser(
                    id="sso-user",
                    email="sso@example.com",
                    roles=["employee"],
                )
            return None

        middleware = AuthMiddleware(external_authenticator=external_auth)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            user = loop.run_until_complete(
                middleware.authenticate(authorization="SSO demo-token")
            )
            self.assertIsNotNone(user)
            assert user is not None
            self.assertEqual(user.id, "sso-user")
        finally:
            loop.close()


class TestCreateAuthMiddleware(unittest.TestCase):
    """create_auth_middleware 函数的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.security.auth_middleware import create_auth_middleware

        middleware = create_auth_middleware()
        self.assertIsNotNone(middleware)

    def test_creation_with_external_authenticator(self):
        """外部認証ハンドラ付きで作成できる."""
        from agentflow.security.auth_middleware import AuthUser, create_auth_middleware

        def external_auth(
            authorization: str | None,
            _api_key: str | None,
        ) -> AuthUser | None:
            if authorization:
                return AuthUser(id="external-user")
            return None

        middleware = create_auth_middleware(external_authenticator=external_auth)

        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            user = loop.run_until_complete(
                middleware.authenticate(authorization="External token")
            )
            self.assertIsNotNone(user)
        finally:
            loop.close()


class TestPermission(unittest.TestCase):
    """Permission 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.security.rbac import Permission

        perm = Permission(
            name="users:read",
            description="Read users",
        )

        self.assertEqual(perm.name, "users:read")
        self.assertEqual(perm.description, "Read users")

    def test_matches_exact(self):
        """精确匹配测试."""
        from agentflow.security.rbac import Permission

        perm = Permission(name="users:read")
        self.assertTrue(perm.matches("users:read"))
        self.assertFalse(perm.matches("users:write"))

    def test_matches_wildcard(self):
        """通配符匹配测试."""
        from agentflow.security.rbac import Permission

        perm = Permission(name="*")
        self.assertTrue(perm.matches("anything"))

    def test_matches_prefix_wildcard(self):
        """前缀通配符匹配测试."""
        from agentflow.security.rbac import Permission

        perm = Permission(name="users:*")
        self.assertTrue(perm.matches("users:read"))
        self.assertTrue(perm.matches("users:write"))
        self.assertFalse(perm.matches("posts:read"))


class TestRole(unittest.TestCase):
    """Role 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.security.rbac import Role, Permission

        role = Role(
            name="editor",
            description="Editor role",
        )
        role.add_permission("read")
        role.add_permission("write")

        self.assertEqual(role.name, "editor")
        self.assertEqual(len(role.permissions), 2)

    def test_has_permission(self):
        """权限检查测试."""
        from agentflow.security.rbac import Role

        role = Role(name="viewer")
        role.add_permission("read")

        self.assertTrue(role.has_permission("read"))
        self.assertFalse(role.has_permission("write"))


class TestRBACManager(unittest.TestCase):
    """RBACManager 的完整测试."""

    def test_default_roles(self):
        """默认角色测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        roles = manager.list_roles()

        role_names = [r.name for r in roles]
        self.assertIn("admin", role_names)
        self.assertIn("user", role_names)
        self.assertIn("readonly", role_names)

    def test_define_role(self):
        """定义角色测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        role = manager.define_role(
            name="reporter",
            permissions=["reports:read"],
            description="Reporter role",
        )

        self.assertEqual(role.name, "reporter")
        self.assertIsNotNone(manager.get_role("reporter"))

    def test_get_role(self):
        """获取角色测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        admin = manager.get_role("admin")

        self.assertIsNotNone(admin)
        self.assertEqual(admin.name, "admin")

    def test_assign_role_to_user(self):
        """分配角色给用户测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        result = manager.assign_role("user123", "admin")

        self.assertTrue(result)
        user_roles = manager.get_user_roles("user123")
        self.assertIn("admin", user_roles)

    def test_revoke_role_from_user(self):
        """从用户撤销角色测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        manager.assign_role("user123", "admin")
        result = manager.revoke_role("user123", "admin")

        self.assertTrue(result)
        user_roles = manager.get_user_roles("user123")
        self.assertNotIn("admin", user_roles)

    def test_has_permission(self):
        """检查权限测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        manager.assign_role("user123", "admin")

        # Admin has wildcard permission
        self.assertTrue(manager.has_permission("user123", "anything"))

    def test_has_permission_denied(self):
        """权限拒绝测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        manager.assign_role("user123", "readonly")

        # readonly only has "read" permission
        self.assertTrue(manager.has_permission("user123", "read"))
        self.assertFalse(manager.has_permission("user123", "write"))

    def test_get_user_permissions(self):
        """获取用户权限测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        manager.assign_role("user123", "user")

        permissions = manager.get_user_permissions("user123")
        self.assertIn("read", permissions)
        self.assertIn("write", permissions)

    def test_has_role(self):
        """检查用户是否拥有角色."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        manager.assign_role("user123", "admin")

        self.assertTrue(manager.has_role("user123", "admin"))
        self.assertFalse(manager.has_role("user123", "guest"))

    def test_to_dict(self):
        """转字典测试."""
        from agentflow.security.rbac import RBACManager

        manager = RBACManager()
        manager.assign_role("user123", "admin")

        data = manager.to_dict()
        self.assertIn("roles", data)
        self.assertIn("user_roles", data)


if __name__ == "__main__":
    unittest.main()
