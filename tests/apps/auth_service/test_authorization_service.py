"""AuthorizationService ロジックテスト.

パーミッション解決、ワイルドカードマッチ、リソースアクセスチェック、
キャッシュ動作をテストする。
"""

from __future__ import annotations

import pytest
from sqlalchemy import select

from apps.auth_service.core.authorization import AuthorizationService, get_authorization_service
from apps.auth_service.db.session import ensure_database_ready, get_db_session
from apps.auth_service.models.user import UserAccount


pytestmark = pytest.mark.asyncio


@pytest.fixture()
async def authz() -> AuthorizationService:
    """AuthorizationService インスタンスを提供."""
    await ensure_database_ready()
    svc = get_authorization_service()
    svc.invalidate_cache()  # テスト間でキャッシュをクリア
    return svc


async def _get_user_id(username: str) -> str:
    """ユーザー名から ID を取得."""
    async with get_db_session() as session:
        account = await session.scalar(
            select(UserAccount).where(UserAccount.username == username)
        )
        assert account is not None, f"ユーザー '{username}' が存在しません"
        return account.id


# ---------------------------------------------------------------------------
# パーミッションマッチ（静的メソッド）
# ---------------------------------------------------------------------------


class TestMatchPermission:
    """match_permission 静的メソッドのテスト."""

    def test_wildcard_matches_all(self) -> None:
        """'*' は全てにマッチ."""
        assert AuthorizationService.match_permission("*", "faq:read") is True
        assert AuthorizationService.match_permission("*", "users:admin") is True
        assert AuthorizationService.match_permission("*", "*") is True

    def test_exact_match(self) -> None:
        """完全一致でマッチ."""
        assert AuthorizationService.match_permission("faq:read", "faq:read") is True
        assert AuthorizationService.match_permission("faq:read", "faq:write") is False

    def test_prefix_match(self) -> None:
        """プレフィックスマッチ: 'faq:*' → 'faq:read' にマッチ."""
        assert AuthorizationService.match_permission("faq:*", "faq:read") is True
        assert AuthorizationService.match_permission("faq:*", "faq:write") is True
        assert AuthorizationService.match_permission("faq:*", "users:read") is False

    def test_no_match(self) -> None:
        """マッチしないケース."""
        assert AuthorizationService.match_permission("faq:read", "users:read") is False
        assert AuthorizationService.match_permission("analytics:read", "faq:read") is False


# ---------------------------------------------------------------------------
# パーミッション解決
# ---------------------------------------------------------------------------


class TestGetUserPermissions:
    """get_user_permissions のテスト."""

    async def test_admin_has_wildcard(self, authz: AuthorizationService) -> None:
        """admin は '*' パーミッションを持つ."""
        user_id = await _get_user_id("admin")
        perms = await authz.get_user_permissions(user_id)
        assert "*" in perms

    async def test_manager_permissions(self, authz: AuthorizationService) -> None:
        """manager のパーミッション確認."""
        user_id = await _get_user_id("tanaka")
        perms = await authz.get_user_permissions(user_id)
        assert "users:read" in perms
        assert "faq:read" in perms
        assert "faq:write" in perms
        assert "analytics:read" in perms
        assert "*" not in perms  # manager は wildcard を持たない

    async def test_employee_permissions(self, authz: AuthorizationService) -> None:
        """employee のパーミッション確認."""
        user_id = await _get_user_id("suzuki")
        perms = await authz.get_user_permissions(user_id)
        assert "users:read" in perms
        assert "faq:read" in perms
        assert "faq:write" not in perms  # employee は write 権限なし


class TestGetUserRoles:
    """get_user_roles のテスト."""

    async def test_admin_roles(self, authz: AuthorizationService) -> None:
        """admin ユーザーのロール確認."""
        user_id = await _get_user_id("admin")
        roles = await authz.get_user_roles(user_id)
        assert "admin" in roles

    async def test_manager_roles(self, authz: AuthorizationService) -> None:
        """manager ユーザーのロール確認."""
        user_id = await _get_user_id("tanaka")
        roles = await authz.get_user_roles(user_id)
        assert "manager" in roles


# ---------------------------------------------------------------------------
# has_permission
# ---------------------------------------------------------------------------


class TestHasPermission:
    """has_permission のテスト."""

    async def test_admin_has_any_permission(self, authz: AuthorizationService) -> None:
        """admin は任意のパーミッションを持つ（'*' マッチ）."""
        user_id = await _get_user_id("admin")
        assert await authz.has_permission(user_id, "faq:write") is True
        assert await authz.has_permission(user_id, "system:admin") is True
        assert await authz.has_permission(user_id, "nonexistent:perm") is True

    async def test_employee_lacks_write(self, authz: AuthorizationService) -> None:
        """employee は faq:write を持たない."""
        user_id = await _get_user_id("suzuki")
        assert await authz.has_permission(user_id, "faq:write") is False
        assert await authz.has_permission(user_id, "faq:read") is True

    async def test_nonexistent_user(self, authz: AuthorizationService) -> None:
        """存在しないユーザーは権限なし."""
        assert await authz.has_permission("nonexistent-user-id", "faq:read") is False


# ---------------------------------------------------------------------------
# リソースアクセスチェック
# ---------------------------------------------------------------------------


class TestCheckResourceAccess:
    """check_resource_access のテスト."""

    async def test_no_mapping_default_open(self, authz: AuthorizationService) -> None:
        """マッピング未設定 → allowed=True（Default-open）."""
        user_id = await _get_user_id("suzuki")
        result = await authz.check_resource_access(
            user_id=user_id,
            resource_type="vector_db",
            resource_id="some_collection",
            required_level="read",
        )
        assert result["allowed"] is True
        assert "default_open" in result["reason"] or "admin_wildcard" in result["reason"]

    async def test_admin_always_allowed(self, authz: AuthorizationService) -> None:
        """admin は常に allowed."""
        user_id = await _get_user_id("admin")
        result = await authz.check_resource_access(
            user_id=user_id,
            resource_type="business_db",
            resource_id="confidential_table",
            required_level="admin",
        )
        assert result["allowed"] is True
        assert result["reason"] == "admin_wildcard"


# ---------------------------------------------------------------------------
# キャッシュ
# ---------------------------------------------------------------------------


class TestCache:
    """キャッシュ動作のテスト."""

    async def test_cache_invalidation(self, authz: AuthorizationService) -> None:
        """キャッシュ無効化が動作する."""
        user_id = await _get_user_id("admin")

        # 1回目: DB から解決
        perms1 = await authz.get_user_permissions(user_id)
        assert "*" in perms1

        # キャッシュ無効化
        authz.invalidate_cache(user_id)

        # 2回目: 再度 DB から解決
        perms2 = await authz.get_user_permissions(user_id)
        assert "*" in perms2

    async def test_global_cache_invalidation(self, authz: AuthorizationService) -> None:
        """全キャッシュ無効化."""
        user_id = await _get_user_id("admin")
        await authz.get_user_permissions(user_id)

        authz.invalidate_cache()  # 全削除
        assert len(authz._cache) == 0
