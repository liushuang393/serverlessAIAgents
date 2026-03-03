"""認可モデル CRUD テスト.

Role, Permission, RolePermission, UserRole, ResourcePermission の
基本的な CRUD 操作をテストする。
"""

from __future__ import annotations

import secrets

import pytest
from sqlalchemy import select

from apps.auth_service.db.session import ensure_database_ready, get_db_session
from apps.auth_service.models.authorization import (
    Permission,
    ResourcePermission,
    Role,
    RolePermission,
    UserRole,
)
from apps.auth_service.models.user import UserAccount


pytestmark = pytest.mark.asyncio


def _gen_id(prefix: str) -> str:
    return f"{prefix}-test-{secrets.token_hex(8)}"


class TestRoleModel:
    """Role モデルの CRUD テスト."""

    async def test_create_role(self) -> None:
        """ロールを作成できる."""
        await ensure_database_ready()
        async with get_db_session() as session:
            role = Role(
                id=_gen_id("role"),
                name=f"test_role_{secrets.token_hex(4)}",
                display_name="テストロール",
                description="テスト用ロール",
                is_system=False,
                priority=5,
            )
            session.add(role)
            await session.commit()

            # 取得確認
            fetched = await session.get(Role, role.id)
            assert fetched is not None
            assert fetched.display_name == "テストロール"
            assert fetched.priority == 5

    async def test_default_roles_seeded(self) -> None:
        """デフォルトロール（admin/manager/employee）がシード済み."""
        await ensure_database_ready()
        async with get_db_session() as session:
            for role_name in ["admin", "manager", "employee"]:
                role = await session.scalar(select(Role).where(Role.name == role_name))
                assert role is not None, f"{role_name} ロールが存在しません"

    async def test_admin_is_system_role(self) -> None:
        """admin はシステムロール."""
        await ensure_database_ready()
        async with get_db_session() as session:
            admin = await session.scalar(select(Role).where(Role.name == "admin"))
            assert admin is not None
            assert admin.is_system is True
            assert admin.priority == 100


class TestPermissionModel:
    """Permission モデルの CRUD テスト."""

    async def test_default_permissions_seeded(self) -> None:
        """デフォルトパーミッションがシード済み."""
        await ensure_database_ready()
        async with get_db_session() as session:
            expected = ["*", "users:read", "users:write", "faq:read", "faq:write"]
            for perm_name in expected:
                perm = await session.scalar(
                    select(Permission).where(Permission.name == perm_name)
                )
                assert perm is not None, f"{perm_name} パーミッションが存在しません"

    async def test_permission_has_resource_type(self) -> None:
        """パーミッションにリソース種別が設定されている."""
        await ensure_database_ready()
        async with get_db_session() as session:
            perm = await session.scalar(
                select(Permission).where(Permission.name == "users:read")
            )
            assert perm is not None
            assert perm.resource_type == "users"
            assert perm.action == "read"


class TestRolePermissionModel:
    """RolePermission モデルのテスト."""

    async def test_admin_has_wildcard_permission(self) -> None:
        """admin ロールが "*" パーミッションを持つ."""
        await ensure_database_ready()
        async with get_db_session() as session:
            admin = await session.scalar(select(Role).where(Role.name == "admin"))
            assert admin is not None
            result = await session.execute(
                select(Permission.name)
                .join(RolePermission, RolePermission.permission_id == Permission.id)
                .where(RolePermission.role_id == admin.id)
            )
            perms = [row[0] for row in result.all()]
            assert "*" in perms

    async def test_employee_permissions(self) -> None:
        """employee ロールの基本パーミッション確認."""
        await ensure_database_ready()
        async with get_db_session() as session:
            emp = await session.scalar(select(Role).where(Role.name == "employee"))
            assert emp is not None
            result = await session.execute(
                select(Permission.name)
                .join(RolePermission, RolePermission.permission_id == Permission.id)
                .where(RolePermission.role_id == emp.id)
            )
            perms = {row[0] for row in result.all()}
            assert "users:read" in perms
            assert "faq:read" in perms
            assert "analytics:read" in perms
            # employee は write 権限を持たない
            assert "faq:write" not in perms


class TestUserRoleModel:
    """UserRole モデルのテスト."""

    async def test_default_users_have_roles(self) -> None:
        """デフォルトユーザーにロールが割り当てられている."""
        await ensure_database_ready()
        async with get_db_session() as session:
            # admin ユーザー
            admin_account = await session.scalar(
                select(UserAccount).where(UserAccount.username == "admin")
            )
            assert admin_account is not None
            result = await session.execute(
                select(Role.name)
                .join(UserRole, UserRole.role_id == Role.id)
                .where(UserRole.user_id == admin_account.id)
            )
            roles = [row[0] for row in result.all()]
            assert "admin" in roles


class TestResourcePermissionModel:
    """ResourcePermission モデルのテスト."""

    async def test_create_resource_permission(self) -> None:
        """リソースパーミッションを作成できる."""
        await ensure_database_ready()
        async with get_db_session() as session:
            admin = await session.scalar(select(Role).where(Role.name == "admin"))
            assert admin is not None

            rp = ResourcePermission(
                id=_gen_id("rsp"),
                role_id=admin.id,
                resource_type="vector_db",
                resource_id=f"test_collection_{secrets.token_hex(4)}",
                permission_level="admin",
            )
            session.add(rp)
            await session.commit()

            fetched = await session.get(ResourcePermission, rp.id)
            assert fetched is not None
            assert fetched.resource_type == "vector_db"
            assert fetched.permission_level == "admin"
