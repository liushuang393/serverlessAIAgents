"""認可 API エンドポイントテスト.

/auth/authorization/* と /auth/admin/* の全エンドポイントをテスト。
"""

from __future__ import annotations

from typing import Any

import httpx
import pytest

from tests.apps.auth_service.conftest import auth_headers


pytestmark = pytest.mark.asyncio


# ---------------------------------------------------------------------------
# ロール CRUD
# ---------------------------------------------------------------------------


class TestRolesAPI:
    """ロール API のテスト."""

    async def test_list_roles_as_admin(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """admin はロール一覧を取得できる."""
        resp = await client.get(
            "/auth/authorization/roles",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 200
        roles = resp.json()
        assert isinstance(roles, list)
        role_names = [r["name"] for r in roles]
        assert "admin" in role_names
        assert "manager" in role_names
        assert "employee" in role_names

    async def test_list_roles_forbidden_for_employee(
        self, client: httpx.AsyncClient, employee_token: str
    ) -> None:
        """employee はロール一覧を取得できない（403）."""
        resp = await client.get(
            "/auth/authorization/roles",
            headers=auth_headers(employee_token),
        )
        assert resp.status_code == 403

    async def test_get_role_detail(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ロール詳細を取得."""
        resp = await client.get(
            "/auth/authorization/roles/admin",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 200
        data = resp.json()
        assert data["name"] == "admin"
        assert data["is_system"] is True
        assert "*" in data["permissions"]

    async def test_create_and_delete_role(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ロール作成 → 削除."""
        headers = auth_headers(admin_token)

        # 作成
        resp = await client.post(
            "/auth/authorization/roles",
            headers=headers,
            json={
                "name": "test_custom_role",
                "display_name": "テストカスタムロール",
                "description": "API テスト用",
                "priority": 25,
            },
        )
        assert resp.status_code == 201
        data = resp.json()
        assert data["name"] == "test_custom_role"

        # 削除
        resp = await client.delete(
            "/auth/authorization/roles/test_custom_role",
            headers=headers,
        )
        assert resp.status_code == 200

    async def test_cannot_delete_system_role(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """システムロールは削除できない."""
        resp = await client.delete(
            "/auth/authorization/roles/admin",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 403

    async def test_update_role(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ロール更新."""
        headers = auth_headers(admin_token)

        # まず作成
        await client.post(
            "/auth/authorization/roles",
            headers=headers,
            json={"name": "update_test_role", "display_name": "更新前"},
        )

        # 更新
        resp = await client.put(
            "/auth/authorization/roles/update_test_role",
            headers=headers,
            json={"display_name": "更新後", "priority": 30},
        )
        assert resp.status_code == 200
        data = resp.json()
        assert data["display_name"] == "更新後"
        assert data["priority"] == 30

        # クリーンアップ
        await client.delete(
            "/auth/authorization/roles/update_test_role",
            headers=headers,
        )


# ---------------------------------------------------------------------------
# パーミッション一覧
# ---------------------------------------------------------------------------


class TestPermissionsAPI:
    """パーミッション API のテスト."""

    async def test_list_permissions(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """パーミッション一覧を取得."""
        resp = await client.get(
            "/auth/authorization/permissions",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 200
        perms = resp.json()
        assert isinstance(perms, list)
        perm_names = [p["name"] for p in perms]
        assert "*" in perm_names
        assert "users:read" in perm_names


# ---------------------------------------------------------------------------
# ロール-パーミッション管理
# ---------------------------------------------------------------------------


class TestRolePermissionsAPI:
    """ロール-パーミッション割り当て API のテスト."""

    async def test_assign_and_remove_permission(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """パーミッション割り当て → 解除."""
        headers = auth_headers(admin_token)

        # テストロール作成
        await client.post(
            "/auth/authorization/roles",
            headers=headers,
            json={"name": "perm_test_role", "display_name": "パーミッションテスト"},
        )

        # パーミッション割り当て
        resp = await client.post(
            "/auth/authorization/roles/perm_test_role/permissions",
            headers=headers,
            json={"permission_name": "faq:admin"},
        )
        assert resp.status_code == 201

        # ロール詳細でパーミッション確認
        resp = await client.get(
            "/auth/authorization/roles/perm_test_role",
            headers=headers,
        )
        assert resp.status_code == 200
        assert "faq:admin" in resp.json()["permissions"]

        # パーミッション解除
        resp = await client.delete(
            "/auth/authorization/roles/perm_test_role/permissions/faq:admin",
            headers=headers,
        )
        assert resp.status_code == 200

        # クリーンアップ
        await client.delete(
            "/auth/authorization/roles/perm_test_role", headers=headers
        )


# ---------------------------------------------------------------------------
# ユーザーロール管理
# ---------------------------------------------------------------------------


class TestUserRolesAPI:
    """ユーザーロール管理 API のテスト."""

    async def test_get_user_roles(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ユーザーのロール一覧を取得."""
        # admin の user_id を取得
        me_resp = await client.get(
            "/auth/me", headers=auth_headers(admin_token)
        )
        user_id = me_resp.json()["user"]["user_id"]

        resp = await client.get(
            f"/auth/authorization/users/{user_id}/roles",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 200
        data = resp.json()
        assert "admin" in data["roles"]

    async def test_get_user_permissions(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ユーザーの有効パーミッション一覧を取得."""
        me_resp = await client.get(
            "/auth/me", headers=auth_headers(admin_token)
        )
        user_id = me_resp.json()["user"]["user_id"]

        resp = await client.get(
            f"/auth/authorization/users/{user_id}/permissions",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 200
        data = resp.json()
        assert "*" in data["permissions"]


# ---------------------------------------------------------------------------
# 認可チェック
# ---------------------------------------------------------------------------


class TestAuthorizationCheck:
    """認可チェック API のテスト."""

    async def test_check_admin_permission(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """admin の認可チェック → allowed=True."""
        resp = await client.post(
            "/auth/authorization/check",
            headers=auth_headers(admin_token),
            json={"permission": "faq:write"},
        )
        assert resp.status_code == 200
        assert resp.json()["allowed"] is True

    async def test_check_employee_lacks_write(
        self, client: httpx.AsyncClient, employee_token: str
    ) -> None:
        """employee が faq:write をチェック → allowed=False."""
        resp = await client.post(
            "/auth/authorization/check",
            headers=auth_headers(employee_token),
            json={"permission": "faq:write"},
        )
        assert resp.status_code == 200
        assert resp.json()["allowed"] is False


# ---------------------------------------------------------------------------
# リソースアクセスチェック
# ---------------------------------------------------------------------------


class TestResourceAccessCheck:
    """リソースアクセスチェック API のテスト."""

    async def test_no_mapping_default_open(
        self, client: httpx.AsyncClient, employee_token: str
    ) -> None:
        """マッピング未設定 → allowed=True."""
        resp = await client.post(
            "/auth/authorization/check-resource",
            headers=auth_headers(employee_token),
            json={
                "resource_type": "vector_db",
                "resource_id": "unmapped_collection",
                "required_level": "read",
            },
        )
        assert resp.status_code == 200
        data = resp.json()
        assert data["allowed"] is True

    async def test_admin_resource_check(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """admin はリソースチェックで常に allowed."""
        resp = await client.post(
            "/auth/authorization/check-resource",
            headers=auth_headers(admin_token),
            json={
                "resource_type": "business_db",
                "resource_id": "confidential",
                "required_level": "admin",
            },
        )
        assert resp.status_code == 200
        assert resp.json()["allowed"] is True
        assert resp.json()["reason"] == "admin_wildcard"


# ---------------------------------------------------------------------------
# リソースパーミッション CRUD
# ---------------------------------------------------------------------------


class TestResourcePermissionsAPI:
    """リソースパーミッション API のテスト."""

    async def test_create_and_list_resource_permissions(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """リソースパーミッション作成 → 一覧確認."""
        headers = auth_headers(admin_token)

        # 作成
        resp = await client.post(
            "/auth/authorization/resource-permissions",
            headers=headers,
            json={
                "role_name": "employee",
                "resource_type": "vector_db",
                "resource_id": "test_restricted_collection",
                "permission_level": "read",
            },
        )
        assert resp.status_code == 201
        rp_id = resp.json()["id"]

        # 一覧確認
        resp = await client.get(
            "/auth/authorization/resource-permissions",
            headers=headers,
        )
        assert resp.status_code == 200
        rps = resp.json()
        assert any(rp["id"] == rp_id for rp in rps)

        # 削除
        resp = await client.delete(
            f"/auth/authorization/resource-permissions/{rp_id}",
            headers=headers,
        )
        assert resp.status_code == 200
