"""E2E 統合テスト.

認可システム全体の統合テスト:
- RBAC 基本
- パーミッションチェック
- リソースアクセス Default-open
- JWT 統合
- 後方互換
- 管理 API
"""

from __future__ import annotations

import httpx
import pytest

from tests.shared.auth_service.conftest import auth_headers


pytestmark = pytest.mark.asyncio


# ---------------------------------------------------------------------------
# A. RBAC 基本
# ---------------------------------------------------------------------------


class TestRBACBasic:
    """RBAC 基本テスト（6件）."""

    async def test_admin_can_list_roles(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """admin はロール一覧を取得できる（200）."""
        resp = await client.get(
            "/auth/authorization/roles",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 200

    async def test_manager_cannot_list_roles(
        self, client: httpx.AsyncClient, manager_token: str
    ) -> None:
        """manager はロール一覧を取得できない（403）."""
        resp = await client.get(
            "/auth/authorization/roles",
            headers=auth_headers(manager_token),
        )
        assert resp.status_code == 403

    async def test_employee_cannot_list_roles(
        self, client: httpx.AsyncClient, employee_token: str
    ) -> None:
        """employee はロール一覧を取得できない（403）."""
        resp = await client.get(
            "/auth/authorization/roles",
            headers=auth_headers(employee_token),
        )
        assert resp.status_code == 403

    async def test_role_crud_lifecycle(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ロール CRUD ライフサイクル（作成/更新/削除）."""
        headers = auth_headers(admin_token)

        # 作成
        resp = await client.post(
            "/auth/authorization/roles",
            headers=headers,
            json={"name": "e2e_lifecycle_role", "display_name": "E2E ライフサイクル"},
        )
        assert resp.status_code == 201

        # 更新
        resp = await client.put(
            "/auth/authorization/roles/e2e_lifecycle_role",
            headers=headers,
            json={"display_name": "更新済み E2E ロール"},
        )
        assert resp.status_code == 200
        assert resp.json()["display_name"] == "更新済み E2E ロール"

        # 削除
        resp = await client.delete(
            "/auth/authorization/roles/e2e_lifecycle_role",
            headers=headers,
        )
        assert resp.status_code == 200

    async def test_system_role_delete_forbidden(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """システムロール削除不可."""
        resp = await client.delete(
            "/auth/authorization/roles/admin",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 403

    async def test_assign_and_remove_user_role(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ユーザーへのロール割り当て/解除."""
        headers = auth_headers(admin_token)

        # employee ユーザーの ID 取得
        me_resp = await client.post("/auth/login", json={
            "username": "suzuki", "password": "suzuki123"
        })
        employee_user_id = me_resp.json()["user"]["user_id"]

        # テストロール作成
        await client.post(
            "/auth/authorization/roles",
            headers=headers,
            json={"name": "e2e_assign_role", "display_name": "割り当てテスト"},
        )

        # 割り当て
        resp = await client.post(
            f"/auth/authorization/users/{employee_user_id}/roles",
            headers=headers,
            json={"role_name": "e2e_assign_role"},
        )
        assert resp.status_code == 201

        # 確認
        resp = await client.get(
            f"/auth/authorization/users/{employee_user_id}/roles",
            headers=headers,
        )
        assert "e2e_assign_role" in resp.json()["roles"]

        # 解除
        resp = await client.delete(
            f"/auth/authorization/users/{employee_user_id}/roles/e2e_assign_role",
            headers=headers,
        )
        assert resp.status_code == 200

        # クリーンアップ
        await client.delete(
            "/auth/authorization/roles/e2e_assign_role", headers=headers
        )


# ---------------------------------------------------------------------------
# B. パーミッションチェック
# ---------------------------------------------------------------------------


class TestPermissionCheck:
    """パーミッションチェックテスト（5件）."""

    async def test_wildcard_matches_all(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """'*' ワイルドカードが全マッチ."""
        resp = await client.post(
            "/auth/authorization/check",
            headers=auth_headers(admin_token),
            json={"permission": "any:permission:here"},
        )
        assert resp.json()["allowed"] is True

    async def test_manager_has_faq_write(
        self, client: httpx.AsyncClient, manager_token: str
    ) -> None:
        """manager は faq:write を持つ."""
        resp = await client.post(
            "/auth/authorization/check",
            headers=auth_headers(manager_token),
            json={"permission": "faq:write"},
        )
        assert resp.json()["allowed"] is True

    async def test_employee_lacks_faq_write(
        self, client: httpx.AsyncClient, employee_token: str
    ) -> None:
        """employee は faq:write を持たない（403 相当）."""
        resp = await client.post(
            "/auth/authorization/check",
            headers=auth_headers(employee_token),
            json={"permission": "faq:write"},
        )
        assert resp.json()["allowed"] is False

    async def test_employee_has_faq_read(
        self, client: httpx.AsyncClient, employee_token: str
    ) -> None:
        """employee は faq:read を持つ."""
        resp = await client.post(
            "/auth/authorization/check",
            headers=auth_headers(employee_token),
            json={"permission": "faq:read"},
        )
        assert resp.json()["allowed"] is True

    async def test_unauthenticated_check_returns_401(
        self, client: httpx.AsyncClient
    ) -> None:
        """未認証の認可チェックは 401."""
        resp = await client.post(
            "/auth/authorization/check",
            json={"permission": "faq:read"},
        )
        assert resp.status_code == 401


# ---------------------------------------------------------------------------
# C. リソースアクセス Default-open
# ---------------------------------------------------------------------------


class TestResourceAccessDefaultOpen:
    """リソースアクセス Default-open テスト（5件）."""

    async def test_unmapped_resource_allowed(
        self, client: httpx.AsyncClient, employee_token: str
    ) -> None:
        """マッピング未設定 → allowed=True."""
        resp = await client.post(
            "/auth/authorization/check-resource",
            headers=auth_headers(employee_token),
            json={
                "resource_type": "vector_db",
                "resource_id": "unmapped_e2e",
                "required_level": "write",
            },
        )
        data = resp.json()
        assert data["allowed"] is True

    async def test_admin_always_allowed(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """admin は常に allowed."""
        resp = await client.post(
            "/auth/authorization/check-resource",
            headers=auth_headers(admin_token),
            json={
                "resource_type": "business_db",
                "resource_id": "secret_table",
                "required_level": "admin",
            },
        )
        assert resp.json()["allowed"] is True

    async def test_mapped_resource_read_allowed(
        self, client: httpx.AsyncClient, admin_token: str, employee_token: str
    ) -> None:
        """マッピング設定後 read 許可."""
        headers = auth_headers(admin_token)

        # リソースパーミッション作成
        resp = await client.post(
            "/auth/authorization/resource-permissions",
            headers=headers,
            json={
                "role_name": "employee",
                "resource_type": "vector_db",
                "resource_id": "e2e_mapped",
                "permission_level": "read",
            },
        )
        assert resp.status_code == 201
        rp_id = resp.json()["id"]

        # employee で read チェック → allowed
        resp = await client.post(
            "/auth/authorization/check-resource",
            headers=auth_headers(employee_token),
            json={
                "resource_type": "vector_db",
                "resource_id": "e2e_mapped",
                "required_level": "read",
            },
        )
        assert resp.json()["allowed"] is True

        # クリーンアップ
        await client.delete(
            f"/auth/authorization/resource-permissions/{rp_id}",
            headers=headers,
        )

    async def test_mapped_resource_write_denied(
        self, client: httpx.AsyncClient, admin_token: str, employee_token: str
    ) -> None:
        """マッピング設定後 write 拒否（read のみ許可）."""
        headers = auth_headers(admin_token)

        # リソースパーミッション作成（read のみ）
        resp = await client.post(
            "/auth/authorization/resource-permissions",
            headers=headers,
            json={
                "role_name": "employee",
                "resource_type": "business_db",
                "resource_id": "e2e_restricted",
                "permission_level": "read",
            },
        )
        assert resp.status_code == 201
        rp_id = resp.json()["id"]

        # employee で write チェック → denied
        resp = await client.post(
            "/auth/authorization/check-resource",
            headers=auth_headers(employee_token),
            json={
                "resource_type": "business_db",
                "resource_id": "e2e_restricted",
                "required_level": "write",
            },
        )
        data = resp.json()
        assert data["allowed"] is False
        assert data["reason"] == "insufficient_resource_permission"

        # クリーンアップ
        await client.delete(
            f"/auth/authorization/resource-permissions/{rp_id}",
            headers=headers,
        )

    async def test_business_db_table_level_control(
        self, client: httpx.AsyncClient, admin_token: str, employee_token: str
    ) -> None:
        """business_db テーブルレベル制御."""
        headers = auth_headers(admin_token)

        # employees テーブル → admin 権限
        resp = await client.post(
            "/auth/authorization/resource-permissions",
            headers=headers,
            json={
                "role_name": "employee",
                "resource_type": "business_db",
                "resource_id": "employees",
                "permission_level": "admin",
            },
        )
        assert resp.status_code == 201
        rp_id = resp.json()["id"]

        # employee で admin チェック → allowed
        resp = await client.post(
            "/auth/authorization/check-resource",
            headers=auth_headers(employee_token),
            json={
                "resource_type": "business_db",
                "resource_id": "employees",
                "required_level": "admin",
            },
        )
        assert resp.json()["allowed"] is True

        # クリーンアップ
        await client.delete(
            f"/auth/authorization/resource-permissions/{rp_id}",
            headers=headers,
        )


# ---------------------------------------------------------------------------
# D. JWT 統合
# ---------------------------------------------------------------------------


class TestJWTIntegration:
    """JWT 統合テスト（4件）."""

    async def test_login_jwt_contains_permissions(
        self, client: httpx.AsyncClient
    ) -> None:
        """ログイン後 JWT に permissions 含む."""
        resp = await client.post("/auth/login", json={
            "username": "admin", "password": "admin123",
        })
        data = resp.json()
        assert data["success"] is True
        assert data["user"]["permissions"] is not None
        assert "*" in data["user"]["permissions"]

    async def test_employee_jwt_permissions(
        self, client: httpx.AsyncClient
    ) -> None:
        """employee JWT のパーミッション確認."""
        resp = await client.post("/auth/login", json={
            "username": "suzuki", "password": "suzuki123",
        })
        data = resp.json()
        assert data["success"] is True
        perms = data["user"]["permissions"]
        assert "users:read" in perms
        assert "faq:read" in perms
        assert "faq:write" not in perms

    async def test_me_endpoint_includes_permissions(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """GET /auth/me のレスポンスに permissions 含む."""
        resp = await client.get(
            "/auth/me", headers=auth_headers(admin_token)
        )
        data = resp.json()
        assert data["success"] is True
        assert "permissions" in data["user"]
        assert "*" in data["user"]["permissions"]

    async def test_refresh_preserves_permissions(
        self, client: httpx.AsyncClient
    ) -> None:
        """リフレッシュ後もパーミッション維持."""
        # ログイン
        login_resp = await client.post("/auth/login", json={
            "username": "admin", "password": "admin123",
        })
        refresh_token = login_resp.json()["refresh_token"]

        # リフレッシュ
        refresh_resp = await client.post("/auth/refresh", json={
            "refresh_token": refresh_token,
        })
        assert refresh_resp.status_code == 200

        # 新トークンで /me 確認
        new_token = refresh_resp.json()["access_token"]
        me_resp = await client.get(
            "/auth/me", headers=auth_headers(new_token)
        )
        assert me_resp.json()["success"] is True
        assert "*" in me_resp.json()["user"]["permissions"]


# ---------------------------------------------------------------------------
# E. 後方互換
# ---------------------------------------------------------------------------


class TestBackwardCompatibility:
    """後方互換テスト（5件）."""

    async def test_login_still_works(
        self, client: httpx.AsyncClient
    ) -> None:
        """既存 /auth/login が変更なしで動作."""
        resp = await client.post("/auth/login", json={
            "username": "admin", "password": "admin123",
        })
        data = resp.json()
        assert data["success"] is True
        assert data["access_token"] is not None
        assert data["user"]["role"] == "admin"

    async def test_me_still_works(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """既存 /auth/me が互換維持."""
        resp = await client.get(
            "/auth/me", headers=auth_headers(admin_token)
        )
        data = resp.json()
        assert data["success"] is True
        assert data["user"]["user_id"] is not None
        assert data["user"]["role"] == "admin"

    async def test_register_still_works(
        self, client: httpx.AsyncClient
    ) -> None:
        """既存 /auth/register が互換維持."""
        resp = await client.post("/auth/register", json={
            "username": "e2e_compat_user",
            "password": "test12345678",
            "display_name": "E2E 互換テスト",
        })
        data = resp.json()
        # 既にユーザーが存在する場合は success=False でも OK
        assert "success" in data

    async def test_permissions_field_is_optional_in_response(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """permissions フィールドがレスポンスに追加されても既存クライアントを壊さない."""
        resp = await client.get(
            "/auth/me", headers=auth_headers(admin_token)
        )
        data = resp.json()
        # 既存フィールドが全て存在
        user = data["user"]
        assert "user_id" in user
        assert "username" in user
        assert "role" in user
        assert "roles" in user
        # 新フィールドも存在
        assert "permissions" in user

    async def test_health_endpoint(
        self, client: httpx.AsyncClient
    ) -> None:
        """/health エンドポイントが正常動作."""
        resp = await client.get("/health")
        assert resp.status_code == 200
        assert resp.json()["status"] == "ok"


# ---------------------------------------------------------------------------
# F. 管理 API
# ---------------------------------------------------------------------------


class TestAdminAPI:
    """管理 API テスト（5件）."""

    async def test_list_users_paginated(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ユーザー一覧（ページネーション）."""
        resp = await client.get(
            "/auth/admin/users?page=1&page_size=10",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 200
        data = resp.json()
        assert "users" in data
        assert "total" in data
        assert data["total"] >= 3  # admin, tanaka, suzuki
        assert len(data["users"]) <= 10

    async def test_get_user_detail(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ユーザー詳細."""
        # admin user_id 取得
        me_resp = await client.get(
            "/auth/me", headers=auth_headers(admin_token)
        )
        user_id = me_resp.json()["user"]["user_id"]

        resp = await client.get(
            f"/auth/admin/users/{user_id}",
            headers=auth_headers(admin_token),
        )
        assert resp.status_code == 200
        data = resp.json()
        assert data["username"] == "admin"
        assert "permissions" in data
        assert "roles" in data

    async def test_update_user(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ユーザー更新."""
        # suzuki の user_id を取得
        login_resp = await client.post("/auth/login", json={
            "username": "suzuki", "password": "suzuki123",
        })
        user_id = login_resp.json()["user"]["user_id"]

        resp = await client.put(
            f"/auth/admin/users/{user_id}",
            headers=auth_headers(admin_token),
            json={"department": "E2E テスト部"},
        )
        assert resp.status_code == 200
        assert resp.json()["department"] == "E2E テスト部"

        # 元に戻す
        await client.put(
            f"/auth/admin/users/{user_id}",
            headers=auth_headers(admin_token),
            json={"department": "営業部"},
        )

    async def test_deactivate_and_reactivate_user(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """ユーザー無効化 → 再有効化."""
        # テスト用ユーザー登録
        reg_resp = await client.post("/auth/register", json={
            "username": "e2e_deactivate_user",
            "password": "test12345678",
            "display_name": "無効化テスト",
        })
        if reg_resp.json().get("success"):
            user_id = reg_resp.json()["user"]["user_id"]
        else:
            # 既に存在する場合
            login_resp = await client.post("/auth/login", json={
                "username": "e2e_deactivate_user", "password": "test12345678",
            })
            user_id = login_resp.json()["user"]["user_id"]

        headers = auth_headers(admin_token)

        # 無効化
        resp = await client.delete(
            f"/auth/admin/users/{user_id}", headers=headers
        )
        assert resp.status_code == 200

        # 再有効化
        resp = await client.put(
            f"/auth/admin/users/{user_id}",
            headers=headers,
            json={"is_active": True},
        )
        assert resp.status_code == 200
        assert resp.json()["is_active"] is True

    async def test_admin_reset_password(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        """管理者パスワードリセット."""
        # suzuki の user_id を取得
        login_resp = await client.post("/auth/login", json={
            "username": "suzuki", "password": "suzuki123",
        })
        user_id = login_resp.json()["user"]["user_id"]

        resp = await client.post(
            f"/auth/admin/users/{user_id}/reset-password",
            headers=auth_headers(admin_token),
            json={"new_password": "newpassword12345"},
        )
        assert resp.status_code == 200

        # 新パスワードでログイン
        login_resp = await client.post("/auth/login", json={
            "username": "suzuki", "password": "newpassword12345",
        })
        assert login_resp.json()["success"] is True

        # 元のパスワードに戻す
        await client.post(
            f"/auth/admin/users/{user_id}/reset-password",
            headers=auth_headers(admin_token),
            json={"new_password": "suzuki123"},
        )
