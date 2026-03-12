"""アクセス制御 API のテスト."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import pytest


if TYPE_CHECKING:
    import httpx


async def _login(
    client: httpx.AsyncClient,
    username: str = "admin",
    password: str = "admin123",
) -> dict[str, Any]:
    response = await client.post(
        "/api/auth/login",
        json={"username": username, "password": password},
    )
    assert response.status_code == 200
    return response.json()


def _auth_headers(token: str) -> dict[str, str]:
    return {"Authorization": f"Bearer {token}"}


@pytest.mark.asyncio
async def test_get_access_matrix(client: httpx.AsyncClient) -> None:
    """GET /api/access/matrix がマトリクスを返す."""
    data = await _login(client)
    token = data["access_token"]

    response = await client.get("/api/access/matrix", headers=_auth_headers(token))
    assert response.status_code == 200

    body = response.json()
    assert "matrix" in body
    matrix = body["matrix"]

    # admin はすべての KB タイプにアクセス可能
    assert matrix["admin"]["internal"] is True
    assert matrix["admin"]["external"] is True
    assert matrix["admin"]["confidential"] is True

    # guest は external のみ
    assert matrix["guest"]["internal"] is False
    assert matrix["guest"]["external"] is True
    assert matrix["guest"]["confidential"] is False


@pytest.mark.asyncio
async def test_update_collection_roles_requires_admin(client: httpx.AsyncClient) -> None:
    """権限変更が admin のみ可能."""
    # employee でログイン
    admin_data = await _login(client, "admin", "admin123")
    admin_token = admin_data["access_token"]

    # まず employee ユーザーを作成
    await client.post(
        "/api/auth/register",
        json={
            "username": "emp_user",
            "password": "employee123",
            "display_name": "Employee User",
        },
    )
    emp_data = await _login(client, "emp_user", "employee123")
    emp_token = emp_data["access_token"]

    # employee で権限変更を試みる → 403
    response = await client.patch(
        "/api/access/collections/internal/roles",
        json={"allowed_roles": ["admin", "guest"]},
        headers=_auth_headers(emp_token),
    )
    assert response.status_code == 403

    # admin で権限変更 → 200
    response = await client.patch(
        "/api/access/collections/internal/roles",
        json={"allowed_roles": ["admin", "guest"]},
        headers=_auth_headers(admin_token),
    )
    assert response.status_code == 200
    body = response.json()
    assert body["updated"] is True
    assert body["matrix"]["guest"]["internal"] is True


@pytest.mark.asyncio
async def test_get_access_matrix_requires_auth(client: httpx.AsyncClient) -> None:
    """GET /api/access/matrix は認証なしで 401."""
    response = await client.get("/api/access/matrix")
    assert response.status_code == 401
