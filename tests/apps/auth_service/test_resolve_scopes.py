"""resolve-scopes API のテスト."""

from __future__ import annotations

import httpx
import pytest


@pytest.fixture
async def seeded_resources(client: httpx.AsyncClient, admin_token: str) -> None:
    """テスト用リソース定義 + resource_permissions をシード（冪等）."""
    headers = {"Authorization": f"Bearer {admin_token}"}

    # リソース定義を登録（409 は既存 → 無視）
    for scope, bk in [
        ("common", "shared"),
        ("manager", "shared"),
        ("confidential", "confidential"),
    ]:
        await client.post(
            "/auth/authorization/resource-definitions",
            json={
                "resource_type": "vector_db",
                "resource_id": f"faq__default__{scope}",
                "display_name": f"FAQ {scope}",
                "app_name": "faq_system",
                "scope": scope,
                "backend_key": bk,
                "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
            },
            headers=headers,
        )

    # resource_permissions: manager → common(read), manager(read)
    # IntegrityError 回避のため重複チェック付き
    existing_rps = await client.get(
        "/auth/authorization/resource-permissions",
        headers=headers,
    )
    existing_ids = set()
    if existing_rps.status_code == 200:
        for rp in existing_rps.json():
            existing_ids.add((rp["role_name"], rp["resource_type"], rp["resource_id"]))

    for rid in ["faq__default__common", "faq__default__manager"]:
        if ("manager", "vector_db", rid) in existing_ids:
            continue
        await client.post(
            "/auth/authorization/resource-permissions",
            json={
                "role_name": "manager",
                "resource_type": "vector_db",
                "resource_id": rid,
                "permission_level": "read",
            },
            headers=headers,
        )


class TestResolveScopes:
    """resolve-scopes エンドポイントのテスト."""

    @pytest.mark.asyncio
    async def test_resolve_manager_scopes(
        self,
        client: httpx.AsyncClient,
        admin_token: str,
        seeded_resources: None,
    ) -> None:
        resp = await client.get(
            "/auth/authorization/resolve-scopes",
            params={
                "role": "manager",
                "app_name": "faq_system",
                "resource_type": "vector_db",
            },
            headers={"Authorization": f"Bearer {admin_token}"},
        )
        assert resp.status_code == 200
        data = resp.json()
        scopes = {s["scope"] for s in data["scopes"]}
        assert "common" in scopes
        assert "manager" in scopes
        assert "confidential" not in scopes  # manager に confidential 権限なし

    @pytest.mark.asyncio
    async def test_resolve_unknown_role_empty(
        self,
        client: httpx.AsyncClient,
        admin_token: str,
        seeded_resources: None,
    ) -> None:
        resp = await client.get(
            "/auth/authorization/resolve-scopes",
            params={
                "role": "unknown_role",
                "app_name": "faq_system",
                "resource_type": "vector_db",
            },
            headers={"Authorization": f"Bearer {admin_token}"},
        )
        assert resp.status_code == 200
        assert len(resp.json()["scopes"]) == 0
