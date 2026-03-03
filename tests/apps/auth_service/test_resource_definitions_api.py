"""resource-definitions API のテスト."""

from __future__ import annotations

import secrets

import httpx
import pytest


def _unique_id(prefix: str = "test") -> str:
    """テスト毎にユニークなリソースIDを生成."""
    return f"{prefix}__{secrets.token_hex(6)}"


class TestResourceDefinitionsAPI:
    """resource-definitions CRUD テスト."""

    @pytest.mark.asyncio
    async def test_list_empty(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        resp = await client.get(
            "/auth/authorization/resource-definitions",
            headers={"Authorization": f"Bearer {admin_token}"},
        )
        assert resp.status_code == 200
        assert isinstance(resp.json(), list)

    @pytest.mark.asyncio
    async def test_create_and_get(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        headers = {"Authorization": f"Bearer {admin_token}"}
        rid = _unique_id("faq__test__common")
        payload = {
            "resource_type": "vector_db",
            "resource_id": rid,
            "display_name": "FAQ 共通ナレッジ",
            "app_name": "faq_system",
            "scope": "common",
            "backend_key": "shared",
            "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
        }
        resp = await client.post(
            "/auth/authorization/resource-definitions",
            json=payload,
            headers=headers,
        )
        assert resp.status_code == 201
        data = resp.json()
        assert data["resource_type"] == "vector_db"
        assert data["scope"] == "common"

        # 一覧取得で存在確認
        resp2 = await client.get(
            "/auth/authorization/resource-definitions",
            params={"app_name": "faq_system"},
            headers=headers,
        )
        assert resp2.status_code == 200
        assert len(resp2.json()) >= 1

    @pytest.mark.asyncio
    async def test_delete(
        self, client: httpx.AsyncClient, admin_token: str
    ) -> None:
        headers = {"Authorization": f"Bearer {admin_token}"}
        rid = _unique_id("test__delete")
        # 作成
        resp = await client.post(
            "/auth/authorization/resource-definitions",
            json={
                "resource_type": "vector_db",
                "resource_id": rid,
                "display_name": "削除テスト",
                "app_name": "test",
                "scope": "common",
            },
            headers=headers,
        )
        assert resp.status_code == 201
        rd_id = resp.json()["id"]

        # 削除
        resp2 = await client.delete(
            f"/auth/authorization/resource-definitions/{rd_id}",
            headers=headers,
        )
        assert resp2.status_code == 200
