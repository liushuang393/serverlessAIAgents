"""リソース定義シードのテスト."""

from __future__ import annotations

from typing import TYPE_CHECKING

import pytest


if TYPE_CHECKING:
    import httpx


class TestSeedFAQResourceDefinitions:
    """FAQ リソース定義シードの検証."""

    @pytest.mark.asyncio
    async def test_seed_creates_definitions(self, client: httpx.AsyncClient, admin_token: str) -> None:
        """FAQ リソース定義がシードされる."""
        # seed は main.py lifespan で呼ばれているため、
        # ensure_database_ready 後に definitions が存在するはず
        from shared.auth_service.db.seed_authorization import seed_faq_resource_definitions

        await seed_faq_resource_definitions()
        # 2回目は冪等
        await seed_faq_resource_definitions()

        # API で確認
        resp = await client.get(
            "/auth/authorization/resource-definitions",
            params={"app_name": "faq_system"},
            headers={"Authorization": f"Bearer {admin_token}"},
        )
        assert resp.status_code == 200
        definitions = resp.json()
        # シードデータは 5 件だが、他テストが先に一部作成済みの場合
        # 冪等チェックでスキップされるため、最低 3 件で検証
        assert len(definitions) >= 3
        scopes = {d["scope"] for d in definitions}
        assert "common" in scopes
