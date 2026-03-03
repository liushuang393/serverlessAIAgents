"""auth_service DB セッション初期化の回帰テスト."""

from __future__ import annotations

import asyncio

import pytest
from apps.auth_service.config import get_settings
from apps.auth_service.db import session as session_module


@pytest.mark.asyncio
async def test_ensure_database_ready_allows_seed_helpers_to_open_session(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """シード処理が get_db_session を呼んでもデッドロックしないことを確認."""
    settings = get_settings()
    original_auto_create = settings.AUTH_DB_AUTO_CREATE
    original_seed_defaults = settings.AUTH_DB_SEED_DEFAULTS

    async def fake_seed_default_users() -> None:
        async with session_module.get_db_session():
            return

    async def fake_seed_authorization() -> None:
        async with session_module.get_db_session():
            return

    try:
        settings.AUTH_DB_AUTO_CREATE = False
        settings.AUTH_DB_SEED_DEFAULTS = True
        await session_module.close_db()

        import apps.auth_service.db.seed as seed_module
        import apps.auth_service.db.seed_authorization as seed_authorization_module

        monkeypatch.setattr(seed_module, "seed_default_users", fake_seed_default_users)
        monkeypatch.setattr(seed_authorization_module, "seed_authorization", fake_seed_authorization)

        await asyncio.wait_for(session_module.ensure_database_ready(), timeout=2.0)
    finally:
        settings.AUTH_DB_AUTO_CREATE = original_auto_create
        settings.AUTH_DB_SEED_DEFAULTS = original_seed_defaults
        await session_module.close_db()
