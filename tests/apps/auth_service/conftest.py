"""auth_service テスト共有フィクスチャ.

テスト用 DB + AsyncClient + 3ユーザートークンを提供。
"""

from __future__ import annotations

import contextlib
import os
from typing import Any

import httpx
import pytest


# テスト用 SQLite DB パスを設定（import 前に環境変数を設定）
os.environ["AUTH_DATABASE_URL"] = "sqlite+aiosqlite:///./test_auth_service.db"
os.environ["AUTH_DB_SEED_DEFAULTS"] = "true"
os.environ["AUTH_DB_AUTO_CREATE"] = "true"
os.environ["JWT_SECRET_KEY"] = "test-secret-key-for-auth-tests-only"
os.environ["AUTHZ_EMBED_PERMISSIONS_IN_JWT"] = "true"
os.environ["AUTHZ_DEFAULT_OPEN"] = "true"
os.environ["AUTHZ_CACHE_TTL_SECONDS"] = "0"  # テストではキャッシュ無効


@pytest.fixture(scope="session")
def auth_app():
    """テスト用 FastAPI アプリを生成."""
    # シングルトンをリセットしてテスト設定を適用
    from shared.auth_service.service import reset_auth_service

    reset_auth_service()

    from shared.auth_service.core.authorization import reset_authorization_service

    reset_authorization_service()

    from shared.auth_service.main import create_app

    return create_app()


@pytest.fixture
async def client(auth_app: Any) -> httpx.AsyncClient:
    """テスト用 AsyncClient を提供."""
    # DB 状態をリセットして現在のイベントループで再初期化
    import shared.auth_service.db.session as db_mod

    if db_mod._is_ready:
        # 既に別のイベントループで初期化済みの場合、リセットして再初期化
        try:
            await db_mod.close_db()
        except Exception:
            db_mod._is_ready = False
            db_mod._ready_lock = None
            db_mod._ready_lock_loop_id = None

    # 現在のイベントループで DB を初期化
    await db_mod.ensure_database_ready()

    transport = httpx.ASGITransport(app=auth_app, raise_app_exceptions=False)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as ac:
        yield ac


@pytest.fixture
async def admin_token(client: httpx.AsyncClient) -> str:
    """admin ユーザーのアクセストークンを取得."""
    resp = await client.post(
        "/auth/login",
        json={
            "username": "admin",
            "password": "admin123",
        },
    )
    data = resp.json()
    assert data["success"] is True, f"admin ログイン失敗: {data}"
    token: str = data["access_token"]
    return token


@pytest.fixture
async def manager_token(client: httpx.AsyncClient) -> str:
    """manager ユーザーのアクセストークンを取得."""
    resp = await client.post(
        "/auth/login",
        json={
            "username": "tanaka",
            "password": "tanaka123",
        },
    )
    data = resp.json()
    assert data["success"] is True, f"manager ログイン失敗: {data}"
    token: str = data["access_token"]
    return token


@pytest.fixture
async def employee_token(client: httpx.AsyncClient) -> str:
    """employee ユーザーのアクセストークンを取得."""
    resp = await client.post(
        "/auth/login",
        json={
            "username": "suzuki",
            "password": "suzuki123",
        },
    )
    data = resp.json()
    assert data["success"] is True, f"employee ログイン失敗: {data}"
    token: str = data["access_token"]
    return token


def auth_headers(token: str) -> dict[str, str]:
    """Authorization ヘッダーを生成."""
    return {"Authorization": f"Bearer {token}"}


@pytest.fixture(scope="session", autouse=True)
def _cleanup_aiosqlite_threads():
    """Ensure aiosqlite worker threads are shut down after the session.

    Without this, the non-daemon ``_connection_worker_thread`` keeps the
    process alive indefinitely after all tests have finished.
    """
    yield
    import asyncio

    import shared.auth_service.db.session as db_mod

    async def _shutdown() -> None:
        with contextlib.suppress(Exception):
            await db_mod.close_db()

    try:
        loop = asyncio.new_event_loop()
        loop.run_until_complete(_shutdown())
        loop.close()
    except Exception:
        pass
