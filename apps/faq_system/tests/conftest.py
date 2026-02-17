import os
from pathlib import Path
from typing import Any, AsyncGenerator

import httpx
import pytest

from apps.faq_system.backend.auth.service import get_auth_service, reset_auth_service_singleton
from apps.faq_system.backend.db import close_db
from apps.faq_system.main import app
from apps.faq_system.routers.dependencies import _services

TEST_DB_PATH = Path("/tmp/faq_auth_test.db")
TEST_DB_URL = f"sqlite+aiosqlite:///{TEST_DB_PATH}"
TEST_JWT_SECRET = "faq-test-jwt-secret"
TEST_PROXY_SECRET = "faq-proxy-secret"


@pytest.fixture(autouse=True)
async def reset_state(monkeypatch: pytest.MonkeyPatch) -> AsyncGenerator[None, None]:
    """各テストの前後で DB/シングルトンを初期化."""
    monkeypatch.setenv("FAQ_DATABASE_URL", TEST_DB_URL)
    monkeypatch.setenv("FAQ_DB_AUTO_CREATE", "true")
    monkeypatch.setenv("FAQ_AUTH_PROVIDER", "local_db")
    monkeypatch.setenv("FAQ_AUTH_DEV_MODE", "true")
    monkeypatch.setenv("FAQ_TRUST_PROXY_AUTH", "false")
    monkeypatch.setenv("FAQ_PROXY_AUTH_REQUIRE_SIGNATURE", "true")
    monkeypatch.setenv("FAQ_PROXY_AUTH_SHARED_SECRET", TEST_PROXY_SECRET)
    monkeypatch.setenv("JWT_SECRET_KEY", TEST_JWT_SECRET)
    monkeypatch.setenv("JWT_EXPIRE_MINUTES", "60")

    if TEST_DB_PATH.exists():
        TEST_DB_PATH.unlink()

    _services.clear()
    await close_db()
    reset_auth_service_singleton()
    await get_auth_service().reset_demo_data()
    yield

    _services.clear()
    await close_db()
    reset_auth_service_singleton()
    if TEST_DB_PATH.exists():
        TEST_DB_PATH.unlink()


@pytest.fixture
async def client() -> AsyncGenerator[httpx.AsyncClient, None]:
    """ASGI クライアント."""
    transport = httpx.ASGITransport(app=app)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as c:
        yield c
