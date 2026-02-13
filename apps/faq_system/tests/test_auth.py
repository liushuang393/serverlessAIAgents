"""FAQ 認証/権限/永続化の統合テスト."""

from __future__ import annotations

import hashlib
import hmac
import json
import time
from pathlib import Path
from typing import Any

import httpx
import pytest

from apps.faq_system.backend.auth.dependencies import resolve_user
from apps.faq_system.backend.auth.service import get_auth_service, reset_auth_service_singleton
from apps.faq_system.backend.db import close_db
from apps.faq_system.main import _services, app


TEST_DB_PATH = Path("/tmp/faq_auth_test.db")
TEST_DB_URL = f"sqlite+aiosqlite:///{TEST_DB_PATH}"
TEST_JWT_SECRET = "faq-test-jwt-secret"
TEST_PROXY_SECRET = "faq-proxy-secret"


@pytest.fixture(autouse=True)
async def reset_state(monkeypatch: pytest.MonkeyPatch) -> Any:
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
async def client() -> Any:
    """ASGI クライアント."""
    transport = httpx.ASGITransport(app=app)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as c:
        yield c


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


def _proxy_signature(
    *,
    method: str,
    path: str,
    username: str,
    display_name: str,
    role: str,
    department: str,
    position: str,
    timestamp: str,
    nonce: str,
    secret: str = TEST_PROXY_SECRET,
) -> str:
    canonical = "\n".join(
        [
            method.upper(),
            path,
            username,
            display_name,
            role,
            department,
            position,
            timestamp,
            nonce,
        ]
    )
    return hmac.new(secret.encode("utf-8"), canonical.encode("utf-8"), hashlib.sha256).hexdigest()


@pytest.mark.asyncio
async def test_login_success_and_me(client: httpx.AsyncClient) -> None:
    data = await _login(client)
    assert data["success"] is True
    assert data["user"]["username"] == "admin"
    assert data["access_token"]
    assert "session_token" in client.cookies

    me = await client.get("/api/auth/me", headers=_auth_headers(data["access_token"]))
    assert me.status_code == 200
    payload = me.json()
    assert payload["success"] is True
    assert payload["user"]["username"] == "admin"


@pytest.mark.asyncio
async def test_login_failure(client: httpx.AsyncClient) -> None:
    response = await client.post(
        "/api/auth/login",
        json={"username": "admin", "password": "wrong-password"},
    )
    assert response.status_code == 200
    payload = response.json()
    assert payload["success"] is False
    assert payload["access_token"] is None


@pytest.mark.asyncio
async def test_change_password_and_relogin(client: httpx.AsyncClient) -> None:
    token = (await _login(client))["access_token"]
    changed = await client.post(
        "/api/auth/password/change",
        headers=_auth_headers(token),
        json={"current_password": "admin123", "new_password": "admin12345"},
    )
    assert changed.status_code == 200
    assert changed.json()["success"] is True

    failed_old = await client.post(
        "/api/auth/login",
        json={"username": "admin", "password": "admin123"},
    )
    assert failed_old.json()["success"] is False

    ok_new = await client.post(
        "/api/auth/login",
        json={"username": "admin", "password": "admin12345"},
    )
    assert ok_new.json()["success"] is True


@pytest.mark.asyncio
async def test_password_reset_flow(client: httpx.AsyncClient) -> None:
    forgot = await client.post("/api/auth/password/forgot", json={"username": "admin"})
    assert forgot.status_code == 200
    forgot_payload = forgot.json()
    assert forgot_payload["success"] is True
    reset_token = forgot_payload["reset_token"]

    reset = await client.post(
        "/api/auth/password/reset",
        json={"reset_token": reset_token, "new_password": "reset1234"},
    )
    assert reset.status_code == 200
    assert reset.json()["success"] is True

    relogin = await client.post(
        "/api/auth/login",
        json={"username": "admin", "password": "reset1234"},
    )
    assert relogin.json()["success"] is True


@pytest.mark.asyncio
async def test_profile_update_persisted(client: httpx.AsyncClient) -> None:
    token = (await _login(client))["access_token"]
    update = await client.patch(
        "/api/auth/profile",
        headers=_auth_headers(token),
        json={
            "display_name": "管理者 三郎",
            "department": "IT統制部",
            "position": "統括",
        },
    )
    assert update.status_code == 200
    assert update.json()["success"] is True

    me = await client.get("/api/auth/me", headers=_auth_headers(token))
    payload = me.json()
    assert payload["user"]["display_name"] == "管理者 三郎"
    assert payload["user"]["department"] == "IT統制部"


@pytest.mark.asyncio
async def test_session_cookie_is_revoked_on_logout(client: httpx.AsyncClient) -> None:
    _ = await _login(client)
    session_token = client.cookies.get("session_token")
    assert session_token

    pre_user = await resolve_user(
        session_token=session_token,
        request_method="GET",
        request_path="/api/auth/me",
    )
    assert pre_user is not None

    logout = await client.post("/api/auth/logout")
    assert logout.status_code == 200
    assert logout.json()["success"] is True

    post_user = await resolve_user(
        session_token=session_token,
        request_method="GET",
        request_path="/api/auth/me",
    )
    assert post_user is None


@pytest.mark.asyncio
async def test_kb_settings_requires_role(client: httpx.AsyncClient) -> None:
    employee = await _login(client, username="suzuki", password="suzuki123")
    denied = await client.patch(
        "/api/kb/settings",
        headers=_auth_headers(employee["access_token"]),
        json={"internal_collection": "internal_denied"},
    )
    assert denied.status_code == 403

    admin = await _login(client)
    updated = await client.patch(
        "/api/kb/settings",
        headers=_auth_headers(admin["access_token"]),
        json={"internal_collection": "internal_v2"},
    )
    assert updated.status_code == 200
    assert updated.json()["success"] is True


@pytest.mark.asyncio
async def test_kb_settings_persisted(client: httpx.AsyncClient) -> None:
    token = (await _login(client))["access_token"]
    patch = await client.patch(
        "/api/kb/settings",
        headers=_auth_headers(token),
        json={
            "internal_collection": "internal_prod_v1",
            "external_collection": "external_prod_v1",
            "default_kb": "external",
        },
    )
    assert patch.status_code == 200

    get1 = await client.get("/api/kb/settings", headers=_auth_headers(token))
    assert get1.status_code == 200
    payload = get1.json()
    assert payload["internal_collection"] == "internal_prod_v1"
    assert payload["external_collection"] == "external_prod_v1"
    assert payload["default_kb"] == "external"


@pytest.mark.asyncio
async def test_proxy_auth_signature_success(
    monkeypatch: pytest.MonkeyPatch,
    client: httpx.AsyncClient,
) -> None:
    monkeypatch.setenv("FAQ_TRUST_PROXY_AUTH", "true")

    timestamp = str(int(time.time()))
    nonce = "nonce-signature-ok"
    signature = _proxy_signature(
        method="GET",
        path="/api/kb/settings",
        username="gateway-admin",
        display_name="Gateway Admin",
        role="admin",
        department="Platform",
        position="Lead",
        timestamp=timestamp,
        nonce=nonce,
    )

    response = await client.get(
        "/api/kb/settings",
        headers={
            "X-Forwarded-User": "gateway-admin",
            "X-Forwarded-Preferred-Username": "Gateway Admin",
            "X-Forwarded-Groups": "admin,platform",
            "X-Forwarded-Department": "Platform",
            "X-Forwarded-Title": "Lead",
            "X-Auth-Timestamp": timestamp,
            "X-Auth-Nonce": nonce,
            "X-Auth-Signature": signature,
        },
    )
    assert response.status_code == 200
    assert "internal_collection" in response.json()


@pytest.mark.asyncio
async def test_proxy_auth_replay_nonce_rejected(
    monkeypatch: pytest.MonkeyPatch,
    client: httpx.AsyncClient,
) -> None:
    monkeypatch.setenv("FAQ_TRUST_PROXY_AUTH", "true")

    timestamp = str(int(time.time()))
    nonce = "nonce-replay"
    signature = _proxy_signature(
        method="GET",
        path="/api/kb/settings",
        username="gateway-admin",
        display_name="Gateway Admin",
        role="admin",
        department="Platform",
        position="Lead",
        timestamp=timestamp,
        nonce=nonce,
    )
    headers = {
        "X-Forwarded-User": "gateway-admin",
        "X-Forwarded-Preferred-Username": "Gateway Admin",
        "X-Forwarded-Groups": "admin,platform",
        "X-Forwarded-Department": "Platform",
        "X-Forwarded-Title": "Lead",
        "X-Auth-Timestamp": timestamp,
        "X-Auth-Nonce": nonce,
        "X-Auth-Signature": signature,
    }

    first = await client.get("/api/kb/settings", headers=headers)
    second = await client.get("/api/kb/settings", headers=headers)
    assert first.status_code == 200
    assert second.status_code == 401


@pytest.mark.asyncio
async def test_proxy_auth_bad_signature_rejected(
    monkeypatch: pytest.MonkeyPatch,
    client: httpx.AsyncClient,
) -> None:
    monkeypatch.setenv("FAQ_TRUST_PROXY_AUTH", "true")

    response = await client.get(
        "/api/kb/settings",
        headers={
            "X-Forwarded-User": "gateway-admin",
            "X-Forwarded-Preferred-Username": "Gateway Admin",
            "X-Forwarded-Groups": "admin,platform",
            "X-Forwarded-Department": "Platform",
            "X-Forwarded-Title": "Lead",
            "X-Auth-Timestamp": str(int(time.time())),
            "X-Auth-Nonce": "nonce-invalid",
            "X-Auth-Signature": "bad-signature",
        },
    )
    assert response.status_code == 401


@pytest.mark.asyncio
async def test_ldap_provider_login_with_mock_users(
    monkeypatch: pytest.MonkeyPatch,
    client: httpx.AsyncClient,
) -> None:
    monkeypatch.setenv("FAQ_AUTH_PROVIDER", "ldap")
    monkeypatch.setenv(
        "FAQ_LDAP_USERS_JSON",
        json.dumps(
            {
                "ldap_admin": {
                    "password": "ldap-pass-123",
                    "display_name": "LDAP Admin",
                    "department": "Infra",
                    "position": "Architect",
                    "role": "admin",
                }
            }
        ),
    )

    await close_db()
    reset_auth_service_singleton()

    response = await client.post(
        "/api/auth/login",
        json={"username": "ldap_admin", "password": "ldap-pass-123"},
    )
    assert response.status_code == 200
    payload = response.json()
    assert payload["success"] is True
    assert payload["user"]["username"] == "ldap_admin"
    assert payload["user"]["role"] == "admin"


@pytest.mark.asyncio
async def test_idp_provider_login_with_mock_users(
    monkeypatch: pytest.MonkeyPatch,
    client: httpx.AsyncClient,
) -> None:
    monkeypatch.setenv("FAQ_AUTH_PROVIDER", "idp")
    monkeypatch.setenv(
        "FAQ_IDP_USERS_JSON",
        json.dumps(
            {
                "idp_user": {
                    "password": "idp-pass-123",
                    "display_name": "IdP User",
                    "department": "Biz",
                    "position": "Owner",
                    "role": "manager",
                }
            }
        ),
    )

    await close_db()
    reset_auth_service_singleton()

    response = await client.post(
        "/api/auth/login",
        json={"username": "idp_user", "password": "idp-pass-123"},
    )
    assert response.status_code == 200
    payload = response.json()
    assert payload["success"] is True
    assert payload["user"]["username"] == "idp_user"
    assert payload["user"]["role"] == "manager"


@pytest.mark.asyncio
async def test_chat_history_persisted(client: httpx.AsyncClient) -> None:
    class DummyFAQAgent:
        async def run(self, _: dict[str, Any]) -> dict[str, Any]:
            return {"answer": "dummy answer", "query_type": "chat"}

    _services["faq_agent"] = DummyFAQAgent()

    token = (await _login(client))["access_token"]
    chat = await client.post(
        "/api/chat",
        headers=_auth_headers(token),
        json={"message": "在庫は？", "session_id": "sess-chat-1"},
    )
    assert chat.status_code == 200
    assert chat.json()["session_id"] == "sess-chat-1"

    history = await client.get(
        "/api/chat/history",
        headers=_auth_headers(token),
        params={"session_id": "sess-chat-1"},
    )
    assert history.status_code == 200
    payload = history.json()
    assert payload["count"] == 2
    assert payload["messages"][0]["role"] == "user"
    assert payload["messages"][1]["role"] == "assistant"
    assert payload["messages"][1]["content"] == "dummy answer"


@pytest.mark.asyncio
async def test_protected_endpoint_requires_auth(client: httpx.AsyncClient) -> None:
    response = await client.get("/api/chat/history", params={"session_id": "s1"})
    assert response.status_code == 401


@pytest.mark.asyncio
async def test_logout_with_token_keeps_jwt_valid_until_expiry(client: httpx.AsyncClient) -> None:
    login_data = await _login(client)
    token = login_data["access_token"]

    logout = await client.post("/api/auth/logout", headers=_auth_headers(token))
    assert logout.status_code == 200

    # JWT はステートレスのため、セッション失効後でも期限内は有効。
    me = await client.get("/api/auth/me", headers=_auth_headers(token))
    assert me.status_code == 200
    assert me.json()["success"] is True


@pytest.mark.asyncio
async def test_chat_stream_persists_result(client: httpx.AsyncClient) -> None:
    class DummyStreamAgent:
        async def run_stream(self, _: dict[str, Any]) -> Any:
            yield {"type": "progress", "progress": 10, "message": "start"}
            yield {
                "type": "result",
                "data": {"answer": "stream answer", "query_type": "chat"},
            }

    _services["faq_agent"] = DummyStreamAgent()
    token = (await _login(client))["access_token"]

    response = await client.post(
        "/api/chat/stream",
        headers=_auth_headers(token),
        json={"message": "stream?", "session_id": "sess-stream-1"},
    )
    assert response.status_code == 200
    assert "text/event-stream" in response.headers["content-type"]

    history = await client.get(
        "/api/chat/history",
        headers=_auth_headers(token),
        params={"session_id": "sess-stream-1"},
    )
    payload = history.json()
    assert payload["count"] == 2
    assert payload["messages"][1]["content"] == "stream answer"


@pytest.mark.asyncio
async def test_proxy_headers_without_trust_are_ignored(client: httpx.AsyncClient) -> None:
    response = await client.get(
        "/api/kb/settings",
        headers={
            "X-Forwarded-User": "no-trust-user",
            "X-Auth-Timestamp": str(int(time.time())),
            "X-Auth-Nonce": "nonce-no-trust",
            "X-Auth-Signature": "ignored",
        },
    )
    assert response.status_code == 401


@pytest.mark.asyncio
async def test_refresh_token_endpoint(client: httpx.AsyncClient) -> None:
    token = (await _login(client))["access_token"]
    refreshed = await client.post("/api/auth/token", headers=_auth_headers(token))
    assert refreshed.status_code == 200
    payload = refreshed.json()
    assert payload["access_token"]
    assert payload["token_type"] == "bearer"
    assert payload["expires_in"] > 0


@pytest.mark.asyncio
async def test_external_provider_password_change_is_rejected(
    monkeypatch: pytest.MonkeyPatch,
    client: httpx.AsyncClient,
) -> None:
    monkeypatch.setenv("FAQ_AUTH_PROVIDER", "ldap")
    monkeypatch.setenv(
        "FAQ_LDAP_USERS_JSON",
        json.dumps(
            {
                "ldap_user": {
                    "password": "ldap-raw-pass",
                    "display_name": "LDAP User",
                    "department": "Infra",
                    "position": "Engineer",
                    "role": "employee",
                }
            }
        ),
    )
    await close_db()
    reset_auth_service_singleton()

    login = await client.post(
        "/api/auth/login",
        json={"username": "ldap_user", "password": "ldap-raw-pass"},
    )
    token = login.json()["access_token"]
    changed = await client.post(
        "/api/auth/password/change",
        headers=_auth_headers(token),
        json={"current_password": "ldap-raw-pass", "new_password": "ldap12345"},
    )
    assert changed.status_code == 200
    payload = changed.json()
    assert payload["success"] is False
    assert "企業認証管理" in payload["message"]


@pytest.mark.asyncio
async def test_chat_endpoint_returns_session_id(client: httpx.AsyncClient) -> None:
    class DummyFAQAgent:
        async def run(self, _: dict[str, Any]) -> dict[str, Any]:
            return {"answer": "hello"}

    _services["faq_agent"] = DummyFAQAgent()
    token = (await _login(client))["access_token"]

    response = await client.post(
        "/api/chat",
        headers=_auth_headers(token),
        json={"message": "こんにちは"},
    )
    assert response.status_code == 200
    payload = response.json()
    assert payload["session_id"].startswith("session-")


@pytest.mark.asyncio
async def test_proxy_auth_supports_sha256_prefix(
    monkeypatch: pytest.MonkeyPatch,
    client: httpx.AsyncClient,
) -> None:
    monkeypatch.setenv("FAQ_TRUST_PROXY_AUTH", "true")
    timestamp = str(int(time.time()))
    nonce = "nonce-sha-prefix"
    digest = _proxy_signature(
        method="GET",
        path="/api/kb/settings",
        username="proxy-user",
        display_name="Proxy User",
        role="admin",
        department="IT",
        position="Lead",
        timestamp=timestamp,
        nonce=nonce,
    )

    response = await client.get(
        "/api/kb/settings",
        headers={
            "X-Forwarded-User": "proxy-user",
            "X-Forwarded-Preferred-Username": "Proxy User",
            "X-Forwarded-Groups": "admin",
            "X-Forwarded-Department": "IT",
            "X-Forwarded-Title": "Lead",
            "X-Auth-Timestamp": timestamp,
            "X-Auth-Nonce": nonce,
            "X-Auth-Signature": f"sha256={digest}",
        },
    )
    assert response.status_code == 200


@pytest.mark.asyncio
async def test_auth_service_singleton_reset() -> None:
    service_1 = get_auth_service()
    reset_auth_service_singleton()
    service_2 = get_auth_service()
    assert service_1 is not service_2
    assert isinstance(service_2, type(service_1))
