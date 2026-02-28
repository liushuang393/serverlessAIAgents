"""セッション管理 API のテスト."""

from __future__ import annotations

from datetime import UTC, datetime, timedelta
from pathlib import Path
from typing import Any

import httpx
import pytest
from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.backend.db import close_db, ensure_database_ready
from apps.faq_system.backend.db.models import ChatMessage
from apps.faq_system.backend.db.session import get_db_session
from apps.faq_system.main import app


TEST_DB_PATH = Path("/tmp/faq_session_test.db")
TEST_DB_URL = f"sqlite+aiosqlite:///{TEST_DB_PATH}"


@pytest.fixture(autouse=True)
async def reset_db(monkeypatch: pytest.MonkeyPatch) -> Any:
    """各テストの前後で DB を初期化."""
    monkeypatch.setenv("FAQ_DATABASE_URL", TEST_DB_URL)
    monkeypatch.setenv("FAQ_DB_AUTO_CREATE", "true")

    if TEST_DB_PATH.exists():
        TEST_DB_PATH.unlink()

    await ensure_database_ready()
    yield
    await close_db()
    if TEST_DB_PATH.exists():
        TEST_DB_PATH.unlink()


# 認証モック
async def mock_require_auth() -> UserInfo:
    return UserInfo(
        user_id="user-session-test",
        username="session_tester",
        display_name="Session Tester",
        role="user",
        department="QA",
        position="Tester",
    )


@pytest.fixture
def mock_auth() -> None:
    app.dependency_overrides[require_auth] = mock_require_auth
    yield
    app.dependency_overrides.pop(require_auth, None)


@pytest.fixture
async def client(mock_auth: None) -> Any:
    """ASGI クライアント."""
    transport = httpx.ASGITransport(app=app)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as c:
        yield c


async def _seed_chat_messages(session_id: str, count: int, user_id: str = "user-session-test") -> None:
    """テスト用メッセージを DB に投入."""
    async with get_db_session() as session:
        for i in range(count):
            session.add(
                ChatMessage(
                    id=f"msg-{session_id}-{i}",
                    session_id=session_id,
                    user_id=user_id,
                    username="tester",
                    role="user" if i % 2 == 0 else "assistant",
                    content=f"Message {i} in {session_id}",
                    transport="test",
                    created_at=datetime.now(tz=UTC) - timedelta(minutes=count - i),
                )
            )


@pytest.mark.asyncio
async def test_list_sessions_empty(client: httpx.AsyncClient) -> None:
    response = await client.get("/api/chat/sessions")
    assert response.status_code == 200
    data = response.json()
    assert data["count"] == 0
    assert data["sessions"] == []


@pytest.mark.asyncio
async def test_list_sessions_with_data(client: httpx.AsyncClient) -> None:
    # 2つのセッションを作成
    await _seed_chat_messages("session-A", 3)
    await _seed_chat_messages("session-B", 5)

    response = await client.get("/api/chat/sessions")
    assert response.status_code == 200
    data = response.json()
    assert data["count"] == 2

    # 新しい順 (session-B が最後に追加されたメッセージを持つはず)
    sessions = data["sessions"]
    assert sessions[0]["session_id"] == "session-B"
    assert sessions[0]["message_count"] == 5
    assert sessions[0]["title"] == "Message 0 in sess…"
    assert len(sessions[0]["title"]) <= 18  # 17文字 + 省略記号

    assert sessions[1]["session_id"] == "session-A"
    assert sessions[1]["message_count"] == 3


@pytest.mark.asyncio
async def test_delete_session(client: httpx.AsyncClient) -> None:
    await _seed_chat_messages("session-DEL", 2)

    # 削除前確認
    list_res = await client.get("/api/chat/sessions")
    assert list_res.json()["count"] == 1

    # 削除実行
    del_res = await client.delete("/api/chat/sessions/session-DEL")
    assert del_res.status_code == 200
    assert del_res.json()["success"] is True

    # 削除後確認
    list_res_after = await client.get("/api/chat/sessions")
    assert list_res_after.json()["count"] == 0

    # DB からも消えているか (history API)
    hist_res = await client.get("/api/chat/history", params={"session_id": "session-DEL"})
    assert hist_res.json()["count"] == 0


@pytest.mark.asyncio
async def test_delete_non_existent_session(client: httpx.AsyncClient) -> None:
    del_res = await client.delete("/api/chat/sessions/session-999")
    assert del_res.status_code == 200
    assert del_res.json()["success"] is False


@pytest.mark.asyncio
async def test_session_isolation(client: httpx.AsyncClient) -> None:
    """他人のセッションが見えないことを確認."""
    await _seed_chat_messages("my-session", 1, user_id="user-session-test")
    await _seed_chat_messages("other-session", 1, user_id="other-user")

    response = await client.get("/api/chat/sessions")
    data = response.json()
    assert data["count"] == 1
    assert data["sessions"][0]["session_id"] == "my-session"


@pytest.mark.asyncio
async def test_session_title_is_trimmed_to_17_chars(client: httpx.AsyncClient) -> None:
    """セッションタイトルは 17 文字以内に制限される."""
    long_text = "これはとても長いタイトル生成テストメッセージです"
    async with get_db_session() as session:
        session.add(
            ChatMessage(
                id="msg-session-title-long",
                session_id="session-title-17",
                user_id="user-session-test",
                username="tester",
                role="user",
                content=long_text,
                transport="test",
                created_at=datetime.now(tz=UTC),
            )
        )

    response = await client.get("/api/chat/sessions")
    assert response.status_code == 200
    target = next((s for s in response.json()["sessions"] if s["session_id"] == "session-title-17"), None)
    assert target is not None
    title = target["title"]
    assert title.endswith("…")
    assert len(title) <= 18
