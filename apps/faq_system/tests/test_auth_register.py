import pytest
from httpx import AsyncClient
from apps.faq_system.backend.auth.service import get_auth_service, AuthProvider

@pytest.mark.asyncio
async def test_register_user_success(client: AsyncClient):
    """ユーザー登録成功シナリオ."""
    # Ensure local db mode
    service = get_auth_service()
    if service._active_provider() != AuthProvider.LOCAL_DB:
        pytest.skip("Skipping registration test in non-local-db mode")

    payload = {
        "username": "newuser1",
        "password": "password123",  # 8 chars+
        "display_name": "New User",
        "department": "Sales",
        "position": "Intern",
        "email": "newuser1@example.com"
    }

    response = await client.post("/api/auth/register", json=payload)
    assert response.status_code == 201
    data = response.json()
    assert data["success"] is True
    assert data["user"]["username"] == "newuser1"
    assert data["access_token"] is not None
    
    # Verify login works with new user
    login_resp = await client.post("/api/auth/login", json={
        "username": "newuser1", 
        "password": "password123"
    })
    assert login_resp.status_code == 200
    assert login_resp.json()["success"] is True

@pytest.mark.asyncio
async def test_register_user_duplicate(client: AsyncClient):
    """重複ユーザー登録失敗シナリオ."""
    service = get_auth_service()
    if service._active_provider() != AuthProvider.LOCAL_DB:
        pytest.skip("Skipping registration test")

    payload = {
        "username": "dupuser",
        "password": "password123",
        "display_name": "Dup User"
    }
    await client.post("/api/auth/register", json=payload)

    # Retry same username
    response = await client.post("/api/auth/register", json=payload)
    data = response.json()
    assert data["success"] is False
    assert "既に使用されています" in data["message"]

@pytest.mark.asyncio
async def test_register_weak_password(client: AsyncClient):
    """弱いパスワードでの登録失敗."""
    payload = {
        "username": "weakpwuser",
        "password": "123", 
        "display_name": "Weak Pw"
    }
    response = await client.post("/api/auth/register", json=payload)
    data = response.json()
    assert data["success"] is False
