"""拡張 Agent ルーティングのテスト."""

from __future__ import annotations

from typing import Any
from unittest.mock import AsyncMock, MagicMock

import httpx
import pytest

from apps.faq_system.routers.agents import AgentType, _agent_cache
from apps.faq_system.main import app


@pytest.fixture
def mock_agents(monkeypatch: pytest.MonkeyPatch) -> dict[str, Any]:
    """各 Agent をモック化."""
    _agent_cache.clear()
    
    mock_internal = AsyncMock()
    mock_internal.run.return_value = {"answer": "internal answer", "query_type": "internal_kb"}
    
    mock_external = AsyncMock()
    mock_external.run.return_value = {"answer": "external answer", "query_type": "external_kb"}
    
    mock_maintenance = AsyncMock()
    mock_maintenance.run.return_value = {"action": "full", "diffs": []}
    
    mock_analytics = AsyncMock()
    mock_analytics.run.return_value = {"answer": "analytics answer", "sql": "SELECT *"}
    
    mock_enhanced = AsyncMock()
    mock_enhanced.run.return_value = {"answer": "enhanced answer", "query_type": "faq"}

    # キャッシュにモックを注入
    _agent_cache[AgentType.INTERNAL_KB.value] = mock_internal
    _agent_cache[AgentType.EXTERNAL_KB.value] = mock_external
    _agent_cache[AgentType.MAINTENANCE.value] = mock_maintenance
    _agent_cache[AgentType.ANALYTICS.value] = mock_analytics
    _agent_cache[AgentType.ENHANCED_FAQ.value] = mock_enhanced

    return {
        "internal": mock_internal,
        "external": mock_external,
        "maintenance": mock_maintenance,
        "analytics": mock_analytics,
        "enhanced": mock_enhanced,
    }


@pytest.fixture
def auth_headers() -> dict[str, str]:
    """認証ヘッダー (test_auth.py と同様にモックユーザーを想定)."""
    # ここでは単純に認証ミドルウェアが通ることを前提とするか、
    # 認証をバイパスする設定が必要だが、test_auth.py の仕組みを再利用するのは複雑。
    # 代わりに Depends(require_auth) をオーバーライドするのが一般的。
    return {"Authorization": "Bearer mock-token"}


@pytest.fixture
async def client(mock_auth: None) -> Any:
    """ASGI クライアント."""
    transport = httpx.ASGITransport(app=app)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as c:
        yield c


# 認証のモック
from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.auth.models import UserInfo

async def mock_require_auth() -> UserInfo:
    return UserInfo(
        user_id="user-1",
        username="testuser",
        display_name="Test User",
        role="admin",
        department="IT",
        position="Developer",
    )

@pytest.fixture
def mock_auth(monkeypatch: pytest.MonkeyPatch) -> None:
    app.dependency_overrides[require_auth] = mock_require_auth
    yield
    app.dependency_overrides.pop(require_auth, None)


@pytest.mark.asyncio
async def test_internal_kb_routing(client: httpx.AsyncClient, mock_agents: dict[str, Any]) -> None:
    response = await client.post(
        "/api/agents/internal-kb/query",
        json={"question": "社内規定について"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["answer"] == "internal answer"
    
    # 呼び出し確認
    mock_agents["internal"].run.assert_called_once()
    call_args = mock_agents["internal"].run.call_args[0][0]
    assert call_args["question"] == "社内規定について"
    assert call_args["context"]["user"]["username"] == "testuser"


@pytest.mark.asyncio
async def test_external_kb_routing(client: httpx.AsyncClient, mock_agents: dict[str, Any]) -> None:
    response = await client.post(
        "/api/agents/external-kb/query",
        json={"question": "製品仕様について"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["answer"] == "external answer"
    mock_agents["external"].run.assert_called_once()


@pytest.mark.asyncio
async def test_maintenance_routing(client: httpx.AsyncClient, mock_agents: dict[str, Any]) -> None:
    response = await client.post(
        "/api/agents/maintenance/analyze",
        json={
            "action": "diff",
            "old_doc": "ver1",
            "new_doc": "ver2"
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["action"] == "full"  # モックの返り値
    
    mock_agents["maintenance"].run.assert_called_once()
    call_args = mock_agents["maintenance"].run.call_args[0][0]
    assert call_args["action"] == "diff"
    assert call_args["old_doc"] == "ver1"


@pytest.mark.asyncio
async def test_analytics_routing(client: httpx.AsyncClient, mock_agents: dict[str, Any]) -> None:
    response = await client.post(
        "/api/agents/analytics/query",
        json={"question": "先月の売上"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["sql"] == "SELECT *"
    
    mock_agents["analytics"].run.assert_called_once()


@pytest.mark.asyncio
async def test_enhanced_faq_routing(client: httpx.AsyncClient, mock_agents: dict[str, Any]) -> None:
    response = await client.post(
        "/api/agents/enhanced-faq/query",
        json={"question": "複合的な質問"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["answer"] == "enhanced answer"
    
    mock_agents["enhanced"].run.assert_called_once()


@pytest.mark.asyncio
async def test_agent_error_handling(client: httpx.AsyncClient, mock_agents: dict[str, Any]) -> None:
    # エラーを発生させる
    mock_agents["internal"].run.side_effect = Exception("Agent internal error")
    
    response = await client.post(
        "/api/agents/internal-kb/query",
        json={"question": "Error trigger"},
    )
    assert response.status_code == 500
    data = response.json()
    assert "Agent internal error" in data["detail"]
