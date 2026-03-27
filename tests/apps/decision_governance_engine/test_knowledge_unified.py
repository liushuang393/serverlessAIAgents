"""DGE 統合知識管理 API テスト.

- GET /api/knowledge/collections: CollectionManager 使用確認
- GET /api/config/rag: per-agent RAG 設定一覧
- PUT /api/config/rag/{agent_id}: per-agent RAG 設定更新
"""

from __future__ import annotations

from contextlib import asynccontextmanager
from typing import Any

import pytest
import pytest_asyncio
from httpx import ASGITransport, AsyncClient
from sqlalchemy import StaticPool
from sqlalchemy.ext.asyncio import async_sessionmaker, create_async_engine

from shared.rag.collection_manager import CollectionManager
from shared.rag.document_manager import DocumentManager
from shared.rag.models import Base as RAGBase


# ---------------------------------------------------------------------------
# テスト用 DB・マネージャー
# ---------------------------------------------------------------------------


@pytest_asyncio.fixture
async def async_engine():
    """SQLite in-memory エンジン."""
    engine = create_async_engine(
        "sqlite+aiosqlite://",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
    )
    async with engine.begin() as conn:
        await conn.run_sync(RAGBase.metadata.create_all)
    yield engine
    await engine.dispose()


@pytest_asyncio.fixture
async def managers(async_engine):
    """CollectionManager / DocumentManager."""
    sf = async_sessionmaker(async_engine, expire_on_commit=False)

    @asynccontextmanager
    async def _factory():
        async with sf() as s:
            yield s

    col_mgr = CollectionManager(session_factory=_factory)
    doc_mgr = DocumentManager(collection_manager=col_mgr, session_factory=_factory)
    return col_mgr, doc_mgr


@pytest_asyncio.fixture
async def collections_client(managers):
    """knowledge_collections ルーター用クライアント（認証バイパス）."""
    col_mgr, doc_mgr = managers
    from apps.decision_governance_engine.routers.auth import UserInfo
    from apps.decision_governance_engine.routers.knowledge_collections import (
        init_managers,
        require_auth,
        router,
    )
    from fastapi import FastAPI

    init_managers(col_mgr, doc_mgr)
    app = FastAPI()
    app.include_router(router)

    mock_user = UserInfo(
        user_id="admin",
        username="admin",
        display_name="Admin",
        department="IT",
        position="Admin",
    )
    app.dependency_overrides[require_auth] = lambda: mock_user

    async with AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as ac:
        yield ac


@pytest_asyncio.fixture
async def config_client(monkeypatch: Any):
    """config ルーター用クライアント（_rag_configs をリセット）."""
    import apps.decision_governance_engine.routers.config as config_mod
    from apps.decision_governance_engine.routers.config import router
    from fastapi import FastAPI

    # テスト間でグローバル状態をリセット
    monkeypatch.setattr(config_mod, "_rag_configs", {})

    app = FastAPI()
    app.include_router(router)

    async with AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as ac:
        yield ac


# ---------------------------------------------------------------------------
# テスト
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_knowledge_api_uses_bizcore_collection_manager(
    collections_client: AsyncClient,
) -> None:
    """GET /api/knowledge/collections が CollectionManager を使って空リストを返すこと."""
    resp = await collections_client.get("/api/knowledge/collections")
    assert resp.status_code == 200
    data = resp.json()
    assert "collections" in data


@pytest.mark.asyncio
async def test_per_agent_rag_config_api(config_client: AsyncClient) -> None:
    """GET /api/config/rag が shu/qi を含む per-agent RAG 設定一覧を返すこと."""
    resp = await config_client.get("/api/config/rag")
    assert resp.status_code == 200
    data = resp.json()
    assert isinstance(data, list)
    agent_ids = [item["agent_id"] for item in data]
    assert "shu" in agent_ids
    assert "qi" in agent_ids


@pytest.mark.asyncio
async def test_per_agent_rag_config_update(config_client: AsyncClient) -> None:
    """PUT /api/config/rag/shu で top_k が変更されること."""
    resp = await config_client.put("/api/config/rag/shu", json={"top_k": 8})
    assert resp.status_code == 200
    data = resp.json()
    assert data["top_k"] == 8
