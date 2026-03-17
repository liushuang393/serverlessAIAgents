"""Code Migration Assistant — 知識ベース管理ルーターのユニットテスト.

CollectionManager / DocumentManager を SQLite in-memory で差し替え、
FastAPI TestClient で各エンドポイントを検証する。
"""

from __future__ import annotations

import io
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager

import pytest
import pytest_asyncio
from httpx import ASGITransport, AsyncClient
from sqlalchemy import StaticPool
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker, create_async_engine

from shared.rag.collection_manager import CollectionManager
from shared.rag.document_manager import DocumentManager
from shared.rag.models import Base as RAGBase


# ---------------------------------------------------------------------------
# テスト用非同期 DB
# ---------------------------------------------------------------------------


@pytest_asyncio.fixture
async def async_engine():
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
async def session_factory(async_engine) -> async_sessionmaker[AsyncSession]:
    return async_sessionmaker(async_engine, expire_on_commit=False)


@pytest_asyncio.fixture
async def managers(session_factory):
    @asynccontextmanager
    async def _factory() -> AsyncGenerator[AsyncSession, None]:
        async with session_factory() as session:
            yield session

    col_mgr = CollectionManager(session_factory=_factory)
    doc_mgr = DocumentManager(collection_manager=col_mgr, session_factory=_factory)
    return col_mgr, doc_mgr


# ---------------------------------------------------------------------------
# テスト用 FastAPI アプリ
# ---------------------------------------------------------------------------


@pytest_asyncio.fixture
async def client(managers):
    col_mgr, doc_mgr = managers

    from apps.code_migration_assistant.backend.knowledge_router import init_managers, router

    init_managers(col_mgr, doc_mgr)

    from fastapi import FastAPI

    app = FastAPI()
    app.include_router(router)

    async with AsyncClient(
        transport=ASGITransport(app=app),
        base_url="http://test",
    ) as ac:
        yield ac


# ===========================================================================
# コレクション CRUD
# ===========================================================================


@pytest.mark.asyncio
async def test_create_collection(client: AsyncClient) -> None:
    resp = await client.post(
        "/api/knowledge/collections",
        json={"collection_name": "migration_patterns", "display_name": "移行パターン"},
    )
    assert resp.status_code == 200
    assert resp.json()["collection"]["collection_name"] == "migration_patterns"


@pytest.mark.asyncio
async def test_create_duplicate(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "dup"})
    resp = await client.post("/api/knowledge/collections", json={"collection_name": "dup"})
    assert resp.status_code == 409


@pytest.mark.asyncio
async def test_list_collections(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "col1"})
    await client.post("/api/knowledge/collections", json={"collection_name": "col2"})
    resp = await client.get("/api/knowledge/collections")
    assert resp.status_code == 200
    assert resp.json()["total"] == 2


@pytest.mark.asyncio
async def test_get_collection(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "get_me"})
    resp = await client.get("/api/knowledge/collections/get_me")
    assert resp.status_code == 200
    assert resp.json()["collection"]["collection_name"] == "get_me"


@pytest.mark.asyncio
async def test_get_not_found(client: AsyncClient) -> None:
    resp = await client.get("/api/knowledge/collections/nope")
    assert resp.status_code == 404


@pytest.mark.asyncio
async def test_update_collection(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "upd"})
    resp = await client.patch("/api/knowledge/collections/upd", json={"display_name": "Updated"})
    assert resp.status_code == 200
    assert resp.json()["collection"]["display_name"] == "Updated"


@pytest.mark.asyncio
async def test_delete_collection(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "del_me"})
    resp = await client.delete("/api/knowledge/collections/del_me")
    assert resp.status_code == 200
    assert resp.json()["deleted"] is True
    resp2 = await client.get("/api/knowledge/collections/del_me")
    assert resp2.status_code == 404


@pytest.mark.asyncio
async def test_collection_stats(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "stat_col"})
    resp = await client.get("/api/knowledge/collections/stat_col/stats")
    assert resp.status_code == 200
    assert resp.json()["document_count"] == 0


# ===========================================================================
# ドキュメント管理
# ===========================================================================


@pytest.mark.asyncio
async def test_upload_document(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "doc_col"})
    resp = await client.post(
        "/api/knowledge/collections/doc_col/documents",
        files={"file": ("rules.txt", io.BytesIO(b"COBOL to Java migration rule set content."), "text/plain")},
    )
    assert resp.status_code == 200
    assert resp.json()["document"]["filename"] == "rules.txt"


@pytest.mark.asyncio
async def test_list_documents(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "list_col"})
    await client.post(
        "/api/knowledge/collections/list_col/documents",
        files={"file": ("a.txt", io.BytesIO(b"First migration doc content here."), "text/plain")},
    )
    await client.post(
        "/api/knowledge/collections/list_col/documents",
        files={"file": ("b.txt", io.BytesIO(b"Second migration doc content here."), "text/plain")},
    )
    resp = await client.get("/api/knowledge/collections/list_col/documents")
    assert resp.status_code == 200
    assert resp.json()["total"] == 2


@pytest.mark.asyncio
async def test_delete_document(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "del_doc_col"})
    upload = await client.post(
        "/api/knowledge/collections/del_doc_col/documents",
        files={"file": ("rm.txt", io.BytesIO(b"Document to remove from knowledge base."), "text/plain")},
    )
    doc_id = upload.json()["document"]["document_id"]
    resp = await client.delete(f"/api/knowledge/collections/del_doc_col/documents/{doc_id}")
    assert resp.status_code == 200
    assert resp.json()["deleted"] is True


@pytest.mark.asyncio
async def test_preview_chunks(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "chunk_col"})
    upload = await client.post(
        "/api/knowledge/collections/chunk_col/documents",
        files={"file": ("big.txt", io.BytesIO(b"word " * 200), "text/plain")},
    )
    doc_id = upload.json()["document"]["document_id"]
    resp = await client.post(
        f"/api/knowledge/collections/chunk_col/documents/{doc_id}/preview-chunks",
        json={"chunk_size": 100, "chunk_overlap": 10},
    )
    assert resp.status_code == 200
    assert resp.json()["total"] >= 1


@pytest.mark.asyncio
async def test_upload_to_missing_collection(client: AsyncClient) -> None:
    resp = await client.post(
        "/api/knowledge/collections/missing/documents",
        files={"file": ("x.txt", io.BytesIO(b"Orphan document content for test."), "text/plain")},
    )
    assert resp.status_code == 404


@pytest.mark.asyncio
async def test_reindex_collection(client: AsyncClient) -> None:
    await client.post("/api/knowledge/collections", json={"collection_name": "reindex_col"})
    resp = await client.post("/api/knowledge/collections/reindex_col/reindex")
    assert resp.status_code == 200
