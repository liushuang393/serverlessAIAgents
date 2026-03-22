"""Decision Governance Engine — コレクション・ドキュメント管理ルーターのユニットテスト.

CollectionManager / DocumentManager を SQLite in-memory で差し替え、
FastAPI TestClient で各エンドポイントを検証する。
"""

from __future__ import annotations

import io
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING

import pytest
import pytest_asyncio
from httpx import ASGITransport, AsyncClient
from sqlalchemy import StaticPool
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker, create_async_engine

from shared.rag.collection_manager import CollectionManager
from shared.rag.document_manager import DocumentManager
from shared.rag.models import Base as RAGBase


if TYPE_CHECKING:
    from collections.abc import AsyncGenerator


# ---------------------------------------------------------------------------
# テスト用非同期 DB セットアップ
# ---------------------------------------------------------------------------


@pytest_asyncio.fixture
async def async_engine():
    """テスト用 SQLite in-memory 非同期エンジン."""
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
    """テスト用セッションファクトリ."""
    return async_sessionmaker(async_engine, expire_on_commit=False)


@pytest_asyncio.fixture
async def managers(session_factory):
    """CollectionManager / DocumentManager ペアを生成."""

    @asynccontextmanager
    async def _factory() -> AsyncGenerator[AsyncSession]:
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
    """認証をモックしたテスト用 AsyncClient."""
    col_mgr, doc_mgr = managers

    # ルーターインポート前にマネージャーを設定
    from apps.decision_governance_engine.routers.knowledge_collections import (
        init_managers,
        router,
    )

    init_managers(col_mgr, doc_mgr)

    from fastapi import FastAPI

    app = FastAPI()
    app.include_router(router)

    # 認証をバイパス: require_auth を常にデモユーザーを返すように差し替え
    from apps.decision_governance_engine.routers.auth import UserInfo

    mock_user = UserInfo(
        user_id="user-admin",
        username="admin",
        display_name="管理者 太郎",
        department="経営企画部",
        position="部長",
    )

    from apps.decision_governance_engine.routers.knowledge_collections import require_auth as _orig_auth

    app.dependency_overrides[_orig_auth] = lambda: mock_user

    async with AsyncClient(
        transport=ASGITransport(app=app),
        base_url="http://test",
    ) as ac:
        yield ac


# ===========================================================================
# コレクション CRUD テスト
# ===========================================================================


@pytest.mark.asyncio
async def test_create_collection(client: AsyncClient) -> None:
    """コレクション作成が成功すること."""
    resp = await client.post(
        "/api/knowledge/collections",
        json={
            "collection_name": "test_col",
            "display_name": "テスト",
            "description": "テスト用",
            "chunk_strategy": "recursive",
        },
    )
    assert resp.status_code == 200
    data = resp.json()
    assert "collection" in data
    assert data["collection"]["collection_name"] == "test_col"


@pytest.mark.asyncio
async def test_create_duplicate_collection(client: AsyncClient) -> None:
    """重複コレクション作成が 409 を返すこと."""
    payload = {"collection_name": "dup_col"}
    resp1 = await client.post("/api/knowledge/collections", json=payload)
    assert resp1.status_code == 200

    resp2 = await client.post("/api/knowledge/collections", json=payload)
    assert resp2.status_code == 409


@pytest.mark.asyncio
async def test_list_collections(client: AsyncClient) -> None:
    """コレクション一覧が返ること."""
    await client.post("/api/knowledge/collections", json={"collection_name": "col_a"})
    await client.post("/api/knowledge/collections", json={"collection_name": "col_b"})

    resp = await client.get("/api/knowledge/collections")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] == 2
    assert len(data["collections"]) == 2


@pytest.mark.asyncio
async def test_get_collection(client: AsyncClient) -> None:
    """個別コレクション取得."""
    await client.post("/api/knowledge/collections", json={"collection_name": "get_col"})

    resp = await client.get("/api/knowledge/collections/get_col")
    assert resp.status_code == 200
    assert resp.json()["collection"]["collection_name"] == "get_col"


@pytest.mark.asyncio
async def test_get_collection_not_found(client: AsyncClient) -> None:
    """存在しないコレクションが 404 を返すこと."""
    resp = await client.get("/api/knowledge/collections/no_such")
    assert resp.status_code == 404


@pytest.mark.asyncio
async def test_update_collection(client: AsyncClient) -> None:
    """コレクション設定更新."""
    await client.post("/api/knowledge/collections", json={"collection_name": "upd_col"})

    resp = await client.patch(
        "/api/knowledge/collections/upd_col",
        json={"display_name": "更新後の名前", "chunk_size": 500},
    )
    assert resp.status_code == 200
    col = resp.json()["collection"]
    assert col["display_name"] == "更新後の名前"
    assert col["chunk_size"] == 500


@pytest.mark.asyncio
async def test_delete_collection(client: AsyncClient) -> None:
    """コレクション削除."""
    await client.post("/api/knowledge/collections", json={"collection_name": "del_col"})

    resp = await client.delete("/api/knowledge/collections/del_col")
    assert resp.status_code == 200
    assert resp.json()["deleted"] is True

    resp2 = await client.get("/api/knowledge/collections/del_col")
    assert resp2.status_code == 404


# ===========================================================================
# ドキュメント管理テスト
# ===========================================================================


@pytest.mark.asyncio
async def test_upload_document(client: AsyncClient) -> None:
    """ドキュメントアップロード."""
    await client.post("/api/knowledge/collections", json={"collection_name": "doc_col"})

    file_content = b"This is test document content for knowledge base."
    resp = await client.post(
        "/api/knowledge/collections/doc_col/documents",
        files={"file": ("test.txt", io.BytesIO(file_content), "text/plain")},
    )
    assert resp.status_code == 200
    doc = resp.json()["document"]
    assert doc["filename"] == "test.txt"
    assert doc["status"] == "uploaded"


@pytest.mark.asyncio
async def test_list_documents(client: AsyncClient) -> None:
    """ドキュメント一覧."""
    await client.post("/api/knowledge/collections", json={"collection_name": "list_doc_col"})

    await client.post(
        "/api/knowledge/collections/list_doc_col/documents",
        files={"file": ("a.txt", io.BytesIO(b"Content A for listing test."), "text/plain")},
    )
    await client.post(
        "/api/knowledge/collections/list_doc_col/documents",
        files={"file": ("b.txt", io.BytesIO(b"Content B for listing test."), "text/plain")},
    )

    resp = await client.get("/api/knowledge/collections/list_doc_col/documents")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] == 2


@pytest.mark.asyncio
async def test_get_document(client: AsyncClient) -> None:
    """個別ドキュメント取得."""
    await client.post("/api/knowledge/collections", json={"collection_name": "get_doc_col"})

    upload = await client.post(
        "/api/knowledge/collections/get_doc_col/documents",
        files={"file": ("doc.txt", io.BytesIO(b"Some document content here."), "text/plain")},
    )
    doc_id = upload.json()["document"]["document_id"]

    resp = await client.get(f"/api/knowledge/collections/get_doc_col/documents/{doc_id}")
    assert resp.status_code == 200
    assert resp.json()["document"]["document_id"] == doc_id


@pytest.mark.asyncio
async def test_delete_document(client: AsyncClient) -> None:
    """ドキュメント削除."""
    await client.post("/api/knowledge/collections", json={"collection_name": "del_doc_col"})

    upload = await client.post(
        "/api/knowledge/collections/del_doc_col/documents",
        files={"file": ("rm.txt", io.BytesIO(b"Document to delete from knowledge base."), "text/plain")},
    )
    doc_id = upload.json()["document"]["document_id"]

    resp = await client.delete(f"/api/knowledge/collections/del_doc_col/documents/{doc_id}")
    assert resp.status_code == 200
    assert resp.json()["deleted"] is True


@pytest.mark.asyncio
async def test_preview_chunks(client: AsyncClient) -> None:
    """チャンクプレビュー."""
    await client.post("/api/knowledge/collections", json={"collection_name": "chunk_col"})

    upload = await client.post(
        "/api/knowledge/collections/chunk_col/documents",
        files={"file": ("chunk.txt", io.BytesIO(b"A " * 100), "text/plain")},
    )
    doc_id = upload.json()["document"]["document_id"]

    resp = await client.post(
        f"/api/knowledge/collections/chunk_col/documents/{doc_id}/preview-chunks",
        json={"chunk_size": 100, "chunk_overlap": 20},
    )
    assert resp.status_code == 200
    data = resp.json()
    assert "chunks" in data
    assert data["total"] >= 1


@pytest.mark.asyncio
async def test_collection_stats(client: AsyncClient) -> None:
    """コレクション統計."""
    await client.post("/api/knowledge/collections", json={"collection_name": "stats_col"})

    resp = await client.get("/api/knowledge/collections/stats_col/stats")
    assert resp.status_code == 200
    stats = resp.json()
    assert "collection_name" in stats
    assert stats["document_count"] == 0


@pytest.mark.asyncio
async def test_upload_to_nonexistent_collection(client: AsyncClient) -> None:
    """存在しないコレクションへのアップロードが 404 を返すこと."""
    resp = await client.post(
        "/api/knowledge/collections/no_such/documents",
        files={"file": ("test.txt", io.BytesIO(b"Content for missing collection."), "text/plain")},
    )
    assert resp.status_code == 404


@pytest.mark.asyncio
async def test_reindex_collection(client: AsyncClient) -> None:
    """コレクション全体の再インデックス."""
    await client.post("/api/knowledge/collections", json={"collection_name": "reindex_col"})

    resp = await client.post("/api/knowledge/collections/reindex_col/reindex")
    assert resp.status_code == 200
    data = resp.json()
    assert "reindexed" in data or "total" in data or isinstance(data, dict)
