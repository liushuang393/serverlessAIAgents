"""RAG 管理統合テスト.

フレームワーク層の CollectionManager / DocumentManager と
各アプリのルーターが正しく連携することを検証する。

テスト対象:
- CollectionManager / DocumentManager のライフサイクル
- フォールバック RBAC によるアクセス制御
- 複数アプリ間のコレクション分離
- ドキュメントアップロード → チャンクプレビュー → 削除のフロー
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

from agentflow.knowledge.collection_manager import CollectionManager
from agentflow.knowledge.document_manager import DocumentManager
from agentflow.knowledge.models import Base as RAGBase
from agentflow.knowledge.rag_access_control import RAGAccessControl
from agentflow.knowledge.scope_resolver import FALLBACK_ROLE_KB_MAP, ScopeResolver


# ---------------------------------------------------------------------------
# 共通フィクスチャ
# ---------------------------------------------------------------------------


@pytest_asyncio.fixture
async def async_engine():
    """テスト用 SQLite in-memory エンジン."""
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


# ===========================================================================
# 1. フレームワーク統合: マルチアプリ コレクション分離
# ===========================================================================


@pytest.mark.asyncio
async def test_multi_app_collection_isolation(managers) -> None:
    """異なるアプリのコレクションが分離されること."""
    col_mgr, _ = managers

    await col_mgr.create_collection(
        collection_name="faq_internal",
        app_name="faq_system",
    )
    await col_mgr.create_collection(
        collection_name="dge_shu_kb",
        app_name="decision_governance_engine",
    )
    await col_mgr.create_collection(
        collection_name="cma_patterns",
        app_name="code_migration_assistant",
    )

    faq_cols = await col_mgr.list_collections(app_name="faq_system")
    dge_cols = await col_mgr.list_collections(app_name="decision_governance_engine")
    cma_cols = await col_mgr.list_collections(app_name="code_migration_assistant")

    assert len(faq_cols) == 1
    assert faq_cols[0].collection_name == "faq_internal"
    assert len(dge_cols) == 1
    assert dge_cols[0].collection_name == "dge_shu_kb"
    assert len(cma_cols) == 1
    assert cma_cols[0].collection_name == "cma_patterns"


# ===========================================================================
# 2. フレームワーク統合: RBAC フォールバック
# ===========================================================================


@pytest.mark.asyncio
async def test_rbac_fallback_filters_collections(managers) -> None:
    """フォールバック RBAC でロールに応じてコレクションがフィルタリングされること."""
    col_mgr, _ = managers

    # 3 種類の KB タイプを含むコレクションを作成
    await col_mgr.create_collection(collection_name="internal_kb", app_name="faq_system")
    await col_mgr.create_collection(collection_name="external_kb", app_name="faq_system")
    await col_mgr.create_collection(collection_name="confidential_kb", app_name="faq_system")

    # admin: 全てアクセス可能
    admin_cols = await col_mgr.resolve_accessible_collections(role="admin", app_name="faq_system")
    assert len(admin_cols) == 3

    # employee: internal + external
    emp_cols = await col_mgr.resolve_accessible_collections(role="employee", app_name="faq_system")
    assert len(emp_cols) == 2
    emp_names = {c.collection_name for c in emp_cols}
    assert "internal_kb" in emp_names
    assert "external_kb" in emp_names
    assert "confidential_kb" not in emp_names

    # guest: external のみ
    guest_cols = await col_mgr.resolve_accessible_collections(role="guest", app_name="faq_system")
    assert len(guest_cols) == 1
    assert guest_cols[0].collection_name == "external_kb"


# ===========================================================================
# 3. ドキュメントライフサイクル統合テスト
# ===========================================================================


@pytest.mark.asyncio
async def test_document_lifecycle(managers) -> None:
    """ドキュメントのアップロード → チャンクプレビュー → 削除フロー."""
    col_mgr, doc_mgr = managers

    # コレクション作成
    await col_mgr.create_collection(
        collection_name="lifecycle_test",
        app_name="test_app",
        chunk_size=200,
        chunk_overlap=20,
    )

    # ドキュメントアップロード
    content = b"This is a test document. " * 50  # 十分な長さ
    record = await doc_mgr.upload_document(
        collection_name="lifecycle_test",
        file_content=content,
        filename="test.txt",
        user_id="test-user",
    )
    assert record.filename == "test.txt"
    assert record.status == "uploaded"

    # チャンクプレビュー
    chunks = await doc_mgr.preview_chunks(
        document_id=record.document_id,
        chunk_size=200,
        chunk_overlap=20,
    )
    assert len(chunks) >= 1

    # ドキュメント一覧確認
    docs = await doc_mgr.list_documents(collection_name="lifecycle_test")
    assert len(docs) == 1

    # ドキュメント削除
    await doc_mgr.delete_document(record.document_id)
    docs_after = await doc_mgr.list_documents(collection_name="lifecycle_test")
    assert len(docs_after) == 0


# ===========================================================================
# 4. RAGAccessControl 統合テスト
# ===========================================================================


class TestRAGAccessControlIntegration:
    """RAGAccessControl と CollectionManager の連携テスト."""

    @pytest.mark.asyncio
    async def test_filter_accessible_collections(self, managers) -> None:
        """CollectionManager 経由でアクセス可能コレクションをフィルタリング."""
        col_mgr, _ = managers

        await col_mgr.create_collection(collection_name="internal_data", app_name="test")
        await col_mgr.create_collection(collection_name="external_data", app_name="test")
        await col_mgr.create_collection(collection_name="confidential_data", app_name="test")

        admin_cols = await RAGAccessControl.filter_accessible_collections(
            collection_manager=col_mgr,
            role="admin",
            app_name="test",
        )
        assert len(admin_cols) == 3

        guest_cols = await RAGAccessControl.filter_accessible_collections(
            collection_manager=col_mgr,
            role="guest",
            app_name="test",
        )
        assert len(guest_cols) == 1
        assert guest_cols[0].collection_name == "external_data"


# ===========================================================================
# 5. ScopeResolver フォールバックテスト
# ===========================================================================


class TestScopeResolverFallback:
    """ScopeResolver のフォールバック RBAC テスト."""

    def test_all_roles_have_external_access(self) -> None:
        """全ロールが external にアクセス可能."""
        for role in FALLBACK_ROLE_KB_MAP:
            assert ScopeResolver.check_kb_type_access(role, "external")

    def test_only_admin_has_confidential_access(self) -> None:
        """admin のみ confidential にアクセス可能."""
        for role in FALLBACK_ROLE_KB_MAP:
            if role == "admin":
                assert ScopeResolver.check_kb_type_access(role, "confidential")
            else:
                assert not ScopeResolver.check_kb_type_access(role, "confidential")


# ===========================================================================
# 6. FAQ System ルーター統合テスト
# ===========================================================================


@pytest_asyncio.fixture
async def faq_client(managers):
    """FAQ collections ルーター用テストクライアント."""
    col_mgr, doc_mgr = managers

    from apps.faq_system.routers.collections import init_managers, router

    init_managers(col_mgr, doc_mgr)

    from apps.faq_system.backend.auth.dependencies import require_auth as faq_require_auth
    from apps.faq_system.backend.auth.models import UserInfo as FAQUserInfo
    from fastapi import FastAPI

    app = FastAPI()
    app.include_router(router)

    mock_user = FAQUserInfo(
        user_id="user-test",
        username="test_admin",
        display_name="テスト管理者",
        role="admin",
    )
    app.dependency_overrides[faq_require_auth] = lambda: mock_user

    async with AsyncClient(
        transport=ASGITransport(app=app),
        base_url="http://test",
    ) as ac:
        yield ac


@pytest.mark.asyncio
async def test_faq_collection_crud_flow(faq_client: AsyncClient) -> None:
    """FAQ コレクション CRUD フロー統合テスト."""
    # 作成
    resp = await faq_client.post(
        "/api/collections",
        json={"collection_name": "integration_test", "display_name": "統合テスト"},
    )
    assert resp.status_code == 200

    # 一覧
    resp = await faq_client.get("/api/collections")
    assert resp.status_code == 200
    assert resp.json()["total"] >= 1

    # 更新
    resp = await faq_client.patch(
        "/api/collections/integration_test",
        json={"display_name": "更新済み"},
    )
    assert resp.status_code == 200
    assert resp.json()["collection"]["display_name"] == "更新済み"

    # 統計
    resp = await faq_client.get("/api/collections/integration_test/stats")
    assert resp.status_code == 200
    assert resp.json()["document_count"] == 0

    # 削除
    resp = await faq_client.delete("/api/collections/integration_test")
    assert resp.status_code == 200


@pytest.mark.asyncio
async def test_faq_document_upload_and_preview(faq_client: AsyncClient) -> None:
    """FAQ ドキュメントアップロード → プレビューフロー."""
    await faq_client.post(
        "/api/collections",
        json={"collection_name": "doc_flow_test"},
    )

    # アップロード
    resp = await faq_client.post(
        "/api/collections/doc_flow_test/documents",
        files={"file": ("guide.txt", io.BytesIO(b"Migration guide content. " * 30), "text/plain")},
    )
    assert resp.status_code == 200
    doc_id = resp.json()["document"]["document_id"]

    # チャンクプレビュー
    resp = await faq_client.post(
        f"/api/collections/doc_flow_test/documents/{doc_id}/preview-chunks",
        json={"chunk_size": 100, "chunk_overlap": 10},
    )
    assert resp.status_code == 200
    assert resp.json()["total"] >= 1

    # 削除
    resp = await faq_client.delete(f"/api/collections/doc_flow_test/documents/{doc_id}")
    assert resp.status_code == 200


# ===========================================================================
# 7. DGE ルーター統合テスト
# ===========================================================================


@pytest_asyncio.fixture
async def dge_client(managers):
    """DGE collections ルーター用テストクライアント."""
    col_mgr, doc_mgr = managers

    from apps.decision_governance_engine.routers.knowledge_collections import (
        init_managers,
        router,
    )

    init_managers(col_mgr, doc_mgr)

    from apps.decision_governance_engine.routers.auth import UserInfo
    from apps.decision_governance_engine.routers.knowledge_collections import require_auth as _auth
    from fastapi import FastAPI

    app = FastAPI()
    app.include_router(router)

    mock_user = UserInfo(
        user_id="user-admin",
        username="admin",
        display_name="テスト管理者",
    )
    app.dependency_overrides[_auth] = lambda: mock_user

    async with AsyncClient(
        transport=ASGITransport(app=app),
        base_url="http://test",
    ) as ac:
        yield ac


@pytest.mark.asyncio
async def test_dge_collection_crud_flow(dge_client: AsyncClient) -> None:
    """DGE コレクション CRUD フロー統合テスト."""
    resp = await dge_client.post(
        "/api/knowledge/collections",
        json={"collection_name": "shu_knowledge", "display_name": "術 KB"},
    )
    assert resp.status_code == 200

    resp = await dge_client.get("/api/knowledge/collections")
    assert resp.status_code == 200
    assert resp.json()["total"] >= 1

    resp = await dge_client.delete("/api/knowledge/collections/shu_knowledge")
    assert resp.status_code == 200


# ===========================================================================
# 8. CMA ルーター統合テスト
# ===========================================================================


@pytest_asyncio.fixture
async def cma_client(managers):
    """CMA knowledge ルーター用テストクライアント."""
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


@pytest.mark.asyncio
async def test_cma_collection_crud_flow(cma_client: AsyncClient) -> None:
    """CMA コレクション CRUD フロー統合テスト."""
    resp = await cma_client.post(
        "/api/knowledge/collections",
        json={"collection_name": "migration_rules", "display_name": "移行ルール"},
    )
    assert resp.status_code == 200

    resp = await cma_client.get("/api/knowledge/collections")
    assert resp.status_code == 200
    assert resp.json()["total"] >= 1

    resp = await cma_client.delete("/api/knowledge/collections/migration_rules")
    assert resp.status_code == 200
