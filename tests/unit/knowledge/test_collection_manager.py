"""CollectionManager と DocumentManager の単体テスト.

コレクションライフサイクル管理・ドキュメント管理・
アクセス制御統合を検証する。
"""

from __future__ import annotations

from typing import Any

import pytest
from sqlalchemy import StaticPool, event
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker, create_async_engine

from shared.rag.collection_manager import CollectionManager
from shared.rag.document_manager import DocumentManager
from shared.rag.models import (
    Base,
    DocumentStatus,
)
from shared.services.rag_service import RerankerType


# ---------------------------------------------------------------------------
# フィクスチャ
# ---------------------------------------------------------------------------


@pytest.fixture
async def engine() -> Any:
    """テスト用 async SQLite エンジン."""
    eng = create_async_engine(
        "sqlite+aiosqlite://",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
    )

    # WAL モード無効化（インメモリ SQLite 用）
    @event.listens_for(eng.sync_engine, "connect")
    def _set_sqlite_pragma(dbapi_conn: Any, _rec: Any) -> None:
        cursor = dbapi_conn.cursor()
        cursor.execute("PRAGMA journal_mode=WAL")
        cursor.close()

    async with eng.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)

    yield eng

    async with eng.begin() as conn:
        await conn.run_sync(Base.metadata.drop_all)
    await eng.dispose()


@pytest.fixture
def session_factory(engine: Any) -> async_sessionmaker[AsyncSession]:
    """テスト用セッションファクトリ."""
    return async_sessionmaker(engine, expire_on_commit=False)


@pytest.fixture
def collection_mgr(session_factory: async_sessionmaker[AsyncSession]) -> CollectionManager:
    """CollectionManager インスタンス."""
    return CollectionManager(session_factory=session_factory)


@pytest.fixture
def document_mgr(
    collection_mgr: CollectionManager,
    session_factory: async_sessionmaker[AsyncSession],
) -> DocumentManager:
    """DocumentManager インスタンス."""
    return DocumentManager(
        collection_manager=collection_mgr,
        session_factory=session_factory,
    )


# ---------------------------------------------------------------------------
# CollectionManager テスト
# ---------------------------------------------------------------------------


class TestCollectionManagerCRUD:
    """コレクション CRUD 操作のテスト."""

    async def test_create_collection(self, collection_mgr: CollectionManager) -> None:
        """コレクション作成の検証."""
        result = await collection_mgr.create_collection(
            collection_name="test_kb",
            app_name="faq_system",
            display_name="テスト知識ベース",
            description="テスト用コレクション",
            chunk_strategy="recursive",
            chunk_size=500,
            chunk_overlap=100,
            embedding_model="text-embedding-ada-002",
            retrieval_method="semantic",
            top_k=5,
            min_similarity=0.3,
            vector_db_type="qdrant",
        )
        assert result is not None
        assert result.collection_name == "test_kb"
        assert result.app_name == "faq_system"
        assert result.display_name == "テスト知識ベース"
        assert result.chunk_size == 500

    async def test_get_collection(self, collection_mgr: CollectionManager) -> None:
        """コレクション取得の検証."""
        await collection_mgr.create_collection(
            collection_name="get_test",
            app_name="faq_system",
            display_name="取得テスト",
        )
        result = await collection_mgr.get_collection("get_test")
        assert result is not None
        assert result.collection_name == "get_test"

    async def test_get_collection_not_found(self, collection_mgr: CollectionManager) -> None:
        """存在しないコレクション取得で None を返す."""
        result = await collection_mgr.get_collection("nonexistent")
        assert result is None

    async def test_list_collections(self, collection_mgr: CollectionManager) -> None:
        """コレクション一覧の検証."""
        await collection_mgr.create_collection(
            collection_name="list_a",
            app_name="faq_system",
        )
        await collection_mgr.create_collection(
            collection_name="list_b",
            app_name="faq_system",
        )
        await collection_mgr.create_collection(
            collection_name="list_c",
            app_name="decision_engine",
        )

        # アプリ名でフィルタ
        faq_collections = await collection_mgr.list_collections(app_name="faq_system")
        assert len(faq_collections) == 2

        # 全件取得
        all_collections = await collection_mgr.list_collections()
        assert len(all_collections) == 3

    async def test_list_collections_by_tenant(self, collection_mgr: CollectionManager) -> None:
        """テナントID でフィルタされるコレクション一覧の検証."""
        await collection_mgr.create_collection(
            collection_name="tenant_a_kb",
            app_name="faq_system",
            tenant_id="tenant_a",
        )
        await collection_mgr.create_collection(
            collection_name="tenant_b_kb",
            app_name="faq_system",
            tenant_id="tenant_b",
        )

        result = await collection_mgr.list_collections(tenant_id="tenant_a")
        assert len(result) == 1
        assert result[0].collection_name == "tenant_a_kb"

    async def test_update_collection(self, collection_mgr: CollectionManager) -> None:
        """コレクション更新の検証."""
        await collection_mgr.create_collection(
            collection_name="update_test",
            app_name="faq_system",
            chunk_size=500,
        )
        updated = await collection_mgr.update_collection(
            collection_name="update_test",
            updates={"chunk_size": 1000, "top_k": 10},
        )
        assert updated is not None
        assert updated.chunk_size == 1000
        assert updated.top_k == 10

    async def test_update_nonexistent_raises(self, collection_mgr: CollectionManager) -> None:
        """存在しないコレクション更新で ValueError を送出."""
        with pytest.raises(ValueError, match="not found"):
            await collection_mgr.update_collection(
                collection_name="nonexistent",
                updates={"chunk_size": 500},
            )

    async def test_delete_collection(self, collection_mgr: CollectionManager) -> None:
        """コレクション削除の検証."""
        await collection_mgr.create_collection(
            collection_name="delete_test",
            app_name="faq_system",
        )
        await collection_mgr.delete_collection("delete_test")
        result = await collection_mgr.get_collection("delete_test")
        assert result is None

    async def test_delete_nonexistent_raises(self, collection_mgr: CollectionManager) -> None:
        """存在しないコレクション削除で ValueError を送出."""
        with pytest.raises(ValueError, match="not found"):
            await collection_mgr.delete_collection("nonexistent")

    async def test_create_duplicate_raises(self, collection_mgr: CollectionManager) -> None:
        """重複コレクション作成で ValueError を送出."""
        await collection_mgr.create_collection(
            collection_name="dup_test",
            app_name="faq_system",
        )
        with pytest.raises(ValueError, match="already exists"):
            await collection_mgr.create_collection(
                collection_name="dup_test",
                app_name="faq_system",
            )


class TestCollectionManagerStats:
    """コレクション統計情報のテスト."""

    async def test_get_collection_stats(self, collection_mgr: CollectionManager) -> None:
        """コレクション統計情報の検証."""
        await collection_mgr.create_collection(
            collection_name="stats_test",
            app_name="faq_system",
        )
        stats = await collection_mgr.get_collection_stats("stats_test")
        assert stats["collection_name"] == "stats_test"
        assert stats["document_count"] == 0

    async def test_stats_nonexistent_raises(self, collection_mgr: CollectionManager) -> None:
        """存在しないコレクションの統計取得で ValueError を送出."""
        with pytest.raises(ValueError, match="not found"):
            await collection_mgr.get_collection_stats("nonexistent")


class TestCollectionManagerRAGService:
    """RAGService 構築テスト."""

    async def test_build_rag_service(self, collection_mgr: CollectionManager) -> None:
        """コレクション設定から RAGService を構築する検証."""
        await collection_mgr.create_collection(
            collection_name="rag_build_test",
            app_name="faq_system",
            chunk_strategy="semantic",
            chunk_size=800,
            chunk_overlap=150,
            top_k=3,
            min_similarity=0.5,
        )
        rag_config = await collection_mgr.build_rag_config("rag_build_test")
        assert rag_config is not None
        assert rag_config.collection == "rag_build_test"
        assert rag_config.chunk_size == 800
        assert rag_config.top_k == 3

    async def test_build_rag_config_nonexistent_raises(
        self, collection_mgr: CollectionManager
    ) -> None:
        """存在しないコレクションの RAGConfig 構築で ValueError を送出."""
        with pytest.raises(ValueError, match="not found"):
            await collection_mgr.build_rag_config("nonexistent")

    @pytest.mark.parametrize(
        ("reranker_name", "expected_type"),
        [
            ("cross_encoder_ruri", RerankerType.CROSS_ENCODER_RURI),
            ("llm_listwise", RerankerType.LLM_LISTWISE),
        ],
    )
    async def test_build_rag_config_supports_new_reranker_types(
        self,
        collection_mgr: CollectionManager,
        reranker_name: str,
        expected_type: RerankerType,
    ) -> None:
        """新規リランカー種別を RAGConfig へ正しくマッピングする."""
        collection_name = f"reranker_{reranker_name}"
        await collection_mgr.create_collection(
            collection_name=collection_name,
            app_name="faq_system",
            reranker=reranker_name,
        )

        rag_config = await collection_mgr.build_rag_config(collection_name)
        assert rag_config.reranker == expected_type


class TestCollectionManagerAccessControl:
    """アクセス制御統合テスト."""

    async def test_resolve_accessible_collections_fallback(
        self, collection_mgr: CollectionManager
    ) -> None:
        """auth_service 未接続時のフォールバック検証."""
        await collection_mgr.create_collection(
            collection_name="internal_kb",
            app_name="faq_system",
        )
        await collection_mgr.create_collection(
            collection_name="external_kb",
            app_name="faq_system",
        )
        await collection_mgr.create_collection(
            collection_name="confidential_kb",
            app_name="faq_system",
        )

        # admin は全てアクセス可
        admin_collections = await collection_mgr.resolve_accessible_collections(
            role="admin",
            app_name="faq_system",
        )
        assert len(admin_collections) == 3

        # guest は external のみ
        guest_collections = await collection_mgr.resolve_accessible_collections(
            role="guest",
            app_name="faq_system",
        )
        assert len(guest_collections) == 1
        assert guest_collections[0].collection_name == "external_kb"

    async def test_provision_tenant(self, collection_mgr: CollectionManager) -> None:
        """テナントプロビジョニングの検証."""
        collections = await collection_mgr.provision_tenant(
            app_name="faq_system",
            tenant_id="new_tenant",
        )
        # デフォルトテンプレートで 3 つ作成される
        assert len(collections) == 3
        names = {c.collection_name for c in collections}
        assert "faq_system__new_tenant__internal" in names
        assert "faq_system__new_tenant__external" in names
        assert "faq_system__new_tenant__confidential" in names


# ---------------------------------------------------------------------------
# DocumentManager テスト
# ---------------------------------------------------------------------------


class TestDocumentManagerUpload:
    """ドキュメントアップロード・管理のテスト."""

    async def test_upload_document(
        self,
        collection_mgr: CollectionManager,
        document_mgr: DocumentManager,
    ) -> None:
        """ドキュメントアップロードの検証."""
        await collection_mgr.create_collection(
            collection_name="upload_test",
            app_name="faq_system",
        )
        doc = await document_mgr.upload_document(
            collection_name="upload_test",
            file_content=b"This is test content for document upload.",
            filename="test.txt",
            user_id="user_1",
        )
        assert doc is not None
        assert doc.filename == "test.txt"
        assert doc.status == DocumentStatus.UPLOADED
        assert doc.content_hash is not None

    async def test_upload_to_nonexistent_collection_raises(
        self, document_mgr: DocumentManager
    ) -> None:
        """存在しないコレクションへのアップロードで ValueError を送出."""
        with pytest.raises(ValueError, match="not found"):
            await document_mgr.upload_document(
                collection_name="nonexistent",
                file_content=b"test content",
                filename="test.txt",
                user_id="user_1",
            )

    async def test_list_documents(
        self,
        collection_mgr: CollectionManager,
        document_mgr: DocumentManager,
    ) -> None:
        """ドキュメント一覧取得の検証."""
        await collection_mgr.create_collection(
            collection_name="list_docs",
            app_name="faq_system",
        )
        await document_mgr.upload_document(
            collection_name="list_docs",
            file_content=b"Doc 1 content",
            filename="doc1.txt",
            user_id="user_1",
        )
        await document_mgr.upload_document(
            collection_name="list_docs",
            file_content=b"Doc 2 content",
            filename="doc2.txt",
            user_id="user_1",
        )

        docs = await document_mgr.list_documents(collection_name="list_docs")
        assert len(docs) == 2

    async def test_get_document(
        self,
        collection_mgr: CollectionManager,
        document_mgr: DocumentManager,
    ) -> None:
        """ドキュメント詳細取得の検証."""
        await collection_mgr.create_collection(
            collection_name="get_doc",
            app_name="faq_system",
        )
        uploaded = await document_mgr.upload_document(
            collection_name="get_doc",
            file_content=b"Detail test content",
            filename="detail.txt",
            user_id="user_1",
        )
        doc = await document_mgr.get_document(uploaded.document_id)
        assert doc is not None
        assert doc.filename == "detail.txt"

    async def test_get_document_not_found(self, document_mgr: DocumentManager) -> None:
        """存在しないドキュメント取得で None を返す."""
        result = await document_mgr.get_document("nonexistent-id")
        assert result is None

    async def test_delete_document(
        self,
        collection_mgr: CollectionManager,
        document_mgr: DocumentManager,
    ) -> None:
        """ドキュメント削除の検証."""
        await collection_mgr.create_collection(
            collection_name="del_doc",
            app_name="faq_system",
        )
        uploaded = await document_mgr.upload_document(
            collection_name="del_doc",
            file_content=b"Delete me",
            filename="delete.txt",
            user_id="user_1",
        )
        await document_mgr.delete_document(uploaded.document_id)
        result = await document_mgr.get_document(uploaded.document_id)
        assert result is None

    async def test_delete_nonexistent_raises(self, document_mgr: DocumentManager) -> None:
        """存在しないドキュメント削除で ValueError を送出."""
        with pytest.raises(ValueError, match="not found"):
            await document_mgr.delete_document("nonexistent-id")


class TestDocumentManagerChunking:
    """チャンクプレビュー・インデックスのテスト."""

    async def test_preview_chunks(
        self,
        collection_mgr: CollectionManager,
        document_mgr: DocumentManager,
    ) -> None:
        """チャンクプレビューの検証."""
        await collection_mgr.create_collection(
            collection_name="chunk_preview",
            app_name="faq_system",
            chunk_strategy="fixed",
            chunk_size=20,
            chunk_overlap=5,
        )
        doc = await document_mgr.upload_document(
            collection_name="chunk_preview",
            file_content=b"A" * 100,
            filename="large.txt",
            user_id="user_1",
        )
        chunks = await document_mgr.preview_chunks(doc.document_id)
        assert len(chunks) > 0
        assert "content" in chunks[0]
        assert "index" in chunks[0]

    async def test_preview_chunks_custom_settings(
        self,
        collection_mgr: CollectionManager,
        document_mgr: DocumentManager,
    ) -> None:
        """カスタム設定でのチャンクプレビュー検証."""
        await collection_mgr.create_collection(
            collection_name="chunk_custom",
            app_name="faq_system",
        )
        doc = await document_mgr.upload_document(
            collection_name="chunk_custom",
            file_content=b"B" * 200,
            filename="custom.txt",
            user_id="user_1",
        )
        chunks = await document_mgr.preview_chunks(
            document_id=doc.document_id,
            chunk_strategy="fixed",
            chunk_size=50,
            chunk_overlap=10,
        )
        assert len(chunks) > 0


class TestDocumentManagerDuplicate:
    """重複検出のテスト."""

    async def test_duplicate_content_detection(
        self,
        collection_mgr: CollectionManager,
        document_mgr: DocumentManager,
    ) -> None:
        """同一コンテンツの重複検出の検証."""
        await collection_mgr.create_collection(
            collection_name="dup_detect",
            app_name="faq_system",
        )
        content = b"Identical content for dedup test"
        doc1 = await document_mgr.upload_document(
            collection_name="dup_detect",
            file_content=content,
            filename="first.txt",
            user_id="user_1",
        )
        doc2 = await document_mgr.upload_document(
            collection_name="dup_detect",
            file_content=content,
            filename="second.txt",
            user_id="user_1",
        )
        # 同一ハッシュだが別ドキュメントとして登録可能
        assert doc1.content_hash == doc2.content_hash
        assert doc1.document_id != doc2.document_id
