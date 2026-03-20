"""FAQ System コレクション管理ルーターのテスト.

CollectionManager / DocumentManager を利用した
コレクション CRUD・ドキュメント管理 API のユニットテスト。
"""

from __future__ import annotations

import asyncio
from contextlib import asynccontextmanager
from typing import Any, AsyncGenerator
from unittest.mock import AsyncMock, patch

import pytest
from sqlalchemy import StaticPool
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker, create_async_engine

from shared.rag.collection_manager import CollectionManager
from shared.rag.document_manager import DocumentManager
from shared.rag.models import Base as RAGBase
from shared.services.base import ServiceResult


# ---------------------------------------------------------------------------
# フィクスチャ: インメモリ DB
# ---------------------------------------------------------------------------


@pytest.fixture()
def event_loop():
    """イベントループを提供."""
    loop = asyncio.new_event_loop()
    yield loop
    loop.close()


@pytest.fixture()
async def session_factory() -> async_sessionmaker[AsyncSession]:
    """テスト用インメモリ SQLite セッションファクトリ."""
    engine = create_async_engine(
        "sqlite+aiosqlite://",
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
    )
    async with engine.begin() as conn:
        await conn.run_sync(RAGBase.metadata.create_all)

    factory = async_sessionmaker(engine, expire_on_commit=False)
    yield factory

    async with engine.begin() as conn:
        await conn.run_sync(RAGBase.metadata.drop_all)
    await engine.dispose()


@pytest.fixture()
async def collection_manager(
    session_factory: async_sessionmaker[AsyncSession],
) -> CollectionManager:
    """CollectionManager インスタンス."""
    return CollectionManager(session_factory=session_factory)


@pytest.fixture()
async def document_manager(
    collection_manager: CollectionManager,
    session_factory: async_sessionmaker[AsyncSession],
) -> DocumentManager:
    """DocumentManager インスタンス."""
    return DocumentManager(
        collection_manager=collection_manager,
        session_factory=session_factory,
    )


@pytest.fixture()
async def sample_collection(
    collection_manager: CollectionManager,
) -> dict[str, Any]:
    """テスト用コレクションを作成."""
    model = await collection_manager.create_collection(
        collection_name="test_collection",
        app_name="faq_system",
        display_name="テスト KB",
        description="テスト用コレクション",
        chunk_strategy="sentence",
        chunk_size=500,
        chunk_overlap=80,
        retrieval_method="hybrid",
        reranker="cohere",
        top_k=8,
    )
    return model.to_dict()


# ---------------------------------------------------------------------------
# コレクション CRUD テスト
# ---------------------------------------------------------------------------


class TestCollectionCRUD:
    """コレクション CRUD エンドポイントのテスト."""

    @pytest.mark.asyncio()
    async def test_create_collection(
        self,
        collection_manager: CollectionManager,
    ) -> None:
        """コレクション作成."""
        model = await collection_manager.create_collection(
            collection_name="new_collection",
            app_name="faq_system",
            display_name="新規 KB",
            chunk_strategy="recursive",
        )
        assert model.collection_name == "new_collection"
        assert model.app_name == "faq_system"
        assert model.chunk_strategy == "recursive"

    @pytest.mark.asyncio()
    async def test_create_duplicate_collection_fails(
        self,
        collection_manager: CollectionManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """重複コレクション作成は失敗."""
        with pytest.raises(ValueError, match="already exists"):
            await collection_manager.create_collection(
                collection_name="test_collection",
                app_name="faq_system",
            )

    @pytest.mark.asyncio()
    async def test_list_collections(
        self,
        collection_manager: CollectionManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """コレクション一覧取得."""
        collections = await collection_manager.list_collections(app_name="faq_system")
        assert len(collections) == 1
        assert collections[0].collection_name == "test_collection"

    @pytest.mark.asyncio()
    async def test_get_collection(
        self,
        collection_manager: CollectionManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """コレクション詳細取得."""
        model = await collection_manager.get_collection("test_collection")
        assert model is not None
        assert model.display_name == "テスト KB"
        assert model.chunk_size == 500

    @pytest.mark.asyncio()
    async def test_update_collection(
        self,
        collection_manager: CollectionManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """コレクション設定更新."""
        updated = await collection_manager.update_collection(
            "test_collection",
            {"chunk_size": 1000, "top_k": 10},
        )
        assert updated.chunk_size == 1000
        assert updated.top_k == 10

    @pytest.mark.asyncio()
    async def test_delete_collection(
        self,
        collection_manager: CollectionManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """コレクション削除."""
        await collection_manager.delete_collection("test_collection")
        model = await collection_manager.get_collection("test_collection")
        assert model is None

    @pytest.mark.asyncio()
    async def test_get_collection_stats(
        self,
        collection_manager: CollectionManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """コレクション統計取得."""
        stats = await collection_manager.get_collection_stats("test_collection")
        assert stats["collection_name"] == "test_collection"
        assert stats["document_count"] == 0


# ---------------------------------------------------------------------------
# ドキュメント管理テスト
# ---------------------------------------------------------------------------


class TestDocumentManagement:
    """ドキュメント管理エンドポイントのテスト."""

    @pytest.mark.asyncio()
    async def test_upload_document(
        self,
        document_manager: DocumentManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """ドキュメントアップロード."""
        doc = await document_manager.upload_document(
            collection_name="test_collection",
            file_content=b"Hello, world!",
            filename="test.txt",
            user_id="admin",
        )
        assert doc.filename == "test.txt"
        assert doc.status == "uploaded"
        assert doc.collection_name == "test_collection"

    @pytest.mark.asyncio()
    async def test_upload_to_nonexistent_collection(
        self,
        document_manager: DocumentManager,
    ) -> None:
        """存在しないコレクションへのアップロードは失敗."""
        with pytest.raises(ValueError, match="not found"):
            await document_manager.upload_document(
                collection_name="nonexistent",
                file_content=b"test",
                filename="test.txt",
            )

    @pytest.mark.asyncio()
    async def test_list_documents(
        self,
        document_manager: DocumentManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """ドキュメント一覧取得."""
        await document_manager.upload_document(
            collection_name="test_collection",
            file_content=b"Doc 1",
            filename="doc1.txt",
        )
        await document_manager.upload_document(
            collection_name="test_collection",
            file_content=b"Doc 2",
            filename="doc2.txt",
        )
        docs = await document_manager.list_documents("test_collection")
        assert len(docs) == 2

    @pytest.mark.asyncio()
    async def test_get_document(
        self,
        document_manager: DocumentManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """ドキュメント詳細取得."""
        uploaded = await document_manager.upload_document(
            collection_name="test_collection",
            file_content=b"content",
            filename="detail.txt",
        )
        doc = await document_manager.get_document(uploaded.document_id)
        assert doc is not None
        assert doc.filename == "detail.txt"

    @pytest.mark.asyncio()
    async def test_preview_chunks(
        self,
        document_manager: DocumentManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """チャンクプレビュー."""
        uploaded = await document_manager.upload_document(
            collection_name="test_collection",
            file_content=b"A" * 2000,
            filename="large.txt",
        )
        chunks = await document_manager.preview_chunks(
            uploaded.document_id,
            chunk_size=500,
            chunk_overlap=50,
        )
        assert len(chunks) > 0
        assert "content" in chunks[0]
        assert "length" in chunks[0]

    @pytest.mark.asyncio()
    async def test_index_document(
        self,
        document_manager: DocumentManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """ドキュメントインデックス."""
        uploaded = await document_manager.upload_document(
            collection_name="test_collection",
            file_content=b"index me with enough content to be indexed properly",
            filename="index.txt",
        )
        mock_result = ServiceResult(
            success=True,
            data={"ids": ["chunk-1"], "count": 1},
        )
        with patch(
            "shared.rag.document_manager.RAGService",
            return_value=AsyncMock(execute=AsyncMock(return_value=mock_result)),
        ):
            indexed = await document_manager.index_document(uploaded.document_id)
        assert indexed.status == "indexed"
        assert indexed.chunk_count >= 1

    @pytest.mark.asyncio()
    async def test_reindex_document(
        self,
        document_manager: DocumentManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """ドキュメント再インデックス."""
        uploaded = await document_manager.upload_document(
            collection_name="test_collection",
            file_content=b"reindex me with enough content for chunking",
            filename="reindex.txt",
        )
        mock_result = ServiceResult(
            success=True,
            data={"ids": ["chunk-1"], "count": 1},
        )
        with patch(
            "shared.rag.document_manager.RAGService",
            return_value=AsyncMock(execute=AsyncMock(return_value=mock_result)),
        ):
            await document_manager.index_document(uploaded.document_id)
            reindexed = await document_manager.reindex_document(uploaded.document_id)
        assert reindexed.status == "indexed"
        assert reindexed.chunk_count >= 1

    @pytest.mark.asyncio()
    async def test_delete_document(
        self,
        document_manager: DocumentManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """ドキュメント削除."""
        uploaded = await document_manager.upload_document(
            collection_name="test_collection",
            file_content=b"delete me",
            filename="delete.txt",
        )
        await document_manager.delete_document(uploaded.document_id)
        doc = await document_manager.get_document(uploaded.document_id)
        assert doc is None

    @pytest.mark.asyncio()
    async def test_reindex_collection(
        self,
        document_manager: DocumentManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """コレクション全体の再インデックス."""
        mock_result = ServiceResult(
            success=True,
            data={"ids": ["chunk-1"], "count": 1},
        )
        with patch(
            "shared.rag.document_manager.RAGService",
            return_value=AsyncMock(execute=AsyncMock(return_value=mock_result)),
        ):
            for i in range(3):
                uploaded = await document_manager.upload_document(
                    collection_name="test_collection",
                    file_content=f"document content number {i} with enough text".encode(),
                    filename=f"doc{i}.txt",
                )
                await document_manager.index_document(uploaded.document_id)
            result = await document_manager.reindex_collection("test_collection")
        assert result["total"] == 3
        assert result["reindexed"] == 3
        assert result["errors"] == 0


# ---------------------------------------------------------------------------
# テストクエリ / アクセス制御テスト
# ---------------------------------------------------------------------------


class TestCollectionAccess:
    """コレクションアクセス制御テスト."""

    @pytest.mark.asyncio()
    async def test_resolve_accessible_collections(
        self,
        collection_manager: CollectionManager,
    ) -> None:
        """ロールベースのコレクションアクセス制御."""
        await collection_manager.create_collection(
            collection_name="faq__t1__internal",
            app_name="faq_system",
            tenant_id="t1",
        )
        await collection_manager.create_collection(
            collection_name="faq__t1__external",
            app_name="faq_system",
            tenant_id="t1",
        )
        await collection_manager.create_collection(
            collection_name="faq__t1__confidential",
            app_name="faq_system",
            tenant_id="t1",
        )

        # admin: 全アクセス
        admin_cols = await collection_manager.resolve_accessible_collections(
            role="admin", app_name="faq_system", tenant_id="t1"
        )
        assert len(admin_cols) == 3

        # guest: external のみ
        guest_cols = await collection_manager.resolve_accessible_collections(
            role="guest", app_name="faq_system", tenant_id="t1"
        )
        assert len(guest_cols) == 1
        assert "external" in guest_cols[0].collection_name

    @pytest.mark.asyncio()
    async def test_build_rag_config(
        self,
        collection_manager: CollectionManager,
        sample_collection: dict[str, Any],
    ) -> None:
        """RAGConfig 構築."""
        config = await collection_manager.build_rag_config("test_collection")
        assert config.collection == "test_collection"
        assert config.top_k == 8
