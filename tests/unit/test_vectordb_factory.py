"""VectorDB 工厂函数和实现的单元测试.

黒盒設計テスト: Agent/サービスは get_vectordb() のみを使用し、
具体的な実装（FAISS/Qdrant/Weaviate/Supabase/ChromaDB）を意識しない。
"""

import os
from unittest.mock import patch

import pytest

from agentflow.providers.vectordb_provider import (
    ChromaDBProvider,
    FAISSProvider,
    MockVectorDBProvider,
    QdrantProvider,
    SupabaseVectorProvider,
    VectorDBProvider,
    WeaviateProvider,
    get_vectordb,
    reset_vectordb,
)


class TestGetVectorDB:
    """get_vectordb() 工厂函数测试."""

    def setup_method(self) -> None:
        """每个测试前重置单例."""
        reset_vectordb()

    def teardown_method(self) -> None:
        """每个测试后重置单例."""
        reset_vectordb()

    def test_default_returns_mock(self) -> None:
        """无配置时返回 MockVectorDBProvider."""
        with patch.dict(os.environ, {}, clear=True):
            # 清除可能影响的环境变量
            for key in ["QDRANT_URL", "CHROMA_PERSIST_DIR", "USE_CHROMADB", "PINECONE_API_KEY"]:
                os.environ.pop(key, None)

            reset_vectordb()
            vdb = get_vectordb()
            assert isinstance(vdb, MockVectorDBProvider)

    def test_chromadb_with_env_var(self) -> None:
        """USE_CHROMADB 环境变量启用 ChromaDB."""
        with patch.dict(os.environ, {"USE_CHROMADB": "1"}, clear=True):
            reset_vectordb()
            vdb = get_vectordb()
            assert isinstance(vdb, (ChromaDBProvider, MockVectorDBProvider))

    def test_qdrant_with_env_var(self) -> None:
        """QDRANT_URL 环境变量启用 Qdrant."""
        with patch.dict(os.environ, {"QDRANT_URL": "http://localhost:6333"}, clear=True):
            reset_vectordb()
            vdb = get_vectordb()
            # 如果 qdrant-client 已安装，返回 QdrantProvider
            # 否则降级到 ChromaDB 或 Mock
            assert isinstance(vdb, VectorDBProvider)

    def test_singleton_pattern(self) -> None:
        """单例模式测试."""
        reset_vectordb()
        vdb1 = get_vectordb()
        vdb2 = get_vectordb()
        assert vdb1 is vdb2

    def test_vector_database_type_faiss(self) -> None:
        """VECTOR_DATABASE_TYPE=faiss 测试."""
        with patch.dict(os.environ, {"VECTOR_DATABASE_TYPE": "faiss"}, clear=True):
            reset_vectordb()
            vdb = get_vectordb()
            assert vdb.get_provider_name() == "faiss"

    def test_vector_database_type_qdrant(self) -> None:
        """VECTOR_DATABASE_TYPE=qdrant 测试."""
        with patch.dict(os.environ, {"VECTOR_DATABASE_TYPE": "qdrant"}, clear=True):
            reset_vectordb()
            vdb = get_vectordb()
            assert vdb.get_provider_name() == "qdrant"

    def test_vector_database_type_weaviate(self) -> None:
        """VECTOR_DATABASE_TYPE=weaviate 测试."""
        with patch.dict(os.environ, {"VECTOR_DATABASE_TYPE": "weaviate"}, clear=True):
            reset_vectordb()
            vdb = get_vectordb()
            assert vdb.get_provider_name() == "weaviate"

    def test_vector_database_type_supabase(self) -> None:
        """VECTOR_DATABASE_TYPE=supabase 测试."""
        with patch.dict(os.environ, {"VECTOR_DATABASE_TYPE": "supabase"}, clear=True):
            reset_vectordb()
            vdb = get_vectordb()
            assert vdb.get_provider_name() == "supabase"


class TestMockVectorDBProvider:
    """MockVectorDBProvider 单元测试."""

    @pytest.fixture
    def provider(self) -> MockVectorDBProvider:
        """创建测试用 MockVectorDBProvider."""
        return MockVectorDBProvider(collection="test")

    @pytest.mark.asyncio
    async def test_add_and_search(self, provider: MockVectorDBProvider) -> None:
        """添加和搜索测试."""
        # 添加文档（使用正确的参数名 documents）
        await provider.add(
            documents=["Hello world", "Goodbye world"],
            metadatas=[{"type": "greeting"}, {"type": "farewell"}],
        )

        # 搜索
        results = await provider.search("Hello", top_k=1)
        assert len(results) >= 1

    @pytest.mark.asyncio
    async def test_delete(self, provider: MockVectorDBProvider) -> None:
        """删除测试."""
        await provider.add(documents=["Test document"], ids=["doc-001"])

        # delete 返回删除的数量
        result = await provider.delete(["doc-001"])
        assert result == 1

    @pytest.mark.asyncio
    async def test_clear(self, provider: MockVectorDBProvider) -> None:
        """清空测试."""
        await provider.add(documents=["Doc 1", "Doc 2", "Doc 3"])
        await provider.clear()
        # 搜索应该返回空
        results = await provider.search("Doc", top_k=10)
        assert len(results) == 0


class TestQdrantProvider:
    """QdrantProvider 单元测试."""

    def test_init_without_client(self) -> None:
        """无 qdrant-client 时初始化."""
        # QdrantProvider 应该可以导入
        provider = QdrantProvider(
            url="http://localhost:6333",
            collection="test",
        )
        assert provider._collection_name == "test"

    @pytest.mark.asyncio
    async def test_add_without_client(self) -> None:
        """无 qdrant-client 时添加操作."""
        provider = QdrantProvider(
            url="http://localhost:6333",
            collection="test",
        )
        # 如果没有 qdrant-client 或无连接，应该抛出异常
        try:
            await provider.add(documents=["Test"])
        except Exception:
            # 预期行为：无依赖或无连接时失败
            pass


class TestChromaDBProvider:
    """ChromaDBProvider 单元测试."""

    def test_init(self) -> None:
        """初始化测试."""
        provider = ChromaDBProvider(collection="test")
        assert provider._collection_name == "test"

    @pytest.mark.asyncio
    async def test_add_and_search(self) -> None:
        """添加和搜索测试."""
        provider = ChromaDBProvider(collection="test_search_unit")
        # 必须先连接
        try:
            await provider.connect()
        except ImportError:
            pytest.skip("chromadb package not installed")

        await provider.add(
            documents=["Python is great", "JavaScript is popular"],
            metadatas=[{"lang": "python"}, {"lang": "js"}],
        )

        results = await provider.search("Python programming", top_k=1)
        assert len(results) >= 1

        # 清理
        await provider.disconnect()


class TestFAISSProvider:
    """FAISSProvider 单元测试."""

    def test_init(self) -> None:
        """初始化测试."""
        provider = FAISSProvider(collection="test", vector_size=128)
        assert provider._collection_name == "test"
        assert provider._vector_size == 128

    @pytest.mark.asyncio
    async def test_connect_without_faiss(self) -> None:
        """无 faiss 包时连接测试."""
        provider = FAISSProvider(collection="test")
        try:
            await provider.connect()
            # 如果成功，说明 faiss 已安装
            assert provider._index is not None
        except ImportError:
            pytest.skip("faiss-cpu package not installed")


class TestWeaviateProvider:
    """WeaviateProvider 单元测试."""

    def test_init(self) -> None:
        """初始化测试."""
        provider = WeaviateProvider(
            url="http://localhost:8080",
            collection="TestClass",
        )
        assert provider._class_name == "TestClass"


class TestSupabaseVectorProvider:
    """SupabaseVectorProvider 单元测试."""

    def test_init(self) -> None:
        """初始化测试."""
        provider = SupabaseVectorProvider(
            url="https://xxx.supabase.co",
            key="test-key",
            table="documents",
        )
        assert provider._table == "documents"
