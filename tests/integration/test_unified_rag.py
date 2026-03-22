"""UnifiedRAGService 統合テスト.

MockProvider を明示注入し、外部サービスなしでフロー全体を検証する。
"""

import asyncio
from unittest.mock import AsyncMock

import pytest

from shared.services.unified_rag import UnifiedRAGService


@pytest.fixture
def mock_embedding() -> AsyncMock:
    """モック Embedding プロバイダ."""
    emb = AsyncMock()
    emb.embed_documents = AsyncMock(return_value=[[0.1, 0.2], [0.3, 0.4]])
    emb.embed_query = AsyncMock(return_value=[0.1, 0.2])
    return emb


@pytest.fixture
def mock_vectordb() -> AsyncMock:
    """モック VectorDB プロバイダ."""
    vdb = AsyncMock()
    vdb.connect = AsyncMock()
    vdb.add_documents = AsyncMock(return_value=["id-1", "id-2"])
    vdb.similarity_search = AsyncMock(
        return_value=[
            {"document": "AgentFlow is a lightweight AI framework.", "score": 0.95},
            {"document": "It supports MCP and A2A protocols.", "score": 0.80},
        ]
    )
    vdb.disconnect = AsyncMock()
    return vdb


@pytest.fixture
def mock_llm() -> AsyncMock:
    """モック LLM プロバイダ."""
    llm = AsyncMock()
    llm.chat = AsyncMock(return_value={"content": "AgentFlow is a lightweight AI agent framework."})
    return llm


@pytest.mark.asyncio
async def test_unified_rag_service_flow(
    mock_embedding: AsyncMock,
    mock_vectordb: AsyncMock,
    mock_llm: AsyncMock,
) -> None:
    """UnifiedRAGService の基本フロー（追加→検索→クエリ）."""
    rag = UnifiedRAGService(
        collection_name="test_collection",
        embedding_provider=mock_embedding,
        vector_db=mock_vectordb,
        llm=mock_llm,
    )

    # ドキュメント追加
    docs = [
        "AgentFlow is a lightweight AI framework.",
        "It supports MCP and A2A protocols.",
    ]
    ids = await rag.add_documents(docs)
    assert len(ids) == 2
    mock_embedding.embed_documents.assert_called_once_with(docs)
    mock_vectordb.add_documents.assert_called_once()

    # 検索
    results = await rag.retrieve("What is AgentFlow?")
    assert len(results) > 0
    assert any("AgentFlow" in r["document"] for r in results)

    # クエリ（検索+生成）
    answer = await rag.query("Tell me about AgentFlow.")
    assert isinstance(answer, str)
    assert len(answer) > 0

    await rag.close()
    mock_vectordb.disconnect.assert_called_once()


if __name__ == "__main__":
    asyncio.run(test_unified_rag_service_flow())
