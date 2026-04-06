"""VectorStoreBackend の引数伝播テスト."""

from __future__ import annotations

from unittest.mock import AsyncMock

import pytest

from apps.faq_system.backend.mcp.backends.base import RetrievalQuery
from apps.faq_system.backend.mcp.backends.vector_store import VectorStoreBackend
from shared.services.base import ServiceResult


@pytest.mark.asyncio
async def test_retrieve_passes_query_and_filters_to_rag_service() -> None:
    """search 実行時に query / filters / top_k を RAGService へそのまま渡す."""
    backend = VectorStoreBackend(collection="faq-test", top_k=5)
    backend._started = True
    backend._rag_service = AsyncMock()
    backend._rag_service.execute = AsyncMock(
        return_value=ServiceResult(success=True, data={"documents": []})
    )

    await backend.retrieve(
        RetrievalQuery(
            query="policy conflict",
            top_k=4,
            filters={"document_group_id": "group-1"},
        )
    )

    backend._rag_service.execute.assert_awaited_once_with(
        action="search",
        query="policy conflict",
        top_k=4,
        filters={"document_group_id": "group-1"},
    )
