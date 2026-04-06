"""Collection test-query service の挙動テスト."""

from __future__ import annotations

from typing import Any
from unittest.mock import AsyncMock

import pytest

from apps.faq_system.backend.mcp.backends.base import (
    RetrievalResult,
    RetrievedDocument,
)
from shared.services.base import ServiceResult
from shared.services.rag_service import ChunkStrategy, RAGConfig, RerankerType


class _FakeBackend:
    """固定結果を返す簡易 backend."""

    def __init__(self, name: str, documents: list[RetrievedDocument]) -> None:
        self.name = name
        self._documents = documents

    async def initialize(self) -> None:
        return None

    async def cleanup(self) -> None:
        return None

    async def retrieve(self, query: Any) -> RetrievalResult:
        return RetrievalResult(
            query=query.query,
            documents=list(self._documents),
            total_found=len(self._documents),
            metadata={"backend": self.name},
        )


class _FakeAnswerService:
    """回答生成だけ差し替える簡易 RAGService."""

    def __init__(self, answer: str) -> None:
        self._answer = answer
        self.start = AsyncMock(return_value=None)

    def _build_context(self, documents: list[Any]) -> str:
        return "\n".join(str(doc.content) for doc in documents)

    async def _generate_answer(self, question: str, context: str) -> str:
        return self._answer


@pytest.mark.asyncio
async def test_hybrid_query_uses_preview_fallback_and_returns_diagnostics() -> None:
    """hybrid では preview backend のヒットを採用し diagnostics に反映する."""
    from apps.faq_system.backend.services.collection_test_query_service import (
        CollectionTestQueryService,
    )

    rag_config = RAGConfig(
        collection="faq-test",
        chunk_strategy=ChunkStrategy.SENTENCE,
        chunk_size=500,
        chunk_overlap=80,
        reranker=RerankerType.BM25,
        top_k=8,
        min_similarity=0.15,
        retrieval_method="hybrid",
    )

    preview_doc = RetrievedDocument(
        doc_id="doc-faq",
        content="Use the latest update notice by effective date.",
        score=0.91,
        source="travel_faq_exceptions.docx",
        metadata={
            "document_id": "doc-faq",
            "document_group_id": "group-1",
            "retrieval_backend": "preview",
        },
    )

    service = CollectionTestQueryService(
        collection_name="faq-test",
        rag_config=rag_config,
        document_manager=AsyncMock(),
        vector_backend_factory=lambda: _FakeBackend("vector:faq-test", []),
        preview_backend_factory=lambda: _FakeBackend("preview:faq-test", [preview_doc]),
        rag_service_factory=lambda config: _FakeAnswerService(
            "Follow the latest update notice by effective date."
        ),
    )

    result = await service.run_test_query(
        query="Which document should win when the policy conflicts?",
        top_k=8,
        expand_related=False,
    )

    assert [doc["source"] for doc in result["documents"]] == [
        "travel_faq_exceptions.docx",
    ]
    assert result["diagnostics"]["retrieval_method"] == "hybrid"
    assert result["diagnostics"]["used_lexical_fallback"] is True
    assert result["diagnostics"]["answer_mode"] == "policy_conflict_guided"


@pytest.mark.asyncio
async def test_conflict_query_postprocesses_answer_with_effective_date() -> None:
    """競合解決系質問では latest update notice と effective date を answer に残す."""
    from apps.faq_system.backend.services.collection_test_query_service import (
        CollectionTestQueryService,
    )

    rag_config = RAGConfig(
        collection="faq-test",
        chunk_strategy=ChunkStrategy.SENTENCE,
        chunk_size=500,
        chunk_overlap=80,
        reranker=RerankerType.BM25,
        top_k=8,
        min_similarity=0.15,
        retrieval_method="hybrid",
    )

    vector_docs = [
        RetrievedDocument(
            doc_id="doc-policy",
            content="When a later update notice conflicts with this policy, the latest effective notice wins.",
            score=0.95,
            source="travel_policy_official_2025.pdf",
            metadata={
                "document_id": "doc-policy",
                "document_group_id": "group-1",
                "retrieval_backend": "vector",
            },
        ),
        RetrievedDocument(
            doc_id="doc-update",
            content="Travel Policy Update Notice 2026-04\nEffective date: 2026-04-01",
            score=0.93,
            source="travel_policy_update_notice_2026.txt",
            metadata={
                "document_id": "doc-update",
                "document_group_id": "group-1",
                "retrieval_backend": "vector",
            },
        ),
    ]

    fake_doc_manager = AsyncMock()
    fake_doc_manager.list_documents = AsyncMock(return_value=[])

    service = CollectionTestQueryService(
        collection_name="faq-test",
        rag_config=rag_config,
        document_manager=fake_doc_manager,
        vector_backend_factory=lambda: _FakeBackend("vector:faq-test", vector_docs),
        preview_backend_factory=lambda: _FakeBackend("preview:faq-test", []),
        rag_service_factory=lambda config: _FakeAnswerService(
            "Follow the update notice when the policy conflicts."
        ),
    )

    result = await service.run_test_query(
        query="When the official policy conflicts with a later update notice, which document should be followed?",
        top_k=8,
        expand_related=False,
    )

    assert "latest update notice" in result["answer"].lower()
    assert "effective date" in result["answer"].lower()
    assert result["diagnostics"]["answer_mode"] == "policy_conflict_guided"
