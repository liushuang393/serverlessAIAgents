"""Reranker core and integration tests."""

from __future__ import annotations

from typing import Any

import pytest

from agentflow.knowledge.reranker import (
    BaseReranker,
    BM25Reranker,
    CrossEncoderRuriReranker,
    LLMListwiseReranker,
    RankedDocument,
    RerankerType,
    get_reranker,
    list_registered_rerankers,
    register_reranker,
    unregister_reranker,
)
from agentflow.services.rag_service import RAGConfig, RAGService


def _build_docs(count: int) -> list[dict[str, Any]]:
    return [
        {
            "id": f"doc-{index}",
            "content": f"Document {index} content for ranking and retrieval.",
            "topic": "test",
        }
        for index in range(count)
    ]


class TestLLMListwiseReranker:
    @pytest.mark.asyncio
    async def test_llm_listwise_basic_ordering(self, monkeypatch: pytest.MonkeyPatch) -> None:
        async def fake_call(self: LLMListwiseReranker, *, prompt: str, model: str) -> str:
            del self, prompt, model
            return '{"ranked_ids": ["doc-2", "doc-0", "doc-1"]}'

        monkeypatch.setattr(LLMListwiseReranker, "_call_llm", fake_call)

        reranker = LLMListwiseReranker(
            options={
                "candidate_pool_size": 20,
                "enable_partial_filtering": False,
                "enable_early_stop": False,
            }
        )
        result = await reranker.rerank("query", _build_docs(3), top_k=2)

        assert [item.original_index for item in result] == [2, 0]

    @pytest.mark.asyncio
    async def test_partial_filtering_rescue_path(self, monkeypatch: pytest.MonkeyPatch) -> None:
        async def fake_call(self: LLMListwiseReranker, *, prompt: str, model: str) -> str:
            del self, model
            if "phase: partial" in prompt:
                return '{"ranked_ids": ["doc-8", "doc-9"]}'
            return '{"ranked_ids": ["doc-8", "doc-9", "doc-0", "doc-1"]}'

        monkeypatch.setattr(LLMListwiseReranker, "_call_llm", fake_call)

        reranker = LLMListwiseReranker(
            options={
                "candidate_pool_size": 10,
                "partial_keep_n": 6,
                "partial_rescue_k": 2,
                "enable_partial_filtering": True,
                "enable_early_stop": False,
            }
        )

        result = await reranker.rerank("query", _build_docs(10), top_k=4)
        assert [item.original_index for item in result[:2]] == [8, 9]

    @pytest.mark.asyncio
    async def test_adaptive_early_stop_breaks_early(self, monkeypatch: pytest.MonkeyPatch) -> None:
        stop_calls: list[str] = []

        async def fake_call(self: LLMListwiseReranker, *, prompt: str, model: str) -> str:
            del self, model
            if "include_stop_signal=true" in prompt:
                stop_calls.append(prompt)
                return '{"ranked_ids": ["doc-0", "doc-1", "doc-2"], "sufficient": true}'
            return '{"ranked_ids": ["doc-0", "doc-1", "doc-2"]}'

        monkeypatch.setattr(LLMListwiseReranker, "_call_llm", fake_call)

        reranker = LLMListwiseReranker(
            options={
                "candidate_pool_size": 9,
                "enable_partial_filtering": False,
                "enable_early_stop": True,
                "early_stop_blocks": 3,
            }
        )

        result = await reranker.rerank("query", _build_docs(9), top_k=3)
        assert len(result) == 3
        assert len(stop_calls) == 1

    @pytest.mark.asyncio
    async def test_combined_partial_and_early_stop(self, monkeypatch: pytest.MonkeyPatch) -> None:
        seen_partial = False
        seen_early_stop = False

        async def fake_call(self: LLMListwiseReranker, *, prompt: str, model: str) -> str:
            nonlocal seen_partial, seen_early_stop
            del self, model
            if "phase: partial" in prompt:
                seen_partial = True
                return '{"ranked_ids": ["doc-8", "doc-7"]}'
            if "include_stop_signal=true" in prompt:
                seen_early_stop = True
                return '{"ranked_ids": ["doc-8", "doc-7", "doc-0"], "sufficient": true}'
            return '{"ranked_ids": ["doc-8", "doc-7", "doc-0"]}'

        monkeypatch.setattr(LLMListwiseReranker, "_call_llm", fake_call)

        reranker = LLMListwiseReranker(
            options={
                "candidate_pool_size": 10,
                "partial_keep_n": 6,
                "partial_rescue_k": 2,
                "enable_partial_filtering": True,
                "enable_early_stop": True,
                "early_stop_blocks": 2,
            }
        )

        result = await reranker.rerank("query", _build_docs(10), top_k=3)
        assert len(result) == 3
        assert seen_partial is True
        assert seen_early_stop is True

    @pytest.mark.asyncio
    async def test_llm_reranker_fallback_to_bm25_on_parse_error(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        async def fake_call(self: LLMListwiseReranker, *, prompt: str, model: str) -> str:
            del self, prompt, model
            return "invalid_response_without_ids"

        bm25_called = {"count": 0}

        async def fake_bm25_rerank(
            self: BM25Reranker,
            query: str,
            documents: list[str | dict[str, Any]],
            top_k: int = 5,
        ) -> list[RankedDocument]:
            del self, query, documents, top_k
            bm25_called["count"] += 1
            return [
                RankedDocument(content="fallback-1", score=0.9, original_index=1, metadata={}),
                RankedDocument(content="fallback-0", score=0.8, original_index=0, metadata={}),
            ]

        monkeypatch.setattr(LLMListwiseReranker, "_call_llm", fake_call)
        monkeypatch.setattr(BM25Reranker, "rerank", fake_bm25_rerank)

        reranker = LLMListwiseReranker(
            options={
                "candidate_pool_size": 10,
                "enable_partial_filtering": False,
                "enable_early_stop": False,
            }
        )
        result = await reranker.rerank("query", _build_docs(3), top_k=2)

        assert bm25_called["count"] == 1
        assert [item.original_index for item in result] == [1, 0]


class TestRerankerRegistry:
    @pytest.mark.asyncio
    async def test_registry_register_unregister_custom_reranker(self) -> None:
        class CustomReranker(BaseReranker):
            @property
            def reranker_type(self) -> RerankerType:
                return RerankerType.BM25

            async def rerank(
                self,
                query: str,
                documents: list[str | dict[str, Any]],
                top_k: int = 5,
            ) -> list[RankedDocument]:
                del query, documents, top_k
                return []

        unregister_reranker("unit_custom")
        register_reranker("unit_custom", lambda options: CustomReranker())

        assert "unit_custom" in list_registered_rerankers()
        reranker = get_reranker("unit_custom")
        assert isinstance(reranker, CustomReranker)

        unregister_reranker("unit_custom")
        assert "unit_custom" not in list_registered_rerankers()

    def test_cross_encoder_ruri_default_model_name(self, monkeypatch: pytest.MonkeyPatch) -> None:
        monkeypatch.delenv("RERANKER_CROSS_ENCODER_RURI_MODEL", raising=False)
        reranker = CrossEncoderRuriReranker()
        assert reranker._model_name == "cl-nagoya/ruri-v3-reranker-310m"


class _EmbeddingStub:
    async def embed_text(self, query: str) -> list[float]:
        del query
        return [0.1, 0.2, 0.3]


class _VectorDBStub:
    def __init__(self) -> None:
        self.last_top_k: int | None = None

    async def search(
        self,
        *,
        query: str,
        query_embedding: list[float],
        top_k: int,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        del query, query_embedding, filter_metadata
        self.last_top_k = top_k
        return [
            {
                "id": f"id-{idx}",
                "document": f"doc-{idx}",
                "distance": 0.1,
                "metadata": {"source": "unit-test"},
            }
            for idx in range(8)
        ]


class _RerankerStub:
    def recommended_search_k(self, top_k: int) -> int:
        del top_k
        return 17

    async def rerank(
        self,
        query: str,
        documents: list[str | dict[str, Any]],
        top_k: int = 5,
    ) -> list[RankedDocument]:
        del query, documents
        return [
            RankedDocument(content=f"doc-{idx}", score=1.0 - idx * 0.1, original_index=idx, metadata={})
            for idx in range(top_k)
        ]


class TestRAGServiceIntegration:
    @pytest.mark.asyncio
    async def test_rag_service_uses_recommended_search_k(self) -> None:
        service = RAGService(RAGConfig(top_k=3))
        vector_stub = _VectorDBStub()

        service._embedding = _EmbeddingStub()
        service._vectordb = vector_stub
        service._reranker = _RerankerStub()

        documents = await service._search_internal("test query")

        assert vector_stub.last_top_k == 17
        assert len(documents) == 3
