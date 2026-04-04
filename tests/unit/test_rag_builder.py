"""Runtime RAG builder tests."""

from __future__ import annotations

from typing import Any

import pytest

from kernel.runtime.rag_builder import build_rag_engine


class _FakeUnifiedRAGService:
    def __init__(self, collection_name: str = "default", **_: Any) -> None:
        self.collection_name = collection_name
        self.retrieve_calls: list[tuple[str, int, dict[str, Any] | None]] = []
        self.query_calls: list[tuple[str, int, dict[str, Any] | None, str | None]] = []

    async def retrieve(
        self,
        query: str,
        top_k: int = 5,
        filter: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        self.retrieve_calls.append((query, top_k, filter))
        return [{"document": "doc", "score": 0.91}]

    async def query(
        self,
        query: str,
        top_k: int = 5,
        filter: dict[str, Any] | None = None,
        system_prompt: str | None = None,
    ) -> str:
        self.query_calls.append((query, top_k, filter, system_prompt))
        return "answer"

    async def close(self) -> None:
        return None


class _FailingPipeline:
    def __init__(self, *_: Any, **__: Any) -> None:
        msg = "legacy pipeline should not be used"
        raise AssertionError(msg)


@pytest.mark.asyncio
async def test_build_rag_engine_uses_unified_runtime_adapter(monkeypatch: pytest.MonkeyPatch) -> None:
    import shared.rag.rag_pipeline as rag_pipeline_module
    import shared.services.unified_rag as unified_rag_module

    monkeypatch.setattr(rag_pipeline_module, "RAGPipeline", _FailingPipeline)
    monkeypatch.setattr(unified_rag_module, "UnifiedRAGService", _FakeUnifiedRAGService)

    engine = await build_rag_engine(
        {
            "enabled": True,
            "collections": ["faq_docs"],
            "default_top_k": 7,
            "score_threshold": 0.35,
        }
    )

    assert engine is not None
    results = await engine.search("policy")
    answer = await engine.query("policy")

    assert len(results) == 1
    assert results[0]["document"] == "doc"
    assert results[0]["score"] == pytest.approx(0.91)
    assert answer == "answer"
