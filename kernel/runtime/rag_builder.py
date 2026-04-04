"""Runtime RAG builder from canonical contracts.rag payloads."""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from kernel.runtime.rag_runtime import RAGRuntime


logger = logging.getLogger(__name__)

RagPayload = dict[str, object]


class RagBootstrapConfig(BaseModel):
    """Normalized runtime RAG bootstrap configuration."""

    enabled: bool = Field(default=False)
    provider: str | None = Field(default=None)
    collections: list[str] = Field(default_factory=list)
    chunk_strategy: str = Field(default="recursive")
    chunk_size: int = Field(default=800)
    chunk_overlap: int = Field(default=120)
    retrieval_method: str = Field(default="hybrid")
    embedding_model: str | None = Field(default=None)
    rerank_model: str | None = Field(default=None)
    default_top_k: int = Field(default=5)
    score_threshold: float | None = Field(default=None)


class UnifiedRAGRuntimeAdapter:
    """Canonical runtime adapter backed by UnifiedRAGService."""

    def __init__(
        self,
        *,
        service: Any,
        top_k: int,
        score_threshold: float | None,
    ) -> None:
        self._service = service
        self._top_k = top_k
        self._score_threshold = score_threshold

    async def search(
        self,
        query: str,
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        results = await self._service.retrieve(
            query,
            top_k=top_k or self._top_k,
            filter=filters,
        )
        threshold = self._score_threshold
        if threshold is None:
            return list(results)

        filtered: list[dict[str, Any]] = []
        for result in results:
            similarity = _resolve_similarity(result)
            if similarity >= threshold:
                normalized = dict(result)
                normalized["score"] = similarity
                normalized.setdefault("distance", max(0.0, 1.0 - similarity))
                filtered.append(normalized)
        return filtered

    async def query(
        self,
        query: str,
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
    ) -> str:
        return await self._service.query(
            query,
            top_k=top_k or self._top_k,
            filter=filters,
        )

    async def close(self) -> None:
        await self._service.close()


async def build_rag_engine(rag_config: RagPayload | None) -> RAGRuntime | None:
    """Build a runtime RAG pipeline from canonical or legacy payloads."""
    normalized = _normalize_rag_payload(rag_config)
    if normalized is None:
        logger.debug("RAG settings not configured; runtime RAG disabled")
        return None

    config = RagBootstrapConfig.model_validate(normalized)
    if not config.enabled:
        logger.debug("RAG explicitly disabled by manifest")
        return None

    try:
        from shared.services.unified_rag import RAGPattern, UnifiedRAGService

        collection_name = config.collections[0] if config.collections else "default"
        retrieval_method = (config.retrieval_method or "hybrid").strip().lower()
        pattern = {
            "basic": RAGPattern.BASIC,
            "hybrid": RAGPattern.HYBRID,
            "advanced": RAGPattern.ADVANCED,
        }.get(retrieval_method, RAGPattern.HYBRID)
        service = UnifiedRAGService(
            collection_name=collection_name,
            pattern=pattern,
        )
        runtime = UnifiedRAGRuntimeAdapter(
            service=service,
            top_k=config.default_top_k,
            score_threshold=config.score_threshold,
        )
        logger.info(
            "Runtime RAG adapter ready: collection=%s top_k=%d pattern=%s",
            collection_name,
            config.default_top_k,
            pattern.value,
        )
        return runtime
    except Exception as exc:
        logger.warning("Runtime RAG bootstrap failed: %s", exc, exc_info=True)
        return None


def _resolve_similarity(result: dict[str, Any]) -> float:
    raw_score = result.get("score")
    if isinstance(raw_score, int | float):
        return float(raw_score)
    return 1.0 - float(result.get("distance", 1.0))


def _normalize_rag_payload(rag_config: RagPayload | None) -> RagPayload | None:
    """Normalize contracts.rag or legacy rag_config payloads."""
    if rag_config is None:
        return None
    if "collections" in rag_config or "default_top_k" in rag_config:
        return rag_config

    enabled = bool(rag_config.get("enabled", False))
    collection = rag_config.get("vector_collection")
    collections: list[str] = []
    if isinstance(collection, str) and collection.strip():
        collections = [collection.strip()]

    return {
        "enabled": enabled,
        "pattern": rag_config.get("pattern"),
        "provider": rag_config.get("vector_provider") if enabled else None,
        "collections": collections if enabled else [],
        "data_sources": rag_config.get("data_sources", []),
        "chunk_strategy": rag_config.get("chunk_strategy", "recursive"),
        "chunk_size": rag_config.get("chunk_size", 800),
        "chunk_overlap": rag_config.get("chunk_overlap", 120),
        "retrieval_method": rag_config.get("retrieval_method", "hybrid"),
        "embedding_model": rag_config.get("embedding_model"),
        "rerank_model": rag_config.get("reranker"),
        "default_top_k": rag_config.get("top_k", 5),
        "score_threshold": rag_config.get("score_threshold"),
        "indexing_schedule": rag_config.get("indexing_schedule"),
    }


__all__ = ["RagBootstrapConfig", "UnifiedRAGRuntimeAdapter", "_normalize_rag_payload", "build_rag_engine"]
